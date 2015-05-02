(ns artengine.core
  (:gen-class)
  (:use [artengine util edit selection polygon key mouse draw]
	[seesaw [core :exclude [select action selection] :as s] graphics color chooser]
	[clojure stacktrace set]
        [clojure.java.io :exclude [copy]])
  (:import [java.awt.event KeyEvent MouseEvent]
           [java.awt Frame Dimension]
           [javax.media.opengl.awt GLCanvas]
           [javax.media.opengl GLEventListener GL GLCapabilities GLProfile GLAutoDrawable GLDrawableFactory]
           [com.jogamp.opengl.util GLReadBufferUtil]
           [javax.imageio ImageIO]))

(def FORMAT 2)

(def rstate (ref {:scene {:format FORMAT :objs {} :layers {1 {:stack [] :name "foo" :edit true :view true}} :layers-ord [1]}
                  :selected-layer 1
                  :selection {}
                  :action-start nil
                  :mode :object
                  :action :normal
                  :trans [1 [0 0]]}))

(def undostack (ref ()))
(def redostack (ref ()))

(add-watch rstate :undo
           (fn [_ _ old-state new-state]
             (when (not= (:scene old-state) (:scene new-state))
               (dosync
                (condp = new-state
                  (first @undostack)
                  (do
                    (alter undostack rest)
                    (alter redostack conj old-state))
                  (first @redostack)
                  (do
                    (alter redostack rest)
                    (alter undostack conj old-state))
                  (do
                    (ref-set redostack ())
                    (alter undostack conj old-state)))))))

(def export-scale (ref 1))

(def old-mp (ref [0 0]))

(def file-path (ref nil))

(def dragging (ref false))

(declare can)

(defn save [path]
  (dosync
   (spit path (:scene @rstate))))

(defn open [path state]
   (let [scene (read-string (slurp path))]
     (if (= (:format scene) FORMAT)
       (do
         (dosync
          (ref-set file-path path))
         (assoc state
           :scene scene
           :selection {}))
       (do
         (alert (str "File has format version " (or (:format scene) 1) " this version of Artengine can only open format version " FORMAT))
         state))))

(declare show-export-gui export-gui exp export-stuff)

(defn update-export-gui []
  (let [[foo bla] (export-stuff @rstate @export-scale)]
    (value! (s/select @exp [:#export0]) (first bla))
    (value! (s/select @exp [:#export1]) (second bla))))

(defn export-stuff [{:keys [scene selection]} scale]
  (let [foo (selected-bbox (:objs scene) selection)]
    [foo
     (map int (plus [0.5 0.5] (mult (apply minus (reverse foo)) scale)))]))

(defn export [path]
  (let [[foo bla] (export-stuff @rstate @export-scale)
        fact (GLDrawableFactory/getDesktopFactory)
        pbuffer (.createOffscreenAutoDrawable fact
                                              nil
                                              (GLCapabilities. (GLProfile/getDefault))
                                              nil
                                              (first bla) (second bla))]
    (.addGLEventListener pbuffer (proxy [GLEventListener] []
                                   (init [d])
                                   (reshape [& xs])
                                   (display [^GLAutoDrawable d]
                                     (let [gl (.getGL2 (.getGL d))
                                           read-buffer-util (GLReadBufferUtil. true false)]
                                       (render-raw gl
                                                   (:scene @rstate)
                                                   [@export-scale (mult (first foo) -1)]
                                                   bla)
                                       (.readPixels read-buffer-util gl false)
                                       (.write read-buffer-util (file path))))))
    (.display pbuffer)))

(defmethod kp [KeyEvent/VK_Q :ctrl] [_ _ _]
  (System/exit 0))

(defmethod kp [KeyEvent/VK_S :ctrl] [_ state _]
  (dosync
   (if-not @file-path
     (ref-set file-path (choose-file :type :save)))
   (if @file-path
     (save @file-path)))
  state)

(defmethod kp [KeyEvent/VK_O :ctrl] [_ state _]
  (if-let [path (choose-file)]
    (open path state)
    state))

(defmethod kp [KeyEvent/VK_B :ctrl] [_ state _]
  (if-let [path (str (choose-file))]
    (let [tex (ImageIO/read (file path))
          width (.getWidth tex)
          height (.getHeight tex)]
      (assoc state
        :scene (add-obj (:scene state)
                        (assoc (obj-from-points [[0 0] [0 height] [width height] [width 0]])
                          :closed true
                          :line-color nil
                          :tex-path path)
                        (:selected-layer state))))
    state))

(defmethod kp [KeyEvent/VK_E :ctrl] [_ state _]
  (show-export-gui)
  (assoc state
    :export true))

(declare layer-radios layers-gui rd lg exp)

(defn do-cancel-export []
  (remove-watch rstate :export)
  (dosync
   (alter rstate dissoc :export)
   (let [newexp (label "")]
     (replace! rd @exp newexp)
     (ref-set exp newexp))))

(defn do-export []
  (if-let [path (choose-file :type "export")]
    (export path))
  (do-cancel-export))

(defmethod kp [KeyEvent/VK_Z :ctrl] [_ state _]
  (or (first @undostack) state))

(defmethod kp [KeyEvent/VK_Z :shift :ctrl] [_ state _]
  (or (first @redostack) state))

(defmethod kp :default [_ state _]
  state)

(defn key-pressed [e]
  (dosync
   (ref-set rstate (kp (vec (concat [(.getKeyCode e)]
                                    (if (.isShiftDown e) [:shift] [])
                                    (if (.isControlDown e) [:ctrl] [])))
                      @rstate
                      @old-mp)))
  (repaint! can))

(defn do-drag [movement]
  (alter rstate assoc-in [:trans 1] (plus movement (get-in @rstate [:trans 1]))))

(defn handle-move [mp]
  (dosync
   (if @dragging
     (do-drag (minus mp @old-mp))
     (ref-set old-mp mp)))
  (repaint! can))

(defn mouse-moved [e]
  (handle-move (get-pos e (:trans @rstate))))

(defn mouse-pressed [e]
  (dosync
   (let [p (get-pos e (:trans @rstate))]
     (condp = (.getButton e)
       MouseEvent/BUTTON1
       (alter rstate md p)
       MouseEvent/BUTTON2
       (ref-set dragging true)
       MouseEvent/BUTTON3
       (ref-set dragging true)
       nil))))

(defn mouse-released [e]
  (request-focus! can)
  (dosync
   (let [p (get-pos e (:trans @rstate))]
     (condp = (.getButton e)
       MouseEvent/BUTTON1
       (alter rstate (fn [state] (dissoc (mp (assoc state :shift (.isShiftDown e))
                                             p)
                                         :shift)))
       MouseEvent/BUTTON2
       (ref-set dragging false)
       MouseEvent/BUTTON3
       (do
         (alter rstate assoc :action :normal)
         (ref-set dragging false))
       nil))
   (repaint! can)))

(defn do-wheel [state rot shift control]
  (cond shift (act state adjust-line rot)
        control (act state adjust-line-middle rot)
        :else (update-in state [:trans 0] #(* % (Math/pow 0.9 rot)))))

(defn mouse-wheeled [e]
  (dosync
   (alter rstate do-wheel (.getWheelRotation e) (.isShiftDown e) (.isControlDown e)))
  (repaint! can))

(defmethod kp [KeyEvent/VK_PLUS] [_ state _]
  (do-wheel state -1 false false))

(defmethod kp [KeyEvent/VK_PLUS :shift] [_ state _]
  (do-wheel state -1 true false))

(defmethod kp [KeyEvent/VK_MINUS] [_ state _]
  (do-wheel state 1 false false))

(defmethod kp [KeyEvent/VK_MINUS :shift] [_ state _]
  (do-wheel state 1 true false))

(defn reshow-layer-gui []
  (repaint! can)
  (let [newlg (layers-gui)]
    (replace! rd @lg newlg)
    (ref-set lg newlg)))

(add-watch rstate :layer
           (fn [_ _ old-state new-state]
             (if-not (= (get-in old-state [:scene :layers-ord])
                        (get-in new-state [:scene :layers-ord]))
               (dosync
                (reshow-layer-gui)))))

(defn show-export-gui []
  (add-watch rstate :export
             (fn [_ _ _ _]
               (update-export-gui)))
  (let [newexp (export-gui)]
    (replace! rd @exp newexp)
    (ref-set exp newexp)))

(defn do-layer-up []
  (dosync
   (alter rstate assoc
          :scene (move-up-layers-ord (:scene @rstate) (:selected-layer @rstate)))))

(defn do-layer-down []
  (dosync
   (alter rstate assoc
          :scene (move-down-layers-ord (:scene @rstate) (:selected-layer @rstate)))))

(defn do-new-layer []
  (dosync
   (let [[newscene newlayeri] (new-layer (:scene @rstate) (:selected-layer @rstate))]
     (alter rstate assoc
            :scene newscene
            :selected-layer newlayeri))))

(defn do-delete-layer []
  (dosync
   (let [newscene (delete-layer (:scene @rstate) (:selected-layer @rstate))]
     (alter rstate assoc
            :scene newscene
            :selected-layer (first (:layers-ord newscene))))))

(defn layer-gui [{:keys [view edit name] :as layer} layeri selected]
  (horizontal-panel :items
                    [(checkbox :selected? view
                               :listen [:item-state-changed
                                        (fn [e]
                                          (dosync
                                           (alter rstate assoc-in [:scene :layers layeri :view]
                                                  (value e)))
                                          (repaint! can))])
                     (checkbox :selected? edit
                               :listen [:item-state-changed
                                        (fn [e]
                                          (dosync
                                           (alter rstate assoc-in [:scene :layers layeri :edit]
                                                  (value e)))
                                          (repaint! can))])
                     (radio :group layer-radios ;:group has to be before :selected?.
                            :selected? selected
                            :listen [:item-state-changed
                                     (fn [e]
                                       (if (value e)
                                         (dosync
                                          (alter rstate assoc :selected-layer layeri))))])
                     (text :text name
                           :columns 10
                           :listen [:focus-lost
                                    (fn [e]
                                      (dosync
                                       (alter rstate assoc-in [:scene :layers layeri :name]
                                              (value e))))])]))

(defn layers-gui []
  (let [{:keys [scene selected-layer]} @rstate
        x (vertical-panel
           :items (concat
                   (for [i (reverse (:layers-ord scene))]
                     (layer-gui (get (:layers scene) i) i (= i selected-layer)))
                   [(horizontal-panel :items [(button :text "+"
                                                      :listen [:mouse-released
                                                               (fn [e] (do-new-layer))])
                                              (button :text "up"
                                                      :listen [:mouse-released
                                                               (fn [e] (do-layer-up))])
                                              (button :text "down"
                                                      :listen [:mouse-released
                                                               (fn [e] (do-layer-down))])
                                              (button :text "-"
                                                      :listen [:mouse-released
                                                               (fn [e] (do-delete-layer))])])]))]
    (listen (difference (set (s/select x [:*])) (set (s/select x [:JTextField])))
            :focus-gained (fn [e] (request-focus! can)))
    x))

(defn do-export-change [e f]
  (dosync
   (let [[foo bla] (export-stuff @rstate @export-scale)
         bar (apply minus (reverse foo))]
     (if-not (= 0 (apply * bar))
       (ref-set export-scale (/ (read-string (value e)) (f bar)))))
   (update-export-gui)))

(defn export-gui []
  (let [[foo bla] (export-stuff @rstate @export-scale)]
    (vertical-panel
     :items
     [(text :text (first bla)
            :id :export0
            :listen [:focus-lost
                     (fn [e] (do-export-change e first))])
      (text :text (second bla)
            :id :export1
            :listen [:focus-lost
                     (fn [e] (do-export-change e second))])
      (horizontal-panel :items [(button :text "export"
                                        :listen [:mouse-released
                                                 (fn [e] (do-export))])
                                (button :text "cancel"
                                        :listen [:mouse-released
                                                 (fn [e] (do-cancel-export))])])])))

(def size (ref [0 0]))

(defn -main []
  (def can (doto (GLCanvas.
                  (doto (GLCapabilities. (GLProfile/getDefault))
                    (.setSampleBuffers true)
                    (.setNumSamples 8)))
             (.setPreferredSize (Dimension. 600 600))))
  (def fr (Frame.))
  (.addGLEventListener can (proxy [GLEventListener] []
                             (init [d])
                             (display [^GLAutoDrawable d]
                               (let [gl (.getGL2 (.getGL d))]
                                 (render gl @rstate @old-mp @size)))
                             (reshape [d x y w h]
                               (dosync
                                (ref-set size [w h])))))
  (def layer-radios (button-group))
  (def lg (ref (layers-gui)))
  (def exp (ref (label "")))
  (def rd (border-panel :north @lg
                        :center :fill-v
                        :south @exp))
  (def fr (Frame.))
  (.add fr (border-panel :center can :east rd))
  (.setFocusTraversalKeysEnabled can false)
  (listen can
          :key-pressed key-pressed
	  :mouse-pressed mouse-pressed
	  :mouse-released mouse-released
	  #{:mouse-moved :mouse-dragged} mouse-moved
	  :mouse-wheel-moved mouse-wheeled)
  (pack! fr)
  (show! fr)
  (request-focus! can)
  nil)
