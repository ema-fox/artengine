(ns artengine.core
  (:gen-class)
  (:use [artengine util edit selection polygon key mouse]
	[seesaw [core :exclude [select action selection] :as s] graphics color chooser]
	[clojure stacktrace set]
        [clojure.java.io :exclude [copy]])
  (:import [java.awt.event KeyEvent MouseEvent]
	   [javax.imageio ImageIO]
	   [java.awt.geom Area]
	   [java.awt.image BufferedImage]))

(def rstate (ref {:scene {:objs {} :layers {1 {:stack [] :name "foo" :edit true :view true}} :layers-ord [1]}
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

(def old-mp (ref [0 0]))

(def file-path (ref nil))

(def dragging (ref false))

(declare can)

(defn save [path]
  (dosync
   (spit path (:scene @rstate))))

(defn open [path state]
  (dosync
   (ref-set file-path path))
  (assoc state :scene (read-string (slurp path))))

(defn paint-handle [g p]
  (set-color g [255 255 255 255])
  (fill-rect g (minus p [1 1]) [3 3])
  (set-color g [0 0 0 255])
  (fill-rect g p [1 1]))

(defn paint-solid-handle [g p]
  (fill-rect g (minus p [1 1]) [3 3]))

(defn paint-handles [g {:keys [ps]}]
  (doseq [[i p] ps]
    (paint-handle g p)))

(declare paint)

(defn paint-sibling [g {:keys [sibling ps ls softs steps] :as x} xs]
  (let [sib (get xs sibling)]
    (if-not steps
      (paint g (dissoc x :sibling) xs)
      (doseq [foo (range steps)]
        (paint g (assoc (dissoc x :sibling)
                   :ps (into {} (map (fn [ia ib]
                                       [ia (avg-point (get (:ps sib) ib) (get ps ia) (/ foo steps))])
                                     ls
                                     (:ls sib))))
               xs)))))

(defn paint [g {:keys [ps ls closed clip fill-color line-color line-width sibling] :as x} xs]
  (if sibling
    (paint-sibling g x xs)
    (draw g (get-polygon x xs)
          (style :background (if fill-color
                               (apply color fill-color))
                 :foreground (if line-color
                               (apply color line-color))
                 :stroke (stroke :width line-width
                                 :cap :round
                                 :join :round)))))

(defn paint-sel [g x color xs]
  (paint g (dissoc (assoc x :line-color color :line-width 1) :clip :fill-color) xs))

(defn render [c g]
  (try
  (dosync
   (set-stroke-width g 1)
   (let [{:keys [layers layers-ord objs] :as scene} (transform (:scene (mp (md @rstate @old-mp) @old-mp))
                                                               (:trans @rstate))]
     (doseq [layeri layers-ord
             i (:stack (get layers layeri))
             :when (:view (get layers layeri))
	     :let [x (get objs i)]]
       (paint g x objs))
     (if (= (:mode @rstate) :object)
       (if-let [sel (get objs (first (keys (select-obj scene (transform-p @old-mp (:trans @rstate)) 20))))]
	 (paint-sel g sel [200 0 200 255] objs)))
     (doseq [obj-i (keys (:selection @rstate)) :let [x (get objs obj-i)]]
       (if (= (:mode @rstate) :mesh)
	 (paint-handles g x)
	 (paint-sel g x [250 200 0 255] objs)))
     (set-color g [250 200 0 255])
     (when (= (:mode @rstate) :mesh)
       (doseq [[obj-i is] (:selection @rstate)
	       i is
	       :let [p (get (:ps (get objs obj-i)) i)]]
	 (paint-solid-handle g p))))
   (set-stroke-width g 1)
   (if (= (:action @rstate) :select)
     (draw-rect g (transform-p (:action-start @rstate) (:trans @rstate))
                (transform-p @old-mp (:trans @rstate))))
   (set-color g [0 0 0 255])
   (.drawString g (str (:mode @rstate)) 10 20)
   (.drawString g (str (:action @rstate)) 10 40))
  (catch Exception e
    (prn @rstate @old-mp)
    (print-cause-trace e)
    (System/exit 0))))

(defn export [path]
  (let [img (BufferedImage. 1000 1000 BufferedImage/TYPE_INT_ARGB)
	g (.getGraphics img)]
    (render nil g)
    (ImageIO/write img "png" (file path))))

(defmethod kp [KeyEvent/VK_ESCAPE] [_ _ _]
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

(defmethod kp [KeyEvent/VK_E :ctrl] [_ state _]
  (if-let [path (choose-file :type "export")]
    (export path))
  state)

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

(defn do-adjust-line [amount]
  (dosync
   (alter rstate act adjust-line amount)))

(defn mouse-pressed [e]
  (dosync
   (let [p (get-pos e (:trans @rstate))]
     (condp = (.getButton e)
       MouseEvent/BUTTON1
       (alter rstate md p)
       MouseEvent/BUTTON2
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
       (alter rstate assoc :action :normal)
       nil))
   (repaint! can)))

(defn mouse-wheeled [e]
  (dosync
   (if (.isShiftDown e)
     (do-adjust-line (.getWheelRotation e))
     (alter rstate assoc-in [:trans 0] (* (get-in @rstate [:trans 0]) (Math/pow 0.9 (.getWheelRotation e))))))
  (repaint! can))

(declare layer-radios layers-gui rd lg)

(defn reshow-layer-gui []
  (repaint! can)
  (let [newlg (layers-gui)]
    (replace! rd @lg newlg)
    (ref-set lg newlg)))

(defn do-layer-up []
  (dosync
   (alter rstate assoc
          :scene (move-up-layers-ord (:scene @rstate) (:selected-layer @rstate)))
   (reshow-layer-gui)))

(defn do-layer-down []
  (dosync
   (alter rstate assoc
          :scene (move-down-layers-ord (:scene @rstate) (:selected-layer @rstate)))
   (reshow-layer-gui)))

(defn do-new-layer []
  (dosync
   (let [[newscene newlayeri] (new-layer (:scene @rstate) (:selected-layer @rstate))]
     (alter rstate assoc
            :scene newscene
            :selected-layer newlayeri))
   (reshow-layer-gui)))

(defn do-delete-layer []
  (dosync
   (let [newscene (delete-layer (:scene @rstate) (:selected-layer @rstate))]
     (alter rstate assoc
            :scene newscene
            :selected-layer (first (:layers-ord newscene))))
   (reshow-layer-gui)))

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

(defn -main []
  (def can (canvas :paint render :background "#808080"))
  (def layer-radios (button-group))
  (def lg (ref (layers-gui)))
  (def rd (border-panel :north @lg
                        :center :fill-v))
  (def fr (frame :content (border-panel
                           :center can
                           :east rd)))
  (.setFocusTraversalKeysEnabled can false)
  (listen can
          :key-pressed key-pressed
	  :mouse-pressed mouse-pressed
	  :mouse-released mouse-released
	  #{:mouse-moved :mouse-dragged} mouse-moved
	  :mouse-wheel-moved mouse-wheeled)
  (show! fr)
  (request-focus! can)
  nil)
