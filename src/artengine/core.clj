(ns artengine.core
  (:gen-class)
  (:use [artengine util edit selection polygon var key]
	[seesaw [core :exclude [select action selection]] graphics color chooser]
	[clojure set stacktrace]
	[clojure.java.io :exclude [copy]])
  (:import [java.awt.event KeyEvent MouseEvent]
	   [javax.imageio ImageIO]
	   [java.awt.geom Area]
	   [java.awt.image BufferedImage]))

(def trans (ref [1 [0 0]]))

(def dragging (ref false))

(declare can)

(defn save [path]
  (dosync
   (spit path @scene)))

(defn open [path]
  (dosync
   (ref-set file-path path)
   (ref-set scene (read-string (slurp path)))))

(defmulti paint (fn [g x objs] (:type x)))

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

(defmethod paint :path
  [g {:keys [ps ls closed clip fill-color line-color line-width sibling] :as x} xs]
  (if sibling
    (paint-sibling g x xs)
    (do
      (when fill-color
        (set-color g fill-color)
        (.fill g (get-polygon x xs)))
      (when line-color
        (set-stroke-width g line-width)
        (set-color g line-color)
        (.draw g (get-polygon x xs))))))

(defmethod paint :sketch
  [g {:keys [ps size]} xs]
  (let [[a b] (for [[i [p0 p1]] ps]
		(Area. (circle p0 p1 size)))]
    (when b
      (.add a b)
      (let [pa (get ps 1)
	    pb (get ps 2)
	    c (->> (direction pa pb)
		   arc<-dir
		   (+ (/ tau 4)))
	    d (dvec<-avec [c size])]
	    (.add a (Area. (polygon (plus pa d) (plus pb d) (minus pb d) (minus pa d))))))
    (draw g a (style :background (color 0 0 0 30)))))

(defn paint-sel [g x color xs]
  (paint g (dissoc (assoc x :line-color color :line-width 1) :clip :fill-color) xs))

(defn selection-dist []
  (/ 20 (first @trans)))

(defn render [c g]
  (dosync
   (set-stroke-width g 1)
   (let [{:keys [stack objs] :as disp-scene}
	 (-> (condp = @action
               :extend
	       (first (extend-objs @scene (keys @selection) @old-mp))
	       :move
	       (let [movement (minus @old-mp @action-start)]
		 (if (= @mode :mesh)
		   (move @scene @selection movement)
		   (move-objs @scene @selection movement)))
	       :new-sketch
	       (new-sketch @scene @old-mp)
	       :end-sketch
	       (end-sketch @scene @selection @old-mp)
	       :rot
	       (rotate-objs @scene @selection @action-start @old-mp)
               :scale
               (if (= @mode :object)
                 (scale-objs @scene @selection @action-start @old-mp)
                 (scale-ps @scene @selection @action-start @old-mp))
               :scale-steps
               (scale-steps @scene @selection @action-start @old-mp)
	       :append
	       (let [obj-i (first (keys @selection))]
		 (assoc-in @scene [:objs obj-i] (append (get-in @scene [:objs obj-i]) @old-mp (/ (selection-dist) 2))))
	       @scene)
	     (transform @trans))]
     (doseq [i stack
	     :let [x (get objs i)]]
       (paint g x objs))
     (if (= @mode :object)
       (if-let [sel (get objs (first (keys (select-obj disp-scene (transform-p @old-mp @trans) 20))))]
	 (paint-sel g sel [200 0 200 255] objs)))
     (doseq [obj-i (keys @selection) :let [x (get objs obj-i)]]
       (if (= @mode :mesh)
	 (paint-handles g x)
	 (paint-sel g x [250 200 0 255] objs)))
     (set-color g [250 200 0 255])
     (when (= @mode :mesh)
       (doseq [[obj-i is] @selection
	       i is
	       :let [p (get (:ps (get objs obj-i)) i)]]
	 (paint-solid-handle g p))))
   (set-stroke-width g 1)
   (if (= @action :select)
     (draw-rect g (transform-p @action-start @trans) (transform-p @old-mp @trans)))
   (set-color g [0 0 0 255])
   (.drawString g (str @mode) 10 20)
   (.drawString g (str @action) 10 40)))

(defn export [path]
  (let [img (BufferedImage. 1000 1000 BufferedImage/TYPE_INT_ARGB)
	g (.getGraphics img)]
    (render nil g)
    (ImageIO/write img "png" (file path))))

(defkey [KeyEvent/VK_ESCAPE]
  (System/exit 0))

(defkey [KeyEvent/VK_S :ctrl]
  (dosync
   (if-not @file-path
     (ref-set file-path (choose-file :type :save)))
   (if @file-path
    (save @file-path))))

(defkey [KeyEvent/VK_O :ctrl]
  (if-let [path (choose-file)]
    (open path)))

(defkey [KeyEvent/VK_E :ctrl]
  (if-let [path (choose-file :type "export")]
    (export path)))

(defn key-pressed [e]
  (dosync
   (if-let [f (get @key-actions (concat [(.getKeyCode e)]
					(if (.isShiftDown e) [:shift] [])
					(if (.isControlDown e) [:ctrl] [])))]
     (f)))
  (repaint! can))

(defn do-move [movement]
  (dosync
   (if (= @mode :mesh)
     (act move movement)
     (act move-objs movement))))

(defn do-drag [movement]
  (alter trans assoc 1 (plus movement (get @trans 1))))

(defn handle-move [mp]
  (dosync
   (if @dragging
     (do-drag (minus mp @old-mp))
     (ref-set old-mp mp)))
  (repaint! can))

(defn mouse-moved [e]
  (handle-move (get-pos e @trans)))

(defn mouse-dragged [e]
  (handle-move (get-pos e @trans)))

(defn do-new-obj [p]
  (alter scene new-obj p)
  (ref-set selection {(last (:stack @scene)) #{}}))

(defn do-new-sketch [p]
  (alter scene new-sketch p)
  (ref-set selection {(last (:stack @scene)) #{}}))

(defn do-end-sketch [p]
  (act end-sketch p))

(defn do-append [p]
  (let [obj-i (first (keys @selection))
	x (append (get (:objs @scene) obj-i) p (/ (selection-dist) 2))]
    (when (:closed x)
      (ref-set action :normal))
    (alter scene assoc-in [:objs obj-i] x)))

(defn do-extend [p]
  (dosync
   (let [[foo [obj-i i]] (extend-objs @scene (keys @selection) p)]
     (ref-set scene foo)
     (ref-set selection (into {} (mapmap (fn [obj-ib _]
					   (if (= obj-ib obj-i)
					     #{i}
					     #{}))
					 @selection))))))

(defn do-clip [p]
  (if-let [clip (first (keys (select-obj @scene p (selection-dist))))]
    (act set-clip clip))
  (ref-set action :normal))

(defn do-pick-style [p]
  (if-let [master (first (keys (select-obj @scene p (selection-dist))))]
    (act pick-style master))
  (ref-set action :normal))

(defn do-adjust-line [amount]
  (act adjust-line amount))

(defn xunion [a b]
  (difference (union a b) (intersection a b)))

(defn do-select
  ([mp shift]
     (dosync
      (if (= @mode :mesh)
	(ref-set selection (merge-with xunion
				       (select-ps @scene @selection mp (selection-dist))
				       (if shift
					 @selection
					 #{})))
	(let [new-selction (select-obj @scene mp (selection-dist))]
	  (ref-set selection (merge (apply dissoc new-selction (keys @selection))
				    (if shift
				      (apply dissoc @selection (keys new-selction))
				      {})))))))
  ([pa pb shift]
     (dosync
      (if (= @mode :mesh)
	(ref-set selection (merge-with union
					 (rect-select @scene @selection pa pb)
					 (if shift
					   @selection
					   #{})))
	(ref-set selection (merge
			    (rect-select-obj @scene pa pb)
			    (if shift
			      @selection
			      {})))))))

(defn mouse-pressed [e]
  (dosync
   (let [p (get-pos e @trans)]
     (condp = (.getButton e)
	 MouseEvent/BUTTON1
       (condp = @action
	   :normal
	 (do
	   (ref-set action :select)
	   (ref-set action-start p))
	 :new-obj
	 (do
	   (do-new-obj p)
	   (ref-set action :append))
	 :append
	 (do-append p)
	 :new-sketch
	 (do
	   (do-new-sketch p)
	   (ref-set action :end-sketch))
	 :end-sketch
	 (do
	   (do-end-sketch p)
	   (ref-set action :normal))
	 :extend
	 (do
	   (do-extend p)
	   (ref-set action-start p)
	   (ref-set action :move))
	 nil)
       MouseEvent/BUTTON2
       (ref-set dragging true)
       nil))))

(defn mouse-released [e]
  (dosync
   (let [p (get-pos e @trans)]
     (condp = (.getButton e)
	 MouseEvent/BUTTON1
       (condp = @action
         :rot
	 (do
	   (act rotate-objs @action-start p)
	   (ref-set action :normal))
         :scale
         (do
           (if (= @mode :object)
             (act scale-objs @action-start p)
             (act scale-ps @action-start p))
           (ref-set action :normal))
         :scale-steps
         (do
           (act scale-steps @action-start p)
           (ref-set action :normal))
	 :move
	 (do
	   (do-move (minus p @action-start))
	   (ref-set action :normal))
	 :clip
	 (do-clip p)
	 :pick-style
	 (do-pick-style p)
	 :select
	 (let [shift (.isShiftDown e)]
	   (if (< (distance @action-start p) 5)
	     (do-select p shift)
	     (do-select @action-start p shift))
	   (ref-set action :normal))
	 nil)
       MouseEvent/BUTTON2
       (ref-set dragging false)
       MouseEvent/BUTTON3
       (ref-set action :normal)
       nil))
   (repaint! can)))

(defn mouse-wheeled [e]
  (dosync
   (if (.isShiftDown e)
     (do-adjust-line (.getWheelRotation e))
     (alter trans assoc 0 (* (get @trans 0) (Math/pow 0.9 (.getWheelRotation e))))))
  (repaint! can))

(defn -main []
  (def can (canvas :paint render :background "#808080"))
  (def fr (frame :content can))
  (listen fr :key-pressed key-pressed)
  (.setFocusTraversalKeysEnabled fr false)
  (listen can
	  :mouse-pressed mouse-pressed
	  :mouse-released mouse-released
	  #{:mouse-moved :mouse-dragged} mouse-moved
	  :mouse-wheel-moved mouse-wheeled)
  (show! fr)
  nil)
