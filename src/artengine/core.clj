(ns artengine.core
  (:gen-class)
  (:use [artengine.util]
	[artengine.edit]
	[artengine.selection]
	[artengine.polygon]
	[seesaw.core :exclude [select action selection]]
	[seesaw.graphics]
	[seesaw.color]
	[seesaw.chooser]
	[clojure.set]
	[clojure.stacktrace]
	[clojure.java.io])
  (:import [java.awt.event KeyEvent MouseEvent]
	   [javax.imageio ImageIO]
	   [java.awt.geom Area]
	   [java.awt.image BufferedImage]))

(def objs (ref {}))

(def selection (ref {}))

(def trans (ref [1 [0 0]]))

(def action-start (ref nil))

(def action (ref :normal))
(def mode (ref :object))

(def dragging (ref false))

(def old-mp (ref [0 0]))

(def key-actions (ref {}))

(declare can)

(defmacro defkey [key & body]
  `(dosync
    (alter key-actions assoc ~key (fn [] ~@body))))

(defn save [path]
  (dosync
   (spit path {:objs @objs :stack @stack})))

(defn open [path]
  (dosync
   (let [x (read-string (slurp path))]
     (ref-set objs (:objs x))
     (ref-set stack (:stack x)))))

(defn paint-handle [g p]
  (fill-rect g (minus p [1 1]) [3 3]))

(defn paint-handles [g {:keys [ps ls]}]
  (set-color g [0 0 0 255])
  (doseq [i ls]
    (paint-handle g (get ps i))))

(defn paint [g {:keys [ps ls closed clip fill-color line-color] :as x} xs]
  (when fill-color
    (set-color g fill-color)
    (.fill g (get-polygon x xs)))
  (when line-color
    (set-color g line-color)
    (if closed
      (.draw g (get-polygon x xs))
      (draw-lines g (map #(get ps %) ls)))))

(defn paint-sketch [g {:keys [ps size]}]
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

(defn render [c g]
  (dosync
   (.setTransform g (make-transformation @trans))
   (set-stroke-width g 1)
   (let [xs (condp = @action
		:extend
	      (first (extend-objs @objs (keys @selection) @old-mp))
	      :move
	      (let [movement (minus @old-mp @action-start)]
		(if (= @mode :mesh)
		  (move @objs @selection movement)
		  (move-objs @objs @selection movement)))
	      :rot
	      (rotate-objs @objs @selection @action-start @old-mp)
	      :append
	      (let [obj-i (first (keys @selection))]
		(assoc @objs obj-i (append (get @objs obj-i) @old-mp)))
	      @objs)]
     (doseq [i @stack
	     :let [x (get xs i)]]
       (if (= (:type x) :sketch)
	 (paint-sketch g x)
	 (paint g x xs)))
     (if (= @mode :object)
       (if-let [sel (get xs (first (keys (select-obj xs @old-mp))))]
	 (paint g (dissoc (assoc sel :line-color [200 0 200 255]) :clip :fill-color) xs)))
     (doseq [obj-i (keys @selection) :let [x (get xs obj-i)]]
       (if (= @mode :mesh)
	 (paint-handles g x)
	 (paint g (dissoc (assoc x :line-color [250 200 0 255]) :clip :fill-color) xs)))
     (set-color g [250 200 0 255])
     (doseq [[obj-i is] @selection
	     i is
	     :let [p (get (:ps (get xs obj-i)) i)]]
       (when (and (= @mode :mesh) (some #{obj-i} (keys @selection)))
	 (paint-handle g p))))
   (if (= @action :select)
     (draw-rect g @action-start @old-mp))
   (.setTransform g (make-transformation [1 [0 0]]))
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
  (if-let [path (choose-file :type :save)]
    (save path)))

(defkey [KeyEvent/VK_O :ctrl]
  (if-let [path (choose-file)]
    (open path)))

(defkey [KeyEvent/VK_E :ctrl]
  (if-let [path (choose-file :type "export")]
    (export path)))

(defkey [KeyEvent/VK_Q]
  (ref-set action :new-sketch))

(defkey [KeyEvent/VK_DELETE]
  (if (= @mode :mesh)
    (do
      (alter objs delete @selection)
      (ref-set selection (into {} (mapmap (constantly #{}) @selection))))
    (do
      (alter objs delete-objs (keys @selection))
      (ref-set selection {}))))

(defkey [KeyEvent/VK_C :shift]
  (alter objs delete-color @selection))

(defkey [KeyEvent/VK_C]
  (if-let [color (get-color)]
    (alter objs set-objs-color @selection color)))

(defkey [KeyEvent/VK_L :shift]
  (alter objs delete-border @selection))

(defkey [KeyEvent/VK_L]
  (if-let [color (get-color)]
    (alter objs set-border-color @selection color)))

(defkey [KeyEvent/VK_D :shift]
  (alter objs delete-objs-deco @selection (keys @selection)))

(defkey [KeyEvent/VK_D] ;todo decoration of non closed objects
  (let [a (filter #(:closed (get @objs (first %))) @selection)
	b (filter #(not (:closed (get @objs %))) (keys @selection))]
    (alter objs deco-objs a b)))

(defkey [KeyEvent/VK_F]
  (ref-set action :clip))

(defkey [KeyEvent/VK_F :shift]
  (alter objs delete-clip @selection))

(defn move-down [stack sel-objs]
  (loop [s stack
	 res []]
    (if (< 1 (count s))
      (let [[a b & d] s]
	(if (and ((set sel-objs) b) (not ((set sel-objs) a)))
	  (recur (cons a d) (conj res b))
	  (recur (cons b d) (conj res a))))
      (concat res s))))

(defkey [KeyEvent/VK_DOWN]
  (alter stack move-down (keys @selection)))

(defkey [KeyEvent/VK_UP]
  (alter stack #(reverse (move-down (reverse %) (keys @selection)))))

(defkey [KeyEvent/VK_TAB]
  (ref-set mode (if (= @mode :object)
		  :mesh
		  :object)))

(defkey [KeyEvent/VK_E]
  (if (= @mode :mesh) 		;todo handle append
    (ref-set action :extend)))

(defkey [KeyEvent/VK_SPACE]
  (ref-set mode :mesh)
  (if (= @action :normal)
    (ref-set action :new-obj)
    (ref-set action :normal)))

(defkey [KeyEvent/VK_R]
  (ref-set action :rot)
  (ref-set action-start @old-mp))

(defkey [KeyEvent/VK_G]
  (ref-set action-start @old-mp)
  (ref-set action :move))

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
     (alter objs move @selection movement)
     (alter objs move-objs @selection movement))))

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
  (let [[xs obj-i] (new-obj @objs p)]
    (ref-set objs xs)
    (ref-set selection {obj-i #{}})
    (alter stack conj obj-i)))

(defn do-new-sketch [p]
  (let [[xs obj-i] (new-sketch @objs p)]
    (ref-set objs xs)
    (ref-set selection {obj-i #{}})
    (alter stack conj obj-i)))

(defn do-end-sketch [p]
  (alter objs end-sketch @selection p))

(defn do-append [p]
  (let [obj-i (first (keys @selection))
	x (append (get @objs obj-i) p)]
    (when (:closed x)
      (ref-set action :normal))
    (alter objs assoc obj-i x)))

(defn do-extend [p]
  (dosync
   (let [[xs [obj-i i]] (extend-objs @objs (keys @selection) p)]
     (ref-set objs xs)
     (ref-set selection (into {} (mapmap (fn [obj-ib _]
					   (if (= obj-ib obj-i)
					     #{i}
					     #{}))
					 @selection))))))

(defn do-clip [p]
  (if-let [clip (first (keys (select-obj @objs p)))]
    (alter objs set-clip @selection clip))
  (ref-set action :normal))

(defn do-adjust-sketch [amount]
  (alter objs adjust-sketch @selection amount))

(defn xunion [a b]
  (difference (union a b) (intersection a b)))

(defn do-select
  ([mp shift]
     (dosync
      (if (= @mode :mesh)
	(ref-set selection (merge-with xunion
				       (select-ps @objs @selection mp)
				       (if shift
					 @selection
					 #{})))
	(let [new-selction (select-obj @objs mp)]
	  (ref-set selection (merge (apply dissoc new-selction (keys @selection))
				    (if shift
				      (apply dissoc @selection (keys new-selction))
				      {})))))))
  ([pa pb shift]
     (dosync
      (if (= @mode :mesh)
	(ref-set selection (merge-with union
					 (rect-select @objs @selection pa pb)
					 (if shift
					   @selection
					   #{})))
	(ref-set selection (merge
			    (rect-select-obj @objs pa pb)
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
	   (alter objs rotate-objs @selection @action-start p)
	   (ref-set action :normal))
	 :move
	 (do
	   (do-move (minus p @action-start))
	   (ref-set action :normal))
	 :clip
	 (do-clip p)
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
     (do-adjust-sketch (.getWheelRotation e))
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
