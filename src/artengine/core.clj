(ns artengine.core
  (:gen-class)
  (:use [artengine.util]
	[artengine.edit]
	[artengine.selection]
	[artengine.polygon]
	[clojure.set]
	[clojure.stacktrace]
	[clojure.java.io])
  (:import [java.awt.event KeyEvent MouseEvent InputEvent]
	   [javax.imageio ImageIO]
	   [java.awt.image BufferedImage]))

(def repaint (ref nil))

(def objs (ref {}))

(def selected-objs (ref #{}))

(def selected-ps (ref {}))

(def trans (ref [1 [0 0]]))

(def action-start (ref nil))

(def action (ref :normal))
(def mode (ref :object))

(def draging (ref false))

(def old-mp (ref [0 0]))

(def key-actions (ref {}))

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

(defn render [g]
  (dosync
   (set-color g [127 127 127 255])
   (fill-rect g [0 0] [1000 1000])
   (.setTransform g (make-transformation @trans))
   (set-stroke-width g 1)
   (let [xs (condp = @action
		:extend
	      (first (extend-objs @objs @selected-objs @old-mp))
	      :move
	      (let [movement (minus @old-mp @action-start)]
		(if (= @mode :mesh)
		  (move @objs @selected-objs @selected-ps movement)
		  (move-objs @objs @selected-objs movement)))
	      :rot
	      (rotate-objs @objs @selected-objs @action-start @old-mp)
	      :append
	      (let [obj-i (first @selected-objs)]
		(assoc @objs obj-i (append (get @objs obj-i) @old-mp)))
	      @objs)]
     (doseq [i @stack
	     :let [x (get xs i)]]
       (paint g x xs))
     (if (= @mode :object)
       (if-let [sel (get xs (first (select-obj xs @old-mp)))]
	 (paint g (dissoc (assoc sel :line-color [200 0 200 255]) :clip :fill-color) xs)))
     (doseq [obj-i @selected-objs :let [x (get xs obj-i)]]
       (if (= @mode :mesh)
	 (paint-handles g x)
	 (paint g (dissoc (assoc x :line-color [250 200 0 255]) :clip :fill-color) xs)))
     (set-color g [250 200 0 255])
     (doseq [[obj-i is] @selected-ps
	     i is
	     :let [p (get (:ps (get xs obj-i)) i)]]
       (when (and (= @mode :mesh) (some #{obj-i} @selected-objs))
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
    (render g)
    (ImageIO/write img "png" (file path))))

(defkey [KeyEvent/VK_ESCAPE]
  (System/exit 0))

(defkey [KeyEvent/VK_S :ctrl]
  (if-let [path (get-save-path)]
    (save path)))

(defkey [KeyEvent/VK_O :ctrl]
  (if-let [path (get-open-path)]
    (open path)))

(defkey [KeyEvent/VK_E :ctrl]
  (if-let [path (get-save-path)]
    (export path)))

(defkey [KeyEvent/VK_DELETE]
  (if (= @mode :mesh)
    (do
      (alter objs delete @selected-objs @selected-ps)
      (ref-set selected-ps {}))
    (do
      (alter objs delete-objs @selected-objs)
      (ref-set selected-ps {})
      (ref-set selected-objs #{}))))

(defkey [KeyEvent/VK_C :shift]
  (alter objs delete-color @selected-objs))

(defkey [KeyEvent/VK_C]
  (if-let [color (get-color)]
    (alter objs set-objs-color @selected-objs color)))

(defkey [KeyEvent/VK_L :shift]
  (alter objs delete-border @selected-objs))

(defkey [KeyEvent/VK_L]
  (if-let [color (get-color)]
    (alter objs set-border-color @selected-objs color)))

(defkey [KeyEvent/VK_D :shift]
  (alter objs delete-objs-deco @selected-objs @selected-objs))

(defkey [KeyEvent/VK_D] ;todo decoration of non closed objects
  (let [a (filter #(:closed (get @objs %)) @selected-objs)
	b (filter #(not (:closed (get @objs %))) @selected-objs)]
    (alter objs deco-objs a b)))

(defkey [KeyEvent/VK_F]
  (ref-set action :clip))

(defkey [KeyEvent/VK_F :shift]
  (alter objs delete-clip @selected-objs))

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
  (alter stack move-down @selected-objs))

(defkey [KeyEvent/VK_UP]
  (alter stack #(reverse (move-down (reverse %) @selected-objs))))

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
  (let [key (.getKeyCode e)
	shift (not= 0 (bit-and InputEvent/SHIFT_MASK (.getModifiers e))) ;todo use isShiftDown
	ctrl (not= 0 (bit-and InputEvent/CTRL_MASK (.getModifiers e)))]
    (dosync
     (if-let [f (get @key-actions (concat [key]
					  (if shift [:shift] [])
					  (if ctrl  [:ctrl]  [])))]
       (f)))
    (@repaint)))

(defn do-move [movement]
  (dosync
   (if (= @mode :mesh)
     (alter objs move @selected-objs @selected-ps movement)
     (alter objs move-objs @selected-objs movement))))

(defn do-drag [movement]
  (alter trans assoc 1 (plus movement (get @trans 1))))

(defn handle-move [mp]
  (dosync
   (if @draging
     (do-drag (minus mp @old-mp))
     (ref-set old-mp mp)))
  (@repaint))

(defn mouse-moved [e]
  (handle-move (get-pos e @trans)))

(defn mouse-dragged [e]
  (handle-move (get-pos e @trans)))

(defn do-new-obj [p]
  (let [[xs obj-i] (new-obj @objs p)]
    (ref-set objs xs)
    (ref-set selected-objs #{obj-i})
    (alter stack conj obj-i)))

(defn do-append [p]
  (let [obj-i (first @selected-objs)
	x (append (get @objs obj-i) p)]
    (when (:closed x)
      (ref-set action :normal))
    (alter objs assoc obj-i x)))

(defn do-extend [p]
  (dosync
   (let [[xs [obj-i i]] (extend-objs @objs @selected-objs p)]
     (ref-set objs xs)
     (ref-set selected-ps {obj-i #{i}}))))

(defn do-clip [p]
  (if-let [clip (first (select-obj @objs p))]
    (alter objs set-clip @selected-objs clip))
  (ref-set action :normal))

(defn xunion [a b]
  (difference (union a b) (intersection a b)))

(defn do-select
  ([mp shift]
     (dosync
      (if (= @mode :mesh)
	(ref-set selected-ps (merge-with xunion
					 (select-ps @objs @selected-objs mp)
					 (if shift
					   @selected-ps
					   #{})))
	(ref-set selected-objs (xunion
				(if shift
				  @selected-objs
				  #{})
				(select-obj @objs mp))))))
  ([pa pb shift]
     (dosync
      (if (= @mode :mesh)
	(ref-set selected-ps (merge-with union
					 (rect-select @objs @selected-objs pa pb)
					 (if shift
					   @selected-ps
					   #{})))
	(ref-set selected-objs (union
				(if shift
				  @selected-objs
				  #{})
				(rect-select-obj @objs pa pb)))))))

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
	 :extend
	 (do
	   (do-extend p)
	   (ref-set action-start p)
	   (ref-set action :move))
	 nil)
       MouseEvent/BUTTON2
       (ref-set draging true)
       nil))))

(defn mouse-released [e]
  (dosync
   (let [p (get-pos e @trans)]
     (condp = (.getButton e)
	 MouseEvent/BUTTON1
       (condp = @action
	   :rot
	 (do
	   (alter objs rotate-objs @selected-objs @action-start p)
	   (ref-set action :normal))
	 :move
	 (do
	   (do-move (minus p @action-start))
	   (ref-set action :normal))
	 :clip
	 (do-clip p)
	 :select
	 (let [shift (not= 0 (bit-and InputEvent/SHIFT_MASK (.getModifiers e)))]
	   (if (< (distance @action-start p) 5)
	     (do-select p shift)
	     (do-select @action-start p shift))
	   (ref-set action :normal))
	 nil)
       MouseEvent/BUTTON2
       (ref-set draging false)
       MouseEvent/BUTTON3
       (ref-set action :normal)
       nil))
   (@repaint)))

(defn -main []
  (dosync  
   (ref-set repaint (start render key-pressed mouse-released mouse-pressed mouse-moved mouse-dragged)))
  nil)
