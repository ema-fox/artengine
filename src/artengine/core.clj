(ns artengine.core
  (:gen-class)
  (:use [artengine.util]
	[artengine.edit]
	[artengine.selection]
	[artengine.polygon]
	[clojure.set]
	[clojure.stacktrace])
  (:import [java.awt.event KeyEvent MouseEvent InputEvent]))

(def repaint (ref nil))

(def objs (ref {}))

(def selected-objs (ref #{}))

(def selected-ps (ref {}))

(def action-start (ref nil))

(def action (ref :normal))
(def mode (ref :object))

(def old-mp (ref [0 0]))

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
   (set-color g [0 0 0 255])
   (.drawString g (str @mode) 10 20)
   (.drawString g (str @action) 10 40)
   (if (= @action :select)
     (draw-rect g @action-start @old-mp))))

(defn do-save []
  (if-let [path (get-save-path)]
    (save path)))

(defn do-open []
  (if-let [path (get-open-path)]
    (open path)
    (@repaint)))

(defn do-delete []
  (if (= @mode :mesh)
    (do
      (alter objs delete @selected-objs @selected-ps)
      (ref-set selected-ps {}))
    (do
      (alter objs delete-objs @selected-objs)
      (ref-set selected-ps {})
      (ref-set selected-objs #{}))))

(defn do-delete-color []
  (alter objs delete-color @selected-objs))

(defn do-set-color []
  (if-let [color (get-color)]
    (alter objs set-objs-color @selected-objs color)))

(defn do-delete-border []
  (alter objs delete-border @selected-objs))

(defn do-set-border-color []
  (if-let [color (get-color)]
    (alter objs set-border-color @selected-objs color)))

(defn do-delete-deco []
  (alter objs delete-objs-deco @selected-objs @selected-objs))

(defn do-deco [] ;todo decoration of non closed objects
  (let [a (filter #(:closed (get @objs %)) @selected-objs)
	b (filter #(not (:closed (get @objs %))) @selected-objs)]
    (alter objs deco-objs a b)))

(defn do-delete-clip []
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

(defn do-down []
  (alter stack move-down @selected-objs))

(defn do-up []
  (alter stack #(reverse (move-down (reverse %) @selected-objs))))

(defn key-pressed [e]
  (let [key (.getKeyCode e)
	shift (not= 0 (bit-and InputEvent/SHIFT_MASK (.getModifiers e)))
	ctrl (not= 0 (bit-and InputEvent/CTRL_MASK (.getModifiers e)))]
    (dosync
     (condp = key
	 KeyEvent/VK_ESCAPE
       (System/exit 0)
       KeyEvent/VK_S
       (if ctrl
	 (do-save))
       KeyEvent/VK_O
       (if ctrl
	 (do-open))
       KeyEvent/VK_TAB
       (ref-set mode (if (= @mode :object)
		       :mesh
		       :object))
       KeyEvent/VK_E
       (if (= @mode :mesh) 		;todo handle append
	 (ref-set action :extend))
       KeyEvent/VK_DELETE
       (do-delete)
       KeyEvent/VK_SPACE
       (do
	 (ref-set mode :mesh)
	 (if (= @action :normal)
	   (ref-set action :new-obj)
	   (ref-set action :normal)))
       KeyEvent/VK_C
       (if shift
	 (do-delete-color)
	 (do-set-color))
       KeyEvent/VK_L
       (if shift
	 (do-delete-border)
	 (do-set-border-color))
       KeyEvent/VK_D
       (if shift
	 (do-delete-deco)
	 (do-deco))
       KeyEvent/VK_F
       (if shift
	 (do-delete-clip)
	 (ref-set action :clip))
       KeyEvent/VK_DOWN
       (do-down)
       KeyEvent/VK_UP
       (do-up)
       KeyEvent/VK_R
       (ref-set action :rot-p)
       KeyEvent/VK_G
       (do
	 (ref-set action-start @old-mp)
	 (ref-set action :move))
       nil))
    (@repaint)))

(defn do-move [movement]
  (dosync
   (if (= @mode :mesh)
     (alter objs move @selected-objs @selected-ps movement)
     (alter objs move-objs @selected-objs movement))))

(defn handle-move [mp]
  (dosync
   (ref-set old-mp mp))
  (@repaint))

(defn mouse-moved [e]
  (handle-move (get-pos e)))
 
(defn mouse-dragged [e]
  (handle-move (get-pos e)))

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
   (if (= (.getButton e) MouseEvent/BUTTON1)
     (condp = @action
	 :normal
       (do
	 (ref-set action :select)
	 (ref-set action-start (get-pos e)))
       :new-obj
       (do
	 (do-new-obj (get-pos e))
	 (ref-set action :append))
       :append
       (do-append (get-pos e))
       :extend
       (do
	 (do-extend (get-pos e))
	 (ref-set action :move))
       nil))))

(defn mouse-released [e]
  (dosync
   (condp = (.getButton e)
       MouseEvent/BUTTON1
     (cond
      (= @action :rot-p)
      (do
	(ref-set action-start (get-pos e))
	(ref-set action :rot))
      (= @action :rot)
      (do
	(alter objs rotate-objs @selected-objs @action-start (get-pos e))
	(ref-set action :normal))
      (= @action :move)
      (do
	(do-move (minus (get-pos e) @action-start))
	(ref-set action :normal))
      (= @action :clip)
      (do-clip (get-pos e))
      (= @action :select)
      (let [mp (get-pos e)
	    shift (not= 0 (bit-and InputEvent/SHIFT_MASK (.getModifiers e)))]
	(if (< (distance @action-start mp) 5)
	  (do-select mp shift)
	  (do-select @action-start mp shift))
	(ref-set action :normal)
	(@repaint)))
     MouseEvent/BUTTON3
     (ref-set action :normal)
     nil))
  (@repaint))

(defn -main []
  (dosync  
   (ref-set repaint (start render key-pressed mouse-released mouse-pressed mouse-moved mouse-dragged)))
  nil)
