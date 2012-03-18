(ns artengine.core
  (:use [artengine.util]
	[artengine.edit]
	[clojure.stacktrace])
  (:import [java.awt.event KeyEvent MouseEvent]
	   [java.awt.geom Area]))

(def repaint (ref nil))

(def objs (ref {1 {:ps {1 [10 10] 2 [10 130] 3 [130 130]}
		   :ls [1 2 3]
		   :fill-color [20 70 10]
		   :closed true
		   :decos [2 3]}
		2 {:ps {1 [200 50] 2 [210 50] 3 [210 60]}
		   :ls [1 2 3]
		   :line-color [0 0 0]
		   :closed false}
		3 {:ps {1 [300 50] 2 [320 50] 3 [320 70]}
		   :ls [1 2 3]
		   :line-color [0 0 0]
		   :closed false}
		4 {:ps {1 [100 20] 2 [20 120] 3 [120 170]}
		   :ls [1 2 3]
		   :fill-color [200 150 50]
		   :closed true
		   :clip 1}}))

(def stack (ref [1 2 3 4]))

(def selected-objs (ref #{1 2}))

(def selected-ps (ref {1 #{2}}))

(def sel-start (ref nil))

(defn scramble [x]
  (mod (* 111 (+ 5 (bit-xor x (bit-shift-right x 2)))) 64))

(defn p-rand-nth [xs n]
  (nth xs (mod (scramble n) (count xs))))

(def action (ref :normal))
(def mode (ref :object))

(def old-mp (ref [0 0]))

(defn paint-handle [g p]
  (fill-rect g (minus p [1 1]) [3 3]))

(defn paint-handles [g {:keys [ps ls]}]
  (set-color g [0 0 0])
  (doseq [i ls]
    (paint-handle g (get ps i))))

(defn prepare-deco [{:keys [ps ls]} arc]
  (let [step1 (for [i ls]
		(avec<-dvec (minus (get ps i) (get ps (first ls)))))
	length (second (last step1))
	corrective-arc (- arc (first (last step1)) (/ tau 2))]
    {:length length
     :ps (for [[a dist] step1]
	   (dvec<-avec [(+ a corrective-arc) dist]))}))

(defn decorate-line [pa pb decos]
  (let [decos (map #(prepare-deco % (arc<-dir (direction pa pb))) decos)
	dist (distance pa pb)
	[n leftover] (loop [n 0
			    dist2 dist]
		       (let [deco (p-rand-nth decos (apply bit-xor n (concat pa pb)))]
			 (if (> dist2 (:length deco))
			   (recur (inc n) (- dist2 (:length deco)))
			   [n dist2])))]
    (loop [i 0
	   dist2 dist
	   res []]
      (let [deco (p-rand-nth decos (apply bit-xor i (concat pa pb)))]
	(if (> dist2 (:length deco))
	  (let [p (avg-point pb pa (/ dist2 dist))]
	    (recur (inc i)
		   (- dist2 (:length deco) (/ leftover n))
		   (concat res (for [pm (butlast (:ps deco))]
				 (plus pm p)))))
	  res)))))

(defn clip-polygon [pol1 pol2]
  (doto (Area. pol2)
    (.intersect (Area. pol1))))

(defn get-polygon [{:keys [ps ls decos clip] :as x} xs]
  (let [pol (make-polygon (if decos
			    (let [deco-objs (for [i decos] (get xs i))]
			      (apply concat (for [[pa pb] (get-lines x)]
					      (decorate-line pa pb deco-objs))))
			    (map #(get ps %) ls)))]
    (if clip
      (clip-polygon pol (get-polygon (get xs clip) xs))
      pol)))

(defn obj-near?
  "determines if x is near or under p"
  [{:keys [closed] :as x} p xs]
  (or (some (fn [[pa pb]]
	      (< (line-p-distance pa pb p) 20))
	    (get-lines x))
      (when closed
	(shape-contains (get-polygon x xs) p))))

(defn paint [g {:keys [ps ls closed clip fill-color line-color] :as x} xs]
  (when fill-color
    (set-color g fill-color)
    (.fill g (get-polygon x xs)))
  (when line-color
    (set-color g line-color)
    (if closed
      (.draw g (get-polygon x xs))
      (draw-lines g (map #(get ps %) ls)))))

(defn select-obj [xs mp]
  (let [seli (->> (map (fn [obj-i]
			 [obj-i (get xs obj-i)])
		       (reverse @stack))
		  (filter (fn [[obj-i x]]
			    (obj-near? x mp xs)))
		  first
		  first)]
    (if seli
      #{seli}
      #{})))

(defn render [g]
  (dosync
   (set-color g [127 127 127])
   (fill-rect g [0 0] [1000 1000])
   (let [xs (condp = @action
		:extend
	      (first (extend-objs @objs @selected-objs @old-mp))
	      :append
	      (let [obj-i (first @selected-objs)]
		(assoc @objs obj-i (append (get @objs obj-i) @old-mp)))
	      @objs)]
     (doseq [[i x] xs]
       (paint g x xs))
     (if-let [sel (get xs (first (select-obj xs @old-mp)))]
       (paint g (dissoc (assoc sel :line-color [200 0 200]) :clip :fill-color) xs))
     (doseq [obj-i @selected-objs :let [x (get xs obj-i)]]
       (if (= @mode :mesh)
	 (paint-handles g x)
	 (paint g (dissoc (assoc x :line-color [250 200 0]) :clip :fill-color) xs)))
     (set-color g [250 200 0])
     (doseq [[obj-i is] @selected-ps
	     i is
	     :let [p (get (:ps (get xs obj-i)) i)]]
       (when (and (= @mode :mesh) (some #{obj-i} @selected-objs))
	 (paint-handle g p))))
   (set-color g [0 0 0])
   (.drawString g (str @mode) 10 20)
   (.drawString g (str @action) 10 40)
   (if (= @action :select)
     (draw-rect g @sel-start @old-mp))))

(defn do-delete []
  (dosync
   (if (= @mode :mesh)
     (do
       (alter objs delete @selected-objs @selected-ps)
       (ref-set selected-ps {}))
     (do
       (alter objs delete-objs @selected-objs)
       (ref-set selected-ps {})
       (ref-set selected-objs #{})))))

(defn key-pressed [e]
  (let [key (.getKeyCode e)]
    (condp = key
	KeyEvent/VK_ESCAPE
      (System/exit 0)
      KeyEvent/VK_TAB
      (dosync
       (ref-set mode (if (= @mode :object)
		       :mesh
		       :object))
       (@repaint))
      KeyEvent/VK_E
      (if (= @mode :mesh) 		;todo handle append
	(dosync				;todo holding e, more than one new point
	 (ref-set action :extend)
	 (@repaint)))
      KeyEvent/VK_DELETE
      (do (do-delete)
	  (@repaint))
      KeyEvent/VK_SPACE
      (dosync
       (ref-set mode :mesh)
       (if (= @action :normal)
	 (ref-set action :new-obj)
	 (ref-set action :normal)))
      KeyEvent/VK_G
      (dosync
       (ref-set action :move))
      nil)))

(defn do-move [movement]
  (dosync
   (if (= @mode :mesh)
     (alter objs move @selected-objs @selected-ps movement)
     (alter objs move-objs @selected-objs movement))))

(defn handle-move [mp]
  (when (= @action :move)
    (do-move (minus mp @old-mp)))
  ;(when (not= @action :normal)
    (@repaint);)
  (dosync
   (ref-set old-mp mp)))

(defn mouse-moved [e]
  (handle-move (get-pos e)))
 
(defn mouse-dragged [e]
  (handle-move (get-pos e)))

(defn selectable-ps [xs obj-is]
  (for [obj-i obj-is]
    [obj-i (for [i (:ls (get xs obj-i))]
	     [(get (:ps (get xs obj-i)) i) i])]))

(defn select [xs sel-objs mp]
  (let [seli (->> (for [[obj-i foos] (selectable-ps xs sel-objs)
			[p i] foos]
		    [p [obj-i i]])
		  (map (fn [[p i]]
			 [(distance mp p) i]))
		  (filter #(< (first %) 30))
		  (sort-by first)
		  first
		  second)]
    (if seli
      {(first seli) #{(second seli)}}
      {})))

(defn rect-select [selis xs sel-objs pa pb]
  (->> (selectable-ps xs @selected-objs)
       (map (fn [[obj-i foos]]
	      [obj-i (set (map second (filter (fn [[p i]]
						(contains pa pb p))
					      foos)))]))
       (into {})
       (merge-with (fn [a b] (into a b)) selis)))

(defn obj-contains [pa pb x]
  (every? (fn [[i p]]
	    (contains pa pb p))
	  (:ps x)))
  
(defn rect-select-obj [sel-obj-is xs pa pb]
  (->> xs
       (filter (fn [[obj-i x]]
		 (obj-contains pa pb x)))
       (map first)
       (into sel-obj-is)))

(defn do-select
  ([mp]
     (dosync
      (if (= @mode :mesh)
	(ref-set selected-ps (select @objs @selected-objs mp))
	(ref-set selected-objs (select-obj @objs mp)))))
  ([pa pb]
     (dosync
      (if (= @mode :mesh)
	(alter selected-ps rect-select @objs @selected-objs pa pb)
	(alter selected-objs rect-select-obj @objs pa pb)))))

(defn do-new-obj [p]
  (let [[xs obj-i] (new-obj @objs p)]
    (ref-set objs xs)
    (ref-set selected-objs #{obj-i})))

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

(defn mouse-pressed [e]
  (dosync
   (if (= (.getButton e) MouseEvent/BUTTON1)
     (condp = @action
	 :normal
       (do
	 (ref-set action :select)
	 (ref-set sel-start (get-pos e)))
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
      (= @action :move)
      (ref-set action :normal)
      (= @action :select)
      (let [mp (get-pos e)]
	(if (< (distance @sel-start mp) 5)
	  (do-select mp)
	  (do-select @sel-start (get-pos e)))
	(ref-set action :normal)
	(@repaint)))
     nil))
  (@repaint))

(defn -main []
  (dosync  
   (ref-set repaint (start render key-pressed mouse-released mouse-pressed mouse-moved mouse-dragged)))
  nil)
