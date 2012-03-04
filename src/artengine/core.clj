(ns artengine.core
  (:use [artengine.util])
  (:import [java.awt.event KeyEvent MouseEvent]
	   [java.awt.geom Area]))

(def repaint (ref nil))

(def ^:dynamic seed)


(def objs (ref {1 {:ps [[10 10] [10 130] [130 130]]
		   :sel [false false false]
		   :closed true
		   :decos [2 3]}
		2 {:ps [[200 50] [210 50] [210 60]]
		   :sel [false false false]
		   :closed false}
		3 {:ps [[300 50] [320 50] [320 70]]
		   :sel [false false false]
		   :closed false}
		4 {:ps [[100 20] [20 120] [120 170]]
		   :sel [false false false]
		   :closed true
		   :clip 1}}))

(def selection (ref [1 2 3 4]))

(def sel-start (ref nil))

(defn set-seed! []
  (set! seed (mod (* 111 (+ 5 (bit-xor seed (bit-shift-right seed 2)))) 64)))

(defn p-rand-nth [xs]
  (set-seed!)
  (nth xs (mod seed (count xs))))

(def mode (ref :normal))

(def old-mp (ref [0 0]))

(defn lcycle [xs]
  (conj (vec (rest xs)) (first xs)))

(defn pairs-cycle [xs]
  (map (fn [x y] [x y])
       xs
       (cons (last xs) (butlast xs))))

(defn get-lines [{:keys [ps closed]}]
  (map (fn [x y] [x y])
       ps
       (if closed
	 (cons (last ps) (butlast ps))
	 (rest ps))))

(defn paint-handles [g {:keys [ps sel]}]
  (doall (map (fn [p selected]
		(set-color g (if selected
			       [250 200 0]
			       [0 0 0]))
		(fill-rect g (minus p [1 1]) [3 3]))
	      ps sel)))

(defn prepare-deco [{:keys [ps]} arc]
  (let [step1 (for [p ps]
		(avec<-dvec (minus p (first ps))))
	length (second (last step1))
	corrective-arc (- arc (first (last step1)) (/ tau 2))]
    {:length length
     :ps (for [[a dist] step1]
	   (dvec<-avec [(+ a corrective-arc) dist]))}))
	


(defn decorate-line [pa pb decos]
  (let [decos (map #(prepare-deco % (arc<-dir (direction pa pb))) decos)
	dist (distance pa pb)
	[n leftover] (binding [seed seed]
		       (loop [n 0
			      dist2 dist]
			 (let [deco (p-rand-nth decos)]
			   (if (> dist2 (:length deco))
			     (recur (inc n) (- dist2 (:length deco)))
			     [n dist2]))))]
    (loop [dist2 dist
	   res []]
      (let [deco (p-rand-nth decos)]
	(if (> dist2 (:length deco))
	  (let [p (avg-point pb pa (/ dist2 dist))]
	    (recur (- dist2 (:length deco) (/ leftover n))
		   (concat res (for [pm (butlast (:ps deco))]
				 (plus pm p)))))
	  res)))))

(defn decorate-polygon [ps decos]
  (apply concat (for [[pa pb] (pairs-cycle ps)]
		  (decorate-line pa pb decos))))

(defn paint-polygon [g {:keys [ps sel]}]
  (fill-polygon g (decorate-polygon ps)))
		

(defn paint-clipped-polygon [g {:keys [ps]} clipps]
  (.fill g (doto (Area. (make-polygon clipps))
	     (.intersect (Area. (make-polygon ps))))))

(defn paint-lines [g {:keys [ps sel]}]
  (draw-lines g ps))

(defn extend-helper [lines p]
  (->> (for [[pa pb i] lines
	     :let [pc (p-on-line pa pb p)]]
	 (if (contains pa pb pc)
	   [pc i]
	   (let [[pd pe] (sort-by #(distance % pc) [pa pb])]
	     [(avg-point pd pe (/ (- (distance pd pe) 1) (distance pd pe))) i])))
       (sort-by #(distance (first %) p))
       first
       second))

(defn extend-obj [{:keys [ps sel closed] :as x} p]
  (let [i (extend-helper (map conj
			      (get-lines x)
			      (if closed
				(range)
				(rest (range))))
			 p)]
    (assoc x
      :ps (concat (take i ps) [p] (drop i ps))
      :sel (concat (repeat i false) [true] (repeat (- (count sel) i) false)))))

(defn clip-polygon [pol1 pol2]
  (doto (Area. pol2)
    (.intersect (Area. pol1))))

(defn get-polygon [{:keys [ps decos clip]} xs]
  (let [pol (make-polygon (if decos
			    (decorate-polygon ps (map (fn [i]
							(get xs i))
						      decos))
			    ps))]
    (if clip
      (clip-polygon pol (get-polygon (get xs clip) xs))
      pol)))

(defn paint [g {:keys [ps closed clip] :as x} xs]
  (if closed
    (do
      (set-color g (if clip
		     [200 150 50]
		     [30 70 20]))
      (.fill g (get-polygon x xs)))
    (draw-lines g ps)))

(defn get-selection []
  (get @objs (first @selection)))

(defn extend-objs [xs i p]
  (assoc xs i (extend-obj (get xs i) p)))

(defn render [g]
  (dosync
   (let [xs (if (= @mode :extend)
	      (extend-objs @objs (first @selection) @old-mp)
	      @objs)]
     (doseq [[i x] xs]
       (binding [seed 0]
	 (paint g x xs))))
   (paint-handles g (get-selection))
   (if (= @mode :select)
     (draw-rect g @sel-start @old-mp))))


(defn key-pressed [e]
  (let [key (.getKeyCode e)]
    (condp = key
	KeyEvent/VK_ESCAPE
      (System/exit 0)
      KeyEvent/VK_T
      (dosync
       (alter selection lcycle)
       (@repaint))
      KeyEvent/VK_E			;todo handle append
      (dosync				;todo holding e, more than one new point
       (ref-set mode :extend)
       (@repaint))
      KeyEvent/VK_G
      (dosync
       (ref-set mode :move))
      nil)))

(defn move [x movement]
  (assoc x
    :ps (map (fn [p selected]
	       (if selected
		 (plus p movement)
		 p))
	     (:ps x)
	     (:sel x))))

(defn do-move [movement]
  (dosync
   (alter-in objs (first @selection) move movement)))

(defn handle-move [mp]
  (when (= @mode :move)
    (do-move (minus mp @old-mp)))
  (when (not= @mode :normal)
    (@repaint))
  (dosync
   (ref-set old-mp mp)))

(defn mouse-moved [e]
  (handle-move (get-pos e)))
 

(defn mouse-dragged [e]
  (handle-move (get-pos e)))


(defn select [x mp]
  (let [is (range (count (:ps x)))
	seli (->>
	      (map (fn [p i]
		     [(distance mp p) i])
		   (:ps x)
		   is)
	      (filter #(< (first %) 30))
	      (sort-by first)
	      first
	      second)]
    (assoc x :sel (map #(= seli %) is))))

(defn rect-select [x pa pb]
  (assoc x
    :sel (map (fn [p s]
		(or s
		    (contains pa pb p)))
	      (:ps x)
	      (:sel x))))

(defn do-select
  ([mp]
     (dosync
      (alter-in objs (first @selection) select mp)))
  ([pa pb]
     (dosync
      (alter-in objs (first @selection) rect-select pa pb))))

(defn do-extend [p]
  (dosync
   (alter objs extend-objs (first @selection) p)))

(defn mouse-pressed [e]
  (dosync
   (cond
    (and (= (.getButton e) MouseEvent/BUTTON1)
	 (= @mode :normal))
    (do
      (ref-set mode :select)
      (ref-set sel-start (get-pos e)))
    (= @mode :extend)
    (do
      (do-extend (get-pos e))
      (ref-set mode :move)))))

(defn mouse-released [e]
  (dosync
   (condp = (.getButton e)
       MouseEvent/BUTTON3
     (do-select (get-pos e))
     MouseEvent/BUTTON1
     (cond
      (= @mode :move)
      (ref-set mode :normal)
      (= @mode :select)
      (do
	(do-select @sel-start (get-pos e))
	(ref-set mode :normal)
	(@repaint)))
     nil))
  (@repaint))

(defn -main []
  (dosync  
   (ref-set repaint (start render key-pressed mouse-released mouse-pressed mouse-moved mouse-dragged)))
  nil)
