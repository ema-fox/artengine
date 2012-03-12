(ns artengine.core
  (:use [artengine.util]
	[clojure.stacktrace])
  (:import [java.awt.event KeyEvent MouseEvent]
	   [java.awt.geom Area]))

(def repaint (ref nil))


(def objs (ref {1 {:ps [[10 10] [10 130] [130 130]]
		   :fill-color [20 70 10]
		   :closed true
		   :decos [2 3]}
		2 {:ps [[200 50] [210 50] [210 60]]
		   :line-color [0 0 0]
		   :closed false}
		3 {:ps [[300 50] [320 50] [320 70]]
		   :line-color [0 0 0]
		   :closed false}
		4 {:ps [[100 20] [20 120] [120 170]]
		   :fill-color [200 150 50]
		   :closed true
		   :clip 1}}))

(def stack (ref [1 2 3 4]))

					;TODO: maybe store selection in a map with objs as keys and ps as set as values
;but what is with selected ps that arent in selected objs?
(def selected-objs (ref #{1 2}))
(def selected-ps (ref #{[1 0] [1 2]}))

(def sel-start (ref nil))

(defn scramble [x]
  (mod (* 111 (+ 5 (bit-xor x (bit-shift-right x 2)))) 64))

(defn p-rand-nth [xs n]
  (nth xs (mod (scramble n) (count xs))))

(def action (ref :normal))
(def mode (ref :object))

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

(defn paint-handle [g p]
  (fill-rect g (minus p [1 1]) [3 3]))

(defn paint-handles [g {:keys [ps]}]
  (set-color g [0 0 0])
  (doseq [p ps]
    (paint-handle g p)))


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

(defn decorate-polygon [ps decos]
  (apply concat (for [[pa pb] (pairs-cycle ps)]
		  (decorate-line pa pb decos))))

(defn paint-clipped-polygon [g {:keys [ps]} clipps]
  (.fill g (doto (Area. (make-polygon clipps))
	     (.intersect (Area. (make-polygon ps))))))

(defn extend-helper [lines p]
  (->> (for [[pa pb i] lines
	     :let [pc (p-on-line pa pb p)]]
	 (if (contains pa pb pc)
	   [pc i]
	   (let [[pd pe] (sort-by #(distance % pc) [pa pb])]
	     [(avg-point pd pe (/ (- (distance pd pe) 1) (distance pd pe))) i])))
       (sort-by #(distance (first %) p))
       first))

					;TODO: scrambles selection
					;handle it in the right caller
(defn extend-obj [{:keys [ps closed] :as x} p]
  (let [i (second (extend-helper (map conj
				      (get-lines x)
				      (if closed
					(range)
					(rest (range))))
				 p))]
    (assoc x
      :ps (concat (take i ps) [p] (drop i ps)))))

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

(defn obj-near?
  "determines if x is near or under p"
  [{:keys [closed] :as x} p xs]
  (or (< (distance (first (extend-helper (get-lines x) p)) p) 20)
      (when closed
	(shape-contains (get-polygon x xs) p))))

(defn paint [g {:keys [ps closed clip fill-color line-color] :as x} xs]
  (when fill-color
    (set-color g fill-color)
    (.fill g (get-polygon x xs)))
  (when line-color
    (set-color g line-color)
    (if closed
      (.draw g (get-polygon x xs))
      (draw-lines g ps))))


(defn extend-objs [xs i p]
  (assoc xs i (extend-obj (get xs i) p)))

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
   (let [xs (if (= @action :extend)
	      (extend-objs @objs (first @selected-objs) @old-mp)
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
     (doseq [[obj-i i] @selected-ps :let [p (nth (:ps (get xs obj-i)) i)]]
       (when (and (= @mode :mesh) (some #{obj-i} @selected-objs))
	 (paint-handle g p))))
   (set-color g [0 0 0])
   (.drawString g (str @mode) 10 20)
   (.drawString g (str @action) 10 40)
   (if (= @action :select)
     (draw-rect g @sel-start @old-mp))))


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
      KeyEvent/VK_G
      (dosync
       (ref-set action :move))
      nil)))

(defn move-ps [x movement selis]
  (assoc x :ps (map-indexed (fn [i p]
			      (if (some #{i} selis)
				(plus p movement)
				p))
			    (:ps x))))


(defn move [xs sel-objs selis movement]
  (into xs
	(map (fn [obj-i]
	       [obj-i (->> selis
			   (filter #(= (first %) obj-i))
			   (map second)
			   (move-ps (get xs obj-i) movement))])
	     sel-objs)))

(defn move-obj [x movement]
  (assoc x
    :ps (for [p (:ps x)]
	  (plus p movement))))

(defn move-objs [xs sel-objs movement]
  (into xs (for [obj-i sel-objs]
	     [obj-i (move-obj (get xs obj-i) movement)])))
		    

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
  (for [obj-i obj-is
	[p i] (map-indexed (fn [i p] [p i])
			   (:ps (get xs obj-i)))]
    [p [obj-i i]]))
			     

(defn select [xs sel-objs mp]
  (let [seli (->> (selectable-ps xs sel-objs)
		  (map (fn [[p i]]
			 [(distance mp p) i]))
		  (filter #(< (first %) 30))
		  (sort-by first)
		  first
		  second)]
    (if seli
      #{seli}
      #{})))

(defn rect-select [selis xs sel-objs pa pb]
  (->> (selectable-ps xs @selected-objs)
       (filter (fn [[p i]]
		 (contains pa pb p)))
       (map second)
       (into selis)))

(defn obj-contains [pa pb x]
  (every? #(contains pa pb %) (:ps x)))
  
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

;TODO: extend extend to extend more than only on object of the selection
(defn do-extend [p]
  (dosync
   (alter objs extend-objs (first @selected-objs) p)))

(defn mouse-pressed [e]
  (dosync
   (cond
    (and (= (.getButton e) MouseEvent/BUTTON1)
	 (= @action :normal))
    (do
      (ref-set action :select)
      (ref-set sel-start (get-pos e)))
    (= @action :extend)
    (do
      (do-extend (get-pos e))
      (ref-set action :move)))))

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
