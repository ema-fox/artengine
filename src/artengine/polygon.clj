(ns artengine.polygon
  (:use [artengine.util]
	[artengine.edit])
  (:import [java.awt.geom Area]))

(defn scramble [x]
  (mod (* 111 (+ 5 (bit-xor x (bit-shift-right x 2)))) 64))

(defn p-rand-nth [xs n]
  (nth xs (mod (scramble n) (count xs))))

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
		       (let [deco (p-rand-nth decos (apply bit-xor n (concat pa pb)))] ;use ids
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
