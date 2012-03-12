(ns artengine.edit
  (:use [artengine.util]))

(defn get-lines [{:keys [ps closed]}]
  (map (fn [x y] [x y])
       ps
       (if closed
	 (cons (last ps) (butlast ps))
	 (rest ps))))

(defn line-p-distance [pa pb p]
  (let [pc (p-on-line pa pb p)
	pf (if (contains pa pb pc)
	     pc
	     (let [[pd pe] (sort-by #(distance % pc) [pa pb])]
	       (plus pd (direction pd pe))))]
    (distance p pf)))

(defn extend-obj [{:keys [ps closed] :as x} p retfn]
  (map (fn [[pa pb] i]
	 [(line-p-distance pa pb p)
	  (fn []
	    (retfn (assoc x
		     :ps (concat (take i ps) [p] (drop i ps)))))])
       (get-lines x)
       (if closed
	 (range)
	 (rest (range)))))

					;TODO: scrambles selection
					;give every point a non changing id
					;instead of refering with index
(defn extend-objs [xs obj-is p]
  (let [foo (->> (for [obj-i obj-is]
		   (extend-obj (get xs obj-i) p #(assoc xs obj-i %)))
		 (apply concat)
		 (sort-by first)
		 first
		 second)]
    (foo)))

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
