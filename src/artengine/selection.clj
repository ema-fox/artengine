(ns artengine.selection
  (use [artengine util polygon edit var]))

(defn obj-near?
  "determines if x is near or under p"
  [{:keys [closed] :as x} p xs dist]
  (or (some (fn [[pa pb]]
	      (< (line-p-distance pa pb p) dist))
	    (get-lines x))
      (when closed
	(shape-contains (get-polygon x xs) p))))

(defn select-obj [xs mp dist]
  (let [seli (->> (map (fn [obj-i]
			 [obj-i (get xs obj-i)])
		       (reverse @stack))
		  (filter (fn [[obj-i x]]
			    (obj-near? x mp xs dist)))
		  first
		  first)]
    (if seli
      {seli #{}}
      {})))

(defn selectable-ps [xs obj-is]
  (for [obj-i (keys obj-is)]
    [obj-i (for [i (:ls (get xs obj-i))]
	     [(get (:ps (get xs obj-i)) i) i])]))

(defn select-ps [xs selection mp dist]
  (let [[sel-obj sel-i] (->> (for [[obj-i foos] (selectable-ps xs selection)
				   [p i] foos]
			       [p [obj-i i]])
			     (map (fn [[p i]]
				    [(distance mp p) i]))
			     (filter #(< (first %) dist))
			     (sort-by first)
			     first
			     second)]
    (into {} (mapmap (fn [obj-i _]
		       (if (= sel-obj obj-i)
			 #{sel-i}
			 #{}))
		     selection))))

(defn rect-select [xs selection pa pb]
  (->> (selectable-ps xs selection)
       (map (fn [[obj-i foos]]
	      [obj-i (set (map second (filter (fn [[p i]]
						(contains pa pb p))
					      foos)))]))
       (into {})))

(defn obj-contains [pa pb x]
  (every? (fn [[i p]]
	    (contains pa pb p))
	  (:ps x)))
  
(defn rect-select-obj [xs pa pb]
  (->> xs
       (filter (fn [[obj-i x]]
		 (obj-contains pa pb x)))
       (mapmap (fn [obj-i x] #{}))
       (into {})))
