(ns artengine.selection
  (use [artengine util polygon edit]))

(defn obj-near?
  "determines if x is near or under p"
  [{:keys [closed] :as x} p xs dist]
  (or (some (fn [[pa pb]]
	      (< (line-p-distance pa pb p) dist))
	    (get-lines x))
      (when closed
	(shape-contains (get-polygon x xs) p))))

(defn select-obj [{:keys [objs stack]} mp dist]
  (let [seli (->> (map (fn [obj-i]
			 [obj-i (get objs obj-i)])
		       (reverse stack))
		  (filter (fn [[obj-i x]]
			    (obj-near? x mp objs dist)))
		  first
		  first)]
    (if seli
      {seli #{}}
      {})))

(defn selectable-ps [objs obj-is]
  (for [obj-i (keys obj-is)]
    [obj-i (for [i (:ls (get objs obj-i))]
	     [(get (:ps (get objs obj-i)) i) i])]))

(defn select-ps [{:keys [stack objs]} selection mp dist]
  (let [[sel-obj sel-i] (->> (for [[obj-i foos] (selectable-ps objs selection)
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

(defn rect-select [{:keys [stack objs]} selection pa pb]
  (->> (selectable-ps objs selection)
       (map (fn [[obj-i foos]]
	      [obj-i (set (map second (filter (fn [[p i]]
						(contains pa pb p))
					      foos)))]))
       (into {})))

(defn obj-contains [pa pb x]
  (every? (fn [[i p]]
	    (contains pa pb p))
	  (:ps x)))
  
(defn rect-select-obj [{:keys [stack objs]} pa pb]
  (->> objs
       (filter (fn [[obj-i x]]
		 (obj-contains pa pb x)))
       (mapmap (fn [obj-i x] #{}))
       (into {})))
