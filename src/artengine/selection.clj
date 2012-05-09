(ns artengine.selection
  (use [artengine util polygon edit]))

(defn obj-near?
  "determines if x is near or under p"
  [{:keys [fill-color] :as x} p xs dist]
  (or (some (fn [[pa pb]]
	      (< (line-p-distance pa pb p) dist))
	    (get-lines x))
      (when fill-color
	(shape-contains (get-polygon x xs) p))))

(defn select-obj [{:keys [objs layers layers-ord]} mp dist]
  (let [seli (->> (map (fn [obj-i]
			 [obj-i (get objs obj-i)])
		       (reverse (for [layeri layers-ord
                                      obj-i (get-in layers [layeri :stack])
                                      :when (get-in layers [layeri :edit])]
                                  obj-i)))
		  (filter (fn [[obj-i x]]
			    (obj-near? x mp objs dist)))
		  first
		  first)]
    (if seli
      {seli #{}}
      {})))

(defn selectable-ps [objs obj-is]
  (for [obj-i (keys obj-is)]
    [obj-i (:ps (get objs obj-i))]))

(defn select-ps [{:keys [stack objs]} selection mp dist]
  (let [[sel-obj sel-i] (->> (for [[obj-i foos] (selectable-ps objs selection)
				   [i p] foos]
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
	      [obj-i (set (map first (filter (fn [[i p]]
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

(defn bbox [ps]
  (if (first ps)
    (let [pa [(apply min (map first ps))
              (apply min (map second ps))]
          pb [(apply max (map first ps))
              (apply max (map second ps))]]
      [pa pb])
    [[0 0] [0 0]]))

(defn selected-bbox [objs selection]
  (bbox (apply concat (for [i (keys selection)]
                        (vals (get-in objs [i :ps]))))))
