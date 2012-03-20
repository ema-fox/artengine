(ns artengine.selection
  (use [artengine.util]
       [artengine.polygon]
       [artengine.edit]))

(def stack (ref [1 2 3 4]))

(def selected-objs (ref #{1 2}))

(def selected-ps (ref {1 #{2}}))

(def sel-start (ref nil))

(defn obj-near?
  "determines if x is near or under p"
  [{:keys [closed] :as x} p xs]
  (or (some (fn [[pa pb]]
	      (< (line-p-distance pa pb p) 20))
	    (get-lines x))
      (when closed
	(shape-contains (get-polygon x xs) p))))

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
