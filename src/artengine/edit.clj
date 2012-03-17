(ns artengine.edit
  (:use [artengine.util]))

(defn get-ilines [{:keys [ps ls closed]}]
  (map vector
       ls
       (if closed
	 (cons (last ls) (butlast ls))
	 (rest ls))))

(defn get-lines [{:keys [ps] :as x}]
  (map (fn [[x y]]
	 [(get ps x) (get ps y)])
       (get-ilines x)))

(defn line-p-distance [pa pb p]
  (let [pc (p-on-line pa pb p)
	pf (if (contains pa pb pc)
	     pc
	     (let [[pd pe] (sort-by #(distance % pc) [pa pb])]
	       (plus pd (direction pd pe))))]
    (distance p pf)))

(defn extend-obj [{:keys [ps ls closed] :as x} p retfn]
  (map (fn [[ia ib] i]
	 (let [pa (get ps ia)
	       pb (get ps ib)]
	   [(line-p-distance pa pb p)
	    (fn []
	      (let [newi (inc (apply max ls))]
		(retfn [(assoc x
			  :ps (assoc ps newi p)
			  :ls (concat (take i ls) [newi] (drop i ls)))
			newi])))]))
       (get-ilines x)
       (if closed
	 (range)
	 (rest (range)))))

(defn extend-objs [xs obj-is p]
  (let [foo (->> (for [obj-i obj-is]
		   (extend-obj (get xs obj-i) p (fn [[x i]]
						  [(assoc xs obj-i x)
						   [obj-i i]])))
		 (apply concat)
		 (sort-by first)
		 first
		 second)]
    (foo)))

(defn move-ps [{:keys [ps] :as x} movement selis]
  (assoc x :ps (into ps (map (fn [i]
			       [i (plus (get ps i) movement)])
			     selis))))

(defn move [xs sel-objs selis movement]
  (into xs
	(map (fn [obj-i]
	       [obj-i (move-ps (get xs obj-i) movement (get selis obj-i))])
	     sel-objs)))

(defn move-obj [x movement]
  (assoc x
    :ps (into {} (for [[i p] (:ps x)]
		   [i (plus p movement)]))))

(defn move-objs [xs sel-objs movement]
  (into xs (for [obj-i sel-objs]
	     [obj-i (move-obj (get xs obj-i) movement)])))
