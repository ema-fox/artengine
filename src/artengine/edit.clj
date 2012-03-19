(ns artengine.edit
  (:use [artengine.util]))

(defn get-new-key [xs]
  (inc (apply max (map first xs))))

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
	      (let [newi (get-new-key ps)]
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

(defn append [{:keys [ps ls] :as x} p]
  (if (< (distance p (get ps (first ls))) 10)
    (assoc x :closed true)
    (let [newi (get-new-key ps)]
      (assoc x
	:ls (conj (vec ls) newi)
	:ps (assoc ps newi p)))))

(defn new-obj [xs p]
  (let [newi (get-new-key xs)]
    [(assoc xs newi {:ps {1 p} :ls [1] :closed false :line-color [0 0 0]})
     newi]))

(defn fix-obj [{:keys [decos] :as x} xs]
  (let [newdecos (filter #(get xs %) decos)]
    (if (seq newdecos)
      (assoc x :decos newdecos)
      (dissoc x :decos))))

(defn fix [xs]
  (into {} (map (fn [[obj-i x]]
		  [obj-i (fix-obj x xs)])
		xs)))

(defn delete-objs [xs sel-objs]
  (fix (apply dissoc xs sel-objs)))

(defn delete-ps [{:keys [ps ls] :as x} selis]
  (assoc x
    :ps (apply dissoc ps selis)
    :ls (filter #(not (contains? selis %)) ls)))

(defn delete [xs sel-objs selis]
  (let [foo (map (fn [obj-i]
		   [obj-i (delete-ps (get xs obj-i) (get selis obj-i #{}))])
		 sel-objs)]
    (delete-objs (into xs foo)
		 (map first (filter #(empty? (:ps (second %))) foo)))))

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
