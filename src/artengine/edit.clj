(ns artengine.edit
  (:use [artengine.util]))

(defn get-new-key [xs]
  (if (first xs)
    (inc (apply max (map first xs)))
    1))

(defn get-ilines [{:keys [ps ls closed]}]
  (map vector
       ls
       (if (and closed (< 1 (count ls)))
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
    [(assoc xs newi {:ps {1 p} :ls [1] :closed false :line-color [0 0 0 255]})
     newi]))

(defn fix-obj [{:keys [decos] :as x} xs]
  (let [newdecos (filter #(get xs %) decos)]
    (if (seq newdecos)
      (assoc x :decos newdecos)
      (dissoc x :decos))))

(defn fix [xs]
  (into {} (mapmap (fn [obj-i x]
		     (fix-obj x xs))
		xs)))

(defn delete-objs [xs sel-objs]
  (fix (apply dissoc xs sel-objs)))

(defn delete-ps [{:keys [ps ls] :as x} selis]
  (assoc x
    :ps (apply dissoc ps selis)
    :ls (filter #(not (contains? selis %)) ls)))

(defn delete [xs sel-objs selis]
  (let [foo (mapmap (fn [obj-i x]
		      (delete-ps x (get selis obj-i #{})))
		 (select-keys xs sel-objs))]
    (delete-objs (into xs foo)
		 (map first (filter #(empty? (:ps (second %))) foo)))))

(defn set-objs-color [xs sel-objs color]
  (into xs (mapmap (fn [obj-i x]
		     (assoc x :fill-color color))
		(select-keys xs sel-objs))))

(defn deco-obj [x obj-is]
  (assoc x
    :decos (into (:decos x) obj-is)))

(defn deco-objs [xs obj-isa obj-isb]
  (into xs (mapmap (fn [obj-i x]
		     (deco-obj x obj-isb))
		   (select-keys xs obj-isa))))

(defn move-ps [{:keys [ps] :as x} movement selis]
  (assoc x :ps (into ps (map (fn [i]
			       [i (plus (get ps i) movement)])
			     selis))))

(defn move [xs sel-objs selis movement]
  (into xs (mapmap (fn [obj-i x]
		     (move-ps x movement (get selis obj-i)))
	     (select-keys xs sel-objs))))

(defn move-obj [x movement]
  (assoc x
    :ps (into {} (for [[i p] (:ps x)]
		   [i (plus p movement)]))))

(defn move-objs [xs sel-objs movement]
  (into xs (mapmap (fn [obj-i x]
		     (move-obj x movement))
		   (select-keys xs sel-objs))))

(defn set-clip [xs sel-objs clip]
  (into xs (mapmap (fn [obj-i x]
		     (assoc x :clip clip))
		   (select-keys xs sel-objs))))
