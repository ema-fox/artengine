(ns artengine.edit
  (:use [artengine.util]
	[clojure.set]))

(defmacro deftool [name args body]
  `(defn ~name [~'xs ~'selection ~@args]
     (into ~'xs (mapmap (fn [~'obj-i ~'selis]
			  (let [~'x (get ~'xs ~'obj-i)]
			    ~body))
			~'selection))))

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

(defn append [{:keys [ps ls] :as x} p dist]
  (if (< (distance p (get ps (first ls))) dist)
    (assoc x :closed true)
    (let [newi (get-new-key ps)]
      (assoc x
	:ls (conj (vec ls) newi)
	:ps (assoc ps newi p)))))

(defn new-obj [xs p]
  (let [newi (get-new-key xs)]
    [(assoc xs newi {:ps {1 p} :ls [1] :closed false :line-color [0 0 0 255] :line-width 1})
     newi]))

(defn new-sketch [xs p]
  (let [newi (get-new-key xs)]
    [(assoc xs newi {:ps {1 p} :ls [1] :type :sketch :size 20})
     newi]))

(deftool end-sketch [p]
  (assoc x
    :ps (assoc (:ps x) 2 p)
    :ls (conj (:ls x) 2)))

(deftool adjust-line [amount]
  (if (= (:type x) :sketch)
    (assoc x
      :size (* (:size x) (Math/pow 0.9 amount)))
    (assoc x
      :line-width (* (:line-width x) (Math/pow 0.9 amount)))))

(defn fix-obj [{:keys [decos] :as x} xs]
  (let [newdecos (filter #(get xs %) decos)]
    (if (seq newdecos)
      (assoc x :decos (set newdecos))
      (dissoc x :decos))))

(defn fix [xs]
  (into {} (mapmap (fn [obj-i x]
		     (fix-obj x xs))
		xs)))

(defn delete-objs [xs selection]
  (fix (apply dissoc xs (keys selection))))

(defn delete-ps [{:keys [ps ls] :as x} selis]
  (assoc x
    :ps (apply dissoc ps selis)
    :ls (filter #(not (contains? selis %)) ls)))

(defn delete [xs selection]
  (let [foo (mapmap (fn [obj-i selis]
		      (delete-ps (get xs obj-i) selis))
		    selection)]
    (delete-objs (into xs foo)
		 (map first (filter #(empty? (:ps (second %))) foo)))))

(deftool delete-color []
  (dissoc x :fill-color))

(deftool set-objs-color [color]
  (assoc x :fill-color color))

(deftool delete-border []
  (dissoc x :line-color))

(deftool set-border-color [color]
  (assoc x :line-color color))

(deftool delete-objs-deco [obj-is]
  (fix-obj (assoc x :decos (difference (:decos x #{}) (set obj-is))) xs))

(deftool deco-objs [obj-is]
  (assoc x :decos (union (:decos x #{}) (set obj-is))))

(defn move-ps [{:keys [ps] :as x} movement selis]
  (assoc x :ps (into ps (map (fn [i]
			       [i (plus (get ps i) movement)])
			     selis))))

(deftool move [movement]
  (move-ps x movement selis))

(defn move-obj [x movement]
  (assoc x
    :ps (into {} (for [[i p] (:ps x)]
		   [i (plus p movement)]))))

(deftool move-objs [movement]
  (move-obj x movement))

(deftool delete-clip []
  (dissoc x :clip))

(deftool set-clip [clip]
  (if (and (not= obj-i clip)
	   (loop [i clip]
	     (let [clipi (:clip (get xs i))]
	       (if clipi
		 (if (= clipi obj-i)
		   false
		   (recur clipi))
		 true))))
    (assoc x :clip clip)
    x))

(deftool pick-style [masteri]
  (let [master (get xs masteri)]
    (merge x (select-keys master [:line-width :line-color :fill-color]))))

(defn rotate-ps [ps pa rot]
  (let [foops (mapmap (fn [i p]
			(let [[a dist] (avec<-dvec (minus p pa))]
			  [(+ a rot) dist]))
		      ps)]
    (into {} (mapmap (fn [i p]
		       (plus pa (dvec<-avec p)))
		     foops))))

(deftool rotate-objs [rot-p1 rot-p2 p]
  (let [[a1 _] (avec<-dvec (minus rot-p2 rot-p1))
	[a2 _] (avec<-dvec (minus p rot-p1))]
    (assoc x :ps (rotate-ps (:ps x) rot-p1 (- a2 a1)))))

(defn transform [xs [scale :as transformation]]
  (into {} (mapmap (fn [obj-i x]
		     (assoc (if (= (:type x) :sketch)
			      (assoc x :size (* scale (:size x)))
			      (assoc x :line-width (* scale (:line-width x))))
		       :ps (into {} (mapmap (fn [i p]
					      (transform-p p transformation))
					    (:ps x)))))
		   xs)))