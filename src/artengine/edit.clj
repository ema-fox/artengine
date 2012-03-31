(ns artengine.edit
  (:use [artengine.util]
	[clojure.set]))

(defmacro deftool [name args body]
  `(defn ~name [{:keys [~'stack ~'objs] :as ~'scene} ~'selection ~@args]
     (assoc ~'scene
       :objs (into ~'objs (mapmap (fn [~'obj-i ~'selis]
				    (let [~'x (get ~'objs ~'obj-i)]
				      ~body))
				  ~'selection)))))

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

(defn extend-objs [{:keys [stack objs] :as scene} obj-is p]
  (let [foo (->> (for [obj-i obj-is]
		   (extend-obj (get objs obj-i) p (fn [[x i]]
						    [(assoc-in scene [:objs obj-i] x)
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

(defn new-obj [{:keys [stack objs] :as scene} p]
  (let [newi (get-new-key objs)]
    (assoc scene
      :objs (assoc objs newi {:ps {1 p} :ls [1] :closed false :line-color [0 0 0 255] :line-width 1})
      :stack (conj stack newi))))

(defn new-sketch [{:keys [stack objs] :as scene} p]
  (let [newi (get-new-key objs)]
    (assoc scene
      :objs (assoc objs newi {:ps {1 p} :ls [1] :type :sketch :size 20})
      :stack (conj stack newi))))

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

(defn fix [{:keys [stack objs] :as scene}]
  (assoc scene
    :objs (into {} (mapmap (fn [obj-i x]
			     (fix-obj x objs))
			   objs))
    :stack (vec (filter #(get objs %) stack))))

(defn delete-objs [{:keys [stack objs] :as scene}  selection]
  (fix (assoc scene
	 :objs (apply dissoc objs (keys selection)))))

(defn delete-ps [{:keys [ps ls] :as x} selis]
  (assoc x
    :ps (apply dissoc ps selis)
    :ls (filter #(not (contains? selis %)) ls)))

(defn delete [{:keys [stack objs] :as scene} selection]
  (let [foo (mapmap (fn [obj-i selis]
		      (delete-ps (get objs obj-i) selis))
		    selection)]
    (delete-objs (assoc scene :objs (into objs foo))
		 (map first (filter #(empty? (:ps (second %))) foo)))))

(deftool delete-color []
  (dissoc x :fill-color))

(deftool set-objs-color [color]
  (assoc x :fill-color color))

(deftool delete-border []
  (dissoc x :line-color))

(deftool set-border-color [color]
  (assoc x :line-color color))

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
	     (let [clipi (:clip (get objs i))]
	       (if clipi
		 (if (= clipi obj-i)
		   false
		   (recur clipi))
		 true))))
    (assoc x :clip clip)
    x))

(deftool pick-style [masteri]
  (let [master (get objs masteri)]
    (merge (dissoc x :line-width :line-color :fill-color) (select-keys master [:line-width :line-color :fill-color]))))

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

(defn move-down [stack sel-objs]
  (loop [s stack
	 res []]
    (if (< 1 (count s))
      (let [[a b & d] s]
	(if (and ((set sel-objs) b) (not ((set sel-objs) a)))
	  (recur (cons a d) (conj res b))
	  (recur (cons b d) (conj res a))))
      (vec (concat res s)))))

(defn move-down-stack [{:keys [stack objs] :as scene} sel-objs]
  (assoc scene :stack (move-down stack sel-objs)))

(defn move-up-stack [{:keys [stack objs] :as scene} sel-objs]
  (assoc scene :stack (-> stack
			  reverse
			  (move-down sel-objs)
			  reverse
			  vec)))

(defn transform [{:keys [stack objs] :as scene} [scale :as transformation]]
  (assoc scene
    :objs (into {} (mapmap (fn [obj-i x]
			     (assoc (if (= (:type x) :sketch)
				      (assoc x :size (* scale (:size x)))
				      (assoc x :line-width (* scale (:line-width x))))
			       :ps (into {} (mapmap (fn [i p]
						      (transform-p p transformation))
						    (:ps x)))))
			   objs))))