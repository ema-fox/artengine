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

(defn get-keylines [key {:keys [closed] ls key}]
  (map vector
       ls
       (if (and closed (< 1 (count ls)))
	 (cons (last ls) (butlast ls))
	 (rest ls))))

(defmulti get-ilines :type)

(defmethod get-ilines :interpolation
  [x]
  (concat (get-keylines :las x) (get-keylines :lbs x)))

(defmethod get-ilines :default
  [x]
  (get-keylines :ls x))

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

(defmulti interpolate-obj :type)

(defmethod interpolate-obj :default
  [x]
  [x []])

(defmethod interpolate-obj :path
  [{:keys [ls ps] :as x}]
  (let [newis (take (count ps) (iterate inc (get-new-key ps)))]
    [(assoc (dissoc x :ls)
       :type :interpolation
       :las ls
       :lbs newis
       :steps 5
       :ps (into ps (map (fn [oldi newi]
			   [newi (get ps oldi)])
			 ls newis)))
     newis]))

(defn interpolate [{:keys [objs] :as scene} selection]
  (let [foo (mapmap (fn [obj-i _]
		      (interpolate-obj (get objs obj-i)))
		    selection)]
    [(assoc scene
       :objs (into objs (mapmap (fn [obj-i [x _]]
				  x)
				foo)))
     (into {} (mapmap (fn [obj-i [_ selis]]
			(set selis))
		      foo))]))

(defmulti extend-obj (fn [x p retf] (:type x)))

;this fn is way to big
(defmethod extend-obj :interpolation
  [{:keys [ps closed softs] :as x} p retfn]
  (apply concat (map (fn [prim sec]
                       (let [{prims prim
                              secs sec} x]
                         (map (fn [[ia ib] i]
                                [(let [pa (get ps ia)
                                       pb (get ps ib)]
                                   (line-p-distance pa pb p))
                                 (fn []
                                   (let [newi (get-new-key ps)]
                                     (retfn [(assoc x
                                               :ps (assoc ps
                                                     newi p
                                                     (inc newi) (avg-point (get ps (nth secs i))
                                                                           (get ps (nth secs (mod (dec i) (count secs))))
                                                                           0.5))
                                               :softs (assoc softs newi false (inc newi) false)
                                               prim (insert-at i prims newi)
                                               sec (insert-at i secs (inc newi)))
                                             newi])))])
                              (get-keylines prim x)
                              (if closed
                                (range)
                                (rest (range))))))
                     [:las :lbs]
                     [:lbs :las])))

(defmethod extend-obj :path
  [{:keys [ps ls closed softs] :as x} p retfn]
  (map (fn [[ia ib] i]
	 [(let [pa (get ps ia)
                pb (get ps ib)]
            (line-p-distance pa pb p))
          (fn []
            (let [newi (get-new-key ps)]
              (retfn [(assoc x
                        :ps (assoc ps newi p)
                        :softs (assoc softs newi false)
                        :ls (insert-at i ls newi))
                      newi])))])
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

(defn append [{:keys [ps ls softs] :as x} p dist]
  (if (< (distance p (get ps (first ls))) dist)
    (assoc x :closed true)
    (let [newi (get-new-key ps)]
      (assoc x
	:ls (conj (vec ls) newi)
	:softs (assoc softs newi false)
	:ps (assoc ps newi p)))))

(defn copy [{:keys [stack objs] :as scene} selection]
  (let [newis (take (count selection) (iterate inc (get-new-key objs)))]
    [(assoc scene
       :stack (into stack newis)
       :objs (into objs (map (fn [obj-i newi]
			       [newi (get objs obj-i)])
			     (keys selection)
			     newis)))
     newis]))

(defn new-obj [{:keys [stack objs] :as scene} p]
  (let [newi (get-new-key objs)]
    (assoc scene
      :objs (assoc objs newi {:ps {1 p}
			      :softs {1 false}
			      :ls [1]
			      :closed false
			      :line-color [0 0 0 255]
			      :line-width 1
			      :type :path})
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

(defn delete-objs [{:keys [objs] :as scene} selection]
  (fix (assoc scene
	 :objs (apply dissoc objs (keys selection)))))

(defn delete-ps [{:keys [ps ls] :as x} selis]
  (assoc x
    :ps (apply dissoc ps selis)
    :ls (filter #(not (contains? selis %)) ls)))

(defn delete [{:keys [objs] :as scene} selection]
  (let [foo (mapmap (fn [obj-i selis]
		      (delete-ps (get objs obj-i) selis))
		    selection)]
    (delete-objs (assoc scene :objs (into objs foo))
		 (into {} (filter #(empty? (:ps (second %))) foo)))))

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

(deftool soft []
  (assoc x :softs (into (:softs x) (map (fn [i]
					  [i true])
					selis))))

(deftool unsoft []
  (assoc x :softs (into (:softs x) (map (fn [i]
					  [i false])
					selis))))

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

(deftool rotate-tool [rot-p1 rot-p2 p]
  (let [[a1 _] (avec<-dvec (minus rot-p2 rot-p1))
	[a2 _] (avec<-dvec (minus p rot-p1))]
    (assoc x :ps (rotate-ps (:ps x) rot-p1 (- a2 a1)))))

(defn selection-avg [{:keys [objs]} selection]
  (let [ps (for [obj-i (keys selection)
                 [i p] (:ps (get objs obj-i))]
             p)]
    (div (reduce plus ps) (count ps))))

(defn rotate-objs [scene selection pa pb]
  (rotate-tool scene selection (selection-avg scene selection) pa pb))

(deftool scale-tool [origin factor]
  (assoc x :ps (into {} (mapmap (fn [i p]
                                  (plus origin (mult (minus p origin) factor)))
                                (:ps x)))))

(defn scale-objs [scene selection pa pb]
  (let [origin (selection-avg scene selection)]
    (scale-tool scene selection origin (/ (distance origin pb) (distance origin pa)))))

(deftool scale-steps-tool [origin factor]
  (if (:steps x)
    (assoc x
      :steps (max 2 (int (* factor (+ 0.5 (:steps x))))))
    x))

(defn scale-steps [scene selection pa pb]
  (let [origin (selection-avg scene selection)]
    (scale-steps-tool scene selection origin (/ (distance origin pb) (distance origin pa)))))

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