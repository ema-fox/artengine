(ns artengine.edit
  (:use [artengine.util]
	[clojure.set]))

(def actions (ref {}))

(defmacro defact [name body]
  `(dosync
    (alter actions assoc ~name (fn ~'[scene selection pa pb] ~body))))

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

(defn get-ilines [{:keys [closed ls]}]
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

(defn extend-obj [{:keys [ps ls closed softs] :as x} p retfn]
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

(defn index-of
  "when x is not in xs returns (count xs)"
  [xs x]
  (count (take-while #(not= x %) xs)))

(defn extend-sibling [{:keys [ps ls] :as x} i]
  (let [newi (get-new-key ps)]
    (assoc x
      :ps (assoc ps
            newi (avg-point (get ps (nth ls i)) (get ps (nth ls (dec i))) 0.5))
      :ls (insert-at i ls newi))))

(defn extend-objs [{:keys [stack objs] :as scene} obj-is p]
  (let [foo (->> (for [obj-i obj-is]
		   (extend-obj (get objs obj-i) p
                               (fn [[x i]]
                                 [(assoc scene
                                    :objs (if-let [sib (:sibling x)]
                                            (assoc objs
                                              obj-i x
                                              sib (extend-sibling (get objs sib)
                                                                  (index-of (:ls x) i)))
                                            (assoc objs
                                              obj-i x)))
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

(defn sibling [{:keys [stack objs] :as scene} selection]
  (let [newis (take (count selection) (iterate inc (get-new-key objs)))]
    [(assoc scene
       :stack (into stack newis)
       :objs (into objs (apply concat (map (fn [obj-i newi]
                                             [[obj-i (assoc (get objs obj-i) :sibling newi)]
                                              [newi (assoc (get objs obj-i)
                                                      :sibling obj-i
                                                      :steps 5)]])
                                           (keys selection)
                                           newis))))
     newis]))

(defn copy-helper [{:keys [sibling clip] :as x} foo]
  (let [newx (if-let [bla (get foo sibling)]
               (assoc x
                 :sibling bla)
               x)]
    (if-let [bla (get foo clip)]
      (assoc newx
        :clip bla)
      newx)))

(defn copy [{:keys [stack objs] :as scene} selection]
  (let [newis (take (count selection) (iterate inc (get-new-key objs)))
        foo (into {} (map vector (keys selection) newis))]
    [(assoc scene
       :stack (into stack (keep #(get foo %) stack))
       :objs (into objs (map (fn [[obj-i newi]]
			       [newi (copy-helper (get objs obj-i) foo)])
                             foo)))
     newis]))

(defn new-obj [{:keys [stack objs] :as scene} p]
  (let [newi (get-new-key objs)]
    (assoc scene
      :objs (assoc objs newi {:ps {1 p}
			      :softs {1 false}
			      :ls [1]
			      :closed false
			      :line-color [0 0 0 255]
			      :line-width 1})
      :stack (conj stack newi))))

(defn new-sketch [{:keys [stack objs] :as scene} p]
  (let [newi (get-new-key objs)]
    (assoc scene
      :objs (assoc objs newi {:ps {1 p} :ls [1] :line-width 20 :line-color [0 0 0 30]})
      :stack (conj stack newi))))

(deftool end-sketch [p]
  (assoc x
    :ps (assoc (:ps x) 2 p)
    :ls (conj (:ls x) 2)))

(deftool close []
  (assoc x :closed true))

(deftool unclose []
  (assoc x :closed false))

(deftool adjust-line [amount]
  (assoc x
    :line-width (* (:line-width x) (Math/pow 0.9 amount))))

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

(defact [:move :mesh]
  (move scene selection (minus pb pa)))

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

(defact [:move :object]
  (move-objs scene selection (minus pb pa)))

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

(defn rotate-factor [origin pa pb]
  (let [[a1 _] (avec<-dvec (minus pa origin))
        [a2 _] (avec<-dvec (minus pb origin))]
    (- a2 a1)))

(deftool rotate-ps-tool [origin factor]
  (assoc x
    :ps (into (:ps x) (map (fn [i]
                             [i (plus origin (dvec<-avec (let [[a dist] (avec<-dvec (minus (get (:ps x) i) origin))]
                                                           [(+ a factor) dist])))])
                           selis))))

(deftool rotate-tool [origin factor]
  (assoc x
    :ps (into {} (mapmap (fn [i p]
                           (let [[a dist] (avec<-dvec (minus p origin))]
                             (plus origin (dvec<-avec [(+ a factor) dist]))))
                         (:ps x)))))

(defn selection-ps-avg [{:keys [objs]} selection]
  (let [ps (for [[obj-i selis] selection
                 [i p] (:ps (get objs obj-i))
                 :when (selis i)]
             p)]
    (div (reduce plus ps) (count ps))))

(defact [:rot :mesh]
  (let [origin (selection-ps-avg scene selection)]
    (rotate-ps-tool scene selection origin (rotate-factor origin pa pb))))

(defn selection-avg [{:keys [objs]} selection]
  (let [ps (for [obj-i (keys selection)
                 [i p] (:ps (get objs obj-i))]
             p)]
    (div (reduce plus ps) (count ps))))

(defact [:rot :object]
  (let [origin (selection-avg scene selection)]
    (rotate-tool scene selection origin (rotate-factor origin pa pb))))

(deftool scale-ps-tool [origin factor]
  (assoc x :ps (into (:ps x) (map (fn [i]
                                    [i (plus origin (mult (minus (get (:ps x) i) origin) factor))])
                                  selis))))

(defn scale-factor [origin pa pb]
  (/ (distance origin pb) (distance origin pa)))

(defact [:scale :mesh]
  (let [origin (selection-ps-avg scene selection)]
    (scale-ps-tool scene selection origin (scale-factor origin pa pb))))

(deftool scale-tool [origin factor]
  (assoc x :ps (into {} (mapmap (fn [i p]
                                  (plus origin (mult (minus p origin) factor)))
                                (:ps x)))))

(defact [:scale :object]
  (let [origin (selection-avg scene selection)]
    (scale-tool scene selection origin (scale-factor origin pa pb))))

(deftool scale-steps-tool [origin factor]
  (if (:steps x)
    (assoc x
      :steps (max 2 (int (* factor (+ 0.5 (:steps x))))))
    x))

(defact [:scale-steps :object]
  (let [origin (selection-avg scene selection)]
    (scale-steps-tool scene selection origin (scale-factor origin pa pb))))

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