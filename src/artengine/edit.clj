(ns artengine.edit
  (:use [artengine util polygon]
	[clojure.set])
  (:import [java.awt.geom Area]))


(def actions (ref {}))

(defmacro defact [name body]
  `(dosync
    (alter actions assoc ~name (fn ~'[scene selection pa pb] ~body))))

(defmacro deftool [name args body]
  `(defn ~name [{:keys [~'objs] :as ~'scene} ~'selection ~@args]
     (assoc ~'scene
       :objs (into ~'objs (mapmap (fn [~'obj-i ~'selis]
                                    (let [~'x (get ~'objs ~'obj-i)]
				      ~body))
				  ~'selection)))))

(defn get-new-key [xs]
  (if (first xs)
    (inc (apply max (keys xs)))
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
  (let [foo (map (fn [[ia ib] i]
                   [(let [pa (get ps ia)
                          pb (get ps ib)]
                      (line-p-distance pa pb p))
                    (fn []
                      (let [newi (get-new-key ps)]
                        (retfn [(assoc x
                                  :ps (assoc ps newi p)
                                  :softs (assoc softs newi (or (get softs ia) (get softs ib)))
                                  :ls (insert-at i ls newi))
                                newi])))])
                 (get-ilines x)
                 (if closed
                   (range)
                   (rest (range))))]
    (if closed
      foo
      (conj foo
            [(distance p (get ps (first ls)))
             (fn []
               (let [newi (get-new-key ps)]
                 (retfn [(assoc x
                           :ps (assoc ps newi p)
                           :softs (assoc softs newi (get softs (first ls)))
                           :ls (concat [newi] ls))
                         newi])))]
            [(distance p (get ps (last ls)))
             (fn []
               (let [newi (get-new-key ps)]
                 (retfn [(assoc x
                           :ps (assoc ps newi p)
                           :softs (assoc softs newi (get softs (last ls)))
                           :ls (concat ls [newi]))
                         newi])))]))))

(defn index-of [xs x]
  (some identity (map-indexed (fn [i y]
                                (if (= y x)
                                  i))
                              xs)))

(defn extend-sibling [{:keys [ps ls softs] :as x} i]
  (let [newi (get-new-key ps)
        ia (nth ls i)
        ib (nth ls (mod (dec i) (count ls)))]
    (assoc x
      :ps (assoc ps newi (avg-point (get ps ia) (get ps ib) 0.5))
      :softs (assoc softs newi (or (get softs ia) (get softs ib)))
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
                 lowest-scored)]
    (if foo
      (foo)
      [scene [nil]])))

(defn append [{:keys [ps ls softs] :as x} p dist]
  (if (< (distance p (get ps (first ls))) dist)
    (assoc x :closed true)
    (let [newi (get-new-key ps)]
      (assoc x
	:ls (conj (vec ls) newi)
	:softs (assoc softs newi true)
	:ps (assoc ps newi p)))))

(defn add-to-stack [layers layeri obj-is]
  (assoc-in layers [layeri :stack] (concat (:stack (get layers layeri)) obj-is)))

(defn sibling [{:keys [layers objs] :as scene} selection layeri]
  (let [newis (take (count selection) (iterate inc (get-new-key objs)))
        newobjs (into objs (apply concat (map (fn [obj-i newi]
                                                (if-not (get-in objs [obj-i :sibling])
                                                  [[obj-i (assoc (get objs obj-i) :sibling newi)]
                                                   [newi (assoc (get objs obj-i)
                                                           :sibling obj-i
                                                           :steps 5)]]))
                                              (keys selection)
                                              newis)))
        newis2 (filter #(get newobjs %) newis)]
    [(assoc scene
       :layers (add-to-stack layers layeri newis2)
       :objs newobjs)
     newis2]))

(defn copy-helper [{:keys [sibling clip] :as x} foo]
  (let [newx (if-let [bla (get foo sibling)]
               (assoc x
                 :sibling bla)
               x)]
    (if-let [bla (get foo clip)]
      (assoc newx
        :clip bla)
      newx)))

(defn copy [{:keys [layers objs] :as scene} selection layeri]
  (let [newis (take (count selection) (iterate inc (get-new-key objs)))
        foo (into {} (map vector (keys selection) newis))]
    [(assoc scene
       :layers (add-to-stack layers layeri (keep #(get foo %) (mapcat (fn [[_ {:keys [stack]}]]
                                                                        stack)
                                                                      layers)))
       :objs (into objs (map (fn [[obj-i newi]]
			       [newi (copy-helper (get objs obj-i) foo)])
                             foo)))
     newis]))

(defn obj-from-points [ps]
  (let [ls (vec (range (count ps)))]
    {:ps (into {} (map vector ls ps))
     :softs (into {} (map #(vector % false) ls))
     :ls ls
     :closed false
     :line-color [0 0 0 255]
     :line-width 1
     :line-middle-width 1}))

(defn add-obj [{:keys [layers objs] :as scene} obj layeri]
  (let [newi (get-new-key objs)]
    (assoc scene
      :objs (assoc objs newi obj)
      :layers (add-to-stack layers layeri [newi]))))

(defn new-obj [scene p layeri]
  (add-obj scene (obj-from-points [p]) layeri))

(defn new-sketch [scene p layeri]
  (add-obj scene
           (assoc (obj-from-points [p])
             :line-width 20
             :line-color [0 0 0 30])
           layeri))

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

(deftool adjust-line-middle [amount]
  (assoc x
    :line-middle-width (* (:line-middle-width x) (Math/pow 0.9 amount))))

(defn fix-obj [{:keys [sibling] :as x} xs]
  (if (and sibling (not (get xs sibling)))
    (dissoc x :sibling)
    x))

(defn fix [{:keys [layers objs] :as scene}]
  (assoc scene
    :objs (into {} (mapmap (fn [obj-i x]
			     (fix-obj x objs))
			   objs))
    :layers (into {} (mapmap (fn [_ {:keys [stack] :as layer}]
                               (assoc layer
                                 :stack (vec (filter #(get objs %) stack))))
                             layers))))

(defn delete-objs [{:keys [objs] :as scene} selection]
  (fix (assoc scene
	 :objs (apply dissoc objs (keys selection)))))

;handle sibling case
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

(deftool obj-soft []
  (assoc x :softs (into {} (mapmap (constantly true)
                                   (:ps x)))))

(deftool obj-unsoft []
  (assoc x :softs (into {} (mapmap (constantly false)
                                   (:ps x)))))

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
    (div (reduce plus ps) (max 1 (count ps)))))

(defact [:rot :mesh]
  (let [origin (selection-ps-avg scene selection)]
    (rotate-ps-tool scene selection origin (rotate-factor origin pa pb))))

(defn selection-avg [{:keys [objs]} selection]
  (let [ps (for [obj-i (keys selection)
                 [i p] (:ps (get objs obj-i))]
             p)]
    (div (reduce plus ps) (max 1 (count ps)))))

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

(deftool scale-axis-tool [origin factor]
  (assoc x
    :ps (into {} (mapmap (fn [i p]
                           (let [[pa0 pa1] (minus p origin)]
                             (plus origin [(* pa0 factor) pa1])))
                         (:ps x)))))

(defact [:scale-axis :object]
  (let [origin (selection-avg scene selection)
        rot (rotate-factor origin (plus origin [1 0]) pb)]
    (-> scene
        (rotate-tool selection origin (* -1 rot))
        (scale-axis-tool selection origin (scale-factor origin pa pb))
        (rotate-tool selection origin rot))))

(deftool scale-steps-tool [origin factor]
  (if (:steps x)
    (assoc x
      :steps (max 2 (int (* factor (+ 0.5 (:steps x))))))
    x))

(defact [:scale-steps :object]
  (let [origin (selection-avg scene selection)]
    (scale-steps-tool scene selection origin (scale-factor origin pa pb))))

(defn intersect? [obja objb objs]
  (not (.isEmpty (doto (Area. (get-polygon obja objs))
                        (.intersect (Area. (get-polygon objb objs)))))))

(defn move-down [stack sel-objs objs]
  (let [sel? (set sel-objs)
        [as bs] (split-with #(not (sel? %)) stack)
        tail (filter #(not (sel? %)) bs)
        sels (filter sel? bs)
        [middle [bfmiddle & begin]] (split-with (fn [obj-i]
                                     (not (some #(intersect? (get objs obj-i) (get objs %) objs)
                                                sel-objs)))
                                   (reverse as))]
    (vec (concat (reverse begin) sels (if bfmiddle [bfmiddle]) (reverse middle) tail))))

(defn move-down-stack [{:keys [layers objs] :as scene} sel-objs layeri]
  (assoc-in scene [:layers layeri :stack] (move-down (get-in layers [layeri :stack])
                                                     (keys sel-objs)
                                                     objs)))

(defn move-up-stack [{:keys [layers objs] :as scene} sel-objs layeri]
  (assoc-in scene [:layers layeri :stack] (-> (get-in layers [layeri :stack])
                                              reverse
                                              (move-down (keys sel-objs) objs)
                                              reverse
                                              vec)))

(defn layer-move-down [layers layers-ord selection]
  (loop [s layers-ord
         layers layers]
    (let [[dest src] s]
      (if src
        (recur (rest s) (assoc layers
                          dest (assoc (get layers dest)
                                 :stack (concat (get-in layers [dest :stack])
                                                (filter #(selection %)
                                                        (get-in layers [src :stack]))))
                          src (assoc (get layers src)
                                :stack (filter #(not (selection %))
                                               (get-in layers [src :stack])))))
        layers))))

(defn move-down-layer [{:keys [layers layers-ord] :as scene} selection]
  (assoc scene
    :layers (layer-move-down layers layers-ord selection)))

(defn move-up-layer [{:keys [layers layers-ord] :as scene} selection]
  (assoc scene
    :layers (layer-move-down layers (reverse layers-ord) selection)))

(defn move-down-layers-ord [{:keys [layers-ord] :as scene} layeri]
  (let [si (index-of layers-ord layeri)
        di (max 0 (dec si))]
    (assoc scene :layers-ord (assoc (vec layers-ord)
                               si (nth layers-ord di)
                               di (nth layers-ord si)))))

(defn move-up-layers-ord [{:keys [layers-ord] :as scene} layeri]
  (let [si (index-of layers-ord layeri)
        di (min (dec (count layers-ord)) (inc si))]
    (assoc scene :layers-ord (assoc (vec layers-ord)
                               si (nth layers-ord di)
                               di (nth layers-ord si)))))

(defn new-layer [{:keys [layers layers-ord] :as scene} layeri]
  (let [newi (get-new-key layers)]
    [(assoc scene
       :layers (assoc layers newi {:stack []
                                   :name ""
                                   :edit true
                                   :view true})
       :layers-ord (apply concat (map #(if (= % layeri)
                                         [% newi]
                                         [%])
                                      layers-ord)))
     newi]))

(defn delete-layer [{:keys [layers layers-ord] :as scene} layeri]
  (if (second layers)
    (assoc (delete-objs scene (into {} (map (fn [i] [i #{}])
                                            (get-in layers [layeri :stack]))))
      :layers (dissoc layers layeri)
      :layers-ord (filter #(not= layeri %) layers-ord))
    scene))

(defn transform [{:keys [objs] :as scene} [scale :as transformation]]
  (assoc scene
    :objs (into {} (mapmap (fn [obj-i x]
			     (assoc x
                               :line-width (* scale (:line-width x))
			       :ps (into {} (mapmap (fn [i p]
						      (transform-p p transformation))
						    (:ps x)))))
			   objs))))

(defn mirror [{:keys [objs] :as scene} [m0 m1]]
  (assoc scene
    :objs (into {} (mapmap (fn [_ {:keys [ps] :as x}]
                             (assoc x
                               :ps (into {} (mapmap (fn [_ [p0 p1]]
                                                      [(+ (* -1 (- p0 m0)) m0) p1])
                                                    ps))))
                           objs))))

(defn join-helper2 [obja objb]
  (let [newis (take (count (:ls objb)) (iterate inc (get-new-key (:ps obja))))]
    (assoc obja
      :ls (concat (:ls obja) newis)
      :ps (into (:ps obja) (map (fn [newi oldi]
                                  [newi (get-in objb [:ps oldi])])
                                newis
                                (:ls objb))))))

(defn every&rest [xs]
  (for [i (range (count xs))]
    [(nth xs i) (concat (take i xs) (drop (inc i) xs))]))

(defn join-helper [objs]
  (let [[x & xs] objs]
    (if (seq xs)
      (recur ((->> (for [[y ys] (every&rest xs)
                            flippedx [true false]
                            flippedy [true false]]
                        [(distance (get-in x [:ps ((if flippedx
                                                     first
                                                     last)
                                                   (:ls x))])
                                   (get-in y [:ps ((if flippedy
                                                     last
                                                     first)
                                                   (:ls y))]))
                         #(conj ys (join-helper2 (if flippedx
                                                   (update-in x [:ls] reverse)
                                                   x)
                                                 (if flippedy
                                                   (update-in y [:ls] reverse)
                                                   y)))])
                      (sort-by first)
                      first
                      second)))
      x)))

(defn join-objs [{:keys [objs] :as scene} selection]
  (fix (assoc scene
         :objs (let [selobjs (keys selection)]
                 (assoc (apply dissoc objs (rest selobjs))
                   (first selobjs) (join-helper (map objs selobjs)))))))
