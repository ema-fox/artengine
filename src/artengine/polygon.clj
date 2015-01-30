(ns artengine.polygon
  (:use [artengine util]
        seesaw.graphics)
  (:import [java.awt BasicStroke Shape]
           [java.awt.geom Area FlatteningPathIterator PathIterator GeneralPath]))

(set! *warn-on-reflection* true)

(def shape-cache (ref {}))

(defn clean-shape-cache [keys]
  (alter shape-cache select-keys (mapcat (fn [k] [[k :filling]
                                                  [k :stroke]])
                                         keys)))
(defn bla [a]
  (let [x (* 2 a)]
    (+ x (* x (- 1 x)))))

(defn quork [p d m]
  (plus p (mult (dir<-arc d) m)))

(defn distances-quux [ps]
  (let [ds (reduce (fn [xs [pa pb]]
                     (conj xs (+ (peek xs) (distance pa pb))))
                   [0]
                   (partition 2 1 ps))
        end (peek ds)
        end (if (== end 0) 1 end)]
    (map #(/ % end)
         ds)))

(defn move-to [^GeneralPath path [^Float p0 ^Float p1]]
  (.moveTo path p0 p1))

(defn line-to [^GeneralPath path [^Float p0 ^Float p1]]
  (.lineTo path p0 p1))

(defn quad-to [^GeneralPath path [^Float pa0 ^Float pa1] [^Float pb0 ^Float pb1]]
  (.quadTo path pa0 pa1 pb0 pb1))

(defn cubic-to [^GeneralPath path [^Float pa0 ^Float pa1] [^Float pb0 ^Float pb1] [^Float pc0 ^Float pc1]]
  (.curveTo path pa0 pa1 pb0 pb1 pc0 pc1))

(defn add-midpoints [ps xs]
  (concat (mapcat (fn [[pa pb] x]
                    (if x
                      [pa (avg-point pa pb 0.5)]
                      [pa]))
                  (partition 2 1 ps)
                  xs)
          [(last ps)]))

(defn make-stroke-quux [xss line-width line-middle-width]
  (let [path (GeneralPath.)]
    (doseq [xs xss]
      (let [[pa pb] xs
            py (nth xs (- (count xs) 2))
            pz (nth xs (- (count xs) 1))
            ys (concat (let [dir (direction pb pa)]
                         [(plus pa (mult dir line-width)) ;; helper point
                          (plus pa (mult dir (* 0.5 line-width)))]) ;; end-cap point
                       xs
                       (let [dir (direction py pz)]
                         [(plus pz (mult dir (* 0.5 line-width)))
                          (plus pz (mult dir line-width))]))
            [as bs] (reduce (fn [[as bs] [dist pa pb pc]]
                              (let [dir1 (+ (arc<-dir (direction [0 0] (plus (direction pa pb)
                                                                             (direction pb pc))))
                                            (/ tau 4))
                                    dir2 (+ dir1 (/ tau 2))
                                    blarg (+ (* 0.5 line-width) (* line-middle-width (bla dist)))]
                                [(conj as (quork pb dir1 blarg))
                                 (conj bs (quork pb dir2 blarg))]))
                            [[] []]
                            (map cons
                                 (distances-quux (rest (butlast ys)))
                                 (partition 3 1 ys)))
            loopp (avg-point (first as) (first bs) 0.5)]
        (move-to path loopp)
        (doseq [[pa pb] (partition 2 (concat (add-midpoints (concat as (reverse bs))
                                                            (concat [false false]
                                                                    (repeat (- (count as) 5) true)
                                                                    [false false true false false]
                                                                    (repeat (- (count bs) 5) true)
                                                                    [false false]))
                                             [loopp]))]
          (quad-to path pa pb))))
    path))

(defn read-path-iterator [^FlatteningPathIterator it]
  (loop [res ()]
    (if (.isDone it)
      res
      (recur (let [fl (float-array 2)
                   r (.currentSegment it fl)]
               (condp = r
                 PathIterator/SEG_MOVETO
                 (do
                   (.next it)
                   (conj res [fl]))
                 PathIterator/SEG_LINETO
                 (do
                   (.next it)
                   (conj (rest res) (conj (first res) fl)))
                 PathIterator/SEG_CLOSE
                 (do
                   (.next it)
                   res)))))))

(defn make-path-foo [[p & ps]]
  (concat [[p]]
          (partition 2 (add-midpoints ps (concat (repeat (- (count ps) 2) true)
                                                 [false])))))

(defn partition-on [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [run (cons (first s) (take-while #(not (f %)) (rest s)))]
       (cons run (partition-on f (drop (count run) s)))))))

(defn make-path-bar [ps]
  (let [foo (->> (butlast ps)
                 (partition-on (fn [[p soft]]
                                 (not soft)))
                 (map #(map first %)))]
    (->> (concat foo [[(first (last ps))]])
         (partition 2 1)
         (map (fn [[[pa & ps] [pb]]]
                (cons pa (concat (or (seq ps) (list (avg-point pa pb 0.5))) [pb])))))))

(defn make-path-qux [ps]
  (let [path (GeneralPath.)]
    (doseq [[pa pb] ps]
      (if pb
        (quad-to path pa pb)
        (move-to path pa)))
    path))

(defn make-closed [xs]
  (if (second (first xs))
    (if (second (last xs))
      (let [loopp [[(avg-point (first (first xs)) (first (last xs)) 0.5) false]]]
        (concat loopp xs loopp))
      (concat [(last xs)] xs))
    (concat xs [(first xs)])))

(defn path-qux [{:keys [ps ls softs closed]}]
  (let [bs (map (fn [i]
                  [(get ps i) (get softs i)])
                ls)]
    (make-path-bar (if closed
                     (make-closed bs)
                     bs))))

(defn make-path [x]
  (let [ds (map make-path-foo (path-qux x))]
    (make-path-qux (apply concat (first ds) (map rest (rest ds))))))

(defn clip-polygon [pol1 pol2 stroke]
  (doto (Area. pol2)
    (.intersect (Area. pol1))
    (.subtract (Area. stroke))))

(defn cached-under [cache key f]
  (if-not (get @cache key)
    (dosync
     (alter cache assoc key (f))))
  (get @cache key))

(declare ensure-clipped)

(defn get-stroke-polygon [{:keys [line-width line-middle-width clip] :as x} xs]
  (cached-under shape-cache [x :stroke]
                (fn []
                  (ensure-clipped (make-stroke-quux (path-qux x) line-width line-middle-width) clip xs))))

(defn get-polygon [{:keys [clip] :as x} xs]
  (cached-under shape-cache [x :filling]
                (fn []
                  (ensure-clipped (make-path x) clip xs))))

(defn ensure-clipped [pol clip xs]
  (if clip
    (let [clipobj (get xs clip)]
      (clip-polygon pol (get-polygon clipobj xs) (get-stroke-polygon clipobj xs)))
    pol))
