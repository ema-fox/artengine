(ns artengine.polygon
  (:use [artengine util]
        seesaw.graphics)
  (:import [java.awt BasicStroke Shape]
           [java.awt.geom Area FlatteningPathIterator PathIterator GeneralPath]))

(set! *warn-on-reflection* true)

(def shape-cache (ref {}))

(defn clean-shape-cache [keys]
  (alter shape-cache select-keys (mapcat (fn [k] [[k true]
                                                  [k false]])
                                         keys)))
(defn bla [a]
  (let [x (* 2 a)]
    (+ x (* x (- 1 x)))))

(defn quork [p d m]
  (plus p (mult (dir<-arc d) m)))

(defn distances [ps]
  (let [ds (reduce (fn [xs [pa pb]]
                     (conj xs (+ (peek xs) (distance pa pb))))
                   [0]
                   (partition 2 1 ps))
        end (peek ds)
        end (if (== end 0) 1 end)]
    (map (fn [p d]
           [p (/ d end)])
         ps
         ds)))

(defn make-stroke [xss line-width line-middle-width]
  (for [xs xss]
    (let [dirs (map (fn [[pa pb]]
                      (arc<-dir (direction pa pb)))
                    (partition 2 1 xs))
          [as bs] (reduce (fn [[as bs] [p dist dir]]
                            (let [dir1 (+ dir (/ tau 4))
                                  dir2 (- dir (/ tau 4))
                                  blarg (+ (* 0.5 line-width) (* line-middle-width (bla dist)))]
                              [(conj as (quork p dir1 blarg))
                               (conj bs (quork p dir2 blarg))]))
                          [[] []]
                          (map conj (distances xs) (cons (first dirs) dirs)))]
      (concat as (reverse bs)))))

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

(defn move-to [^GeneralPath path [^Float p0 ^Float p1]]
  (.moveTo path p0 p1))

(defn line-to [^GeneralPath path [^Float p0 ^Float p1]]
  (.lineTo path p0 p1))

(defn get-stroke [^Shape pol line-width line-middle-width]
  (let [foo (make-stroke (read-path-iterator (FlatteningPathIterator. (.getPathIterator ^Shape pol nil) 0.1))
                         line-width line-middle-width)
        path (GeneralPath.)]
    (doseq [[p & ps] foo]
      (move-to path p)
      (doseq [p ps]
        (line-to path p)))
    path))

(defn clip-polygon [pol1 pol2 line-width line-middle-width]
  (doto (Area. pol2)
    (.intersect (Area. pol1))
    (.subtract (Area. (get-stroke pol2 line-width line-middle-width)))))

(defn quad-to [^GeneralPath path [^Float pa0 ^Float pa1] [^Float pb0 ^Float pb1]]
  (.quadTo path pa0 pa1 pb0 pb1))

(defn make-path-foo [[p & ps]]
  (concat [[p]]
          (map (fn [pa pb]
                 [pa (avg-point pa pb 0.5)])
               ps
               (butlast (rest ps)))
          [(take-last 2 ps)]))

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

(defn make-path [{:keys [ps ls softs closed]} continous]
  (let [bs (map (fn [i]
                  [(get ps i) (get softs i)])
                ls)
        cs (if closed
             (let [bla [[(avg-point (get ps (first ls)) (get ps (last ls)) 0.5) false]]]
               (concat bla bs bla))
             bs)
        ds (map make-path-foo (make-path-bar cs))]
    (make-path-qux (apply concat (first ds) (map (if continous
                                                   rest
                                                   identity)
                                                 (rest ds))))))

(defn get-polygon [{:keys [clip] :as x} xs continous]
  (if-not (get @shape-cache [x continous])
    (let [pol (make-path x continous)]
      (dosync
       (alter shape-cache assoc
              [x continous] (if clip
                              (let [clipobj (get xs clip)]
                                (clip-polygon pol (get-polygon clipobj xs true) (:line-width clipobj) (:line-middle-width clipobj)))
                              pol)))))
  (get @shape-cache [x continous]))
