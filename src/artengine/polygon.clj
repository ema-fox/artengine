(ns artengine.polygon
  (:use [artengine util edit])
  (:import [java.awt.geom Area GeneralPath]))

(defn clip-polygon [pol1 pol2]
  (doto (Area. pol2)
    (.intersect (Area. pol1))))

(defn move-to [path [p0 p1]]
  (.moveTo path p0 p1))

(defn quad-to [path [pa0 pa1] [pb0 pb1]]
  (.quadTo path pa0 pa1 pb0 pb1))

(defn line-to [path [p0 p1]]
  (.lineTo path p0 p1))

(defn make-path [{:keys [ps ls softs] :as x}]
  (let [path (GeneralPath.)]
    (when (second ps)
      (move-to path (avg-point (get ps (first ls)) (get ps (second ls)) 0.5))
      (dorun (map (fn [ia ib ic]
		    (let [pa (get ps ia)
			  pb (get ps ib)
			  pc (get ps ic)]
		      (if (get softs ib)
			(quad-to path pb (avg-point pb pc 0.5))
			(do
			  (line-to path pb)
			  (line-to path (avg-point pb pc 0.5))))))
		  ls
		  (concat (rest ls) (take 1 ls))
		  (concat (drop 2 ls) (take 2 ls)))))
    path))

(defn get-polygon [{:keys [clip] :as x} xs]
  (let [pol (make-path x)]
    (if clip
      (clip-polygon pol (get-polygon (get xs clip) xs))
      pol)))
