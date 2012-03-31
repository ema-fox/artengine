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

(defn make-path [ps]
  (let [path (GeneralPath.)]
    (when (second ps)
      (move-to path (avg-point (first ps) (second ps) 0.5))
      (dorun (map (fn [pa pb pc]
		    (if (< (Math/abs (- (first (avec<-dvec (minus pa pb)))
					(first (avec<-dvec (minus pb pc))))) 1)
		      (quad-to path pb (avg-point pb pc 0.5))
		      (do
			(line-to path pb)
			(line-to path (avg-point pb pc 0.5)))))
		  ps
		  (concat (rest ps) (take 1 ps))
		  (concat (drop 2 ps) (take 2 ps)))))
    path))

(defn get-polygon [{:keys [ps ls decos clip] :as x} xs]
  (let [pol (make-path (map #(get ps %) ls))]
    (if clip
      (clip-polygon pol (get-polygon (get xs clip) xs))
      pol)))
