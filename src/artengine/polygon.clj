(ns artengine.polygon
  (:use [artengine util]
        seesaw.graphics)
  (:import [java.awt BasicStroke]
           [java.awt.geom Area GeneralPath]))

(set! *warn-on-reflection* true)

(def shape-cache (ref {}))

(defn clean-shape-cache [keys]
  (alter shape-cache select-keys keys))

(defn clip-polygon [pol1 pol2 stroke-width]
  (doto (Area. pol2)
    (.intersect (Area. pol1))
    (.subtract (Area. (.createStrokedShape (BasicStroke. (max 0 (- stroke-width 0.01))
                                                         BasicStroke/CAP_ROUND
                                                         BasicStroke/JOIN_ROUND)
                                           pol2)))))

(defn move-to [^GeneralPath path [^Float p0 ^Float p1]]
  (.moveTo path p0 p1))

(defn quad-to [^GeneralPath path [^Float pa0 ^Float pa1] [^Float pb0 ^Float pb1]]
  (.quadTo path pa0 pa1 pb0 pb1))

(defn line-to [^GeneralPath path [^Float p0 ^Float p1]]
  (.lineTo path p0 p1))

(defn make-path [{:keys [ps ls softs closed] :as x}]
  (let [foo #(if closed
               %
               (drop-last 2 %))
        path (GeneralPath.)]
    (when (second ps)
      (let [bar (avg-point (get ps (first ls)) (get ps (second ls)) 0.5)]
        (move-to path (if closed
                        bar
                        (get ps (first ls))))
        (if-not closed
          (line-to path bar)))
      (dorun (map (fn [ia ib ic]
		    (let [pa (get ps ia)
			  pb (get ps ib)
			  pc (get ps ic)]
		      (if (get softs ib)
			(quad-to path pb (avg-point pb pc 0.5))
			(do
			  (line-to path pb)
			  (line-to path (avg-point pb pc 0.5))))))
		  (foo ls)
		  (foo (concat (rest ls) (take 1 ls)))
		  (foo (concat (drop 2 ls) (take 2 ls)))))
      (if-not closed
        (line-to path (get ps (last ls)))))
    path))

(defn get-polygon [{:keys [clip] :as x} xs]
  (if-not (get @shape-cache x)
    (let [pol (make-path x)]
      (dosync
       (alter shape-cache assoc
              x (if clip
                  (clip-polygon pol (get-polygon (get xs clip) xs) (:line-width (get xs clip)))
                  pol)))))
  (get @shape-cache x))
