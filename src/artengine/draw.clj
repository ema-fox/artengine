(ns artengine.draw
  (:use [artengine util polygon edit mouse selection]
        [seesaw color graphics]))

(defn paint-handle [g p]
  (set-color g [255 255 255 255])
  (fill-rect g (minus p [1 1]) [3 3])
  (set-color g [0 0 0 255])
  (fill-rect g p [1 1]))

(defn paint-solid-handle [g p]
  (fill-rect g (minus p [1 1]) [3 3]))

(defn paint-handles [g {:keys [ps]}]
  (doseq [[i p] ps]
    (paint-handle g p)))

(declare paint)

(defn paint-sibling [g {:keys [sibling ps ls softs steps] :as x} xs]
  (let [sib (get xs sibling)]
    (if-not steps
      (paint g (dissoc x :sibling) xs)
      (doseq [foo (range steps)]
        (paint g (assoc (dissoc x :sibling)
                   :ps (into {} (map (fn [ia ib]
                                       [ia (avg-point (get (:ps sib) ib) (get ps ia) (/ foo steps))])
                                     ls
                                     (:ls sib))))
               xs)))))

(defn paint [g {:keys [ps ls closed clip fill-color line-color line-width sibling] :as x} xs]
  (if sibling
    (paint-sibling g x xs)
    (draw g (get-polygon x xs)
          (style :background (if fill-color
                               (apply color fill-color))
                 :foreground (if line-color
                               (apply color line-color))
                 :stroke (stroke :width line-width
                                 :cap :round
                                 :join :round)))))

(defn paint-sel [g x color xs]
  (paint g (dissoc (assoc x :line-color color :line-width 1) :clip :fill-color) xs))


(defn render-raw [g {:keys [layers layers-ord objs]}]
  (doseq [layeri layers-ord
          i (:stack (get layers layeri))
          :when (:view (get layers layeri))
          :let [x (get objs i)]]
    (paint g x objs)))


(defn render [g {:keys [trans mode action action-start selection export] :as state} mousep]
  (let [{:keys [objs] :as scene} (transform (:scene (mp (md state mousep) mousep)) trans)]
    (render-raw g scene)
    (if (= mode :object)
      (if-let [sel (get objs (first (keys (select-obj scene (transform-p mousep trans) 20))))]
        (paint-sel g sel [200 0 200 255] objs)))
    (doseq [obj-i (keys selection) :let [x (get objs obj-i)]]
      (if (= mode :mesh)
        (paint-handles g x)
        (paint-sel g x [250 200 0 255] objs)))
    (set-stroke-width g 1)
    (if export
      (apply draw-rect g (selected-bbox objs selection)))
    (set-color g [250 200 0 255])
    (when (= mode :mesh)
      (doseq [[obj-i is] selection
              i is
              :let [p (get (:ps (get objs obj-i)) i)]]
        (paint-solid-handle g p))))
  (if (= action :select)
    (draw-rect g (transform-p action-start trans) (transform-p mousep trans)))
  (set-color g [0 0 0 255])
  (.drawString g (str mode) 10 20)
  (.drawString g (str action) 10 40))