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

(defn avg-color [ca cb foo]
  (if (or ca cb)
    (map (fn [a b]
           (int (+ (* b foo) (* a (- 1 foo)))))
         (or ca (assoc cb 3 0))
         (or cb (assoc ca 3 0)))))

(defn expand-sibling [{:keys [sibling ps ls softs steps sibling] :as x} xs]
  (if sibling
    (let [sib (get xs sibling)]
      (if-not steps
        [(dissoc x :sibling)]
        (for [foo (range steps)
              :let [bar (/ foo steps)]]
          (assoc (dissoc x :sibling)
            :fill-color (avg-color (:fill-color x) (:fill-color sib) bar)
            :line-color (avg-color (:line-color x) (:line-color sib) bar)
            :line-width (+ (* (- 1 bar) (:line-width x)) (* bar (:line-width sib)))
            :ps (into {} (map (fn [ia ib]
                                [ia (avg-point (get (:ps sib) ib) (get ps ia) bar)])
                              ls
                              (:ls sib)))))))
    [x]))

(defn paint [g {:keys [fill-color line-color line-width] :as x} xs]
  (draw g (get-polygon x xs [0 0])
        (style :background (if fill-color
                             (apply color fill-color))
               :foreground (if line-color
                             (apply color line-color))
               :stroke (stroke :width line-width
                               :cap :round
                               :join :round))))



(defn expand-sel [x color xs]
  (expand-sibling (dissoc (assoc x :line-color color :line-width 1) :clip :fill-color) xs))

(defn gather-sels [selection color xs]
  (apply concat (for [i (keys selection)]
                  (expand-sel (get xs i) color xs))))

(defn gather-objs [{:keys [layers layers-ord objs]}]
  (for [layeri layers-ord
        i (:stack (get layers layeri))
        :when (:view (get layers layeri))
        obj (expand-sibling (get objs i) objs)]
    obj))

(defn render-objs [g paint-objs objs]
  (doseq [obj paint-objs]
    (paint g obj objs)))

(defn render-raw [g scene]
  (render-objs g (gather-objs scene) scene))

(defn render [g {:keys [trans mode action action-start selection export] :as state} mousep]
  (let [state2 (mp (md state mousep) mousep)
        {:keys [objs] :as scene} (transform (:scene state2) trans)
        paint-objs (concat (gather-objs scene)
                           (if (= mode :object)
                             (concat (gather-sels (:selection state2) [200 0 200 255] objs)
                                     (gather-sels selection [255 200 0 255] objs))))]
    (render-objs g paint-objs objs)
    (if (= mode :mesh)
      (doseq [obj-i (keys selection) :let [x (get objs obj-i)]]
        (paint-handles g x)))
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