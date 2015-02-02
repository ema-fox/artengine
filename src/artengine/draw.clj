(ns artengine.draw
  (:use [artengine util polygon edit mouse selection]
        [clojure.java.io :only [file]]
        [seesaw color graphics])
  (:import [java.awt.geom FlatteningPathIterator PathIterator]
           [java.awt BasicStroke Shape Font]
           [java.awt.geom GeneralPath]
           [com.jogamp.opengl.util.awt TextRenderer]
           [com.jogamp.opengl.util.texture Texture TextureIO]
           [com.jogamp.common.nio Buffers]
           [java.nio FloatBuffer]
           [javax.media.opengl GL GLCapabilities GLProfile GL2 GLAutoDrawable]
           [javax.media.opengl.glu GLU GLUtessellatorCallback]))

(set! *warn-on-reflection* true)

(def tex-buffer (let [buf (Buffers/newDirectFloatBuffer 8)]
                  (doseq [f [0.0 1.0
                             0.0 0.0
                             1.0 0.0
                             1.0 1.0]]
                    (.put buf (float f)))
                  (.flip buf)
                  buf))

(def cache (ref {}))

(def tex-cache (ref {}))

(defn set-color [^GL2 gl [c0 c1 c2 c3]]
  (.glColor4f gl (float (/ c0 255.0)) (float (/ c1 255.0)) (float (/ c2 255.0)) (float (/ c3 255.0))))

(defn draw-rect [^GL2 gl [pa0 pa1] [pb0 pb1]]
  (doto gl
    (.glBegin GL/GL_LINE_LOOP)
    (.glVertex2d pa0 pa1)
    (.glVertex2d pa0 pb1)
    (.glVertex2d pb0 pb1)
    (.glVertex2d pb0 pa1)
    (.glEnd)))

(defn paint-handle [^GL2 gl [p0 p1] size]
  (set-color gl [255 255 255 255])
  (.glRectf gl (- p0 (* 2 size)) (- p1 (* 2 size)) (+ p0 (* 2 size)) (+ p1 (* 2 size)))
  (set-color gl [0 0 0 255])
  (.glRectf gl (- p0 size) (- p1 size) (+ p0 size) (+ p1 size)))

(defn paint-solid-handle [^GL2 gl [p0 p1] size]
  (.glRectf gl (- p0 (* 2 size)) (- p1 (* 2 size)) (+ p0 (* 2 size)) (+ p1 (* 2 size))))

(defn paint-handles [g {:keys [ps]} size]
  (doseq [[i p] ps]
    (paint-handle g p size)))

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

(defn tess-vertex [tess xs]
  (GLU/gluTessVertex tess (double-array xs) 0 xs))

(defn tess-recorder []
  (let [res (ref ())
        tess (GLU/gluNewTess)
        tessCB (proxy [GLUtessellatorCallback] []
                 (begin [type]
                   (dosync
                    (alter res conj [type])))
                 (end [])
                 (vertex [x]
                   (dosync
                    (ref-set res (conj (rest @res) (conj (first @res) x)))))
                 (combine [coords data weight ^objects outData]
                   (aset outData 0 (vec coords)))
                 (error [errnum]
                   (prn :error (.gluErrorString (GLU.) errnum))
                   (System/exit 1)))]
    (GLU/gluTessCallback tess GLU/GLU_TESS_VERTEX tessCB)
    (GLU/gluTessCallback tess GLU/GLU_TESS_BEGIN tessCB)
    (GLU/gluTessCallback tess GLU/GLU_TESS_END tessCB)
    (GLU/gluTessCallback tess GLU/GLU_TESS_ERROR tessCB)
    (GLU/gluTessCallback tess GLU/GLU_TESS_COMBINE tessCB)
    [tess (fn []
            (GLU/gluDeleteTess tess)
            @res)]))

(defn to-buffered [xss]
  (for [[type & xs] xss
        :let [buf (Buffers/newDirectFloatBuffer (* 2 (count xs)))]]
    (do
      (doseq [[^Float p0 ^Float p1] xs]
        (.put buf p0)
        (.put buf p1))
      (.flip buf)
      [type buf])))

(defn record-fill [pss]
  (let [[tess resfn] (tess-recorder)]
    (GLU/gluTessProperty tess GLU/GLU_TESS_WINDING_RULE GLU/GLU_TESS_WINDING_NONZERO)
    (GLU/gluTessBeginPolygon tess nil)
    (doseq [ps pss]
      (GLU/gluTessBeginContour tess)
      (doseq [[p0 p1] ps]
        (tess-vertex tess [p0 p1 0]))
      (GLU/gluTessEndContour tess))
    (GLU/gluTessEndPolygon tess)
    (to-buffered (resfn))))

(defn pol->buf [^Shape pol]
  (-> (.getPathIterator pol nil)
      (FlatteningPathIterator. 0.1)
      read-path-iterator
      record-fill))

(defn tess-obj [x xs]
  [(pol->buf (get-stroke-polygon x xs))
   (pol->buf (get-polygon x xs))])

(defn retrive-obj [obj xs]
  (when-not (@cache obj)
    (dosync
     (alter cache assoc
            obj (tess-obj obj xs))))
  (@cache obj))

(defn paint-points [^GL2 gl xss]
  (doseq [[type & xs] xss]
    (.glBegin gl type)
    (doseq [[p0 p1] xs]
      (.glVertex2d gl p0 p1))
    (.glEnd gl)))

(defn paint-buffered-points [^GL2 gl xss]
  (doseq [[type ^FloatBuffer buf] xss]
    (.rewind buf)
    (.glVertexPointer gl 2 GL2/GL_FLOAT 0 buf)
    (.glDrawArrays gl type 0 (/ (.limit buf) 2))))

(defn paint-tex [^GL2 gl ^Texture tex ps]
  (.glEnable gl GL2/GL_TEXTURE_2D)
  (.glEnableClientState gl GL2/GL_TEXTURE_COORD_ARRAY)
  (set-color gl [255 255 255 255])
  (.bind tex gl)
  (.glTexCoordPointer gl 2 GL2/GL_FLOAT 0 ^FloatBuffer tex-buffer)
  (let [buf (Buffers/newDirectFloatBuffer 8)]
    (doseq [[^Float p0  ^Float p1] (take 4 ps)]
      (.put buf p0)
      (.put buf p1))
    (.flip buf)
    (.glVertexPointer gl 2 GL2/GL_FLOAT 0 buf))
  (.glDrawArrays gl GL2/GL_QUADS 0 4)
  (.glDisable gl GL2/GL_TEXTURE_2D)
  (.glEnableClientState gl GL2/GL_VERTEX_ARRAY))

(defn load-tex [tex-path]
  (cached-under tex-cache tex-path #(TextureIO/newTexture (file tex-path) false)))

(defn paint [gl {:keys [fill-color line-color tex-path] :as x} xs]
  (let [[line fill] (retrive-obj x xs)]
    (when fill-color
      (set-color gl fill-color)
      (paint-buffered-points gl fill))
    (when tex-path
      (paint-tex gl
                 (load-tex tex-path)
                 (map (:ps x) (:ls x))))
    (when line-color
      (set-color gl line-color)
      (paint-buffered-points gl line))))

(defn expand-sel [x color xs size]
  (expand-sibling (dissoc (assoc x :line-color color :line-width size :line-middle-width 0) :tex-path :clip :fill-color) xs))

(defn gather-sels [selection color xs size]
  (apply concat (for [i (keys selection)]
                  (expand-sel (get xs i) color xs size))))

(defn gather-objs [{:keys [layers layers-ord objs]}]
  (for [layeri layers-ord
        i (:stack (get layers layeri))
        :when (:view (get layers layeri))
        obj (expand-sibling (get objs i) objs)]
    obj))

(defn render-objs [^GL2 gl paint-objs objs]
  (dosync
   (clean-shape-cache paint-objs)
   (alter cache select-keys paint-objs))
  (.glEnableClientState gl GL2/GL_VERTEX_ARRAY)
  (doseq [obj paint-objs]
    (paint gl obj objs))
  (.glDisableClientState gl GL2/GL_VERTEX_ARRAY))

(defn prepare-gl [^GL2 gl trans size]
  (.glMatrixMode gl GL2/GL_MODELVIEW)
  (.glLoadIdentity gl)
  (.glTranslatef gl -1 1 0)
  (let [[a0 a1] size
        [bar [b0 b1]] trans]
    (.glViewport gl 0 0 a0 a1)
    (.glScalef gl (/ 2 a0) (/ -2 a1) 1)
    (.glScalef gl bar bar 1)
    (.glTranslatef gl b0 b1 0))
  (.glClearColor gl 0.5 0.5 0.5 1)
  (.glClear gl GL/GL_COLOR_BUFFER_BIT)
  (.glEnable gl GL/GL_BLEND)
  (.glBlendFunc gl GL/GL_SRC_ALPHA GL/GL_ONE_MINUS_SRC_ALPHA))

(defn render-raw [^GL2 gl scene trans size]
  (prepare-gl gl trans size)
  (render-objs gl (gather-objs scene) (:objs scene))
  (.glFlush gl))

(defn render [^GL2 gl {:keys [trans mode action action-start selection export] :as state}
              mousep size]
  (let [state2 (mp (md state (plus mousep [0.1 0.1])) (plus mousep [0.1 0.1]))
        {:keys [objs] :as scene} (:scene state2)
               sel-size (/ 1 (first trans))
        paint-objs (concat (gather-objs scene)
                           (if (= mode :object)
                             (concat (gather-sels (:selection state2) [200 0 200 255] objs sel-size)
                                     (gather-sels selection [255 200 0 255] objs sel-size))))]
    (prepare-gl gl trans size)
    (render-objs gl paint-objs objs)
    (if (= mode :mesh)
      (doseq [obj-i (keys selection) :let [x (get objs obj-i)]]
        (paint-handles gl x sel-size)))
    (if export
      (apply draw-rect gl (selected-bbox objs selection)))
    (set-color gl [250 200 0 255])
    (when (= mode :mesh)
      (doseq [[obj-i is] selection
              i is
              :let [p (get (:ps (get objs obj-i)) i)]]
        (paint-solid-handle gl p sel-size))))
  (if (= action :select)
    (draw-rect gl action-start mousep))
  (let [rnd (TextRenderer. (Font. "SansSerif" Font/PLAIN 12))]
    (.beginRendering rnd (first size) (second size))
    (.setColor rnd 0 0 0 1)
    (.draw rnd (str mode) 10 20)
    (.draw rnd (str action) 10 40)
    (.endRendering rnd)
    (.dispose rnd))
  (.glFlush gl))
