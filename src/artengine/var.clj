(ns artengine.var)

(def scene (ref {:objs {} :stack []}))

(def selection (ref {}))

(def action-start (ref nil))

(def mode (ref :object))
(def action (ref :normal))

(def old-mp (ref [0 0]))

(def rot-p (ref nil))

(def file-path (ref nil))
