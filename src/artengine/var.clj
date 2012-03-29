(ns artengine.var)

(def objs (ref {}))

(def stack (ref []))

(def selection (ref {}))

(def action-start (ref nil))

(def mode (ref :object))
(def action (ref :normal))

(def old-mp (ref [0 0]))

(def rot-p (ref nil))