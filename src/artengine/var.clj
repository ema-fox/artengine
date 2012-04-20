(ns artengine.var)

(def rstate (ref {:scene {:objs {} :stack []}
                  :selection {}
                  :action-start nil
                  :mode :object
                  :action :normal}))

(def old-mp (ref [0 0]))

(def file-path (ref nil))
