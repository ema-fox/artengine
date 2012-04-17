(ns artengine.key
  (:use [artengine util edit var])
  (:import [java.awt.event KeyEvent]))

(def key-actions (ref {}))

(defmacro defkey [key & body]
  `(dosync
    (alter key-actions assoc ~key (fn [] ~@body))))

(defn act [f & args]
  (apply alter scene f @selection args))

(defkey [KeyEvent/VK_Q]
  (ref-set action :new-sketch))

(defkey [KeyEvent/VK_DELETE]
  (if (= @mode :mesh)
    (do
      (act delete)
      (ref-set selection (into {} (keep #(if (get (:objs @scene) %)
                                           [% #{}])
                                        (keys @selection)))))
    (do
      (act delete-objs)
      (ref-set selection {}))))

(defkey [KeyEvent/VK_C :shift]
  (act delete-color))

(defkey [KeyEvent/VK_C]
  (if-let [color (get-color (or (->> (keys @selection)
                                     (keep #(:fill-color (get (:objs @scene) %)))
                                     first)
                                [0 0 0 255]))]

    (act set-objs-color color)))

(defkey [KeyEvent/VK_L :shift]
  (act delete-border))

(defkey [KeyEvent/VK_L]
  (if-let [color (get-color (or (->> (keys @selection)
                                     (keep #(:line-color (get (:objs @scene) %)))
                                     first)
                                [0 0 0 255]))]
    (act set-border-color color)))

(defkey [KeyEvent/VK_O]
  (act close))

(defkey [KeyEvent/VK_O :shift]
  (act unclose))

(defkey [KeyEvent/VK_U]
  (act soft))

(defkey [KeyEvent/VK_U :shift]
  (act unsoft))

(defkey [KeyEvent/VK_F]
  (ref-set action :clip))

(defkey [KeyEvent/VK_F :shift]
  (act delete-clip))

(defkey [KeyEvent/VK_DOWN]
  (alter scene move-down-stack (keys @selection)))

(defkey [KeyEvent/VK_UP]
  (alter scene move-up-stack (keys @selection)))

(defkey [KeyEvent/VK_TAB]
  (ref-set mode (if (= @mode :object)
		  :mesh
		  :object)))

(defkey [KeyEvent/VK_E]
  (if (= @mode :mesh) 		;todo handle append
    (ref-set action :extend)))

(defkey [KeyEvent/VK_SPACE]
  (ref-set mode :mesh)
  (if (= @action :normal)
    (ref-set action :new-obj)
    (ref-set action :normal)))

(defkey [KeyEvent/VK_W]
  (let [[newscene newselection] (sibling @scene @selection)]
    (ref-set scene newscene)
    (ref-set selection (into {} (map (fn [i] [i #{}])
                                     newselection)))
    (ref-set mode :object)
    (ref-set action-start @old-mp)
    (ref-set action :move)))

(defkey [KeyEvent/VK_D]
  (let [[newscene newselection] (copy @scene @selection)]
    (ref-set scene newscene)
    (ref-set selection (into {} (map (fn [i] [i #{}])
				     newselection)))
    (ref-set action-start @old-mp)
    (ref-set action :move)))

(defkey [KeyEvent/VK_R]
  (ref-set action :rot)
  (ref-set action-start @old-mp))

(defkey [KeyEvent/VK_S]
  (ref-set action :scale)
  (ref-set action-start @old-mp))

(defkey [KeyEvent/VK_M]
  (ref-set action :scale-steps)
  (ref-set action-start @old-mp))

(defkey [KeyEvent/VK_G]
  (ref-set action-start @old-mp)
  (ref-set action :move))

(defkey [KeyEvent/VK_Y]
  (ref-set action :pick-style))