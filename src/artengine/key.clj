(ns artengine.key
  (:use [artengine util edit var])
  (:import [java.awt.event KeyEvent]))

(def key-actions (ref {}))

(defmacro defkey [key & body]
  `(dosync
    (alter key-actions assoc ~key (fn [] ~@body))))

(defn act [f & args]
  (apply alter objs f @selection args))

(defkey [KeyEvent/VK_Q]
  (ref-set action :new-sketch))

(defkey [KeyEvent/VK_DELETE]
  (if (= @mode :mesh)
    (do
      (act delete)
      (ref-set selection (into {} (mapmap (constantly #{}) @selection))))
    (do
      (act delete-objs)
      (ref-set selection {}))))

(defkey [KeyEvent/VK_C :shift]
  (act delete-color))

(defkey [KeyEvent/VK_C]
  (if-let [color (get-color)]
    (act set-objs-color color)))

(defkey [KeyEvent/VK_L :shift]
  (act delete-border))

(defkey [KeyEvent/VK_L]
  (if-let [color (get-color)]
    (act set-border-color color)))

(defkey [KeyEvent/VK_D :shift]
  (act delete-objs-deco (keys @selection)))

(defkey [KeyEvent/VK_D] ;todo decoration of non closed objects
  (let [a (filter #(:closed (get @objs (first %))) @selection)
	b (filter #(not (:closed (get @objs %))) (keys @selection))]
    (alter objs deco-objs a b)))

(defkey [KeyEvent/VK_F]
  (ref-set action :clip))

(defkey [KeyEvent/VK_F :shift]
  (act delete-clip))

(defn move-down [stack sel-objs]
  (loop [s stack
	 res []]
    (if (< 1 (count s))
      (let [[a b & d] s]
	(if (and ((set sel-objs) b) (not ((set sel-objs) a)))
	  (recur (cons a d) (conj res b))
	  (recur (cons b d) (conj res a))))
      (concat res s))))

(defkey [KeyEvent/VK_DOWN]
  (alter stack move-down (keys @selection)))

(defkey [KeyEvent/VK_UP]
  (alter stack #(reverse (move-down (reverse %) (keys @selection)))))

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

(defkey [KeyEvent/VK_R]
  (ref-set action :rot)
  (ref-set action-start @old-mp))

(defkey [KeyEvent/VK_G]
  (ref-set action-start @old-mp)
  (ref-set action :move))
