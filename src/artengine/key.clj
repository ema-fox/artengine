(ns artengine.key
  (:use [artengine util edit])
  (:import [java.awt.event KeyEvent]))

(defmulti kp (fn [key state p] key))

(defn act [state f & args]
  (assoc state
    :scene (apply f (:scene state) (:selection state) args)))

(defmethod kp [KeyEvent/VK_Q] [_ state _]
  (assoc state :action :new-sketch))

(defmethod kp [KeyEvent/VK_DELETE] [_ {:keys [scene selection mode] :as state} _]
  (if (= mode :mesh)
    (let [newscene (delete scene selection)]
      (assoc state
        :scene newscene
        :selection (into {} (keep #(if (get (:objs newscene) %)
                                     [% #{}])
                                  (keys selection)))))
    (assoc state
      :scene (delete-objs scene selection)
      :selection {})))

(defmethod kp [KeyEvent/VK_C :shift] [_ state _]
  (act state delete-color))

(defmethod kp [KeyEvent/VK_C] [_ {:keys [scene selection] :as state} _]
  (if-let [color (get-color (or (->> (keys selection)
                                     (keep #(:fill-color (get (:objs scene) %)))
                                     first)
                                [0 0 0 255]))]

    (act state set-objs-color color)
    state))

(defmethod kp [KeyEvent/VK_L :shift] [_ state _]
  (act state delete-border))

(defmethod kp [KeyEvent/VK_L] [_ {:keys [scene selection] :as state} _]
  (if-let [color (get-color (or (->> (keys selection)
                                     (keep #(:line-color (get (:objs scene) %)))
                                     first)
                                [0 0 0 255]))]
    (act state set-border-color color)
    state))

(defmethod kp [KeyEvent/VK_J] [_ state _]
  (act state join-objs))

(defmethod kp [KeyEvent/VK_O] [_ state _]
  (act state close))

(defmethod kp [KeyEvent/VK_O :shift] [_ state _]
  (act state unclose))

(defmethod kp [KeyEvent/VK_U] [_ {:keys [mode] :as state} _]
  (if (= mode :mesh)
    (act state soft)
    (act state obj-soft)))

(defmethod kp [KeyEvent/VK_U :shift] [_ {:keys [mode] :as state} _]
  (if (= mode :mesh)
    (act state unsoft)
    (act state obj-unsoft)))

(defmethod kp [KeyEvent/VK_F] [_ state _]
  (assoc state :action :clip))

(defmethod kp [KeyEvent/VK_F :shift] [_ state _]
  (act state delete-clip))

(defmethod kp [KeyEvent/VK_DOWN] [_ {:keys [selected-layer] :as state} _]
  (act state move-down-stack selected-layer))

(defmethod kp [KeyEvent/VK_UP] [_ {:keys [selected-layer] :as state} _]
  (act state move-up-stack selected-layer))

(defmethod kp [KeyEvent/VK_PAGE_DOWN] [_ state _]
  (act state move-down-layer))

(defmethod kp [KeyEvent/VK_PAGE_UP] [_ state _]
  (act state move-up-layer))

(defmethod kp [KeyEvent/VK_TAB] [_ {:keys [mode] :as state} _]
  (assoc state
    :mode (if (= mode :object)
            :mesh
            :object)))

(defmethod kp [KeyEvent/VK_E] [_ {:keys [mode] :as state} _]
  (if (= mode :mesh) 		;todo handle append
    (assoc state :action :extend)
    state))

(defmethod kp [KeyEvent/VK_SPACE] [_ {:keys [action] :as state} _]
  (assoc state
    :mode :mesh
    :action :new-obj))

(defmethod kp [KeyEvent/VK_W] [_ {:keys [scene selection selected-layer] :as state} p]
  (let [[newscene newselection] (sibling scene selection selected-layer)]
    (assoc state
      :scene newscene
      :selection (into {} (map (fn [i] [i #{}])
                               newselection))
      :mode :object
      :action-start p
      :action :move)))

(defmethod kp [KeyEvent/VK_D] [_ {:keys [scene selection selected-layer] :as state} p]
  (let [[newscene newselection] (copy scene selection selected-layer)]
    (assoc state
      :scene newscene
      :selection (into {} (map (fn [i] [i #{}])
                               newselection))
      :action-start p
      :action :move)))

(defmethod kp [KeyEvent/VK_R] [_ state p]
  (assoc state
    :action :rot
    :action-start p))

(defmethod kp [KeyEvent/VK_S] [_ state p]
  (assoc state
    :action :scale
    :action-start p))

(defmethod kp [KeyEvent/VK_S :shift] [_ state p]
  (assoc state
    :action :scale-axis
    :action-start p))

(defmethod kp [KeyEvent/VK_M] [_ state p]
  (assoc state
    :action :scale-steps
    :action-start p))

(defmethod kp [KeyEvent/VK_G] [_ state p]
  (assoc state
    :action :move
    :action-start p))

(defmethod kp [KeyEvent/VK_Y] [_ state p]
  (assoc state :action :pick-style))

(defmethod kp [KeyEvent/VK_T] [_ {:keys [scene] :as state} p]
  (assoc state
    :action :mirror))
