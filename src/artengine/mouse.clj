(ns artengine.mouse
  (:use [artengine edit util selection]
        [clojure set]))

(defmulti mp (fn [{:keys [action]} _]
               action))

(defmethod mp :default [{:keys [scene selection action mode action-start] :as state} p]
  (if-let [af (@actions [action mode])]
    (assoc state
      :scene (af scene selection action-start p)
      :action :normal)
    state))

(defmulti md (fn [{:keys [action]} _]
               action))

(defmethod md :default [state _]
  state)

(defmethod md :extend [{:keys [scene selection] :as state} p]
  (let [[newscene [obj-i i]] (extend-objs scene (keys selection) p)]
    (assoc state
      :scene newscene
      :selection (into {} (mapmap (fn [obj-ib _]
                                    (if (= obj-ib obj-i)
                                      #{i}
                                      #{}))
                                  selection))
      :action :move
      :action-start p)))

(defmethod mp :new-sketch [{:keys [scene] :as state} p]
  (let [newscene (new-sketch scene p)]
    (assoc state
      :scene newscene
      :selection {(last (:stack newscene)) #{}}
      :action :end-sketch)))

(defmethod mp :new-obj [{:keys [scene] :as state} p]
  (let [newscene (new-obj scene p)]
    (assoc state
      :scene newscene
      :selection {(last (:stack newscene)) #{}}
      :action :append)))

(defmethod mp :end-sketch [{:keys [scene selection] :as state} p]
  (assoc state
    :scene (end-sketch scene selection p)
    :action :normal))

(defmethod md :normal [state p]
  (assoc state
    :action :select
    :action-start p))

(defn selection-dist [trans]
  (/ 20 (first trans)))

(defmethod mp :clip [{:keys [scene selection trans] :as state} p]
  (assoc (if-let [clip (first (keys (select-obj scene p (selection-dist trans))))]
           (assoc state :scene (set-clip scene selection clip))
           state)
    :action :normal))

(defmethod mp :pick-style [{:keys [scene selection trans] :as state} p]
  (assoc (if-let [master (first (keys (select-obj scene p (selection-dist trans))))]
           (assoc state :scene (pick-style scene selection master))
           state)
    :action :normal))

(defmethod mp :append [{:keys [scene selection trans] :as state} p]
  (let [obj-i (first (keys selection))
	x (append (get (:objs scene) obj-i) p (/ (selection-dist trans) 2))]
    (assoc-in (if (:closed x)
                (assoc state :action :normal)
                state)
              [:scene :objs obj-i] x)))

(defn xunion [a b]
  (difference (union a b) (intersection a b)))

(defmethod mp :select [{:keys [scene selection mode shift action-start trans] :as state} p]
  (assoc state
    :selection (if (< (distance p action-start) (* 0.25 (selection-dist trans)))
                 (if (= mode :mesh)
                   (merge-with xunion
                               (select-ps scene selection p (selection-dist trans))
                               (if shift
                                 selection
                                 {}))
                   (let [new-selction (select-obj scene p (selection-dist trans))]
                     (merge (apply dissoc new-selction (keys selection))
                            (if shift
                              (apply dissoc selection (keys new-selction))
                              {}))))
                 (if (= mode :mesh)
                   (merge-with union
                               (rect-select scene selection action-start p)
                               (if shift
                                 selection
                                 {}))
                   (merge (rect-select-obj scene action-start p)
                          (if shift
                            selection
                            {}))))
    :action :normal))
