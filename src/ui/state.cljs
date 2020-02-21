(ns shadow-tailor.state
  (:require [hx.hooks :refer [<-state <-effect <-ref]]
            ["react" :as react]))

(def ^:dynamic *app-db*
  (atom {}))

(defn query
  "Very minimal pull for maps, collects vals into a flat map
   from a nested data structure"
  ([db query-v]
   (query db query-v {:collected (transient {})
                      :depth 0}))
  ([db query-v opts]
   (let [{:keys [collected depth]} opts
         result (reduce-kv (fn [m k v]
                             (cond
                               (keyword? v) (assoc! collected v (get db k))
                               (map? v) (query (get db k) v (update opts :depth inc))))
                           collected
                           query-v)]
     (if (zero? depth)
       (persistent! result) result))))


(defn <-query
  "A query hook, re-runs the pull every time the db updates or the query changes"
  [query-v]
  (let [id (random-uuid)
        [result setResult] (react/useState nil)
        set-result-from (fn [db]
                          (let [new-result (query db query-v)]
                            (when (and (not (identical? new-result result))
                                       (not (= new-result result)))
                              (setResult new-result))))]
    (<-effect
     (fn []
       (set-result-from @*app-db*)
       (add-watch *app-db* id
                  (fn [_ _ _ new-db]
                    (set-result-from new-db)))
       (fn []
         (remove-watch *app-db* id)))
     #js [query-v])
    result))

(defmulti handle-event (fn [db event-v] (first event-v)))

(defmethod handle-event
  :app-db/register
  [db [_ opts]]
  (let [{:keys [id]} opts]
    (assoc db id opts)))

(defmethod handle-event
  :app-db/unregister
  [db [_ id]]
  (dissoc db id))

(defn dispatch!
  [event-v]
  (swap! *app-db* (fn [db] (handle-event db event-v))))

(defn <-register
  "Registers a thing with an id into the database on mount, and removes it on unmount"
  [opts]
  (let [{:keys [id]} opts]
    (<-effect
     (fn []
       (println "register" opts)
       (dispatch! [:app-db/register opts])
       (fn []
         (println "unregister")
         (dispatch! [:app-db/unregister id])))
     #js [])))
