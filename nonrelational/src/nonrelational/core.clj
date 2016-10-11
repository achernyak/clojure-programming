(ns nonrelational.core
  (:gen-class)
  (:use [com.ashafa.clutch :only (create-database with-db put-document
                                                  get-document delete-document)
         :as clutch]
        [com.ashafa.clutch.view-server :only (view-server-exec-string)]))

(def db (create-database "repl-crud"))

(put-document db {:_id "foo" :some-data "bar"})

(put-document db (assoc *1 :other-data "quux"))

(get-document db "foo")

(delete-document db *1)

(get-document db "foo")

(put-document db {:_id "foo"
                  :data ["bar" {:details ["bat" false 42]}]})

(->> (get-document db "foo")
     :data
     second
     :details
     (filter number?))

;; Views

(clutch/bulk-update (create-database "logging")
                    [{:evt-type "auth/new-user" :username "Chas"}
                     {:evt-type "auth/new-user" :username "Dave"}
                     {:evt-type "sales/purchase" :username "Chas" :product ["widget1"]}
                     {:evt-type "sales/purchase" :username "Robin" :product ["widget14"]}
                     {:evt-type "sales/RFQ" :username "Robin" :budget 20000}])

(clutch/save-view "logging" "jsviews"
                  (clutch/view-server-fns :javascript
                                          {:type-counts
                                           {:map "function(doc) {
emit(doc['evt-type'], null);
}"
                                            :reduce "function (keys, vals, rereduce) {
return rereduce ? sum(vals) : vals.length;
}"}}))

(clutch/get-view "logging" "jsviews" :type-counts {:group true})

(->> (clutch/get-view "logging" "jsviews" :type-counts {:group true})
     (map (juxt :key :value))
     (into {}))

(clutch/configure-view-server "http://localhost:5984" (view-server-exec-string))

(clutch/save-view "logging" "clj-views"
                  (clutch/view-server-fns :clojure
                                          {:type-counts
                                           {:map (fn [doc]
                                                   [[(:evt-type doc) nil]])
                                            :reduce (fn [keys vals rereduce]
                                                      (if rereduce
                                                        (reduce + vals)
                                                        (count vals)))}}))

(->> (clutch/get-view "logging" "clj-views" :type-counts {:group true})
     (map (juxt :key :value))
     (into {}))

;; Message Queue
(clutch/create-database "changes")
(def a (clutch/change-agent "changes"))
(clutch/start-changes a)
(add-watch a :echo (fn [key agent previous-change change]
                     (println "change received:" change)))


(clutch/bulk-update "changes" [{:_id "doc4"}])

(ns eventing.processing)

(derive 'sales/lead-generation 'processing/realtime)
(derive 'sals/purchase 'processing/realtime)

(derive 'security/all )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
