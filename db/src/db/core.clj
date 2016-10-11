(ns db.core
  (:gen-class)
  (:require [clojure.java.jdbc :as jdbc])
  (:use [korma db core]))

q(def db-spec {:classname "org.sqlite.JDBC"
              :subprotocol "sqlite"
              :subname "test.db"})

(jdbc/with-connection db-spec
  (jdbc/create-table :authors
                     [:id "integer primary key"]
                     [:first_name "varchar"]
                     [:lasat_name "varchar"]))

(jdbc/with-connection db-spec
  (jdbc/insert-records :authors
                       {:first_name "Chas" :lasat_name "Emerick"}
                       {:first_name "Christophe" :lasat_name "Grand"}
                       {:first_name "Brian" :lasat_name "Carper"}))

(jdbc/with-connection db-spec
  (jdbc/with-query-results res ["SELECT * FROM authors"]
    (doall (map #(str (:first_name %) " " (:last_name %)) res))))

(jdbc/with-connection db-spec
  (jdbc/with-query-results res ["SELECT * FROM authors WHERE id = ?" 2]
    (doall res)))

(defn fetch-results [db-spec query]
  (jdbc/with-connection db-spec
    (jdbc/with-query-results res query
      (doall res))))

(jdbc/with-connection db-spec
  (jdbc/drop-table :authors))

(jdbc/with-db-connection db-spec
  (jdbc/transaction
   (jdbc/delete-rows :authors ["id = ?" 2])))

(defn setup
  []
  (jdbc/with-db-connection db-spec
    (jdbc/create-table-ddl :country
                           [[:id "integer primary key"]
                            [:country "varchar"]])
    (jdbc/create-table-ddl :author
                           [[:id "integer primary key"]
                            [:country_id "integer contraint fk_country_id
                                     references country (id)"]]
                       [:first_name "varchar"]
                       [:last_name "varchar"])
    (jdbc/insert! :country
                         {:id 1 :country "USA"}
                         {:id 2 :country "Canada"}
                         {:id 3 :country "France"})
    (jdbc/insert! :author
                         {:first_name "Chas" :last_name "Emerick" :country_id 1}
                         {:first_name "Christopher" :last_name "Grand" :country_id 3}
                         {:first_name "Brian" :last_name "Carper" :country_id 2}
                         {:first_name "Mark" :last_name "Twain" :country_id 1})))

(setup)

(defdb korma-db db-spec)

(declare author)

(defentity country
  (pk :id)
  (has-many author))

(defentity author
  (pk :id)
  (table :author)
  (belongs-to country))

(select author
        (with country)
        (where {:first_name "Chas"}))

(select author
        (with country)
        (where (like :first_name "Ch%"))
        (order :last_name :asc)
        (limit 1)
        (offset 1))

(select author
        (fields :first_name :last_name)
        (where (or (like :last_name "C%")
                   (= :first_name "Mark"))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


