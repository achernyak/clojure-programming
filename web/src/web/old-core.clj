(ns web.core
  (:gen-class)
  (:use [ring.adapter.jetty :only (run-jetty)]
        [ring.middleware.params :only (wrap-params)]))

(defn app*
  [{:keys [uri params]}]
  {:body (format "You requested %s with query %s" uri params)})

(def app (wrap-params app*))

(def server (run-jetty #'app {:port 3000 :join? false}))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
