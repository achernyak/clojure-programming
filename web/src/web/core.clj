(ns web.core
  (:gen-class)
  (:require [ring.util.response]
            (compojure handler route))
  (:use [compojure.core :only (GET PUT POST defroutes)]
        [ring.adapter.jetty :only (run-jetty)]))

(def ^:private counter (atom 0))
(def ^:private mappings (ref {}))

;; State

(defn url-for
  [id]
  (@mappings id))

(defn shorten!
  "Stores teh given URL under a new unique identifier, or the given identifier
  if provided. Returns the identifier as a string.
  Modifies the globabl mapping accordingly."
  ([url]
   (let [id (swap! counter inc)
         id (Long/toString id 36)]
     (or (shorten! url id)
         (recur url))))
  ([url id]
   (dosync
    (when-not (@mappings id)
      (alter mappings assoc id url)
      id))))

;; Handlers

(defn retain
  [& [url id :as args]]
  (if-let [id (apply shorten! args)]
    {:status 201
     :headers {"Location" id}
     :body (list "URL " url " assigned the short identifier " id)}
    {:status 409 :body (format "Short URL %s is already taken" id)}))

(defn redirect
  [id]
  (if-let [url (url-for id)]
    (ring.util.response/redirect url)
    {:status 404 :body (str "No such short URL: " id)}))

;; Routes

(defroutes app*
  (GET "/" request "Welcome!")
  (PUT "/:id" [id url] (retain url id))
  (POST "/" [url] (retain url))
  (GET "/:id" [id] (redirect id))
  (GET "/list/" [] (interpose "\n" (keys @mappings)))
  (compojure.route/not-found "Sorry, there's nothing here."))

(def app (compojure.handler/api app*))

(def server (run-jetty #'app {:port 4000 :join? false}))
