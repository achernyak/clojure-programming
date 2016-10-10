(defprotocol Bark
  (bark [this]))

(defrecord Chihuahua []
  Bark
  (bark [this] "Yip!"))

(defrecord Mastiff []
  Bark
  (bark [this] "Woof!"))

(defrecord PetStore [dog])

(defn main
  [dog]
  (let [store (PetStore. dog)]
    (println (bark (:dog store)))))

(main (Chihuahua.))
(main (Mastiff.))

(extend-protocol Bark
  java.util.Map
  (bark [this]
    (or (:bark this)
        (get this "bark"))))

(main (doto (java.util.HashMap.)
        (.put "bark" "Ouah!")))

(main {:bark "Wan-wan!"})

(defn configured-petstore
  []
  (-> "petstore-config.clj"
      slurp
      read-string
      map->PetStore))

(configured-petstore)
