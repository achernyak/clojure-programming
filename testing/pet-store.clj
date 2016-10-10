(use 'clojure.test)

(defprotocol Bark
  (bark [this]))

(defrecord Chihuahua [weight price]
  Bark
  (bark [this] "Yip!"))

(defrecord PetStore [dog])

(defn configure-petstore
  []
  (-> "petstore-config.clj"
      slurp
      read-string
      map->PetStore))

(def ^:private dummy-petstore (PetStore. (Chihuahua. 12 "$84.50")))

(defn petstore-config-fixture
  [f]
  (let [file (java.io.File. "petstore-config.clj")]
    (try
      (spit file (with-out-str (pr dummy-petstore)))
      (f)
      (finally
        (.delete file)))))

(deftest test-configured-petstore
  (is (= (configure-petstore) dummy-petstore)))

(use-fixtures :once petstore-config-fixture)

(run-tests)
