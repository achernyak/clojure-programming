(set! *warn-on-reflection* true)

(defn capitalize
  [s]
  (-> s
      (.charAt 0)
      Character/toUpperCase
      (str (.substring s 1))))

(time (doseq [s (repeat 100000 "foo")]
        (capitalize s)))

(defn fast-capitalize
  [^String s]
  (-> s
   (.charAt 0)
   Character/toUpperCase
   (str (.substring s 1))))

(time (doseq [s (repeat 100000 "foo")]
        (fast-capitalize s)))

(defn split-name
  [user]
  (zipmap [:first :last]
          (.split (:name user) " ")))

(defn split-name
  [user]
  (zipmap [:first :last]
          (.split ^String (:name user) " ")))

(defn file-extension
  ^String [^java.io.File f]
  (-> (re-seq #"\.(.+)" (.getName f))
      first
      second))
