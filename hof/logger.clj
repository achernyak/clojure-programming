(defn print-logger [writer]
  #(binding [*out* writer]
     (println %)))

(def writer (java.io.StringWriter.))

(def retained-logger (print-logger writer))

(require 'clojure.java.io)

(defn file-logger [file]
  #(with-open [f (clojure.java.io/writer file :append true)]
     ((print-logger f) %)))

