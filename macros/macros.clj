(require '(clojure [string :as str]
                   [walk :as walk]))

(defmacro reverse-it
  [form]
  (walk/postwalk #(if (symbol? %)
                    (symbol (str/reverse (name %)))
                    %)
                 form))

(reverse-it
 (qesod [gra (egnar 5)]
        (nltnirp (cni gra))))
