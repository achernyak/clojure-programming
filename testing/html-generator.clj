(use 'clojure.test)
(require 'clojure.string)

(defmacro are* [f & body]
  `(are [x# y#] (~'= (~f x#) y#)
     ~@body))

(deftest test-addition
  (are [x y z] (= x (+ y z))
    10 7 3
    20 10 10
    100 89 11))

(deftest test-tostring
  (are* str
        10 "10"
        :foo ":foo"
        "identity" "identity"))

(declare html attrs)

(deftest test-html
  (are* html
        [:html]
        "<html></html>"

        [:a [:b]]
        "<a><b></b></a>"

        [:a {:href "/"} "Home"]
        "<a href=\"/\">Home</a>"

        [:div "foo" [:span "bar"] "baz"]
        "<div>foo<span>bar</span>baz</div>"))

(deftest test-attrs
  (are* (comp clojure.string/trim attrs)
        nil ""

        {:foo "bar"}
        "foo=\"bar\""

        (sorted-map :a "b" :c "d")
        "a=\"b\" c=\"d\""))

(defn attrs
  [attr-map]
  {:pre [(or (map? attr-map)
              (nil? attr-map))]}
  (->> attr-map
       (mapcat (fn [[k v]] [\space (name k) "=\"" v "\""]))
       (apply str)))

(defn html
  [x]
  {:pre [(if (sequential? x)
           (some #(-> x first %) [keyword? symbol? string?])
           (not (map? x)))]
   :post [(string? %)]}
  (if-not (sequential? x)
    (str x)
    (let [[tag & body] x
          [attr-map body] (if (map? (first body))
                            [(first body) (rest body)]
                            [nil body])]
      (str "<" (name tag) (attrs attr-map) ">"
           (apply str (map html body))
           "</" (name tag) ">"))))

(html [:html
       [:head [:title "Propaganda"]]
       [:body [:p "Visit us at "
               [:a {:href "http://clojurebook.com"}
                "our website"]
               "."]]])
