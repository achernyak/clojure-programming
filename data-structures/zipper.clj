(require '[clojure.zip :as z])

(def v [[1 2 [3 4]] [5 6]])

(-> v z/vector-zip z/node)
(-> v z/vector-zip z/down z/node)
(-> v z/vector-zip z/down z/right z/node)

(defn html-zip [root]
  (z/zipper
   vector?
   (fn [[tagname & xs]]
     (if (map? (first xs)) (next xs) xs))
   (fn [[tagname & xs] children]
     (into (if (map? (first xs)) [tagname (first xs)] [tagname]) children))
   root))

(defn wrap
  "Wraps the current node in the specified tag and attributes."
  ([loc tag]
   (z/edit loc #(vector tag %)))
  ([loc tag attrs]
   (z/edit loc #(vector tag attrs %))))

(def h [:body [:h1 "Clojure"]
        [:p "What a wonderful language!"]])

(-> h html-zip z/down z/right z/down (wrap :b) z/root)
