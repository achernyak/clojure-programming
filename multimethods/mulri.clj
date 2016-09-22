(defmulti fill
  "Fill a xml/html node (as per clojure.xml)
  with the provided value."
  (fn [node value] (:tag node))
  :default nil)

(defmethod fill :div
  [node value]
  (assoc node :content [(str value)]))

(defmethod fill :input
  [node value]
  (assoc-in node [:attrs :value] (str value)))

(fill {:tag :div} "hello")
(fill {:tag :input} "hello")

(defmethod fill :default
  [node value]
  (assoc-in node [:attrs :value] [(str value)]))

(fill {:span :input} "hello")

(defmethod fill nil
  [node value]
  (assoc node :content [(str value)]))
