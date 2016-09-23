(ns-unmap *ns* 'fill)

(def fill-hierarchy (-> (make-hierarchy)
                        (derive :input.radio ::checkable)
                        (derive :input.checkbox ::checkable)
                        (derive ::checkable :input)
                        (derive :input.text :input)
                        (derive :input.hidden :input)))

(defn- fill-dispatch [node value]
  (if-let [type (and (= :input (:tag node))
                     (-> node :attrs :type))]
    (keyword (str "input." type))
    (:tag node)))

(defmulti fill
  "Fill a xml/html node (as per clojure.xml)
  with the provided value."
  #'fill-dispatch
  :default nil
  :hierarchy #'fill-hierarchy)

(defmethod fill nil
  [node value]
  (assoc node :content [(str value)]))

(defmethod fill :input [node value]
  (assoc-in [:attrs :value] (str value)))

(defmethod fill ::checkable [node value]
  (if (= value (-> node :attrs :value))
    (assoc-in node [:attrs :checked] "checked")
    (update-in node [:attrs] dissoc :checked)))

(defmethod fill nil [node value]
  (if (= :input (:tag node))
    (do
      (alter-var-root #'fill-hierarchy
                      derive (fill-dispatch node value) :input)
      (fill node value))
    (assoc node :content [(str value)])))

(fill {:tag :input
       :attrs {:value "first choice"
               :type "checkbox"}}
      "first choice")

(fill *1 "off")

