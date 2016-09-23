(ns-unmap *ns* 'fill)

(def input-hierarchy (-> (make-hierarchy)
                        (derive :input.radio ::checkable)
                        (derive :input.checkbox ::checkable)))

(defn- fill-dispatch [node value]
    (:tag node))

(defmulti fill
  "Fill a xml/html node (as per clojure.xml)
  with the provided value."
  #'fill-dispatch
  :default nil)

(defmulti fill-input
  "Fill an input field."
  (fn [node value] (-> node :attrs :type))
  :default nil
  :hierarchy #'input-hierarchy)

(defmethod fill nil [node value]
  [node value]
  (assoc node :content [(str value)]))

(defmethod fill :input [node value]
  (fill-input node value))

(defmethod fill-input nil [node value]
  (assoc-in node [:attrs :value] (str value)))

(defmethod fill-input ::checkable [node value]
  (if (= value (-> node :attrs :value))
    (assoc-in node [:attrs :checked] "checked")
    (update-in node [:attrs] dissoc :checked)))

(fill {:tag :input
       :attrs {:value "first choice"
               :type "checkbox"}}
      "first choice")

(fill *1 "off")

(fill {:tag :input
       :attrs {:type "date"}}
      "20110820")
