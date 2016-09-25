(ns-unmap *ns* 'fill)

(def input-hierarchy (-> (make-hierarchy)
                        (derive :input.radio ::checkable)
                        (derive :input.checkbox ::checkable)))

(defn- fill-dispatch [node value]
  (if-let [type (and (= :input (:tag node))
                     (-> node :attrs :type))]
    [(keyword (str "input." type)) (class value)]
    [(:tag node) (class value)]))

(defmulti fill
  "Fill a xml/html node (as per clojure.xml)
  with the provided value."
  #'fill-dispatch
  :default nil
  :hierarchy #'fill-hierarchy)

(defmethod fill nil [node value]
  [node value]
  (if (= :input (:tag node))
    (do
      (alter-var-root #'fill-hierarchy
                      derive (first (fill-dispatch node value)) :input)
      (fill node value))
    (assoc node :content [(str value)])))

(defmethod fill
  [:input Object] [node value]
  (assoc-in node [:attrs :value] (str value)))

(defmethod fill [::checkable clojure.lang.IPersistentSet]
  [node value]
  (if (contains? value (-> node :attrs :value))
    (assoc-in node [:attrs :checked] "checked")
    (update-in node [:attrs] dissoc :checked)))

(defmulti fill-input
  "Fill an input field."
  (fn [node value] (-> node :attrs :type))
  :default nil
  :hierarchy #'input-hierarchy)

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

(ns-unmap *ns* 'run)

(defmulti run "Executes the computation." type)

(defmethod run Runnable
  [x]
  (.run x))

(defmethod run java.util.concurrent.Callable
  [x]
  (.call x))

(defmethod run :runnable-map
  [m]
  (run (:run m)))

(prefer-method run java.util.concurrent.Callable Runnable)

(run ^{:type :runnable-map}
  {:run #(println "hello!") :other :data})

(run #(println "hello!"))

(def priorities (atom {:911-call :high
                       :evacuation :high
                       :pothole-report :low
                       :tree-down :low}))

(defmulti route-message
  (fn [message] (@priorities (:type message))))

(defmethod route-message :low
  [{:keys [type]}]
  (println (format "Oh, there's another %s. Put it in the log." (name type))))

(defmethod route-message :high
  [{:keys [type]}]
  (println (format "Alert the authorities, there's a %s!" (name type))))

(route-message {:type :911-call})

(route-message {:type :tree-down})
