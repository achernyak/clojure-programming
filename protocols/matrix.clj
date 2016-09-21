(defprotocol Matrix
  "Protocol for working with 2d datastrictures."
  (lookup [matrix i j])
  (update [matrix i j value])
  (rows [matrix])
  (cols [matrix])
  (dims [matrix]))

(extend-protocol Matrix
  clojure.lang.IPersistentVector
  (lookup [vov i j]
    (get-in vov [i j]))
  (update [vov i j value]
    (assoc-in vov [i j] value))
  (rows [vov]
    (seq vov))
  (cols [vov]
    (apply map vector vov))
  (dims [vov]
    [(count vov) (count (first vov))]))

(extend-protocol Matrix
  nil
  (lookup [x i j])
  (update [x i j value])
  (rows [x] [])
  (cols [x] [])
  (dims [x] [0 0]))

(defn vov
  "Creates a vecor of h w-item vectors."
  [h w]
  (vec (repeat h (vec (repeat w nil)))))

(def matrix (vov 3 4))

(update matrix 1 2 :x)

(rows (update matrix 1 2 :x))

(cols (update matrix 1 2 :x))

(extend-protocol Matrix
  (Class/forName "[[D")
  (lookup [matrix i j]
    (aget matrix i j))
  (update [matrix i j value]
    (let [clone (aclone matrix)]
      (aset clone i
            (doto (aclone (aget clone i))
              (aset j value)))
      clone))
  (rows [matrix]
    (map vec matrix))
  (cols [matrix]
    (apply map vector matrix))
  (dims [matrix]
    (let [rs (count matrix)]
      (if (zero? rs)
        [0 0]
        [rs (count (aget matrix 0))]))))

(def matrix (make-array Double/TYPE 2 3))

(rows matrix)

(rows (update matrix 1 1 3.4))

(lookup (update matrix 1 1 3.4) 1 1)

(cols (update matrix 1 1 3.4))

(dims matrix)

(defrecord Point [x y]
  Matrix
  (lookup [pt i j]
    (when (zero? j)
      (case i
        0 x
        1 y)))
  (update [pt i j value]
    (if (zero? j)
      (condp = i
        0 (Point. value y)
        1 (Point. x value))
      pt))
  (rows [pt] [[x] [y]])
  (cols [pt] [[x y]])
  (dims [pt] [2 1]))

(defrecord Point [x y])

(extend-protocol Matrix
  Point
  (lookup [pt i j]
    (when (zero? j)
      (case i
        0 (:x pt)
        1 (:y pt))))
  (update [pt i j value]
    (if (zero? j)
      (condp = i
        0 (Point. value (:y pt))
        1 (Point. (:x pt) value))
      pt))
  (rows [pt] [[(:x pt)] [(:y pt)]])
  (cols [pt] [[(:x pt) (:y pt)]])
  (dims [pt] [2 1]))

(defn listener
  "Creates an AWT/Swing `ActonListener` that delegates to the given function."
  [f]
  (reify
    java.awt.event.ActionListener
    (actionPerformed [this e]
      (f e))))

(.listFiles (java.io.File. ".")
            (reify
              java.io.FileFilter
              (accept [this f]
                (.isDirectory f))))

(def abstract-matrix-impl
  {:cols (fn [pt]
           (let [[h w] (dims pt)]
             (map
              (fn [x] (map #(lookup pt x %) (range 0 w)))
              (range 0 h))))
   :rows (fn [pt]
           (apply map vector (cols pt)))})

(extend Point
  Matrix
  (assoc abstract-matrix-impl
         :lookup (fn [pt i j]
                   (when (zero? j)
                     (case i
                       0 (:x pt)
                       1 (:y pt))))
         :update (fn [pt i j value]
                   (if (zero? j)
                     (condp = i
                       0 (Point. value (:y pt))
                       1 (Point. (:x pt) value))
                     pt))
         :dims (fn [pt] [2 1])))

(defprotocol Measurable
  "A protocol for retrieving the dimensions of widgets."
  (width [measurable] "Returns the width in px.")
  (height [measurable] "Returns the height in px."))

(defrecord Button [text])

(extend-type Button
  Measurable
  (width [btn]
    (* 8 (-> btn :text count)))
  (height [btn] 8))

(def bordered
  {:width #(* 2 (:border-width %))
   :height #(* 2 (:border-height %))})

(defn combine
  "Takes two funciton f and g and returns a fn that takes a variable number
  of args, applies them to f and g and then returns teh result of
  (op rf rg) where rf and rg are the results of the calls to f and g."
  [op f g]
  (fn [& args]
    (op (apply f args) (apply g args))))

(defrecord BorderedButton [text border-width border-height])

(extend BorderedButton
  Measurable
  (merge-with (partial combine +)
              (get-in Measurable [:impls Button])
              bordered))

(let [btn (Button. "Hello World")]
        [(width btn) (height btn)])

(let [bbtn (BorderedButton. "Hello World" 6 4)]
        [(width bbtn) (height bbtn)])

(defn scaffold
  "Given an interface, returns a 'hollow' body suitable for use with `deftype`."
  [interface]
  (doseq [[iface methods] (->> interface
                               .getMethods
                               (map #(vector (.getName (.getDeclaringClass %))
                                             (symbol (.getName %))
                                             (count (.getParameterTypes %))))
                               (group-by first))]
    (println (str "  " iface))
    (doseq [[_ name argcount] methods]
      (println
       (str "    "
            (list name (into '[this] (take argcount (repeatedly gensym)))))))))

(scaffold clojure.lang.IPersistentSet)

(declare empty-array-set)
(def ^:private ^:const max-size 4)

(deftype ArraySet [^objects items
                   ^int size
                   ^:unsynchronized-mutable ^int hashcode]
  clojure.lang.IPersistentSet
  (get [this x]
    (loop [i 0]
      (when (< i size)
        (if (= x (aget items i))
          (aget items i)
          (recur (inc i))))))
  (contains [this x]
    (boolean
     (loop [i 0]
       (when (< i size)
         (or (= x (aget items i)) (recur (inc i)))))))
  (disjoin [this x]
    (loop [i 0]
      (if (== i size)
        this
        (if (not= x (aget items i))
          (recur (inc i))
          (ArraySet. (doto (aclone items)
                       (aset i (aget items (dec size)))
                       (aset (dec size) nil))
                     (dec size)
                     -1)))))
  clojure.lang.IPersistentCollection
  (count [this] size)
  (cons [this x]
    (cond
      (.contains this x) this
      (== size max-size) (into #{x} this)
      :else (ArraySet. (doto (aclone items)
                         (aset size x))
                       (inc size)
                       -1)))
  (empty [this] empty-array-set)
  (equiv [this that] (.equals this that))
  clojure.lang.Seqable
  (seq [this] (take size items))
  Object
  (hashCode [this]
    (when (== -1 hashcode)
      (set! hashcode (int (areduce items idx rest 0
                                   (unchecked-add-int rest (hash (aget items idx)))))))
    hashcode)
  (equals [this that]
    (or
     (identical? this that)
     (and (instance? java.util.Set that)
          (= (count this) (count that))
          (every? #(contains? this %) that))))
  clojure.lang.IFn
  (invoke [this key] (.get this key))
  (applyTo [this args]
    (when (not= 1 (count args))
      (throw (clojure.lang.ArityException. (count args) "ArraySet")))
    (this (first args)))
  java.util.Set
  (isEmpty [this] (zero? size))
  (size [this] size)
  (toArray [this array]
    (.toArray ^java.util.Collection (sequence items) array))
  (toArray [this] (into-array (seq this)))
  (iterator [this] (.iterator ^java.util.Collection (sequence this)))
  (containsAll [this coll]
    (every? #(contains? this %) coll)))

(def ^:private empty-array-set (ArraySet. (object-array max-size) 0 -1))

(defn array-set
  "Creates an array-backed set containing the given values."
  [& vals]
  (into empty-array-set vals))

(defn microbenchmark
  [f & {:keys [size trials] :or {size 4 trials 1e6}}]
  (let [items (repeatedly size gensym)]
    (time (loop [s (apply f items)
                 n trials]
            (when (pos? n)
              (doseq [x items] (contains? s x))
              (let [x (rand-nth items)]
                (recur (-> s (disj x) (conj x)) (dec n))))))))

(doseq [n (range 1 5)
        f [#'array-set #'hash-set]]
  (print n (-> f meta :name) ": ")
  (microbenchmark @f :size n))
