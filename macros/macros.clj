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

(let [defs '((def x 123)
             (def y 456))]
  `(do ~@defs))

(defmacro auto-gensyms
  [& numbers]
  `(let [x# (rand-int 10)]
     (+ x# ~@numbers)))

(auto-gensyms 1 2 3 4 5)

(defmacro our-doto [expr & forms]
  (let [obj (gensym "obj")]
    `(let [~obj ~expr]
       ~@(map (fn [[f & args]]
                `(~f ~obj ~@args)) forms)
       ~obj)))

(defmacro with
  [name & body]
  `(let [~name 5]
     ~@body))

(with bar (+ 10 bar))

(with foo (+ 40 foo))

(defmacro simplify
  [expr]
  (let [locals (set (keys &env))]
    (if (some locals (flatten expr))
      expr
      (do
        (println "Precomputing: " expr)
        (list `quote (eval expr))))))

(defn f
  [a b c]
  (+ a b c (simplify (apply + (range 5e7)))))

(f 1 2 3)

(defn f'
  [a b c]
  (simplify (apply + a b c (range 5e7))))

(f' 1 2 3)

(defmacro ontology
  [& triples]
  (every? #(or (== 3 (count %))
               (throw (IllegalArgumentException.
                       (format "`%s` provided to `%s` on line %s have < 3 elements"
                               %
                               (first &form)
                               (-> &form meta :line)))))
          triples)
  )

(defmacro OR
  ([] nil)
  ([x]
   (let [result (with-meta (gensym "res") (meta &form))]
     `(let [~result ~x]
        ~result)))
  ([x & next]
   (let [result (with-meta (gensym "res") (meta &form))]
     `(let [or# ~x
            ~result (if or# or# (OR ~@next))]
        ~result))))

(defn preserve-metadata
  "Ensures that the body contains `expr` will carrry the metadata
  from `&form`."
  [&form expr]
  (let [res (with-meta (gensym "res") (meta &form))]
    `(let [~res ~expr]
       ~res)))

(defmacro OR
  "Save as `clojure.core/or`, but preserves user-supplied metadata
  (e.g. type hits)."
  ([] nil)
  ([x] (preserve-metadata &form x))
  ([x & next]
   (preserve-metadata &form `(let [or# ~x]
                               (if or# or# (OR ~@next))))))

(defmacro if-all-let [bindings then else]
  (reduce (fn [subform binding]
            `(if-let [~@binding] ~subform ~else))
          then (reverse (partition 2 bindings))))

(defn macroexpand1-env [env form]
  (if-let [[x & xs] (and (seq? form) (seq form))]
    (if-let [v (and (symbol? x) (resolve x))]
      (if (-> v meta :macro)
        (apply @v form env xs)
        form)
      form)
    form))

(defn macroexpand1-env [env form]
  (if-all-let [[x & xs] (and (seq? form) (seq form))
               v (and (symbol? x) (resolve x))
               _ (-> v meta :macro)]
              (apply @v form env xs)
              form))

(defn ensure-seq [x]
  (if (seq? x) x (list x)))

(defn insert-second
  "Insert x as the second item in seq y."
  [x ys]
  (let [ys (ensure-seq ys)]
    (list* (first ys) x (rest ys))))

(defmacro thread
  "Thread x through successive forms."
  ([x] x)
  ([x form] (insert-second x form))
  ([x form & more] `(thread (thread ~x ~form) ~@more)))

(thread [1 2 3] (conj 4) reverse println)

(defn thread-fns
  ([x] x)
  ([x form] (form x))
  ([x form & more] (apply thread-fns (form x) more)))


