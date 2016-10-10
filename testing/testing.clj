(use 'clojure.test)

(deftest test-foo
  (is (= 1 1)))

(with-test
  (defn hello [name]
    (str "Hello, " name))
  (is (= (hello "Brian") "Hello, Brian"))
  (is (= (hello nil) "Hello, nil")))

(hello "Judy")

((:test (meta #'hello)))

(run-tests)

(alter-meta! #'hello dissoc :test)

(deftest a
  (is (== 0 (- 3 2))))

(deftest b (a))

(deftest c (b))

(c)

(defn some-fixture
  [f]
  (try
    ;; set up database connections, load test data,
    ;; mock out function using `with-redefs` or `bindings`, etc.
    (f)
    (finally
      ;; clean up databse connections, files, etc.
      )))
