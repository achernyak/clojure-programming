(require '[clojure.set :as set])

(set/union #{1 2 3} #{4 5 6})

(use '(clojure [string :only (join) :as str]
               [set :exclude (join)]))

(import 'java.util.Date 'java.text.SimpleDateFormat)

(.format (SimpleDateFormat. "MM/dd/yyyy") (Date.))

(import '(java.util Arrays Collections))

(->> (iterate inc 0)
     (take 5)
     into-array
     Arrays/asList
     Collections/max)

(ns examples.ns
  (:refer-clojure :exclude [next replace])
  (:require (clojure [string :as string]
                     [set :as set])
            [clojure.java.shell :as sh])
  (:use (clojure zip xml))
  (:import java.util.Date
           java.text.SimpleDateFormat
           (java.util.concurrent Executors
                                 LinkedBlockingQueue)))

( (\a \b \c))

(rest "abc")

(defn permutations [s]
  (->
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (clojure.core/remove #{x} s)))))
     [s])
   seq))
