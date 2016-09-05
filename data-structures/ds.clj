(defn interpolate
  "Takes a collection of points (as [x y] tuples), returning a function
  which is a linear interpolation between those points."
  [points]
  (let [results (into (sorted-map) (map vec points))]
    (fn [x] (let [[xa ya] (first (rsubseq results <= x))
                 [xb yb] (first (subseq results > x))]
             (if (and xa xb)
               (/ (+ (* ya (- xb x)) (* yb (- x xa)))
                  (- xb xa))
               (or ya yb))))))

(def f (interpolate [[0 0] [10 10] [15 5]]))

(map f [2 10 12])
