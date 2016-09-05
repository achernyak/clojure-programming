(defn empty-board
  [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn populate
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living-cells))

(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))

(defn neighbours [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn count-neighbours
  [board loc]
  (count (filter #(get-in board %) (neighbours loc))))

(defn indexed-step
  [board]
  (let [w (count board)
        h (count (first board))]
    (loop [new-board board
           x 0
           y 0]
      (cond
        (>= x w) new-board
        (>= y h) (recur new-board (inc x) 0)
        :else
        (let [new-liveness
              (case (count-neighbours board [x y])
                2 (get-in board [x y])
                3 :on
                nil)]
          (recur (assoc-in new-board [x y] new-liveness) x (inc y)))))))

(defn indexed-step2 [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
     (fn [new-board x]
       (reduce
        (fn [new-board y]
          (let [new-liveness
                (case (count-neighbours board [x y])
                  2 (get-in board [x y])
                  3 :on
                  nil)]
            (assoc-in new-board [x y] new-liveness)))
        new-board (range h)))
     board (range w))))

(defn indexed-step3 [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
     (fn [new-board [x y]]
       (let [new-liveness
             (case (count-neighbours board [x y])
               2 (get-in board [x y])
               3 :on
               nil)]
         (assoc-in new-board [x y] new-liveness)))
     board (for [x (range h) y (range w)] [x y]))))

(defn window [coll]
  (partition 3 1 (concat [nil] coll [nil])))

(defn cell-block
  [[left mid right]]
  (window (map vector
               (or left (repeat nil)) mid (or right (repeat nil)))))

(defn window
  ([coll] (window nil coll))
  ([pad coll]
   (partition 3 1 (concat [pad] coll [pad]))))

(defn cell-block [[left mid right]]
  (window (map vector left mid right)))

(defn liveness
  [block]
  (let [[_ [_ center _] _] block]
    (case (- (count (filter #{:on} (apply concat block)))
             (if (= :on center) 1 0))
      2 center
      3 :on
      nil)))

(defn- step-row [rows-triple]
  (vec (map liveness (cell-block rows-triple))))

(defn index-free-step [board]
  (vec (map step-row (window (repeat nil) board))))

(defn step [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

(defn stepper
  [neighbours birth? survive?]
  (fn [cells]
    (set (for [[loc n] (frequencies (mapcat neighbours cells))
               :when (if (cells loc) (survive? n) (birth? n))]
           loc))))
