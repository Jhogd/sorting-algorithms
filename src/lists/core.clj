(ns lists.core)


(defmulti add :kind)
(defmulti remove :kind)
(defmulti get :kind)
(defmulti size :kind)

(defn create-array-list []
  (let [data  (atom (make-array Integer/TYPE 10))
        size  (atom 0)]
    {:kind :array :data data :size size}))


(defn ensure-capacity [arr-list]
  (let [data (:data arr-list)
        current-size (count @data)]
    (when (= @(:size arr-list) current-size)
      (swap! data #(into-array  (concat % (make-array Integer/TYPE current-size)))))))

(defn array-get [arr index]
  (if (zero? index)
    (first arr)
    (recur (rest arr) (dec index))))

(defmethod add :array [arr-list element]
  (let [data (:data arr-list)
        size (:size arr-list)]
    (ensure-capacity arr-list)
    (aset @data @size element)
    (swap! (:size arr-list) inc)
    arr-list))

(defmethod get :array [arr-list index]
  (let [data (:data arr-list)
        size (:size arr-list)]
    (if (and (integer? index) (>= index 0) (< index @size))
      (array-get (vec @data) index)
      (throw (Exception. "Index out of bounds")))))

(defn array-set [arr index value]
  (if (zero? index)
    (conj (rest arr) value)
    (assoc arr index value)))

(defmethod remove :array [arr-list index]
  (let [data (:data arr-list)
        size (:size arr-list)]
    (if (and (integer? index) (>= index 0) (< index @size))
      (do
        (dotimes [i (- @size (inc index))]
          (array-set (vec @data) (+ index i) (array-get (vec @data) (+ (inc index) i))))
        (swap! (:size arr-list) dec)
        (array-set (vec @data) @size 0))
      (throw (Exception. "Index out of bounds")))))

(defmethod size :array [arr-list]
  @(:size arr-list))


(defn create-linked-list []
  (atom {:kind :linked-list :head nil}))

(defn create-node [data]
  (atom {:data data :next nil}))


(defn last-node [list]
  (loop [current (:head list)]
    (if (nil? (:next @current))
      current
      (recur (:next @current)))))

(defn add-list [list data]
  (let [new-node (create-node data)]
    (if (nil? (:head @list))
      (swap! list assoc-in [:head] new-node)
      (let [last-node (last-node @list)]
        (swap! last-node assoc-in [:next] new-node)))))




(defn get-list [list index]
    (loop [i 0
           current (:head @list)]
      (if (= i index)
        (:data @current)
        (recur (inc i) (:next @current)))))

(defn size-list [list]
    (loop [count 0 node (:head @list)]
      (if (nil? node)
        count
        (recur (inc count) (:next @node)))))


(defn remove-list [list index]
  (if (nil? (:head @list))
    list
    (let [size (size-list list)]
      (if (or (>= index size) (<= index -1))
        list
        (let [values (vec (for [i (range size)
                                :when (not= i index)]
                            (get-list list i)))]
          (swap! list @(create-linked-list))
          (doseq [value values]
            (add-list list value))
          )))))

(defn display-list [list]
  (loop [current (:head @list)]
  (if (nil? current)
    nil
    (do
      (print (:data @current) "->")
      (recur (:next @current))))))

(def my-array (create-array-list))
(def my-list (create-linked-list))


(doseq [i (range 1 10)] (add my-array i))
(doseq [i (range 1 10)] (add-list my-list i))

(defn -main [& args]
  (println "\n Array-list----------")
  (println (vec @(:data my-array)))
  (println (get my-array 8))
  (println (remove my-array 8))
  (println "\n linked-node---------")
  (println (get-list my-list 0))
  (println (get-list my-list 5))
  (println (get-list my-list 7))
  (remove-list my-list 1)
  (display-list my-list)
  (println my-list)



  )


