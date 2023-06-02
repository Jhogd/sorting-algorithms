(ns lists.core)

(defn create-array-list []
  (let [data  []
        size  0]
    {:kind :array :data data :size size}))

(defmulti Add :kind)
(defmulti Remove :kind)
(defmulti Get :kind)
(defmulti Size :kind)


(defmethod Add :array [arr-list element]
  (let [new-data (conj (:data arr-list) element)
        new-size (inc (:size arr-list))]
    (assoc arr-list :data new-data :size new-size)))

(defmethod Get :array [arr-list index]
  (let [data (:data arr-list)
        size (:size arr-list)]
    (if (and (integer? index) (>= index 0) (< index size))
      (loop [current data i index]
        (if (empty? current)
          nil
          (if (zero? i)
            (first current)
            (recur (rest current) (dec i))))))))

(defmethod Remove :array [arr-list index]
  (let [data (:data arr-list)
        size (:size arr-list)]
    (if (and (integer? index) (>= index 0) (< index size))
      (let [new-data (vec (concat (subvec data 0 index) (subvec data (inc index))))
            new-size (dec size)]
        (assoc arr-list :data new-data :size new-size)))))

(defmethod Size :array [arr-list]
  (:size arr-list))

(defn create-node [data]
  {:kind :linked-list :data data :next nil})

(defn last-node [list]
  (loop [current (:head list)]
    (if (nil? (:head current))
      current
      (recur (:next current)))))

(defmethod Add :linked-list [list data]
  (let [new-node (create-node data)]
    (if (nil? (:head list))
      (assoc list :head new-node)
      (let [last-node (last-node list)]
        (assoc-in last-node [:next] new-node)
        list))))

(defn remove-node [node index]
  (if (nil? node)
    nil
    (if (= index 1)
      (:next node)
      (assoc-in node [:next] (remove-node (:next node) (dec index))))))

(defmethod Remove :linked-list [list index]
  (if (nil? (:head list))
    list
    (if (= index 0)
      (dissoc list :head)
      (assoc list :head (remove-node (:head list) index))))
  )


(defmethod Get :linked-list [list index]
    (loop [i 0
           current (:head list)]
      (if (or (nil? current) (= i index))
        (:data current)
        (recur (inc i) (:next current)))))

(defmethod Size :linked-list [list]
    (loop [count 0 node (:head list)]
      (if (nil? node)
        count
        (recur (inc count) (:next node)))))

(defn display-list [list]
  (if (nil? (:head list))
    nil
    (do
      (print (:data (:head list)) "->")
      (display-list (assoc list :head (:next (:head list)))))))

(def my-list {:kind :linked-list :head nil})
(def my-list (Add my-list 12))

(def my-array (create-array-list))
(def my-array (Add my-array 5))

(defn -main [& args]
  (display-list my-list)
  (println "\n----------")
  (println (:data my-array))
  (println (Get my-array 0))
  (println (Get my-list 0))
  (println (:data (Remove my-array 0)))
  (display-list (Remove my-list 0))
  (time (Get my-array 0))
  (time (Get my-list 0))
  )


