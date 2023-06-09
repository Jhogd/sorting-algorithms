(ns lists.core-spec
  (:require [speclj.core :refer :all]
            [lists.core :refer :all]))
(def l (atom nil))
(def n (atom nil))
(def mylist (create-linked-list))
(def mylist2 (create-linked-list))
(def mylist3 (create-linked-list))
(def mylist4 (create-linked-list))
(def mylist5 (create-linked-list))



(defn common-tests []
  (list

  (it " gets a value from a list given an index"
    (doseq [i (range 1 5)] (add @l i))
    (should= 3 (get @l 2))
    (should= 4 (get @l 3)))


  (it "removes a value from a list given index"
    (doseq [i (range 1 5)] (add @l i))
    (remove @l 3)
    (should= 3 (size @l))
    (should= 1 (get @l 0))
    (should= 2 (get @l 1))
    (should= 3 (get @l 2))
    )

  (it "returns the size of the array-list"
    (doseq [i (range 1 5)] (add @l i))
    (should= 4 (size @l)))

  #_(it "creates a new linked list"
      (should= {:kind :linked-list, :head nil} @(create-linked-list)))


  #_(it "gets the size of the linked-list"
      (should= 1 (size {:kind :linked-list, :head {:kind :linked-list, :data 10, :next nil}})))

  #_(it "prints the linked-list"
      (should= "10 ->" (with-out-str (display {:kind :linked-list, :head {:kind :linked-list, :data 10, :next nil}}))))

  ))

(describe "array-list"
  (before (reset! l (create-array-list)))

  (it "instantiates an array-list"
    (should= 0 (size @l))
    (should= [0 0 0 0 0 0 0 0 0 0] (vec @(:data @l))))

  (it "adds element to the array-list"
    (doseq [i (range 1 5)] (add @l i))
    (should= [1 2 3 4 5 0 0 0 0 0] (vec @(:data (add @l 5))))

  (common-tests)
  )
(describe "linked-list"
  (before (reset! n (create-linked-list)))
  (it "instantiates a linked-list"
    (should= {:kind :linked-list :head nil}
             @(create-linked-list)))

  (it "creates a new node"
    (should= {:data 5 :next  nil} @(create-node 5)))


  (it "retrieves the last node"
    (should= {:data 5, :next nil}
             @(last-node
               (swap! mylist assoc-in [:head] (create-node 5))))
    )

  (it "adds to the linked-list"
    (add-list mylist2 1)
    (add-list mylist2 2)
    (add-list mylist2 3)
    (add-list mylist2 4)
    (should= 1 (:data @(:head @mylist2)))
    (should= 2 (:data @(:next @(:head @mylist2))))
    )
  (it "removes node given an index"
    (add-list mylist3 4)
    (add-list mylist3 2)
    (add-list mylist3 1)
    (add-list mylist3 6)
    (remove-list mylist3 0)
    (remove-list mylist3 1)
    (should= 2 (get-list mylist3 0))
    (should= 6 (get-list mylist3 1))
    )

  (it "gets data value from"
    (add-list mylist4 3)
    (add-list mylist4 2)
    (add-list mylist4 6)
    (add-list mylist4 8)
    (should= 3 (get-list mylist4 0))
    (should= 8 (get-list mylist4 3))
    (should= 6 (get-list mylist4 2))
    (should= 2 (get-list mylist4 1))
    )

  (it "gets the current size of the linked-list"
    (add-list mylist5 3)
    (add-list mylist5 2)
    (add-list mylist5 6)
    (add-list mylist5 8)
    (should= 4 (size-list mylist5))
    (should= 0 (size-list (create-linked-list)))
    )

  (it "displays the linked-node"
    (should= "3 ->2 ->6 ->8 ->"  (with-out-str (display-list mylist5))))

  (common-tests)
  ))
