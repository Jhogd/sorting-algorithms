(ns lists.core-spec
  (:require [speclj.core :refer :all]
            [lists.core :refer :all]))

(describe "creating linked-lists and array-lists from scratch"
  (it "instantiates an array-list"
    (should= {:kind :array :data []
              :size 0} (create-array-list)))

  (it "adds element to the array-list"
    (should= {:kind :array, :data [1 2 3 4], :size 4} (Add {:kind :array, :data [1 2 3], :size 3} 4) ))

  (it " gets a value from a array-list given an index"
    (should= 3 (Get {:kind :array, :data [1 2 3 4], :size 4} 2))
    (should= 4 (Get {:kind :array, :data [1 2 3 4], :size 4} 3)))

  (it "removes a value from an array-list given index"
    (should= {:kind :array, :data [1 2 3], :size 3}
            (Remove {:kind :array, :data [1 2 3 4] :size 4} 3)))

  (it "returns the size of the array-list"
    (should= 4 (Size {:kind :array, :data [1 2 3 4], :size 4})))

  (it "creates a new linked list"
    (should= {:kind :linked-list :data 5 :next nil} (create-node 5)))

  (it "adds to the end of the linked-list"
    (should= {:kind :linked-list, :head {:kind :linked-list, :data 10, :next nil}} (Add {:kind :linked-list :head nil} 10)))

  (it "removes from a linked-list"
    (should= {:kind :linked-list}  (Remove {:kind :linked-list, :head {:kind :linked-list, :data 10, :next nil}} 0)))

  (it "gets a value from a given index"
    (should= 10 (Get {:kind :linked-list, :head {:kind :linked-list, :data 10, :next nil}} 0)))

  (it "gets the size of the linked-list"
    (should= 1 (Size {:kind :linked-list, :head {:kind :linked-list, :data 10, :next nil}})))

  (it "prints the linked-list"
    (should= "10 ->" (with-out-str (display-list {:kind :linked-list, :head {:kind :linked-list, :data 10, :next nil}}))))

  )
