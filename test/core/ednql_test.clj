(ns core.ednql-test
  (:require [core.ednql :as sut]
            [clojure.test :as t]))

(t/deftest type-conversion
  (t/is (= "null" (sut/type-conversion nil)))
  (t/is (= 1 (sut/type-conversion 1)))
  (t/is (= "'some'"
           (sut/type-conversion "some")
           (sut/type-conversion 'some)))
  (t/is (= "key" (sut/type-conversion :key))))

(t/deftest sql-el
  (t/testing "conditions"
    (t/is (= "(? > ?)" (sut/sql-el [:cond/> 2 1])))
    (t/is (= "((? > ?) and (? = ?))"
             (sut/type-conversion
              [:cond/and
               [:cond/> 2 1]
               [:cond/= 1 1]])))))
