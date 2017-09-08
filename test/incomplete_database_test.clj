(ns incomplete-database-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(def incomplete-database "
	varon(juan).
	varon
")

(deftest incomplete-database-fact-test
  (testing "varon(juan) should be true"
    (is (= (evaluate-query incomplete-database "varon(juan)")
           true))) 
  (testing "varon(maria) should be false"
    (is (= (evaluate-query incomplete-database "varon(maria)")
           false))) 
  (testing "mujer(cecilia) should be false"
    (is (= (evaluate-query incomplete-database "mujer(cecilia)")
           false))) 
  (testing "padre(juan,pepe) should be false"
    (is (= (evaluate-query incomplete-database "padre(juan,pepe)")
           false))) 
  (testing "padre(mario,pepe) should be false"
    (is (= (evaluate-query incomplete-database "padre(mario,pepe)")
           false))))

(deftest incomplete-database-rule-test
  (testing "hijo(pepe,juan) should be false"
    (is (= (evaluate-query incomplete-database "hijo(pepe,juan)")
           false))) 
  (testing "hija(maria,roberto) should be false"
    (is (= (evaluate-query incomplete-database "hija(maria,roberto)")
           false))))
