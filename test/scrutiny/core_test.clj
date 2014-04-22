(ns scrutiny.core-test
  (:require [clojure.test :refer :all]
            [scrutiny.core :refer :all]))

(def test-map
  {:username "Joe"
   :password "abcdef"
   :confirmation "abcdef"
   :gender "male"
   :middle-name ""
   :age 45
   :birthday nil
   :nickname "joe dirt"})

(def test-true #(hash-map :msg "success" :pred true))
(def test-false #(hash-map :msg "fail" :pred false))

(deftest result-test
  (testing "true validation"
    (is (= test-map (result test-map "bad" true))
        "result should equal original map")

    (is (= nil (:scrutiny (result test-map "bad" true)))
        "scrutiny key should be nil"))

  (testing "false validation"
    (is (not= test-map (result test-map "bad" false))
        "result should not equal original map")

    (is (= "bad" (:scrutiny (result test-map "bad" false)))
        "scrutiny key should be bad")

    (is (= (assoc test-map :scrutiny "bad")
           (result test-map "bad" false))
        "map should be the same except for scrutiny key")))

(deftest scrutinize-test
  (testing "scrutiny key"
    (is (= test-map (scrutinize test-map test-true))
        "result should equal original map")

    (is (= nil (:scrutiny (scrutinize test-map test-true)))
        "scrutiny key should be nil"))

  (testing "No scrutiny key "
    (is (not= test-map (scrutinize test-map test-false))
        "result should not equal original map")

    (is (= "fail" (:scrutiny (scrutinize test-map test-false)))
        "return custom msg on failed message and false predicate")

    (is (= (assoc test-map :scrutiny "fail")
           (scrutinize test-map test-false))
        "map should be the same except for scrutiny key")))

(deftest present-test
  (testing "key(s) - present"
    (is (= test-map (present test-map :username :gender))
        "result should equal original map"))

  (testing "key(s) - FAIL present"
    (is (not= test-map (present test-map :username :user))
        "should not equal original map")

    (is (= ":user is not present"
           (:scrutiny (present test-map :user :not-here)))
        "should fail on first failure")))

(deftest required-test
  (testing "key(s) - required"
    (is (= test-map (required test-map :username :gender))
        "result should equal original map"))

  (testing "key(s) - fail required"
    (is (not= test-map (required test-map :birthday))
        "fail on nil")

    (is (not= test-map (required test-map :middle-name))
        "fail on empty string")

    (is (= ":birthday was empty or nil"
           (:scrutiny (required test-map :birthday :middle-name))))))

(deftest confirmation-test
  (testing "key(s) - confirmation"
    (is (= test-map (confirmation test-map
                                  :confirmation :password :age :age))
        "result should equal original map"))

  (testing "key(s) - fail confirmation"
    (is (not= test-map (confirmation test-map
                                     :age :age :password :username))
        "should not equal original map")

    (is (= ":age and :gender do not match"
           (:scrutiny (confirmation test-map
                                    :age :gender :password :username)))
        "should fail with msg on first failure")))

(deftest length-test
  (testing "key(s) - length"
    (is (= test-map (length test-map #(> % 40) :age))
        "result should equal orignal map"))

  (testing "key(s) - fail length"
    (is (= ":age failed length function"
           (:scrutiny (length test-map #(> % 45) :age)))
        "should fail with message")))

(deftest inclusion-test
  (testing "key(s) - inclusion"
    (is (= test-map (inclusion test-map #(some #{%} (range 18 46)) :age))
        "result should equal original map"))

  (testing "key(s) - fail inclusion"
    (is (= ":age failed inclusion function"
           (:scrutiny (inclusion test-map #(some #{%} (range 50 60)) :age)))
        "should fail with message")))

(deftest exclusion-test
  (testing "key(s) - exclusion"
    (is (= test-map (exclusion test-map #(some #{%} (range 50 70)) :age))
        "result should equal original map"))

  (testing "key(s) - fail exclusion"
    (is (= ":age failed exclusion function"
           (:scrutiny (exclusion test-map #(some #{%} (range 18 46)) :age)))
        "should fail with message")))

(deftest match-test
  (testing "key(s) - match"
    (is (= test-map (match test-map #"[a-zA-Z]" :username :gender))
        "result should equal original map"))

  (testing "key(s) - fail match"
    (is (= ":username failed match function"
           (:scrutiny (match test-map #"\d+" :username :gender)))
        "should fail with message")))

(deftest custom-test
  (testing "key(s) - custom"
    (is (= test-map (custom test-map #(string? %) "" :gender))
        "result should equal original map"))

  (testing "key(s) - fail custom"
    (is (= "wrong"
           (:scrutiny (custom test-map #(number? %) "wrong"
                              :username :gender)))
        "should fail with message")))

(deftest scrutiny
  (testing "no failures"
    (is (= test-map
           (-> test-map
               (present :password :gender :confirmation)
               (confirmation :password :confirmation :username :username)
               (inclusion #(some #{%} (range 18 46)) :age)
               (exclusion #(some #{%} ["admin" "administrator"]) :username)
               (match #"[a-zA-Z]" :password)
               (length #(and (< (count %) 15)
                             (> (count %) 2)) :username :password)
               (required :password :confirmation)
               (custom #(number? %) "Age must be a number" :age)))))

  (testing "failure"
    (is (= ":password failed match function"
           (:scrutiny (-> test-map
                          (match #"\d+" :password)
                          (match #"\d+" :username))))
        "returns first failure")))


