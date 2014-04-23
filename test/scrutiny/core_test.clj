(ns scrutiny.core-test
  (:require [clojure.test :refer :all]
            [scrutiny.core :refer :all]))

(def test-map
  {:username "Joe"
   :password "abcdef"
   :confirmation "abcdef"
   :gender "male"
   :init "jd"
   :age 45
   :nickname "joe dirt"})

(deftest scrutinize-test
  (testing "true predicate"
    (is (= test-map (scrutinize test-map :username true "bad")))
    (is (= nil (:scrutiny (scrutinize test-map :username true "bad")))))

  (testing "false predicate"
    (is (not= test-map (scrutinize test-map :age false "bad")))
    (is (= {:age "bad"} (:scrutiny (scrutinize test-map :age false "bad"))))
    (is (= test-map
           (dissoc (scrutinize test-map :age false "bad") :scrutiny)))))

(deftest present-test
  (testing "present"
    (is (= test-map (present test-map :username :gender)))
    (is (= nil (:scrutiny (present test-map :age :gender)))))

  (testing "fail present"
    (is (not= test-map (present test-map :username :user)))
    (is (= {:user "user must be present"
            :not-here "not-here must be present"}
           (:scrutiny (present test-map :user :not-here :username))))))

(deftest required-test
  (testing "required"
    (is (= test-map (required test-map :username :age))))

  (testing "fail required"
    (is (not= test-map (required (assoc test-map :init nil))))
    (is (not= test-map (required (assoc test-map :init "") :init)))
    (is (= {:nada "nada is required"}
           (:scrutiny (required test-map :username :nada))))))

(deftest confirmation-test
  (testing "confirmation"
    (is (= test-map (confirmation test-map
                                  :confirmation :password :age :age))))

  (testing "fail confirmation"
    (is (= {:age "age and gender do not match"
            :password "password and username do not match"}
           (:scrutiny (confirmation test-map
                                    :age :gender :password :username))))))

(deftest length-test
  (testing "length"
    (is (= test-map (length test-map #(> % 40) :age)))
    (is (= test-map (length test-map #(>= (count %) 6) :password))))

  (testing "fail length"
    (is (= {:age "age does not meet length requirements"}
           (:scrutiny (length test-map #(> % 45) :age))))))

(deftest match-test
  (testing "match"
    (is (= test-map (match test-map #"[a-zA-Z]" :username :gender))))

  (testing "fail match"
    (is (= {:username "username does not meet requirements"
            :gender "gender does not meet requirements"}
           (:scrutiny (match test-map #"\d+" :username :gender))))))

(deftest custom-test
  (testing "custom"
    (is (= test-map (custom test-map #(string? %) "" :gender)))
    (is (= test-map
           (custom test-map #(some #{%} ["admin" "user"]) :username))))

  (testing "fail custom"
    (is (= {:username "username wrong username"
            :gender "gender wrong gender"}
           (:scrutiny (custom test-map #(number? %)
                              "%1$s wrong %1$s" :username :gender))))))

(deftest scrutiny
  (testing "no failures"
    (is (= test-map
           (-> test-map
               (present :password :gender :confirmation)
               (confirmation :password :confirmation :username :username)
               (match #"[a-zA-Z]" :password)
               (length #(and (< (count %) 15)
                             (> (count %) 2)) :username :password)
               (required :password :confirmation)
               (custom #(number? %) "Age must be a number" :age)))))

  (testing "failure"
    (is (= {:password "password does not meet requirements"
            :nada "nada must be present"}
           (:scrutiny (-> test-map
                          (match #"\d+" :password)
                          (present :nada :username)))))
    (is (= test-map
           (dissoc (-> test-map
                       (match #"\d+" :password)
                       (present :nada :username))
                   :scrutiny)))))


