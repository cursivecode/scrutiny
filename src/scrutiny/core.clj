(ns scrutiny.core)

(def test-map
  {:username "Joe"
   :password "abcdef"
   :confirmation "abcdef"
   :gender "male"
   :age 45
   :nickname "joe dirt"})

(defn result
  "returns the map or map with scrutiny message based on result."
  [m good-validation msg]
  (if good-validation
    m
    (conj m {:scrutiny msg})))

(defn present
  "Checks if key is present"
  [m k]
  (when-not (m :scrutiny)
    (result m
            (not= false (get m k false))
            (str k " is not present"))))

(defn confirmation
  "Check to see if key matches another key"
  [m k k2]
  (when-not (m :scrutiny)
    (result m
            (= (m k) (m k2))
            (str k " and " k2 " do not match"))))

(defn inclusion
  "Checks to see if key returns a truthy value from function"
  [m k f]
  (result m
          (f (m k))
          (str k " failed inclusion function")))

(defn exclusion
  "Checks to see if key returns a truthy value from function"
  [m k f]
  (result m
          (not (f (m k)))
          (str k " failed exclusion function")))

(-> test-map
    (present :password)
    (confirmation :password :confirmation)
    (exclusion :age #(some #{%} (range 0 46)))
    )
