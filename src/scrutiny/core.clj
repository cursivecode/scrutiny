(ns scrutiny.core)

(def test-map
  {:username "Joe"
   :password "abcdef"
   :confirmation "abcdef"
   :gender "male"
   :middle-name ""
   :age 45
   :nickname "joe dirt"})

(defn result
  "returns the map or map with scrutiny message based on result."
  [m good-validation msg]
  (if good-validation
    m
    (conj m {:scrutiny msg})))

(defn present-helper
  "checks if key is present"
  [m k]
  (if (m :scrutiny)
    m
    (result m
            (not= false (get m k false))
            (str k " is not present"))))

(defn present
  "reduce over keys calling present-helper"
  [m & kys]
  (reduce present-helper m kys))

(defn required-helper
  ""
  [m k]
  (if (m :scrutiny)
    m
    (result m
            (not (or (empty? (m k))
                     (nil? (m k))))
            (str k " was empty or nil"))))

(defn required
  ""
  [m & kys]
  (reduce required-helper m kys))

(defn confirmation
  "Check to see if key matches another key"
  [m k k2]
  (if (m :scrutiny)
    m
    (result m
            (= (m k) (m k2))
            (str k " and " k2 " do not match"))))

(defn length
  ""
  [m f k]
  (if (m :scrutiny)
    m
    (result m
            (f (m k))
            (str k " failed length function"))))

(defn inclusion
  "Checks to see if key returns a truthy value from function"
  [m f k]
  (if (m :scrutiny)
    m
    (result m
            (f (m k))
            (str k " failed inclusion function"))))

(defn exclusion
  "Checks to see if key returns a truthy value from function"
  [m f k]
  (if (m :scrutiny)
    m
    (result m
            (not (f (m k)))
            (str k " failed exclusion function"))))

(defn match
  ""
  [m regex k]
  (if (m :scrutiny)
    m
    (result m
            (re-find regex (str (m k)))
            (str k " failed match function"))))

(defn custom-helper
  ""
  [msg f m k]
  (if (m :scrutiny)
    m
    (result m
            (f (m k))
            msg)))

(defn custom
  [m f msg & kys]
  (reduce (partial custom-helper msg f) m kys))


(-> test-map
    (present :password :gender :confirmation)
    (confirmation :password :confirmation)
    (inclusion #(some #{%} (range 0 46)) :age)
    (match #"[a-zA-Z]" :password)
    (length #(> (count %) 2) :username)
    (required :password :confirmation)
    (custom #(number? %) "Age must be a number" :age)
    )
