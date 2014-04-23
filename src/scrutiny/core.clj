(ns scrutiny.core
  (:require [clojure.string :as str]))

(defn scrutinize
  "Returns the map or map with scrutiny message based on result."
  [m k pred msg]
  (if pred
    m
    (update-in m [:scrutiny] assoc k msg)))

(defn msg
  "Using this fn because calling name on nil throws an exception."
  [& input]
  (str/replace (apply str input) #":" ""))

(defn- present-helper
  "Checks if key is not in map. Uses custom return key 
   to make sure the key is missing, and not just false or nil."
  [m k]
  (let [pred (not= :scr-false (get m k :scr-false))
        msg (msg k " must be present")]
    (scrutinize m k pred msg)))

(defn present [m & kys]
  (reduce present-helper m kys))

(defn- required-helper
  "Check to see if key in map is empty or nil.
   - extra check for numbers in order to avoid exception on empty?"
  [m k]
  (let [res (k m)
        pred (or (number? res)
                 (not (or (empty? res) (nil? res))))
        msg (msg k " is required")]
    (scrutinize m k pred msg)))

(defn required [m & kys]
  (reduce required-helper m kys))

(defn- confirmation-helper
  "Check to see if key equals another key.
   Failure will be reported to the first k in the pair"
  [m [k k2]]
  (let [pred (= (k m) (k2 m))
        msg (msg k " and " k2 " do not match")]
    (scrutinize m k pred msg)))

(defn confirmation [m & kys]
  {:pre [(even? (count kys))]}
  (reduce confirmation-helper m (partition 2 kys)))

(defn- length-helper
  "Checks result of applying f to value."
  [f m k]
  (let [pred (f (k m))
        msg (msg k " does not meet length requirements")]
    (scrutinize m k pred msg)))

(defn length [m f & kys]
  (reduce (partial length-helper f) m kys))

(defn- match-helper
  "Checks to see if regex expression finds anything.
    -turns value into a string"
  [regex m k]
  (let [pred (re-find regex (str (k m)))
        msg (msg k " does not meet requirements")]
    (scrutinize m k pred msg)))

(defn match [m regex & kys]
  (reduce (partial match-helper regex) m kys))

(defn- custom-helper
  "Checks to see if function returns a truthy value.
   Also applies custom function message."
  [f message m k]
  (let [res (k m)
        pred (f res)
        msg  (msg (format message k))]
    (scrutinize m k pred msg)))

(defn custom
  [m f msg & kys]
  (reduce (partial custom-helper f msg) m kys))
