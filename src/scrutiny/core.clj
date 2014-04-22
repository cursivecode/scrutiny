(ns scrutiny.core)

(defn result
  "Returns the map or map with scrutiny message based on result."
  [m msg validation]
  (if validation
    m
    (conj m {:scrutiny msg})))

(defn scrutinize
  "Check to see if map has scrutiny, if so returns map.
   Otherwise, the function call result with map, msg, and pred."
  [m f]
  (if (:scrutiny m)
    m
    (let [{:keys [msg pred]} (f)]
      (result m msg pred))))

(defn- present-helper
  "Checks to see if k is not in map.  Uses a custom false key."
  [m k]
  (scrutinize m #(hash-map :msg (str k " is not present")
                           :pred (not= :scr-false (get m k :scr-false)))))

(defn present [m & kys]
  (reduce present-helper m kys))

(defn- required-helper
  "Checks if k is empty or nil - meant for strings"
  [m k]
  (scrutinize m #(hash-map :msg (str k " was empty or nil")
                           :pred (not (or (empty? (k m))
                                          (nil? (k m)))))))

(defn required [m & kys]
  (reduce required-helper m kys))

(defn- confirmation-helper
  "Check to see if key equals another key"
  [m [k k2]]
  (scrutinize m #(hash-map :msg (str k " and " k2 " do not match")
                           :pred (= (k m) (k2 m)))))

(defn confirmation [m & kys]
  {:pre [(even? (count kys))]}
  (reduce confirmation-helper m (partition 2 kys)))

(defn- length-helper
  "Checks result of applying f to value."
  [f m k]
  (scrutinize m #(hash-map :msg (str k " failed length function")
                           :pred (f (k m)))))

(defn length [m f & kys]
  (reduce (partial length-helper f) m kys))

(defn- inclusion-helper
  "Checks to see if key returns a truthy value from function"
  [f m k]
  (scrutinize m #(hash-map :msg (str k " failed inclusion function")
                           :pred (f (k m)))))

(defn inclusion [m f & kys]
  (reduce (partial inclusion-helper f) m kys))

(defn- exclusion-helper
  "Checks to see if key returns a truthy value from function"
  [f m k]
  (scrutinize m #(hash-map :msg (str k " failed exclusion function")
                           :pred (not (f (k m))))))

(defn exclusion [m f & kys]
  (reduce (partial exclusion-helper f) m kys))

(defn- match-helper
  "Checks to see if regex expression finds anything."
  [regex m k]
  (scrutinize m #(hash-map :msg (str k " failed match function")
                           :pred (re-find regex (str (k m))))))

(defn match [m regex & kys]
  (reduce (partial match-helper regex) m kys))

(defn- custom-helper
  "Checks to see if function returns a truthy value.
   Also applies custom function message."
  [msg f m k]
  (scrutinize m #(hash-map :msg msg
                           :pred (f (m k)))))

(defn custom
  [m f msg & kys]
  (reduce (partial custom-helper msg f) m kys))
