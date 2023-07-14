(ns tlc-lisp-interpreter.functions.utils)

(declare error)
(declare env-to-map)

(defn error
  [& messages]
  (apply list (concat '(*error*) messages)))

(defn- lower-symbol
  [pos, elem]
  (cond
    (even? pos) (cond
                  (nil? elem) nil
                  :else (symbol (.toLowerCase (str elem))))
    :else elem))

(defn env-to-map
  [env]
  (cond
    (empty? env) (hash-map)
    :else (apply assoc {} (map-indexed lower-symbol env))))