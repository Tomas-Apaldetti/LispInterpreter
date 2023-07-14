(ns tlc-lisp-interpreter.core
  (:gen-class)
  (:require [tlc-lisp-interpreter.functions.principales :refer [repl]]))


(defn -main
  []
  (repl))