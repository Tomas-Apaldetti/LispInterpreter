(ns tlc-lisp-interpreter.functions.primitives_test
  (:require [clojure.test :refer [deftest is testing]]
            [tlc-lisp-interpreter.functions.primitives :as prim]))

(deftest fnc-ge-test
  (testing "Prueba de la funcion: fnc-ge"
    (is (= '(*error* too-few-args) (prim/fnc-ge ())))
    (is (= '(*error* too-few-args) (prim/fnc-ge '(1))))
    (is (= 't (prim/fnc-ge '(2 1))))
    (is (= 't (prim/fnc-ge '(1 1))))
    (is (= nil (prim/fnc-ge '(1 2))))
    (is (= '(*error* number-expected A) (prim/fnc-ge '(A 1))))
    (is (= '(*error* number-expected A) (prim/fnc-ge '(1 A))))
    (is (= '(*error* number-expected A) (prim/fnc-ge '(A B))))
    (is (= '(*error* too-many-args) (prim/fnc-ge '(1 2 3))))))

(deftest fnc-gt-test
  (testing "Prueba de la funcion: fnc-gt"
    (is (= '(*error* too-few-args) (prim/fnc-gt ())))
    (is (= '(*error* too-few-args) (prim/fnc-gt '(1))))
    (is (= 't (prim/fnc-gt '(2 1))))
    (is (= nil (prim/fnc-gt '(1 1))))
    (is (= nil (prim/fnc-gt '(1 2))))
    (is (= '(*error* number-expected A) (prim/fnc-gt '(A 1))))
    (is (= '(*error* number-expected A) (prim/fnc-gt '(1 A))))
    (is (= '(*error* number-expected A) (prim/fnc-gt '(A B))))
    (is (= '(*error* too-many-args) (prim/fnc-gt '(1 2 3))))))

(deftest fnc-lt-test
  (testing "Prueba de la funcion: fnc-gt"
    (is (= '(*error* too-few-args) (prim/fnc-lt ())))
    (is (= '(*error* too-few-args) (prim/fnc-lt '(1))))
    (is (= nil (prim/fnc-lt '(2 1))))
    (is (= nil (prim/fnc-lt '(1 1))))
    (is (= 't (prim/fnc-lt '(1 2))))
    (is (= '(*error* number-expected A) (prim/fnc-lt '(A 1))))
    (is (= '(*error* number-expected A) (prim/fnc-lt '(1 A))))
    (is (= '(*error* number-expected A) (prim/fnc-lt '(A B))))
    (is (= '(*error* too-many-args) (prim/fnc-lt '(1 2 3))))))

(deftest fnc-add-test
  (testing "Prueba de la funcion: fnc-add"
    (is (= 7 (prim/fnc-add '(3 4))))
    (is (= 12 (prim/fnc-add '(3 4 5))))
    (is (= 18 (prim/fnc-add '(3 4 5 6))))
    (is (= '(*error* number-expected A) (prim/fnc-add '(A 4 5 6))))
    (is (= '(*error* number-expected A) (prim/fnc-add '(3 A 5 6))))
    (is (= '(*error* number-expected A) (prim/fnc-add '(3 A 5 B))))
    (is (= '(*error* too-few-args) (prim/fnc-add '())))
    (is (= '(*error* too-few-args) (prim/fnc-add '(3))))))

(deftest fnc-env-test
  (testing "Prueba de la funcion: fnc-env"
    (is (= '(a 1 b 2 c 3 d 4) (prim/fnc-env '() '(a 1 b 2) '(c 3 d 4))))
    (is (= '(*error* too-many-args) (prim/fnc-env '(5) '(a 1 b 2) '(c 3 d 4))))))

(deftest fnc-sub-test
  (testing "Prueba de la funcion: fnc-sub"
    (is (= -3 (prim/fnc-sub '(3))))
    (is (= -1 (prim/fnc-sub '(3 4))))
    (is (= -6 (prim/fnc-sub '(3 4 5))))
    (is (= -12 (prim/fnc-sub '(3 4 5 6))))
    (is (= '(*error* number-expected A) (prim/fnc-sub '(A 4 5 6))))
    (is (= '(*error* number-expected A) (prim/fnc-sub '(3 A 5 6))))
    (is (= '(*error* number-expected A) (prim/fnc-sub '(3 A 5 B))))
    (is (= '(*error* too-few-args) (prim/fnc-sub '())))))

(deftest fnc-equal-test
  (testing "Prueba de la funcion: fnc-equal"
    (is (= 't (prim/fnc-equal '(1 1))))
    (is (= 't (prim/fnc-equal '(A a))))
    (is (= 't (prim/fnc-equal '(a a))))
    (is (= 't (prim/fnc-equal '(a A))))
    (is (= 't (prim/fnc-equal '("1" "1"))))
    (is (= 't (prim/fnc-equal '(nil NIL))))
    (is (= 't (prim/fnc-equal '(() NIL))))
    (is (= 't (prim/fnc-equal '(Nil NIL))))
    (is (= nil (prim/fnc-equal '(1 2))))
    (is (= nil (prim/fnc-equal '(A B))))
    (is (= nil (prim/fnc-equal '("1" 1))))
    (is (= '(*error* too-few-args) (prim/fnc-equal ())))
    (is (= '(*error* too-few-args) (prim/fnc-equal '(A))))
    (is (= '(*error* too-many-args) (prim/fnc-equal '(A a A))))))

(deftest fnc-append-test
  (testing "Prueba de la funcion: fnc-append"
    (is (= '(*error* too-few-args) (prim/fnc-append '((1 2)))))
    (is (= '(*error* too-many-args) (prim/fnc-append '((1 2) (3) (4 5)))))
    (is (= '(*error* list expected 3) (prim/fnc-append '((1 2) 3))))
    (is (= '(*error* list expected A) (prim/fnc-append '((1 2) A))))
    (is (= '(1 2 3) (prim/fnc-append '((1 2) (3)))))
    (is (= '(1 2) (prim/fnc-append '((1 2) ()))))
    (is (= '(1 2) (prim/fnc-append '(() (1 2)))))
    (is (= '(1 2) (prim/fnc-append '((1 2) nil))))
    (is (= nil (prim/fnc-append '(nil nil))))
    (is (= nil (prim/fnc-append '(() ()))))))

(deftest fnc-terpri-test
  (testing "Prueba de la funcion: fnc-terpri"
    (is (= nil (prim/fnc-terpri ())))
    (is (= '(*error* not-implemented) (prim/fnc-terpri '(1))))
    (is (= '(*error* not-implemented) (prim/fnc-terpri '(1 2)))))
  (testing "Prueba de la funcion: fnc-terpri. Efectos secundarios"
    (let [sep (. (. System (getProperties)) (get "line.separator"))]
      (is (= sep (with-out-str (prim/fnc-terpri ()))))
      (is (= "" (with-out-str (prim/fnc-terpri '(1)))))
      (is (= "" (with-out-str (prim/fnc-terpri '(1 2))))))))

(deftest fnc-reverse-test
  (testing "Prueba de la funcion: fnc-reverse"
    (is (= '(3 2 1) (prim/fnc-reverse '((1 2 3)))))
    (is (= '(1) (prim/fnc-reverse '((1)))))
    (is (= '() (prim/fnc-reverse '(()))))
    (is (= '(*error* list expected A) (prim/fnc-reverse '(A))))
    (is (= '(*error* too-few-args) (prim/fnc-reverse '())))
    (is (= '(*error* too-many-args) (prim/fnc-reverse '(A 2))))))

(deftest fnc-read-test
  (testing "Prueba de la funcion: fnc-read"
    (is (= 1 (with-in-str "1" (prim/fnc-read ()))))
    (is (= (symbol "a") (with-in-str "a" (prim/fnc-read ()))))
    (is (= '(hola mundo) (with-in-str "(hola\r\nmundo)" (prim/fnc-read ()))))
    (is (= '(hola mundo) (with-in-str "(hola\nmundo)" (prim/fnc-read ()))))
    (is (= (symbol "hola") (with-in-str "hola" (prim/fnc-read ()))))
    (is (= "hola" (with-in-str "\"hola\"" (prim/fnc-read ()))))
    (is (= nil (with-in-str "()" (prim/fnc-read ()))))
    (is (= nil (with-in-str "nil" (prim/fnc-read ()))))
    (is (= '(*error* not-implemented) (prim/fnc-read '(1))))
    (is (= '(*error* not-implemented) (prim/fnc-read '(1 2))))))