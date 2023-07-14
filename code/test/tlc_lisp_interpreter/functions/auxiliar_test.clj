(ns tlc-lisp-interpreter.functions.auxiliar-test
  (:require [clojure.test :refer [deftest is testing]]
            [tlc-lisp-interpreter.functions.auxiliar :as aux]))

(deftest buscar-test
  (testing "Prueba de la funcion: buscar"
    (is (= 3 (aux/buscar 'c '(a 1 b 2 c 3 d 4 e 5))))
    (is (= 5 (aux/buscar 'e '(a 1 b 2 c 3 d 4 e 5))))
    (is (= "cinco" (aux/buscar 'e '(a 1 b 2 c 3 d 4 e "cinco"))))
    (is (= '(1 2 3) (aux/buscar 'e '(a 1 b 2 c 3 d 4 e (1 2 3)))))
    (is (= 5 (aux/buscar 'E '(a 1 b 2 c 3 d 4 e 5))))
    (is (= 5 (aux/buscar 'e '(a 1 b 2 c 3 d 4 E 5))))
    (is (= '(*error* unbound-symbol f) (aux/buscar 'f '(a 1 b 2 c 3 d 4 e 5))))))

(deftest error?-test
  (testing "Prueba de la funcion: error?"
    (is (true? (aux/error? (list '*error*))))
    (is (true? (aux/error? (list '*ERROR* 'too-few-args))))
    (is (true? (aux/error? (list '*error* 'too-few-args))))
    (is (true? (aux/error? '(*error* too-few-args))))
    (is (false? (aux/error? (list 'too-few-args))))
    (is (false? (aux/error? '*error*)))
    (is (false? (aux/error? ())))
    (is (false? (aux/error? nil)))))

(deftest func-lisp-nil?-test
  (testing "Prueba de la funcion: func-lisp-nil?"
    (is (true? (aux/func-lisp-nil? ())))
    (is (true? (aux/func-lisp-nil? nil)))
    (is (true? (aux/func-lisp-nil? 'nil)))
    (is (true? (aux/func-lisp-nil? 'Nil)))
    (is (true? (aux/func-lisp-nil? 'NIL)))
    (is (false? (aux/func-lisp-nil? "Nil")))))

(deftest igual?-test
  (testing "Prueba de la funcion: igual?"
    (is (true? (aux/igual? 1 1)))
    (is (false? (aux/igual? 1 2)))
    (is (true? (aux/igual? 'a 'a)))
    (is (true? (aux/igual? 'A 'A)))
    (is (true? (aux/igual? 'A 'a)))
    (is (false? (aux/igual? 'a 'B)))
    (is (true? (aux/igual? '(a b c) '(A B C))))
    (is (false? (aux/igual? '(a b c) '(A B D))))
    (is (false? (aux/igual? '(a b c) '(a b c d))))
    (is (true? (aux/igual? nil nil)))
    (is (true? (aux/igual? nil 'nil)))
    (is (true? (aux/igual? 'nil 'NIL)))
    (is (true? (aux/igual? 'NIL 'NIL)))
    (is (true? (aux/igual? 'NIL 'Nil)))
    (is (true? (aux/igual? nil ())))
    (is (false? (aux/igual? '(nil) ())))
    (is (true? (aux/igual? () ())))
    (is (true? (aux/igual? "a" "a")))
    (is (false? (aux/igual? "a" "A")))
    (is (false? (aux/igual? 'a "a")))
    (is (false? (aux/igual? 'a "A")))))

(deftest revisar-fnc-test
  (testing "Prueba de la funcion: revisar-fnc"
    (is (= '(*error* too-few-args) (aux/revisar-fnc '(*error* too-few-args))))
    (is (nil? (aux/revisar-fnc '(too-few-args))))
    (is (nil? (aux/revisar-fnc '(*error*))))
    (is (nil? (aux/revisar-fnc nil)))
    (is (nil? (aux/revisar-fnc ())))))

(deftest revisar-lae-test
  (testing "Prueba de la funcion: revisar-lae"
    (is (nil? (aux/revisar-lae '(1 2 3))))
    (is (nil? (aux/revisar-lae 'nil)))
    (is (nil? (aux/revisar-lae ())))
    (is (= '(*error* too-few-args) (aux/revisar-lae '(1 (*error* too-few-args) 3))))
    (is (= '(*error* too-few-args) (aux/revisar-lae '(1 (*error* too-few-args) (*error* too-many-args) 3))))))

(deftest actualizar-amb-test
  (testing "Prueba de la funcion: actualizar-amb"
    (is (= '(a 1 b 2 c 3 d 4) (aux/actualizar-amb '(a 1 b 2 c 3) 'd 4)))
    (is (= '(a 1 b 4 c 3) (aux/actualizar-amb '(a 1 b 2 c 3) 'b 4)))
    (is (= '(a 1 b 4 c 3) (aux/actualizar-amb '(a 1 b 2 c 3) 'B 4)))
    (is (= '(a 1 b 2 c 3) (aux/actualizar-amb '(a 1 b 2 c 3) 'b (list '*error* 'mal 'hecho))))
    (is (= '(b 7) (aux/actualizar-amb '() 'b 7)))))

(deftest controlar-aridad-test
  (testing "Prueba de la funcion: controlar-aridad"
    (is (= 3 (aux/controlar-aridad '(1 2 3) 3)))
    (is (= 1 (aux/controlar-aridad '(1) 1)))
    (is (= 0 (aux/controlar-aridad '() 0)))
    (is (= '(*error* too-few-args) (aux/controlar-aridad '(1 2) 3)))
    (is (= '(*error* too-few-args) (aux/controlar-aridad '() 1)))
    (is (= '(*error* too-many-args) (aux/controlar-aridad '(1) 0)))
    (is (= '(*error* too-many-args) (aux/controlar-aridad '(1 2) 1)))))