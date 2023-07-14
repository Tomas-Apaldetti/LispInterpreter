(ns tlc-lisp-interpreter.functions.primitives
  (:require
   [tlc-lisp-interpreter.functions.auxiliar :refer [controlar-aridad error? igual?]]
   [tlc-lisp-interpreter.functions.utils :refer [error]]))

(declare fnc-ge)
(declare fnc-gt)
(declare fnc-lt)
(declare fnc-add)
(declare fnc-env)
(declare fnc-not) ;;Enunciado
(declare fnc-sub)
(declare fnc-cons) ;;Enunciado
(declare fnc-list) ;;Enunciado
(declare fnc-null) ;;Enunciado
(declare fnc-read)
(declare fnc-rest) ;;Enunciado
(declare fnc-equal)
(declare fnc-first) ;;Enunciado
(declare fnc-listp) ;;Enunciado
(declare fnc-prin3) ;;Enunciado
(declare fnc-append)
(declare fnc-length) ;;Enunciado
(declare fnc-terpri)
(declare fnc-reverse)


(defn fnc-cons
  "Devuelve la inserción de un elem en la cabeza de una lista."
  [lae]
  (let [ari (controlar-aridad lae 2)]
    (cond
      (seq? ari) ari
      (or (seq? (second lae)) (igual? (second lae) nil)) (cons (first lae) (second lae))
      :else (list '*error* 'not-implemented))))


(defn fnc-first
  "Devuelve el primer elemento de una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (igual? (first lae) nil) nil
      (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
      :else (ffirst lae))))


(defn fnc-length
  "Devuelve la longitud de una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (or (seq? (first lae)) (igual? (first lae) nil)) (count (first lae))
      :else (list '*error* 'arg-wrong-type (first lae)))))


(defn fnc-list
  "Devuelve una lista formada por los args."
  [lae]
  (if (< (count lae) 1) nil lae))


(defn fnc-listp
  "Devuelve 't' si un elemento es una lista."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (seq? (first lae)) 't
      :else nil)))


(defn fnc-not
  "Niega el argumento."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (igual? (first lae) nil) 't
      :else nil)))


(defn fnc-null
  "Devuelve 't' si un elemento es 'nil' en TLC-Lisp."
  [lae]
  (fnc-not lae))


(defn fnc-prin3
  "Imprime un elemento y lo devuelve."
  [lae]
  (cond
    (< (count lae) 1) (list '*error* 'too-few-args)
    (> (count lae) 1) (list '*error* 'not-implemented)
    (not (seq? (first lae))) (do (print (first lae)) (flush) (first lae))
    :else (do (print (map #(if (igual? % nil) nil %) (first lae))) (flush) (first lae))))


(defn fnc-rest
  "Devuelve una lista sin su 1ra. posición."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (seq? ari) ari
      (igual? (first lae) nil) nil
      (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
      :else (nfirst lae))))

;;==========Funciones realizadas por el alumno==========
(defn check-number-lae
  "Devuelve la lae si no contiene errores, error correspondiente en caso de que si "
  ([lae]
   (cond
     (nil? (first lae)) (error 'too-few-args)
     :else (let [not-nums (drop-while number? lae)]
             (cond
               (nil? (first not-nums)) lae
               :else (error 'number-expected (first not-nums))))))
  ([lae length]
   (let  [res (controlar-aridad lae length)]
     (cond
       (error? res) res
       :else (check-number-lae lae)))))

(defn- traducir-LISP
  [f e1 e2]
  (if (f e1 e2)
    't
    nil))

(defn fnc-ge
  "Devuelve t si el primer numero es mayor o igual que el segundo; si no, nil."
  [lae]
  (let [controlled-lae (check-number-lae lae 2)]
    (cond
      (error? controlled-lae) controlled-lae
      :else (traducir-LISP >= (first lae) (second lae)))))

(defn fnc-gt
  "Devuelve t si el primer numero es mayor que el segundo; si no, nil."
  [lae]
  (let [controlled-lae (check-number-lae lae 2)]
    (cond
      (error? controlled-lae) controlled-lae
      :else (traducir-LISP > (first lae) (second lae)))))

(defn fnc-lt
  "Devuelve t si el primer numero es menor que el segundo; si no, nil."
  [lae]
  (let [controlled-lae (check-number-lae lae 2)]
    (cond
      (error? controlled-lae) controlled-lae
      :else (traducir-LISP < (first lae) (second lae)))))


(defn fnc-add
  "Suma los elementos de una lista. Minimo 2 elementos."
  [lae]
  (let [controlled-lae (check-number-lae lae)]
    (cond
      (error? controlled-lae) controlled-lae
      (< (count lae) 2) (error 'too-few-args)
      :else (apply + lae))))

(defn fnc-env
  "Devuelve la fusion de los ambientes global y local."
  [base g-env l-env]
  (cond
    (nil? (first base)) (fnc-append (list g-env l-env))
    :else (error 'too-many-args)))

(defn fnc-sub
  "Resta los elementos de un lista. Minimo 1 elemento."
  [lae]
  (let [controlled-lae (check-number-lae lae)]
    (cond
      (error? controlled-lae) controlled-lae
      :else (apply - lae))))

(defn fnc-equal
  "Compara 2 elementos. Si son iguales, devuelve t. Si no, nil."
  [lae]
  (let [ari (controlar-aridad lae 2)]
    (cond
      (error? ari) ari
      :else (traducir-LISP igual? (first lae) (second lae)))))

;;TODO = Funciona con los nils de clojure o TLC-LISP
(defn fnc-append
  "Devuelve el resultado de fusionar 2 sublistas."
  [lae]
  (let [ari (controlar-aridad lae 2)]
    (cond
      (error? ari) ari
      (not (or (seq? (first lae)) (nil? (first lae)))) (error 'list 'expected (first lae))
      (not (or (seq? (second lae)) (nil? (second lae)))) (error 'list 'expected (second lae))
      :else (let [res (apply list (concat (first lae) (second lae)))]
              (cond
                (empty? res) nil
                :else res)))))


(defn fnc-terpri
  "Imprime un salto de línea y devuelve nil."
  [lae]
  (let [ari (controlar-aridad lae 0)]
    (cond
      (error? ari) (error 'not-implemented)
      :else (do (newline) (flush)))))

(defn fnc-reverse
  "Devuelve una lista con sus elementos en orden inverso."
  [lae]
  (let [ari (controlar-aridad lae 1)]
    (cond
      (error? ari) ari
      (seq? (first lae)) (reverse (first lae))
      :else (error 'list 'expected (first lae)))))

(defn fnc-read
  "Devuelve la lectura de un elemento de TLC-LISP desde la terminal/consola."
  [lae]
  (let [ari (controlar-aridad lae 0)]
    (cond
      (error? ari) (error 'not-implemented)
      :else (let [input (read)]
              (cond
                (igual? input nil) nil
                :else input)))))