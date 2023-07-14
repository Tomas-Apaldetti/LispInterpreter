(ns tlc-lisp-interpreter.functions.auxiliar
  (:require clojure.java.io
            [clojure.string :refer [lower-case]]
            [tlc-lisp-interpreter.functions.utils :refer [env-to-map error]]))


(declare buscar)
(declare error?)
(declare igual?)
(declare imprimir)  ;;Enunciado
(declare revisar-fnc)
(declare revisar-lae)
(declare actualizar-amb)
(declare controlar-aridad)


(defn imprimir
  "Imprime, con un salto de linea al final, lo recibido devolviendo 
    el mismo valor. Tambien muestra los errores."
  ([elem]
   (cond
     (not (seq? elem)) (if (igual? elem \space)
                         (do (flush) elem)
                         (do (prn (if (igual? elem nil) nil elem)) (flush) elem))
     (error? elem) (imprimir elem elem)
     :else (do (prn (map #(if (igual? % nil) nil %) elem)) (flush) elem)))
  ([lis orig]
   (if (nil? lis)
     (do (prn) (flush) orig)
     (do (pr (first lis)) (print " ") (imprimir (next lis) orig)))))

;;==========Funciones realizadas por el alumno==========

(defn buscar
  "Busca una clave en un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...]
   devuelve y el valor asociado. Devuelve un mensaje de error si no la encuentra."
  [sym, env]
  (let [mapped (env-to-map env)
        new-sym (symbol (lower-case (str sym)))]
    (mapped new-sym (error 'unbound-symbol sym))))


(defn error?
  "Devuelve true o false, segun sea o no el arg. un mensaje de error (una lista con *error* como primer elemento)."
  [err?]
  (cond
    (list? err?) (= "*error*" (lower-case (str (first err?))))
    :else false))


(defn func-lisp-nil?
  "Devuelve true o false, dependiendo de si el argumento es considerado nil en TLC-LISP.
  Los elementos que son considerados nill son: 
  ()  
  Symbol: nil en cualquiera de sus combinaciones entre mayuscula y minuscula."
  [lisp-nil?]
  (cond
    (list? lisp-nil?) (= 0 (count lisp-nil?))
    (symbol? lisp-nil?) (= "nil" (lower-case (str lisp-nil?)))
    :else (nil? lisp-nil?)))

(defn igual?
  "Verifica la igualdad entre dos elementos al estilo de TLC-LISP (case-insensitive)."
  [element-one element-two]
  (cond
    (and (number? element-one) (number? element-two)) (= element-one element-two)
    (and (symbol? element-one) (symbol? element-two)) (= (lower-case (str element-one)) (lower-case (str element-two))) ;; Any two symbols
    (and (func-lisp-nil? element-one) (func-lisp-nil? element-two)) true
    (and (list? element-one) (list? element-two)) (cond
                                                    (= (count element-one) (count element-two)) (every? true? (map igual? element-one element-two))
                                                    :else false)
    :else (= element-one element-two)))

(defn revisar-fnc
  "Si la lista es un mensaje de error, lo devuelve; si no, devuelve nil."
  [fnc]
  (cond
    (and (error? fnc) (> (count fnc) 1)) fnc
    :else nil))

(defn not-error?
  [err?]
  (not (error? err?)))

(defn revisar-lae
  "Devuelve el primer elemento que es un mensaje de error. Si no hay ninguno, devuelve nil."
  [lae]
  (first (remove not-error? lae)))


(defn actualizar-amb
  "Devuelve un ambiente actualizado con una clave (nombre de la variable o funcion) y su valor. 
  Si el valor es un error, el ambiente no se modifica. De lo contrario, se le carga o reemplaza el valor."
  [env, sym, res]
  (cond
    (error? res) env
    (not (symbol? sym)) env
    :else (let [newSym (symbol (lower-case (str sym)))
                newEnv  (assoc (env-to-map env) newSym res)]
            (mapcat vector (keys newEnv) (vals newEnv)))))


(defn controlar-aridad
  "Si la longitud de una lista dada es la esperada, devuelve esa longitud.
   Si no, devuelve una lista con un mensaje de error (una lista con *error* como primer elemento)."
  [lae, expected]
  (let [actual (count lae)]
    (cond
      (= actual expected) actual
      (> actual expected) (error 'too-many-args)
      :else (error 'too-few-args))))