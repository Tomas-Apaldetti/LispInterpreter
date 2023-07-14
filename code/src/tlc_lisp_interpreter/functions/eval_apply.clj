(ns tlc-lisp-interpreter.functions.eval_apply
  #_{:clj-kondo/ignore [:refer-all]}
  (:require clojure.java.io
            [clojure.string :refer [ends-with? lower-case]]
            [tlc-lisp-interpreter.functions.auxiliar :refer [actualizar-amb
                                                             buscar
                                                             controlar-aridad error? igual? imprimir revisar-fnc revisar-lae]]
            [tlc-lisp-interpreter.functions.primitives :refer :all]
            [tlc-lisp-interpreter.functions.utils :refer [error]]))


(declare evaluar) ;;Completada desde base enunciado
(declare evaluar-de)
(declare evaluar-if)
(declare evaluar-or)
(declare evaluar-cond) ;;Enunciado
(declare evaluar-eval) ;;Enunciado
(declare evaluar-exit) ;;Enunciado
(declare evaluar-load) ;;Enunciado
(declare evaluar-setq)
(declare evaluar-quote) ;;Enunciado
(declare evaluar-lambda) ;;Enunciado
(declare evaluar-escalar)
(declare evaluar-clausulas-en-cond) ;;Enunciado
(declare evaluar-secuencia-en-cond) ;;Enunciado


(declare aplicar)
(declare aplicar-lambda)
(declare aplicar-funcion-primitiva) ;;Completada desde base enunciado
(declare aplicar-lambda-simple) ;;Enunciado
(declare aplicar-lambda-multiple) ;;Enunciado

(declare cargar-arch) ;;Enunciado


; Evalua una macro COND. Siempre devuelve una lista con un resultado y un ambiente.
(defn evaluar-cond
  "Evalua una forma 'cond' en TLC-LISP."
  [expre amb-global amb-local]
  (evaluar-clausulas-en-cond (next expre) amb-global amb-local))


(defn evaluar-eval
  "Evalua una forma 'eval' en TLC-LISP."
  [expre amb-global amb-local]
  (let [ari (controlar-aridad (next expre) 1)]
    (cond
      (seq? ari) ari
      (and (seq? (second expre)) (igual? (first (second expre)) 'quote)) (evaluar (second (second expre)) amb-global amb-local)
      :else (evaluar (second expre) amb-global amb-local))))


(defn evaluar-exit
  "Sale del interprete de TLC-LISP."
  [expre amb-global _]
  (cond
    (< (count (next expre)) 1) (list nil nil)
    :else (list (list '*error* 'too-many-args) amb-global)))


(defn evaluar-lambda
  "Evalua una forma 'lambda' en TLC-LISP."
  [expre amb-global _]
  (cond
    (< (count (next expre)) 1) (list (list '*error* 'list 'expected nil) amb-global)
    (and (not (igual? (second expre) nil)) (not (seq? (second expre))))
    (list (list '*error* 'list 'expected (second expre)) amb-global)
    :else (list expre amb-global)))


(defn evaluar-load
  "Evalua una forma 'load' en TLC-LISP. Carga en el ambiente un archivo 'expre' con código en TLC-LISP."
  [expre amb-global amb-local]
  (cond
    (< (count (next expre)) 1) (list (list '*error* 'too-few-args) amb-global)
    (> (count (next expre)) 1) (list (list '*error* 'not-implemented) amb-global)
    :else (list \space (cargar-arch amb-global amb-local (second expre)))))


(defn evaluar-quote
  "Evalua una forma 'quote' de TLC-LISP."
  [expre amb-global _]
  (if (igual? (second expre) nil)
    (list nil amb-global)
    (list (second expre) amb-global)))

(defn evaluar-clausulas-en-cond
  "Une 'evaluar-cond' con 'evaluar-secuencia-en-cond'."
  [expre amb-global amb-local]
  (if (nil? expre)
    (list nil amb-global)
    (let [res-eval (evaluar (ffirst expre) amb-global amb-local)]
      (cond
        (error? (first res-eval)) res-eval
        (not (igual? (first res-eval) nil)) (evaluar-secuencia-en-cond (nfirst expre) (second res-eval) amb-local)
        :else (recur (next expre) (second res-eval) amb-local)))))

; Evalua (con evaluar) secuencialmente las sublistas de una lista y devuelve el valor de la ultima evaluacion.
; Si alguna evaluacion devuelve un error, sera la ultima que se evalue. 
(defn evaluar-secuencia-en-cond [lis amb-global amb-local]
  (if (nil? (next lis))
    (evaluar (first lis) amb-global amb-local)
    (let [res-eval (evaluar (first lis) amb-global amb-local)]
      (if (error? (first res-eval))
        res-eval
        (recur (next lis) (second res-eval) amb-local)))))



(defn aplicar
  "Aplica a la lista de argumentos 'lae' la función 'fnc' en los ambientes dados."
  ([fnc lae amb-global amb-local]
   (aplicar (revisar-fnc fnc) (revisar-lae lae) fnc lae amb-global amb-local))
  ([resu1 resu2 fnc lae amb-global amb-local]
   (cond
     (error? resu1) (list resu1 amb-global)
     (error? resu2) (list resu2 amb-global)
     (not (seq? fnc)) (list (aplicar-funcion-primitiva fnc lae amb-global amb-local) amb-global)
     :else (aplicar-lambda fnc lae amb-global amb-local))))

(defn aplicar-lambda
  "Aplica la forma lambda 'fnc' a la lista de argumentos 'lae'."
  [fnc lae amb-global amb-local]
  (cond
    (< (count lae) (count (second fnc))) (list '(*error* too-few-args) amb-global)
    (> (count lae) (count (second fnc))) (list '(*error* too-many-args) amb-global)
    (nil? (next (nnext fnc))) (aplicar-lambda-simple fnc lae amb-global amb-local)
    :else (aplicar-lambda-multiple fnc lae amb-global amb-local)))


(def prim-dic {"add"     fnc-add
               "append"  fnc-append
               "cons"    fnc-cons
               "equal"   fnc-equal
               "first"   fnc-first
               "ge"      fnc-ge
               "gt"      fnc-gt
               "length"  fnc-length
               "list"    fnc-list
               "listp"   fnc-listp
               "lt"      fnc-lt
               "not"     fnc-not
               "null"    fnc-null
               "prin3"   fnc-prin3
               "read"    fnc-read
               "rest"    fnc-rest
               "reverse" fnc-reverse
               "sub"     fnc-sub
               "terpri"  fnc-terpri
               "+"       fnc-add
               "-"       fnc-sub})

(defn aplicar-funcion-primitiva
  "Aplica una funcion primitiva a una 'lae' (lista de argumentos evaluados)."
  [fnc lae amb-global amb-local]
  (let [func (prim-dic (lower-case (str fnc)))]
    (cond
      (igual? fnc 'env) (fnc-env lae amb-global amb-local)
      (not (nil? func)) (func lae)
      :else (list '*error* 'non-applicable-type fnc))))


(defn aplicar-lambda-simple
  "Evalua una forma lambda 'fnc' con un cuerpo simple."
  [fnc lae amb-global amb-local]
  (let [lista-params-args (reduce concat (map list (second fnc) lae)),
        nuevo-amb-local (cond
                          (empty? amb-local) lista-params-args
                          (empty? lista-params-args) amb-local
                          :else (apply concat (apply assoc (apply assoc {} amb-local) lista-params-args)))]
    (evaluar (first (nnext fnc)) amb-global nuevo-amb-local)))


(defn aplicar-lambda-multiple
  "Evalua una forma lambda 'fnc' cuyo cuerpo contiene varias expresiones."
  [fnc lae amb-global amb-local]
  (aplicar (cons 'lambda (cons (second fnc) (next (nnext fnc))))
           lae
           (second (aplicar-lambda-simple fnc lae amb-global amb-local))  ; Nuevo ambiente global
           amb-local))

(defn cargar-arch
  ([amb-global amb-local arch]
   (let [nomb (first (evaluar arch amb-global amb-local))]
     (if (error? nomb)
       (do (imprimir nomb) amb-global)
       (let [nm (clojure.string/lower-case (str nomb)),
             nom (if (and (> (count nm) 4) (ends-with? nm ".lsp")) nm (str nm ".lsp")),
             ret (try (with-open [in (java.io.PushbackReader. (clojure.java.io/reader nom))]
                        (binding [*read-eval* false] (try (let [res (evaluar (read in) amb-global nil)]
                                                            (cargar-arch (second res) nil in res))
                                                          (catch Exception _ (imprimir nil) amb-global))))
                      (catch java.io.FileNotFoundException _ (imprimir (list '*error* 'file-open-error 'file-not-found nom '1 'READ)) amb-global))]
         ret))))
  ([amb-global _ in res]
   (try (let [res (evaluar (read in) amb-global nil)] (cargar-arch (second res) nil in res))
        (catch Exception _ (imprimir (first res)) amb-global))))

;;==========Funciones realizadas por el alumno==========

(defn evaluar-escalar
  "Evalua una expresion escalar consultando, si corresponde, los ambientes local y global. Devuelve una lista con el resultado y un ambiente."
  [expre amb-global amb-local]
  (cond
    (not (symbol? expre)) (list expre amb-global)
    :else (let [local-hit (buscar expre amb-local)]
            (cond
              (error? local-hit) (list (buscar expre amb-global) amb-global)
              :else (list local-hit amb-global)))))

(defn- form-lambda
  [expre]
  (let [args (nth expre 2 nil)
        body (nthnext expre 3)]
    (cond
      (nil? body) (list 'lambda args)
      :else (apply list 'lambda args body))))

(defn evaluar-de
  "Evalua una forma 'de'. Devuelve una lista con el resultado y un ambiente actualizado con la definicion."
  [expre amb-global _]
  (cond
    (not (seq? expre)) (error 'list 'expected expre)
    (not (seq? amb-global)) (error 'list 'expected amb-global)
    :else (let [identifier (nth expre 1 nil)
                args (nth expre 2 nil)]
            (cond
              (not (seq? args)) (list (error 'list 'expected args) amb-global)
              (igual? identifier nil) (list (error 'cannot-set identifier) amb-global)
              (not (symbol? identifier)) (list (error 'symbol 'expected identifier) amb-global)
              :else (list identifier (actualizar-amb amb-global identifier (form-lambda expre)))))))

(defn evaluar-setq
  "Evalua una forma 'setq'. Devuelve una lista con el resultado y un ambiente actualizado."
  [expre amb-global amb-local]
  (let [identifier (second expre)
        to-set (nth expre 2 nil)
        rest (nthnext expre 3)]
    (cond
      (nil? to-set) (list (error 'list 'expected to-set) amb-global)
      (igual? identifier nil) (list (error 'cannot-set identifier) amb-global)
      (not (symbol? identifier)) (list (error 'symbol 'expected identifier) amb-global)
      (nil? rest)  (let [v (evaluar to-set amb-global amb-local)
                         res (first v)
                         new-amb (second v)]
                     (list res (actualizar-amb new-amb identifier res)))
      :else (let [v (evaluar to-set amb-global amb-local)
                  res (first v)
                  new-amb (second v)]
              (evaluar-setq (apply list 'setq rest) (actualizar-amb new-amb identifier res) amb-local)))))

(defn evaluar-or
  "Evalua una forma 'or'. Devuelve una lista con el resultado y un ambiente."
  [expre amb-global amb-local]
  (if (>= 1 (count expre))
    (list nil amb-global)
    (let [sig (second expre)
          v (evaluar sig amb-global amb-local)
          res (first v)
          new-amb (second v)]
      (cond
        (not (igual? res nil)) (list res new-amb)
        :else (evaluar-or (apply list 'or (nnext expre)) new-amb amb-local)))))


(defn evaluar-if
  "Evalua una forma 'if'. Devuelve una lista con el resultado y un ambiente eventualmente modificado."
  [expre amb-global amb-local]
  (let [condition (second expre)
        then (nth expre 2 nil)
        expre-len (count expre)
        else (nth expre (-  (max expre-len 4) 1) nil)]
    (cond
      (> 2 expre-len) (list nil amb-global)
      :else (let [v (evaluar condition amb-global amb-local)
                  res (first v)
                  new-amb (second v)]
              (cond
                (error? res) (list res new-amb)
                (igual? res nil) (let [v-false (evaluar else new-amb amb-local)
                                       res-false (first v-false)
                                       new-amb-false (second v-false)]
                                   (list res-false new-amb-false))
                :else (let [v-true (evaluar then new-amb amb-local)
                            res-true (first v-true)
                            new-amb-true (second v-true)]
                        (list res-true new-amb-true)))))))

(def eval-dic {"cond"   evaluar-cond
               "de"     evaluar-de
               "eval"   evaluar-eval
               "exit"   evaluar-exit
               "if"     evaluar-if
               "lambda" evaluar-lambda
               "load"   evaluar-load
               "or"     evaluar-or
               "quote"  evaluar-quote
               "setq"   evaluar-setq})

(defn evaluar
  "Evalua una expresion 'expre' en los ambientes global y local. Devuelve un lista con un valor resultante y un ambiente."
  [expre amb-global amb-local]
  (if (or (igual? expre nil)
          (and (seq? expre)
               (or (empty? expre) (error? expre)))) ; si 'expre' es nil, () o error, devolverla intacta
    (list expre amb-global)                         ; de lo contrario, evaluarla
    (cond
      (not (seq? expre))             (evaluar-escalar expre amb-global amb-local)
      :else (let [func (eval-dic (lower-case (str (first expre))))]
              (cond
                (not (nil? func)) (func expre amb-global amb-local)
                :else  (let [res-eval-1 (evaluar (first expre) amb-global amb-local),
                             res-eval-2 (reduce (fn [x y] (let [res-eval-3 (evaluar y (first x) amb-local)]
                                                            (cons (second res-eval-3) (concat (next x) (list (first res-eval-3))))))
                                                (cons (list (second res-eval-1)) (next expre)))]
                         (aplicar (first res-eval-1) (next res-eval-2) (first res-eval-2) amb-local)))))))