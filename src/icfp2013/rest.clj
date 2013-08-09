(ns icfp2013.rest
  (:refer-clojure :exclude [eval not and or])
  (:require [clj-http.client :as http]
            [cheshire.core   :as json]
            [clojure.string  :as s]
            [clojure.set :refer [intersection]]
            [clojure.math.combinatorics :as combo]))


;;;;;; REST interface
(def url "http://icfpc2013.cloudapp.net")
(def paths #{"myproblems", "train", "eval", "guess"})
(def api-key "0149QA5D2p0BwWJlreVjrkKOeszOaTyYxYRBcifDvpsH1H")
(def op1 #{'not 'shl1 'shr1 'shr4 'shr16})
(def op2 #{'and 'or 'xor 'plus})
(defn- gen-uri [path]
  (str url "/" path "?auth=" api-key))
(defn post [path body] 
  (let [resp (http/post (gen-uri path) 
                        {:accept :json 
                         :body (json/encode body) 
                         :content-type "application/json"})]
    (when (= 200 (:status resp)) 
      (json/decode (:body resp)))))

(defn train [{:keys [size operators] :as body}]
  (post "train" body))

(defn status []
  (post "status" nil))

(defn guess [{:keys [id program] :as guess}]
  (post "guess" guess))

(defn eval [{:keys [id program arguments] :as ev}]
  {:pre [(clojure.core/or (clojure.core/and id (clojure.core/not program))
             (clojure.core/and (clojure.core/not id) program))
         (every? string? arguments)]}
  (post "eval" ev))

(defn all-problems []
  (post "myproblems" nil))

;;;;;;;;;;; metadata (size of program etc)

(defn size [p]
  (cond 
    (number? p) 1
    (symbol? p) 1
    (= 'if0 (first p)) (reduce + 1 (map size (rest p)))
    (= 'fold (first p)) (reduce + 1 (map size (rest p)))
    (= 'lambda (first p)) (+ 1 (size (last p)))
    (some op1 p) (+ 1 (size (second p)))
    (some op2 p) (+ 1 (size (second p)) (size (last p)))))

(defn op [p]
  (set (map name (filter (-> #{'if0 'fold} (into op1) (into op2)) (flatten p)))))

(defn operators [p]
  (if (clojure.core/and (seq? p) 
                        (= 'fold (first p))
                        (= 0 (nth p 2)))
    (conj (op (-> p (nth 3) (nth 2))) "tfold")
    (op p)))

;;;;;;;;;;; eval generated \BV programs

(defn fold [n start lambda]
  (loop [loops 0, acc start]
    (if (= loops 8)
      acc
      (recur (inc loops) (lambda acc (bit-and 0xff (bit-shift-right n (* 8 loops))))))))

(defn shl1 [^long x]
  (bit-shift-left x 1))

(defn shr1 [^long x]
  (icfp2013.Bitshift/bitShiftRight x (long 1)))

(defn shr4 [^long x]
  (icfp2013.Bitshift/bitShiftRight x (long 4)))

(defn shr16 [^long x]
  (icfp2013.Bitshift/bitShiftRight x (long 16)))

(defn not [^long x]
  (bit-not x))

(defn and [^long x ^long y]
  (bit-and x y))

(defn xor [^long x ^long y]
  (bit-xor x y))

(defn plus [^long x ^long y]
  (unchecked-add x y))

(defn rewrite [form]
  (if (seq? form)
    (let [[op & [a b c]] form] 
      (condp = op
        'lambda (list 'fn (vec a) b)
        'if0 (list 'if (list 'zero? a) b c) 
        form))
    form))

(defn eval-program [program argument]
  (clojure.core/eval (list (clojure.walk/postwalk rewrite program) argument)))

;;;;;;;;;;;;;; generate programs

(defn enumerate-valid-programs [size valid-ops symbols]
  (cond (contains? valid-ops "tfold") ;;special case
    (list 'fold '... 0 (list 'lambda (list 'acc 'y) '...)) ;;; |...|+|...|=size-2
    (= 1 size) (conj symbols 0 1) ;; |p| is 1, meaning we can only return literals
    (= 2 size) (for [op (intersection valid-ops op1), constant (conj symbols 0 1)] (list op constant) )
    (= 3 size) (concat
                 ;op2 and two constants
                 (for [op (intersection valid-ops op2), c1 (conj symbols 0 1), c2 (conj symbols 0 1)] (list op c1 c2) )
                 ;op2 of op1 and one constant
                 (apply concat (for [op (intersection valid-ops op2), expr (enumerate-valid-programs 2 valid-ops symbols), c (conj symbols 0 1)] 
                                 (list (list op expr c)
                                       (list op c expr)))))
;    (= 4 size) (concat
;                  ;if0
;                  ;op2
;                  ;op1
;                  (for [op (intersection valid-ops op1), expr (enumerate-valid-programs (dec size) valid-ops symbols)] ))
     ))

(comment
  (let [x (read-string "(lambda (x) (if0 x 1 0))")] 
    (eval-program x 0)))