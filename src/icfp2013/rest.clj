(ns icfp2013.rest
  (:refer-clojure :exclude [eval not and or])
  (:require [clj-http.client :as http]
            [cheshire.core   :as json]
            [clojure.string  :as s]
            [clojure.core.match :refer [match]])
  (:import java.net.URLEncoder))


;;;;;; REST interface
(def url "http://icfpc2013.cloudapp.net")
(def paths #{"myproblems", "train", "eval", "guess"})
(def api-key "0149QA5D2p0BwWJlreVjrkKOeszOaTyYxYRBcifDvpsH1H")

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

(comment
  (let [x (read-string "(lambda (x) (if0 x 1 0))")] 
    (eval-program x 0)))