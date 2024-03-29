(ns icfp2013.rest
  (:refer-clojure :exclude [eval not and or])
  (:require [clj-http.client :as http]
            [cheshire.core   :as json]
            [clojure.string  :as s]
            [clojure.core.match :refer [match]]
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

(defonce poster-agent (agent (sorted-set)))

(defn post [path body] 
  (let [answer (promise)] 
    (send-off poster-agent
              (fn [times] 
                (let [now (System/currentTimeMillis)
                      times (remove #(< % (- now 20000)) times)
                      n (count times)
                      oldest (if (> n 0) (apply min times) 0)
                      sleeping-time (+ 5000 (- 20000 (- now oldest)))] ;let's add a second to the sleep, ping etc.
                  (when (= n 5) (println "throtteling for" sleeping-time) (Thread/sleep sleeping-time))
                  (let [resp (http/post (gen-uri path) 
                                        {:accept :json 
                                         :body (json/encode body) 
                                         :throw-exceptions false
                                         :content-type "application/json"})]
                    (deliver answer
                             (if (= 200 (:status resp)) 
                               (json/decode (:body resp) true)
                               resp))
                    (conj times now)))))
    @answer))

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
      (recur (inc loops) (lambda (bit-and 0xff (bit-shift-right n (* 8 loops))) acc)))))

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

(defn or [^long x ^long y]
  (bit-or x y))

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

(defn compile-program [program]
  (clojure.core/eval (list 'fn ['x] (clojure.walk/postwalk rewrite program))))

(defn eval-program [program argument]
  (clojure.core/eval (list (clojure.walk/postwalk rewrite program) argument)))

;;;;;;;;;;;;;; generate programs
(defn stirling-ish-combinations [n k] 
  (if (= k 1)
    [[n]]
    (apply concat 
           (for [current-n (range 1 (inc (- n (dec k)))) 
                 :let [combs (stirling-ish-combinations (- n current-n) (dec k))]]  
             (map #(conj % current-n) combs)))))
(alter-var-root #'stirling-ish-combinations memoize)

(defn enumerate-valid-programs [size valid-ops symbols]
  (distinct(if (= 1 size) 
    (conj symbols 0 1)
    (let [folds (when (contains? valid-ops 'tfold) 
                  (for [[n1 n2] (stirling-ish-combinations (- size 3) 2),
                        e1 (enumerate-valid-programs n1 (disj valid-ops 'tfold) symbols)
                        e2 (enumerate-valid-programs n2 (disj valid-ops 'tfold) (conj symbols 'y 'acc))]
                    (list 'fold e1 0 (list 'lambda (list 'y 'acc) e2))))
          valid-ops (disj valid-ops 'tfold)]
      (if (not-empty folds) 
        folds
        (concat
          (when (contains? valid-ops 'if0) 
            (for [[n1 n2 n3] (stirling-ish-combinations (dec size) 3),
                  expr (enumerate-valid-programs n1 valid-ops symbols)
                  then (enumerate-valid-programs n2 valid-ops symbols)
                  else (enumerate-valid-programs n3 valid-ops symbols)]
              (list 'if0 expr then else)))
          (when (contains? valid-ops 'fold) 
            (for [[n1 n2 n3] (stirling-ish-combinations (- size 2) 3),
                  e1 (enumerate-valid-programs n1 (disj valid-ops 'fold) symbols)
                  e2 (enumerate-valid-programs n2 (disj valid-ops 'fold) symbols)
                  e3 (enumerate-valid-programs n3 (disj valid-ops 'fold) symbols)]
              (list 'fold e1 e2 (list 'lambda (list 'acc 'y) e3))))
          (for [op (intersection valid-ops op2)
                [n1 n2] (stirling-ish-combinations (dec size) 2),
                p1 (enumerate-valid-programs n1 valid-ops symbols),
                p2 (enumerate-valid-programs n2 valid-ops symbols)
                :let [;_ (println op p1 p2 (= op 'and) (= p1 0) (= p2 0))
                      p (cond 
                          (clojure.core/and (= op 'and) (clojure.core/or (= p1 0) (= p2 0))) 0
                          (clojure.core/and (= op 'and) (= p1 1) (= p2 1)) 1
                          (clojure.core/and (= op 'and) (= p1 p2)) p1
                          (clojure.core/and (= op 'or) (= p1 p2)) p1
                          (clojure.core/and (= op 'or) (clojure.core/or (= p1 1) (= p2 1))) 1
                          (clojure.core/and (= op 'or) (= p1 0)) p2
                          (clojure.core/and (= op 'or) (= p2 0)) p1
                          (clojure.core/and (= op 'xor) (= p1 p2)) 0
                          (clojure.core/and (= op 'xor) (= p1 0)) p2
                          (clojure.core/and (= op 'xor) (= p2 0)) p1
                          :else (list op p1 p2))]]
            p)
          (for [op (intersection valid-ops op1),
                p (enumerate-valid-programs (dec size) valid-ops symbols)
                :let [p (if (clojure.core/and (= op 'not) (seq? p) (= 'not (first p))) ;eliminate (not (not ...))
                          (second p)
                          p)]]
            (list op p))))))))

(defn maybe-valid? [program inputs outputs]
  (every? true? (map #(= %2 (program %1)) inputs outputs)))

(defn hex-str->long [^String s]
  (.longValue (BigInteger. (.substring s 2) 16)))

(defn candidate-programs [{:keys [size operators id]}]
  (let [ops (set (map symbol operators)) _ (println "using ops" ops)
        programs (apply concat (for [n (range 3 size)] (enumerate-valid-programs n ops ['x]))); _ (def programs programs)
        r (java.util.Random.)
        inputs (concat [0 1 2 3 4 0xFF 0xFFFFFFFF -1 0x0000aa0000aa0000
                0x123456789abcdeff
                -144115188075855872
                -65148
                -9223372036854775808
                0x0000000000000022] (repeatedly 240 #(.nextLong r))) _ (def inputs inputs)
        {:keys [status outputs message]} (eval {:id id :arguments (map #(java.lang.Long/toHexString %) inputs)}) 
        _ (println outputs)
        outputs (map hex-str->long outputs)] (println status message size ops #_(count programs)) (def outputs outputs) 
    (keep (fn [[text p]] (when (maybe-valid? p inputs outputs) text)) (pmap (juxt identity compile-program) programs))))

(defn try-to-solve [task]
  (let [candidates (candidate-programs task)
        p (first candidates)]
    (if (not-empty candidates)
      (do
        ;(println "there are" (count candidates) "candidates ")
        (println "using program of size" (size p))
        ((juxt println guess) {:id (:id task) :program (str "(lambda (x) " p ")")}))
      candidates)))

  (def pp clojure.pprint/pprint)
  (defn e [] (agent-error poster-agent))
  (defn restart [] (restart-agent poster-agent (sorted-set)))
  
(comment
  
  (def easy (filter #(clojure.core/and (<= (:size %) 11) (false? (:solved %)) (< 0 (:timeLeft %)) ) ps))

  (def ps (all-problems)) 
  (def easy (sort-by (juxt size (comp count :operators)) 
                     (filter #(clojure.core/and (<= (:size %) 13) 
                                                (empty? (intersection #{"fold"} (set (:operators %)))) 
                                                (nil? (:solved %)) ) ps)))
  (pp easy)
  
  (doseq [task easy] (println (try-to-solve task)) (flush))
  (e)
  (restart)
  
  )