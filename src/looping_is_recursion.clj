(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [b e acc]
                 (if (zero? e)
                   acc
                   (recur b (dec e) (* acc b))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (let [helper (fn [[n & more]] 
                 (if more 
                   (recur more) 
                   n))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (cond 
    (and (empty? seq1)(empty? seq2))
    true
    (or (empty? seq1)(empty? seq2))
    false
    (= (first seq1) (first seq2))
    (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [p pred
         a a-seq
         i 0]
    (if (p (first a)) 
      (if (= 0 i) nil i)
      (if (empty? (rest a)) 
        nil
        (recur p (rest a) (inc i))))))

(defn avg [a-seq]
  (loop [a a-seq
         sum 0
         i 0]
    (if (empty? a)
      (if (= i  0)
        "You cannot count average for an empty sequence"
        (/ sum i))
      (recur (rest a) (+ sum (first a)) (inc i)))))

(defn parity [a-seq]
  (loop [a-set #{}
         a a-seq]
    (if (empty? a) 
      a-set
      (if (contains? a-set (first a))
        (recur (disj a-set (first a)) (rest a))
        (recur (conj a-set (first a)) (rest a))))))


(defn fib [n]
  (if (zero? n)  [0 1]
      (let [[a b] (fib (quot n 2)) 
            c (* a (- (* 2 b) a))
            d (+ (* a a)(* b b))]
        (if (even? n)
          [c d]
          [d (+ c d)]))))

(defn fast-fibo [n]
  (if (< n 0) 
    "impossible"
    (first (fib n))))
  
(defn cut-at-repetition [a-seq]
  (loop [result []
         a a-seq
         a-set #{}]
    (cond 
      (empty? a) result
      (contains? a-set (first a)) result
      :else (recur 
             (conj result (first a)) 
             (rest a) 
             (conj a-set (first a))))))
