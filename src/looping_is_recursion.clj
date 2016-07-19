(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc k n]
                 (cond
                   (= n 1) acc
                   (zero? n) 1
                   :else (recur (* k acc) k (dec n))))]
    (helper base base exp)))


(power 2 0)
(power 2 1)
(power 2 5)
(power 2 2)
                         
(power 7 0)                    
  

(defn last-element [a-seq]
  (let [helper (fn [acc xs]
                 (if (empty? xs)
                   acc
                   (recur (first xs) (rest xs))))]
    (helper nil a-seq)))

(last-element [1 2 3])
(last-element [])

(not (= true ))

(defn seq= [seq1 seq2]
  (if (and (empty? seq1) (empty? seq2))
    true
    (let [helper (fn [xs ys]
                   (cond
                     (not (= (first xs) (first ys))) false
                     (and (empty? xs) (empty? ys)) true
                     (not (= (empty? xs) (empty? ys))) false
                     :else (recur (rest xs) (rest ys))))]
      (helper seq1 seq2))))




(seq= [1 2 3] '())

(seq= [1 2 3] '(1 2 3))

(seq= '() [])

(seq= [1 2 4] '(1 2 2))

(seq= [1 2 3] [1 2 3 4])
(seq= [1 3 5] [])

(defn find-first-index [pred a-seq]
  (loop [acc 0
         f pred
         xs a-seq]
    (cond
      (empty? xs) nil
      (f (first xs)) acc
      :else (recur (inc acc) f (rest xs)))))


(find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
(find-first-index zero? [1 1 3 7 2])                          ;=> nil
(find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
(find-first-index nil? [])                                    ;=> nil






(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [sum-of-xs 0
           acc 0
           xs a-seq]
      (if (empty? xs)
        (/ sum-of-xs acc)
        (recur (+ sum-of-xs (first xs)) (inc acc) (rest xs))))))

(avg [1 2 3])   
(avg [0 0 0 4]) ;=> 1
(avg [1 0 0 1]) ;=> 1/2 ;; or 0.5




(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (if (empty? a-seq)
    #{}
    (loop [a-set #{}
           xs a-seq]
      (if (empty? xs)
        a-set
        (recur (toggle a-set (first xs)) (rest xs))))))


(defn fast-fibo [n]
  (if (zero? n)
    0
    (loop [a 0
           b 1
           k 0
           acc 0]
      (if (= k n)
        acc
        (recur b acc (inc k) (+ acc b))))))


(defn cut-at-repetition [a-seq]
  (if (empty? a-seq)
    []
    (loop [xs a-seq
           ys #{}
           n 0]
      (cond
        (empty? xs) (apply vector (take n a-seq))
        (contains? ys (first xs)) (apply vector (take n a-seq))
        :else (recur (rest xs) (conj ys (first xs)) (inc n))))))

(cut-at-repetition [1 1 1 1])


(take 5 [1 2 3 4 5])

(apply vector #{1 2})

(cut-at-repetition [:cat :dog :milk 1 3])





