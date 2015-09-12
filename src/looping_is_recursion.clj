(ns looping-is-recursion)

(defn power [base exp]
  (let [pow (fn [acc n]
              (if (zero? n)
                acc
                (recur (* acc base) (dec n))))]
    (pow 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first  a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [n 0
         coll a-seq]
    (cond
      (nil? (first coll)) nil
      (pred (first coll)) n
      :else (recur (inc n) (rest coll)))))

(defn avg [a-seq]
  (loop [n 0
         acc 0
         coll a-seq]
    (if
      (empty? coll) (if (zero? n) nil (/ acc n))
      (recur (inc n) (+ acc (first coll)) (rest coll)))))


(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [m #{}
           coll a-seq]
      (if (empty? coll)
        m
        (recur (toggle m (first coll)) (rest coll))))))

(defn fast-fibo [n]
  (cond
    (< n 1) 0
    (= n 1) 1
    :else (loop [n0 0
                 n1 1
                 c 1]
            (if (= c n)
              n1
              (recur n1 (+ n1 n0) (inc c))))))

(defn cut-at-repetition [a-seq]
  (loop [so-far []
         to-go a-seq]
    (let [f (first to-go)]
      (if (or (empty? to-go) (contains? (set so-far) f))
        so-far
        (recur (conj so-far f) (rest to-go))))))

