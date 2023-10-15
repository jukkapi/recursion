(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))


(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))


(defn max-element [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))


(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (not (= (first a-seq) elem))
      (sequence-contains? elem (rest a-seq))
    :else
     true))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))
    :else []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
    a-seq
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq))
          true
        (= (first a-seq) (first b-seq))
          (seq= (rest a-seq) (rest b-seq))
        :else false))



(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) []
      (cons (f (first seq-1) (first seq-2))
        (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (zero? k) 1
    (zero? (dec k)) n
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond (zero? n) 0
        (= n 1) 1
  :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) []
    (= 1 how-many-times) [what-to-repeat]
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))


(defn my-range [up-to]
  (cond
    (<= up-to 0) '()
    (= up-to 1) '(0)
    :else (cons (dec up-to) (my-range (dec up-to)) )))

(defn tails [a-seq]
  (if (empty? a-seq) ['()]
      (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (rest (map concat (tails a-seq) (inits a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [inc-count (fn [elem] (if (contains? freqs elem)
                                 (assoc freqs elem (inc (get freqs elem)))
                                 (assoc freqs elem 1)))]
      (my-frequencies-helper (inc-count (first a-seq)) (rest a-seq)))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) []
    (let [rep-gen (fn [elem] (repeat (second elem) (first elem)))]
    (concat (rep-gen (first a-map)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (<= n 0) '()
      (concat [(first coll)] (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (<= n 0) coll
    (concat [] (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [seq-half (int (/ (count a-seq) 2))]
    [(my-take seq-half a-seq) (seq (my-drop seq-half a-seq))]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (not (empty? a-seq)) (not (empty? b-seq)))
      (if (> (first a-seq) (first b-seq))
       (cons (first b-seq) (seq-merge (rest b-seq) a-seq))
       (cons (first a-seq) (seq-merge (rest a-seq) b-seq)))
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq))


(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) a-seq
    (= (count a-seq) 1) a-seq
    :else (seq-merge (merge-sort (first (halve a-seq)))
                     (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

