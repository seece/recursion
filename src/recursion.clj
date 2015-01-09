(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))


(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      (first coll)
      (my-last (rest coll)))))


(defn max-element [a-seq]
  (apply max a-seq))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))


(defn longest-sequence [a-seq]
  (if (singleton? a-seq)
    (first a-seq)
    (if (empty? a-seq)
      (first a-seq)
     (seq-max
      (first a-seq)
      (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq

    (if (pred? (first a-seq))
     (cons (first a-seq) (my-filter pred? (rest a-seq)))
     (my-filter pred? (rest a-seq)))
  ))

(defn sequence-contains? [elem a-seq]
  (let [e (first a-seq)]
    (if (empty? a-seq) false
      (or
        (= e elem)
        (sequence-contains? elem (rest a-seq))))))



(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (conj (my-take-while pred? (rest a-seq)) (first a-seq))
      '())))

(defn my-drop-while [pred? a-seq]

    (if (empty? a-seq) a-seq
      (if-not (pred? (first a-seq))
        a-seq
        (my-drop-while pred? (rest a-seq)))))


(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
  (and
   (= (first a-seq) (first b-seq))
   (seq= (rest a-seq) (rest b-seq))
  )))

(defn my-map [f seq-1 seq-2]
 (if (or (empty? seq-1) (empty? seq-2))
   '()
  (conj
   (if (or (empty? (rest seq-1)) (empty? (rest seq-2)))
     '()
     (my-map f (rest seq-1) (rest seq-2)))
   (f (first seq-1) (first seq-2)))))


(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))
   ))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (conj (my-repeat (dec how-many-times) what-to-repeat) what-to-repeat)))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (conj (my-range (dec up-to)) (dec up-to))))


(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () a-seq)
    (cons (seq a-seq) (tails (rest a-seq)))) )

(defn inits [a-seq]
   (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map-indexed (fn [i x] (concat (subvec a-seq i) (subvec a-seq 0 i))) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
  (let [e (first a-seq)]
    (my-frequencies-helper
     (assoc freqs e (if (contains? freqs e)
                     (inc (get freqs e))
                     1)
    ) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
  (let [[k v] (first a-map)]
   (concat (repeat v k) (un-frequencies (rest a-map))))
    ))

(defn my-take [n coll]
  (if (and (>= n 1) (not (empty? coll)))
  (conj (my-take (dec n) (rest coll)) (first coll))
    ()))

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

