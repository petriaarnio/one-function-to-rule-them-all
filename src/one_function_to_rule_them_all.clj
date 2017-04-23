(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (let [helper (fn [str1 str2]
                 (concat str1 " " str2))]
    (if (not (empty? a-seq))
      (apply str (reduce helper a-seq))
      "")))

(defn my-interpose [x a-seq]
  (let [helper (fn [eka toka]
                 (if (empty? eka)
                   (conj [] toka)
                   (conj eka x toka)))]
    (sequence (reduce helper [] a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [count e]
                  (if (not (= e nil))
                    (inc count)
                    count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [a b]
                   (cons b a))]
    (reduce reverser '() a-seq)))

(defn min-max-element [a-seq]
  (let [min-max (fn [acc a]
                  (let [min (get acc 0)
                        max (get acc 1)]
                    (cond
                      (and (= min nil) (= max nil)) (conj acc a a)
                      (or (< a min)) (assoc acc 0 a)
                      (or (= max nil) (> a max)) (assoc acc 1 a)
                    :else acc)))]
    (reduce min-max [] a-seq)))

(defn insert [sorted-seq n]
  (let [seqn (cons n '())]
    (loop [acc '()
           x-seq sorted-seq]
      (let [first-x (first x-seq)]
        (cond
          ;;(empty? sorted-seq) seqn
          (empty? x-seq) (concat acc seqn)
          (<= n first-x) (concat acc seqn x-seq)
          :else (recur (concat acc (cons first-x '())) (rest x-seq)))))))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                (if (contains? a-set elem)
                  (disj a-set elem)
                  (conj a-set elem)))]
  (reduce toggle (set []) a-seq)))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))

(defn count-params
  ([& more]
   (count more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p1] (fn [x] (p1 x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
   (fn [x] ((reduce pred-and (pred-and p1 p2) more) x))))

(defn my-map
  ([f a-seq]
   (let [helper (fn [acc e]
                  (concat acc (cons (f e) '())))]
     (reduce helper '() a-seq)))
  ([f a b & more]
    (let [helper2 (fn [a-seq b-seq]
                   (loop [acca '()
                     aseq a-seq
                     bseq b-seq]
                      (let [aelem (first aseq)
                            belem (first bseq)]
                        (if (or (= aelem nil) (= belem nil))
                          acca
                          (recur (concat acca (cons (f aelem belem) '())) (next aseq) (next bseq))))))]
     (reduce helper2 (helper2 a b) more))))
