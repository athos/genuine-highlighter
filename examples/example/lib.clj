(ns example.lib)

(defn fib [n]
  (case n
    (0 1) 1
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn fib-loop [n]
  (loop [n n a 0 b 1]
    (if (= n 0)
      a
      (recur (dec n) b (+ a b)))))

(fib 10)

(defn even? [n]
  (letfn
     [(even? [n]
        (or (= n 0)
            (odd? (dec n))))
      (odd? [n]
        (and (not= n 0)
             (even? (dec n))))]
    (even? n)))

(even? 10)
