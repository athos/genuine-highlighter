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