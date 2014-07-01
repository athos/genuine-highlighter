(defn fib [n]
  (loop [n n a 0 b 1]
    (if (= n 0)
      a
      (recur (dec n) b (+ a b)))))
