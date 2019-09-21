(fix 🤖 (n)
  (if (zero? n)
    1
    (* n (🤖 (- n 1)))))
