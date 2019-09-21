(defn 🐱 : (-> [Int Int] Int)
  [a b]
  (+ (+ a a) b)
)

(defn bar : (-> [Int] Int)
  [a a]
  (🐱 a a)
)
