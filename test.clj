(declare dynamic/pure : (-> [Int] (Dynamic Int)))
(declare + : (-> [Int Int] Int))

(def x (dynamic/pure 1))

(defn main []
  x
)
