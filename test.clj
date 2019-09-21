(declare dynamic/pure : (forall [a] (-> [a] (Dynamic a))))
(declare + : (-> [Int Int] Int))

(def x (dynamic/pure 1))

(defn main []
  x
)
