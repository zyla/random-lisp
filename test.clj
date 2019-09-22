(declare dynamic/pure : (forall [a] (-> [a] (Dynamic a))))
(declare dynamic/lift2 :
  (forall [a b c]
    (-> [(-> [a b] c) (Dynamic a) (Dynamic b)] (Dynamic c))))

(declare + : (-> [Int Int] Int))

(def x (dynamic/pure 1))

(defn main []
  (dynamic/lift2 + x x)
)
