(declare dynamic/pure : (forall [a] (-> [a] (Dynamic a))))
(declare dynamic/lift2 :
  (forall [a b c]
    (-> [(-> [a b] c) (Dynamic a) (Dynamic b)] (Dynamic c))))

(declare + : (-> [Int Int] Int))

(defn main []
  (+ (dynamic/pure 1) 2)
)
