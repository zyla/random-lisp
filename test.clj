(declare dynamic/pure : (forall [a] (-> [a] (Dynamic a))))

(declare dynamic/bind :
  (forall [a b]
    (-> [(Dynamic a) (-> [a] (Dynamic b))] (Dynamic b))))

(declare + : (-> [Int Int] Int))

(def d (dynamic/pure 1))
(def s 1)

(def s+s (+ s s))
(def d+d (+ d d))
(def d+s (+ d s))
(def s+d (+ s d))
