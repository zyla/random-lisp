(declare print : (forall [a] (-> [a] Unit)))

(declare dynamic/pure : (forall [a] (-> [a] (Dynamic a))))

(declare dynamic/bind :
  (forall [a b]
    (-> [(Dynamic a) (-> [a] (Dynamic b))] (Dynamic b))))

(declare dynamic/subscribe :
  (forall [a]
    (-> [(Dynamic a) (-> [a] Unit)] Unit)))

(declare ref/new :
  (forall [a]
    (-> [a] (Ref a))))

(declare ref/read :
  (forall [a]
    (-> [(Ref a)] (Dynamic a))))

(declare ref/write :
  (forall [a]
    (-> [(Ref a) a] Unit)))

(declare + : (-> [Int Int] Int))

(def d (dynamic/pure 1))
(def s 1)

(def s+s (+ s s))
(def d+d (+ d d))
(def d+s (+ d s))

(def ex1 (dynamic/pure d))

(def main
  (dynamic/subscribe (+ d d) (fn [(x Int)] (print x))))
