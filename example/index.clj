(declare print : (forall [a] (-> [a] Unit)))
(declare concat : (-> [String String] String))
(declare int->string : (-> [Int] String))

(declare dynamic/pure : (forall [a] (-> [a] (Dynamic a))))

(declare dynamic/bind :
  (forall [a b]
    (-> [(Dynamic a) (-> [a] (Dynamic b))] (Dynamic b))))

(declare dynamic/subscribe :
  (forall [a]
    (-> [(Dynamic a) (-> [a] Unit)] Unit)))

(declare ref/new :
  (forall [a]
    (-> [a] (Dynamic a))))

(declare ref/write :
  (forall [a]
    (-> [(Dynamic a) a] Unit)))

(defn debug-subscribe [(name String) (dyn (Dynamic Int))]
  (dynamic/subscribe dyn (fn [(x Int)] (print (concat (concat name ": ") (int->string x))))))

(declare + : (-> [Int Int] Int))

(def x (ref/new 1))
(def y (ref/new 10))

(def _ (debug-subscribe "x" x))
(def _ (debug-subscribe "y" y))
(def _ (debug-subscribe "(+ x 100)" (+ x 100)))
(def _ (debug-subscribe "(+ (+ x 200) 100)" (+ (+ x 200) 100)))

(def _ (ref/write x 2))
