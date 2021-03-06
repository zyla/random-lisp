; Unit
; (Array Int)
; (-> [(Array a) (Array a)] (Array a))
; (forall [a b c] (-> (-> [a] b) (-> [b] c) (-> [a] c)))

(declare print : (forall [a] (-> [a] Unit)))

; print :: forall a. a -> Unit

(declare concat : (-> [String String] String))
(declare array/concat : (forall [a] (-> [(Array a) (Array a)] (Array a))))
(declare int->string : (-> [Int] String))
(declare + : (-> [Int Int] Int))
(declare - : (-> [Int Int] Int))
(declare if : (forall [a] (-> [Boolean a a] a)))
(declare false : Boolean)
(declare true : Boolean)
(declare not : (-> [Boolean] Boolean))

; (f a b c)
; x
; ""
; 5
; (fn [(a Int) (b Int)] (+ a b))
; (do e1 e2 e3)
; (let [(a e1) (e e2)] ...)



(declare dynamic/pure : (forall [a] (-> [a] (Dynamic a))))

(declare dynamic/bind :
  (forall [a b]
    (-> [(Dynamic a) (-> [a] (Dynamic b))] (Dynamic b))))

(declare dynamic/subscribe :
  (forall [a]
    (-> [(Dynamic a) (-> [a] Unit)] Unit)))

(declare dynamic/read :
  (forall [a]
    (-> [(Dynamic a)] a)))

(declare ref/new :
  (forall [a]
    (-> [a] (Dynamic a))))

(declare ref/write :
  (forall [a]
    (-> [(Dynamic a) (Dynamic a)] Unit)))

(defn debug-subscribe [(name String) (dyn (Dynamic Int))]
  (dynamic/subscribe dyn (fn [(x Int)] (print (concat (concat name ": ") (int->string x))))))

(defn debug-subscribe-str [(name String) (dyn (Dynamic String))]
  (dynamic/subscribe dyn (fn [(x String)]
      (print (concat (concat name ": ") x)))))

(declare ref/write-constant : (forall [a] (-> [(Dynamic a) a] Unit)))

(def main
  (do
    (debug-subscribe "pure 5" (dynamic/pure 5))
    (debug-subscribe "5 >>= (+1)"
        (dynamic/bind (dynamic/pure 5)
                      (fn [(x Int)] (dynamic/pure (+ x 1)))))

    (let [
      (count (ref/new 0))
      (x (ref/new 0))
      (y (ref/new 0))
    ]
    (debug-subscribe "count" count)
    (debug-subscribe-str "x, y"
      (concat (int->string x) (concat ", " (int->string y))))
    (debug-subscribe-str "x" (int->string x))

    (ref/write x (+ x 1))
    (ref/write y 5)
    (ref/write y 10)
    )
  ))

(defn test []
  (let [(x (ref/new 0))]
    (ref/write-constant x (+ x 1)))
)

(declare el : (-> [String (Array Prop) (-> [] Unit)] Unit))
(declare text : (-> [(Dynamic String)] Unit))
(declare on-click : (-> [(-> [] Unit)] Prop))
(declare on-input : (-> [(-> [String] Unit)] Prop))
(declare attr : (-> [String (Dynamic String)] Prop))
(declare attr-if : (-> [(Dynamic Boolean) String (Dynamic String)] Prop))

(declare render-in-body : (-> [(-> [] Unit)] Unit))

; Hack, as we can't yet type an empty array
(declare no-props : (Array Prop))

(defn text-input [(props (Array Prop)) (ref (Dynamic String))]
  (el "input"
      (array/concat props
        [(on-input (fn [(value String)] (ref/write ref value)))
         (attr "value" ref)])
    (fn [] (do))))

(def order-example (fn []
  (let [
    (order-id (ref/new 1755))
    (restaurant-name (ref/new "Venezia"))
    (customer-name (ref/new "Jan Kowalski"))
    (customer-phone (ref/new "123"))
    (confirmed (ref/new false))

    (details-row
      (fn [(label String) (body (-> [] Unit))]
        (el "tr" no-props (fn []
          (el "th" no-props (fn [] (text label)))
          (el "td" no-props body)
        ))))
  ]

  (render-in-body (fn []
    (el "table" no-props (fn []
      (details-row "Order id" (fn [] (text (int->string order-id))))
      (details-row "Restaurant" (fn [] (text restaurant-name)))
      (details-row "Customer" (fn [] (text (concat (concat customer-name ", ") customer-phone))))
      (details-row "Status" (fn [] (text (if confirmed "Confirmed" "Waiting"))))
    ))
    (el "div" no-props (fn []
      (el "label" no-props (fn [] (text "Customer name: ")))
      (text-input no-props customer-name)))
    (el "div" no-props (fn []
      (el "button" [(on-click (fn [] (ref/write customer-phone (concat customer-phone "7"))))]
        (fn [] (text "Change phone")))))
    (el "div" no-props (fn []
      (el "label" no-props (fn [] (text "Restaurant: ")))
      (text-input no-props restaurant-name)))
    (el "div" no-props (fn []
      (el "button"
          [(on-click (fn [] (ref/write confirmed true)))
           (attr-if confirmed "disabled" "disabled") ]
        (fn [] (text "Confirm")))))
    (el "div" no-props (fn []
      (el "button"
          [(on-click (fn [] (ref/write confirmed false)))
           (attr-if (not confirmed) "disabled" "disabled") ]
        (fn [] (text "Unconfirm")))))
  )))
))

(def counter-example (fn []
  (let [
    (count (ref/new 0))
  ]

  (render-in-body (fn []
    (el "h2" no-props (fn [] (text "Counter")))
    (el "div" no-props (fn [] (text (int->string count))))
    (el "div" no-props (fn []
      (el "button" [(on-click (fn [] (ref/write count (+ count 1))))]
        (fn [] (text "Increment")))))
    (el "div" no-props (fn []
      (el "button" [(on-click (fn [] (ref/write count (- count 1))))]
        (fn [] (text "Decrement")))))
  ))
)))

(def main
  (order-example))
