;; What do we want?

;; 1. Have a single description / web page template
;; 2. Have it rendered to static HTML + minimal _residual JS_ needed to update the dynamic parts.
;;
;; where "minimal" - doesn't contain parts of the layout that don't change

;; Simplifying assumptions:
;; - Only static structure. No lists.

;; How to represent templates?

;; Example:

;; <div class="user-info">
;;   <p><strong>First name:</strong><span class="first-name">John</span></p>
;;   <p><strong>Last name:</strong><span class="last-name">Smith</span></p>
;; </div>

(def render-user-info
  (lambda (user)
    (el "div" [:class "user-info"] [
      (el "p" [] [
        (el "strong" [] [ (text "First name:") ])
        (el "span" [:class "first-name"] [ (text (:first-name user)) ])])
      (el "p" [] [
        (el "strong" [] [ (text "Last name:") ])
        (el "span" [:class "last-name"] [ (text (:last-name user)) ])])
    ])))

(def my-user { :first-name "John" :last-name "Smith" })
(render-static (render-user-info my-user))

;; ==>

;; <div class="user-info">
;;   <p><strong>First name:</strong><span class="first-name">John</span></p>
;;   <p><strong>Last name:</strong><span class="last-name">Smith</span></p>
;; </div>

(def my-new-user { :first-name "John" :last-name "Appleseed" })
(update-page (render-user-info my-new-user))

;; ==>

;; document.querySelector('span.first-name').innerText = 'John';
;; document.querySelector('span.last-name').innerText = 'Appleseed';


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def user-info-template
  (el "div" [:class "user-info"] [
    (el "p" [] [
      (el "strong" [] [ (text "First name:") ])
      (dyn-text-el "span" [:class "first-name"] (lambda (user) (:first-name user)))
    ])])
    (el "p" [] [
      (el "strong" [] [ (text "Last name:") ])
      (dyn-text-el "span" [:class "last-name"] (lambda (user) (:last-name user)))
    ]))

;; <div class="user-info">
;;   <p><strong>First name:</strong><span class="first-name" data-plisp-id="1">John</span></p>
;;   <p><strong>Last name:</strong><span class="last-name" data-plisp-id="2">Smith</span></p>
;; </div>

(def render-static
  (lambda (template env)
    (match template
      ((el ,tag ,attrs ,children)
         (concat-strings
           "<" tag " " (render-attrs attrs) ">"
           (concat-map (lambda (x) (render-static x env) children)
           "</" tag ">")))
      ((dyn-text-el ,tag ,attrs ,text-fn)
        (let (attrs' (assign attrs :data-plisp-id (next-id)))
         (concat-strings
           "<" tag " " (render-attrs attrs') ">"
           (text-fn env)
           "</" tag ">"))))))


(def render-attrs (lambda (attrs) ...))


;; Returns a function that when called, updates the DOM with new env
(def render-dynamic
  (lambda (template)
    (lambda (env)
      (match template
        ((el ,tag ,attrs ,children)
           (map (lambda (x) ((render-dynamic x) env) children)))
        ((dyn-text-el ,tag ,attrs ,text-fn)
          (let
            (id (next-id))
            (node (query-selector (concat-strings "[data-plisp-id=" (int->str id) "]")))
           (set-inner-text node (text-fn env))))))))


((render-dynamic user-info-template) my-new-user)


;; Shift stuff around so that the template is examined outside the returned closure

(def render-dynamic
  (lambda (template)
    (match template
      ((el ,tag ,attrs ,children)
        (let (children-update-fns (concat-map render-dynamic children))
         (lambda (env)
           (map (lambda (update-fn) (update-fn env) children-update-fns)))))
      ((dyn-text-el ,tag ,attrs ,text-fn)
        (let (id (next-id))
         [(lambda (env)
           (set-inner-text
             (query-selector (concat-strings "[data-plisp-id=" (int->str id) "]")
             (text-fn env))))])))))

(code (render-dynamic user-info-template))

;; ==>

(lambda (env)
  (map (lambda (update-fn) (update-fn env)) [
    (lambda (env) (map (lambda (update-fn) (update-fn env)) []))
    (lambda (env)
      (set-inner-text
         (query-selector (concat-strings "[data-plisp-id=" (int->str 1) "]")
         ((lambda (user) (:first-name user)) env))))

    ;; useless cruft by non-dynamic elements
    (lambda (env) (map (lambda (update-fn) (update-fn env)) [
      (lambda (env) (map (lambda (update-fn) (update-fn env)) []))
      (lambda (env) (map (lambda (update-fn) (update-fn env)) []))
    ]))

    ;; useless cruft by non-dynamic elements
    (lambda (env) (map (lambda (update-fn) (update-fn env)) []))

    (lambda (env)
      (set-inner-text
         (query-selector (concat-strings "[data-plisp-id=" (int->str 2) "]")
         ((lambda (user) (:last-name user)) env))))
    ]))

;; we really want this:
(lambda (env)
  (do
     (set-inner-text
        (query-selector (concat-strings "[data-plisp-id=" (int->str 1) "]")
        ((lambda (user) (:first-name user)) env)))
     (set-inner-text
        (query-selector (concat-strings "[data-plisp-id=" (int->str 2) "]")
        ((lambda (user) (:last-name user)) env)))
  ))

;; and really really:
;; node pre-caching, and inlining
(let
  (node1 (query-selector (concat-strings "[data-plisp-id=" (int->str 1) "]"))
  (node2 (query-selector (concat-strings "[data-plisp-id=" (int->str 2) "]"))
 (lambda (env)
   (do
      (set-inner-text node1 (:first-name env)
      (set-inner-text node2 (:last-name env))))


