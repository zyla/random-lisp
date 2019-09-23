A toy programming language for GUI programming. Compiles to JavaScript.

The main feature is that the language is integrated with at FRP-like system,
and computations over dynamically changing values look just like pure
computations.

```clojure
; A simple pure function
(defn add-two-static-numbers
  [(x Int) (y Int)]
  (+ x y))

; With the same code we can operate on dynamic values, mixing them with pure ones
; The result type of this function is `(Dynamic Int)`
(defn add-dynamic-to-static-number
  [(x (Dynamic Int)) (y Int)]
  (+ x y))
```

## Example

Here's the obligatory Counter example:

```clojure
(def example-counter
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
))
```

Compiles to the following JS:

```javascript
var example$minuscounter = (() => {
    var count = ref$slashnew(0);
    return render$minusin$minusbody(() => (() => {
        el("h2", no$minusprops, () => text(dynamic$slashpure("Counter")));
        el("div", no$minusprops, () => text(dynamic$slashbind(count, _$$7 => dynamic$slashpure(int$minus$gtstring(_$$7)))));
        el("div", no$minusprops, () => el("button", [ on$minusclick(() => ref$slashwrite(count, dynamic$slashbind(count, _$$8 => dynamic$slashpure($plus(_$$8, 1))))) ], () => text(dynamic$slashpure("Increment"))));
        return el("div", no$minusprops, () => el("button", [ on$minusclick(() => ref$slashwrite(count, dynamic$slashbind(count, _$$9 => dynamic$slashpure($minus(_$$9, 1))))) ], () => text(dynamic$slashpure("Decrement"))));
    })());
})();
```

Notice how the compiler inserted a bunch of `dynamic/pure` and `dynamic/bind` noise in the appropriate places.
