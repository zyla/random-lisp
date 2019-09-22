const _addListener = (listeners, l) => listeners.push(l);

const _removeListener = (listeners, l) => {
  var i = listeners.indexOf(l);
  if(i !== -1) { listeners.splice(i, 1); }
};

const _notify = (listeners, value) => {
  for(var l of listeners) {
    l(value);
  }
};

const ref$slashnew = (value) => {
  var listeners = [];
  return {
    _read: () => value,
    _addListener: (l) => { _addListener(listeners, l); },
    _removeListener: (l) => { _removeListener(listeners, l) },
    _write: (newValue) => {
      value = newValue;
      _notify(listeners, value);
    }
  };
};

const ref$slashread = (ref) => ref;

const ref$slashwrite = (ref, value) => ref._write(value._read());

const dynamic$slashread = (dyn) => dyn._read();

const dynamic$slashpure = (value) => ({
  _read: () => value,
  _addListener: () => {},
  _removeListener: () => {},
});

const dynamic$slashbind = (outer, cont) => {
  var inner = null;
  var value = null;

  const listeners = [];

  const onInnerChange = (innerValue) => {
    value = innerValue;
    _notify(listeners, value);
  };

  const onOuterChange = (value) => {
    if(inner) {
      inner._removeListener(onInnerChange);
    }
    inner = cont(value);
    inner._addListener(onInnerChange);
    onInnerChange(inner._read());
  };

  const init = () => {
    outer._addListener(onOuterChange); // FIXME: this listener leaks
    inner = cont(outer._read());
    inner._addListener(onInnerChange);
    value = inner._read();
  }

  init();
  
  return {
    _read: () => value,
    _addListener: (l) => { _addListener(listeners, l); },
    _removeListener: (l) => { _removeListener(listeners, l) },
  };
}

const dynamic$slashsubscribe = (dyn, l) => dyn._addListener(l);
const $plus = (a, b) => a + b;
const $minus = (a, b) => a - b;
const print = (x) => console.log(x);
const concat = (a, b) => a + b;
const array$slashconcat = (a, b) => a.concat(a, b);
const not = (b) => !b;

const int$minus$gtstring = (value) => "" + value;

// DOM stuff

var currentParent;

// (declare el : (-> [String (Array Prop) (-> [] Unit)] Unit))
const el = (tag, props, body) => {
  const parent = currentParent;
  const e = document.createElement(tag);
  for(var p of props) { p(e); }
  parent.appendChild(e);
  currentParent = e;
  body();
  currentParent = parent;
};

// (declare text : (-> [(Dynamic String)] Unit))
const text = (dyn) => {
  const node = document.createTextNode(dyn._read());
  currentParent.appendChild(node);
  dyn._addListener((value) => { node.textContent = value });
};

// (declare render-in-body : (-> [(-> [] Unit)] Unit))
const render$minusin$minusbody = (widget) => {
  currentParent = document.body;
  widget();
};

// (declare no-props : (Array Prop))
const no$minusprops = [];

// (declare on-click : (-> [(-> [] Unit)] Prop))
const on$minusclick = (handler) => (el) => el.addEventListener('click', handler);

// (declare on-input : (-> [(-> [String] Unit)] Prop))
const on$minusinput = (handler) => (el) => el.addEventListener('input', () => handler(el.value));

// (declare attr : (-> [String (Dynamic String)] Prop))
const attr = (name, dyn) => (el) => {
  el.setAttribute(name, dyn._read());
  dyn._addListener((value) => { el.setAttribute(name, value); });
};

const attr$minusif = (condD, name, valueD) => (el) => {
  const update = (cond, value) => {
    if(cond) {
      el.setAttribute(name, value);
    } else {
      el.removeAttribute(name);
    }
  };
  update(condD._read(), valueD._read());
  valueD._addListener((value) => { update(condD._read(), value); });
  condD._addListener((cond) => { update(cond, valueD._read()); });
};

const if$ = (cond, then, else_) => cond ? then : else_;


// (declare print : (forall [a] (-> [a] Unit)))
// const print = undefined;

// (declare concat : (-> [String String] String))
// const concat = undefined;

// (declare array/concat : (forall [a] (-> [(Array a) (Array a)] (Array a))))
// const array$slashconcat = undefined;

// (declare int->string : (-> [Int] String))
// const int$minus$gtstring = undefined;

// (declare dynamic/pure : (forall [a] (-> [a] (Dynamic a))))
// const dynamic$slashpure = undefined;

// (declare dynamic/bind : (forall [a b] (-> [(Dynamic a) (-> [a] (Dynamic b))] (Dynamic b))))
// const dynamic$slashbind = undefined;

// (declare dynamic/subscribe : (forall [a] (-> [(Dynamic a) (-> [a] Unit)] Unit)))
// const dynamic$slashsubscribe = undefined;

// (declare dynamic/read : (forall [a] (-> [(Dynamic a)] a)))
// const dynamic$slashread = undefined;

// (declare ref/new : (forall [a] (-> [a] (Dynamic a))))
// const ref$slashnew = undefined;

// (declare ref/write : (forall [a] (-> [(Dynamic a) (Dynamic a)] Unit)))
// const ref$slashwrite = undefined;

// (def debug-subscribe : (-> [String (Dynamic Int)] Unit)
//  (fn ((name String) (dyn (Dynamic Int))) (dynamic/subscribe dyn (fn ((x Int)) (print (concat (concat name ": ") (int->string x)))))))
var debug$minussubscribe = ((name,dyn) => dynamic$slashsubscribe(dyn,((x) => print(concat(concat(name,": "),int$minus$gtstring(x))))));

// (declare + : (-> [Int Int] Int))
// const $plus = undefined;

// (declare - : (-> [Int Int] Int))
// const $minus = undefined;

// (declare el : (-> [String (Array Prop) (-> [] Unit)] Unit))
// const el = undefined;

// (declare text : (-> [(Dynamic String)] Unit))
// const text = undefined;

// (declare on-click : (-> [(-> [] Unit)] Prop))
// const on$minusclick = undefined;

// (declare on-input : (-> [(-> [String] Unit)] Prop))
// const on$minusinput = undefined;

// (declare attr : (-> [String (Dynamic String)] Prop))
// const attr = undefined;

// (declare attr-if : (-> [(Dynamic Boolean) String (Dynamic String)] Prop))
// const attr$minusif = undefined;

// (declare render-in-body : (-> [(-> [] Unit)] Unit))
// const render$minusin$minusbody = undefined;

// (declare if : (forall [a] (-> [Boolean a a] a)))
// const if$ = undefined;

// (declare false : Boolean)
// const false = undefined;

// (declare true : Boolean)
// const true = undefined;

// (declare not : (-> [Boolean] Boolean))
// const not = undefined;

// (declare no-props : (Array Prop))
// const no$minusprops = undefined;

// (def text-input : (-> [(Array Prop) (Dynamic String)] Unit)
//  (fn ((props (Array Prop)) (ref (Dynamic String))) (el "input" (array/concat props [(on-input (fn ((value String)) (ref/write ref (dynamic/pure value)))) (attr "value" ref)]) (fn () (do)))))
var text$minusinput = ((props,ref) => el("input",array$slashconcat(props,[on$minusinput(((value) => ref$slashwrite(ref,dynamic$slashpure(value)))),attr("value",ref)]),(() => ((()=>{})()))));

// (def order-example : Unit
//  (let [(order-id (ref/new 1755)) (restaurant-name (ref/new "Venezia")) (customer-name (ref/new "Jan Kowalski")) (customer-phone (ref/new "123")) (confirmed (ref/new false)) (details-row (fn ((label String) (body (-> [] Unit))) (el "tr" no-props (fn () (do (el "th" no-props (fn () (text (dynamic/pure label)))) (el "td" no-props body))))))] (render-in-body (fn () (do (el "table" no-props (fn () (do (details-row "Order id" (fn () (text (dynamic/bind order-id (fn ((_$0 Int)) (dynamic/pure (int->string _$0))))))) (details-row "Restaurant" (fn () (text restaurant-name))) (details-row "Customer" (fn () (text (dynamic/bind (dynamic/bind customer-name (fn ((_$1 String)) (dynamic/pure (concat _$1 ", ")))) (fn ((_$2 String)) (dynamic/bind customer-phone (fn ((_$3 String)) (dynamic/pure (concat _$2 _$3))))))))) (details-row "Status" (fn () (text (dynamic/bind confirmed (fn ((_$4 Boolean)) (dynamic/pure (if _$4 "Confirmed" "Waiting")))))))))) (el "div" no-props (fn () (do (el "label" no-props (fn () (text (dynamic/pure "Customer name: ")))) (text-input no-props customer-name)))) (el "div" no-props (fn () (el "button" [(on-click (fn () (ref/write customer-phone (dynamic/bind customer-phone (fn ((_$5 String)) (dynamic/pure (concat _$5 "7")))))))] (fn () (text (dynamic/pure "Change phone")))))) (el "div" no-props (fn () (do (el "label" no-props (fn () (text (dynamic/pure "Restaurant: ")))) (text-input no-props restaurant-name)))) (el "div" no-props (fn () (el "button" [(on-click (fn () (ref/write confirmed (dynamic/pure true)))) (attr-if confirmed "disabled" (dynamic/pure "disabled"))] (fn () (text (dynamic/pure "Confirm")))))) (el "div" no-props (fn () (el "button" [(on-click (fn () (ref/write confirmed (dynamic/pure false)))) (attr-if (dynamic/bind confirmed (fn ((_$6 Boolean)) (dynamic/pure (not _$6)))) "disabled" (dynamic/pure "disabled"))] (fn () (text (dynamic/pure "Unconfirm")))))))))))
var order$minusexample = ((()=>{var order$minusid = ref$slashnew(1755);var restaurant$minusname = ref$slashnew("Venezia");var customer$minusname = ref$slashnew("Jan Kowalski");var customer$minusphone = ref$slashnew("123");var confirmed = ref$slashnew(false);var details$minusrow = ((label,body) => el("tr",no$minusprops,(() => ((()=>{el("th",no$minusprops,(() => text(dynamic$slashpure(label))));return el("td",no$minusprops,body);})()))));return render$minusin$minusbody((() => ((()=>{el("table",no$minusprops,(() => ((()=>{details$minusrow("Order id",(() => text(dynamic$slashbind(order$minusid,((_$$0) => dynamic$slashpure(int$minus$gtstring(_$$0)))))));details$minusrow("Restaurant",(() => text(restaurant$minusname)));details$minusrow("Customer",(() => text(dynamic$slashbind(dynamic$slashbind(customer$minusname,((_$$1) => dynamic$slashpure(concat(_$$1,", ")))),((_$$2) => dynamic$slashbind(customer$minusphone,((_$$3) => dynamic$slashpure(concat(_$$2,_$$3)))))))));return details$minusrow("Status",(() => text(dynamic$slashbind(confirmed,((_$$4) => dynamic$slashpure(if$(_$$4,"Confirmed","Waiting")))))));})())));el("div",no$minusprops,(() => ((()=>{el("label",no$minusprops,(() => text(dynamic$slashpure("Customer name: "))));return text$minusinput(no$minusprops,customer$minusname);})())));el("div",no$minusprops,(() => el("button",[on$minusclick((() => ref$slashwrite(customer$minusphone,dynamic$slashbind(customer$minusphone,((_$$5) => dynamic$slashpure(concat(_$$5,"7")))))))],(() => text(dynamic$slashpure("Change phone"))))));el("div",no$minusprops,(() => ((()=>{el("label",no$minusprops,(() => text(dynamic$slashpure("Restaurant: "))));return text$minusinput(no$minusprops,restaurant$minusname);})())));el("div",no$minusprops,(() => el("button",[on$minusclick((() => ref$slashwrite(confirmed,dynamic$slashpure(true)))),attr$minusif(confirmed,"disabled",dynamic$slashpure("disabled"))],(() => text(dynamic$slashpure("Confirm"))))));return el("div",no$minusprops,(() => el("button",[on$minusclick((() => ref$slashwrite(confirmed,dynamic$slashpure(false)))),attr$minusif(dynamic$slashbind(confirmed,((_$$6) => dynamic$slashpure(not(_$$6)))),"disabled",dynamic$slashpure("disabled"))],(() => text(dynamic$slashpure("Unconfirm"))))));})())));})());

// (def counter-example : Unit
//  (let [(count (ref/new 0))] (render-in-body (fn () (do (el "h2" no-props (fn () (text (dynamic/pure "Counter")))) (el "div" no-props (fn () (text (dynamic/bind count (fn ((_$7 Int)) (dynamic/pure (int->string _$7))))))) (el "div" no-props (fn () (el "button" [(on-click (fn () (ref/write count (dynamic/bind count (fn ((_$8 Int)) (dynamic/pure (+ _$8 1)))))))] (fn () (text (dynamic/pure "Increment")))))) (el "div" no-props (fn () (el "button" [(on-click (fn () (ref/write count (dynamic/bind count (fn ((_$9 Int)) (dynamic/pure (- _$9 1)))))))] (fn () (text (dynamic/pure "Decrement")))))))))))
var counter$minusexample = ((()=>{var count = ref$slashnew(0);return render$minusin$minusbody((() => ((()=>{el("h2",no$minusprops,(() => text(dynamic$slashpure("Counter"))));el("div",no$minusprops,(() => text(dynamic$slashbind(count,((_$$7) => dynamic$slashpure(int$minus$gtstring(_$$7)))))));el("div",no$minusprops,(() => el("button",[on$minusclick((() => ref$slashwrite(count,dynamic$slashbind(count,((_$$8) => dynamic$slashpure($plus(_$$8,1)))))))],(() => text(dynamic$slashpure("Increment"))))));return el("div",no$minusprops,(() => el("button",[on$minusclick((() => ref$slashwrite(count,dynamic$slashbind(count,((_$$9) => dynamic$slashpure($minus(_$$9,1)))))))],(() => text(dynamic$slashpure("Decrement"))))));})())));})());
