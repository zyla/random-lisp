function _addListener(listeners, l) {
  listeners.push(l);
}

function _removeListener(listeners, l) {
  var i = listeners.indexOf(l);
  if(i !== -1) { listeners.splice(i, 1); }
}

function _notify(listeners, value) {
  for(var l of listeners) {
    l(value);
  }
}

function ref$slashnew(value) {
  var listeners = [];
  return {
    read: () => value,
    addListener: (l) => { _addListener(listeners, l); },
    removeListener: (l) => { _removeListener(listeners, l) },
    write: (newValue) => {
      value = newValue;
      _notify(listeners, value);
    }
  };
}

function ref$slashread(ref) {
  return ref;
}

function ref$slashwrite(ref, value) {
  ref.write(value);
}

function dynamic$slashpure(value) {
  return {
    read: () => value,
    addListener: () => {},
    removeListener: () => {},
  };
}

function dynamic$slashbind(outer, cont) {
  var listeners = [];
  var inner = null;
  var value = null;

  var onInnerChange = (innerValue) => {
    value = innerValue;
    _notify(listeners, value);
  };

  var onOuterChange = (value) => {
    if(inner) {
      inner.removeListener(onInnerChange);
    }
    inner = cont(value);
    inner.addListener(onInnerChange);
    onInnerChange(inner.read());
  };

  function init() {
    outer.addListener(onOuterChange); // FIXME: this listener leaks
    inner = cont(outer.read());
    value = inner.read();
  }

  init();
  
  return {
    read: () => value,
    addListener: (l) => { _addListener(listeners, l); },
    removeListener: (l) => { _removeListener(listeners, l) },
  };
}

function dynamic$slashsubscribe(dyn, l) {
  dyn.addListener(l);
}

function $plus(a, b) {
  return a + b;
}

function print(x) {
  console.log(x);
}

function concat(a, b) {
  return a + b;
}

function int$minus$gtstring(value) {
  return "" + value;
}


// (def debug-subscribe : (-> [String (Dynamic Int)] Unit)
//  (fn ((name String) (dyn (Dynamic Int))) (dynamic/subscribe dyn (fn ((x Int)) (print (concat (concat name ": ") (int->string x)))))))
var debug$minussubscribe = (function (name,dyn) { return dynamic$slashsubscribe(dyn,(function (x) { return print(concat(concat(name,": "),int$minus$gtstring(x))); })); });

// (def x : (Dynamic Int)
//  (ref/new 1))
var x = ref$slashnew(1);

// (def y : (Dynamic Int)
//  (ref/new 10))
var y = ref$slashnew(10);

// (def _ : Unit
//  (debug-subscribe "x" x))
var _ = debug$minussubscribe("x",x);

// (def _ : Unit
//  (debug-subscribe "y" y))
var _ = debug$minussubscribe("y",y);

// (def _ : Unit
//  (debug-subscribe "(+ x 100)" (dynamic/bind x (fn ((_$0 Int)) (dynamic/pure (+ _$0 100))))))
var _ = debug$minussubscribe("(+ x 100)",dynamic$slashbind(x,(function (_$$0) { return dynamic$slashpure($plus(_$$0,100)); })));

// (def _ : Unit
//  (debug-subscribe "(+ (+ x 200) 100)" (dynamic/bind (dynamic/bind x (fn ((_$1 Int)) (dynamic/pure (+ _$1 200)))) (fn ((_$2 Int)) (dynamic/pure (+ _$2 100))))))
var _ = debug$minussubscribe("(+ (+ x 200) 100)",dynamic$slashbind(dynamic$slashbind(x,(function (_$$1) { return dynamic$slashpure($plus(_$$1,200)); })),(function (_$$2) { return dynamic$slashpure($plus(_$$2,100)); })));

// (def _ : Unit
//  (ref/write x 2))
var _ = ref$slashwrite(x,2);
