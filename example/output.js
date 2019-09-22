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

const ref$slashwrite = (ref, value) => ref._write(value);

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
const print = (x) => console.log(x);
const concat = (a, b) => a + b;

const int$minus$gtstring = (value) => "" + value;


// (def debug-subscribe : (-> [String (Dynamic Int)] Unit)
//  (fn ((name String) (dyn (Dynamic Int))) (dynamic/subscribe dyn (fn ((x Int)) (print (concat (concat name ": ") (int->string x)))))))
var debug$minussubscribe = ((name,dyn) => dynamic$slashsubscribe(dyn,((x) => print(concat(concat(name,": "),int$minus$gtstring(x))))));

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
var _ = debug$minussubscribe("(+ x 100)",dynamic$slashbind(x,((_$$0) => dynamic$slashpure($plus(_$$0,100)))));

// (def _ : Unit
//  (debug-subscribe "(+ (+ x 200) 100)" (dynamic/bind (dynamic/bind x (fn ((_$1 Int)) (dynamic/pure (+ _$1 200)))) (fn ((_$2 Int)) (dynamic/pure (+ _$2 100))))))
var _ = debug$minussubscribe("(+ (+ x 200) 100)",dynamic$slashbind(dynamic$slashbind(x,((_$$1) => dynamic$slashpure($plus(_$$1,200)))),((_$$2) => dynamic$slashpure($plus(_$$2,100)))));

// (def _ : Unit
//  (ref/write x 2))
var _ = ref$slashwrite(x,2);
