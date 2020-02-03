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

const dynamic$slashsubscribe = (dyn, l) => {
  l(dyn._read());
  dyn._addListener(l);
};
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
