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
    read: () => value,
    addListener: (l) => { _addListener(listeners, l); },
    removeListener: (l) => { _removeListener(listeners, l) },
    write: (newValue) => {
      value = newValue;
      _notify(listeners, value);
    }
  };
};

const ref$slashread = (ref) => ref;

const ref$slashwrite = (ref, value) => ref.write(value);

const dynamic$slashpure = (value) => ({
  read: () => value,
  addListener: () => {},
  removeListener: () => {},
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
      inner.removeListener(onInnerChange);
    }
    inner = cont(value);
    inner.addListener(onInnerChange);
    onInnerChange(inner.read());
  };

  const init = () => {
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

const dynamic$slashsubscribe = (dyn, l) => dyn.addListener(l);
const $plus = (a, b) => a + b;
const print = (x) => console.log(x);
const concat = (a, b) => a + b;

const int$minus$gtstring = (value) => "" + value;
