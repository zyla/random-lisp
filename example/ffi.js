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
