const _addListener = (listeners, l) => listeners.push(l);

const _removeListener = (listeners, l) => {
    var i = listeners.indexOf(l);
    if (i !== -1) {
        listeners.splice(i, 1);
    }
};

const _notify = (listeners, value) => {
    for (var l of listeners) {
        l(value);
    }
};

const ref$slashnew = value => {
    var listeners = [];
    return {
        _read: () => value,
        _addListener: l => {
            _addListener(listeners, l);
        },
        _removeListener: l => {
            _removeListener(listeners, l);
        },
        _write: newValue => {
            value = newValue;
            _notify(listeners, value);
        }
    };
};

const ref$slashread = ref => ref;

const ref$slashwrite = (ref, value) => ref._write(value._read());

const dynamic$slashread = dyn => dyn._read();

const dynamic$slashpure = value => ({
    _read: () => value,
    _addListener: () => {},
    _removeListener: () => {}
});

const dynamic$slashbind = (outer, cont) => {
    var inner = null;
    var value = null;
    const listeners = [];
    const onInnerChange = innerValue => {
        value = innerValue;
        _notify(listeners, value);
    };
    const onOuterChange = value => {
        if (inner) {
            inner._removeListener(onInnerChange);
        }
        inner = cont(value);
        inner._addListener(onInnerChange);
        onInnerChange(inner._read());
    };
    const init = () => {
        outer._addListener(onOuterChange);
        inner = cont(outer._read());
        inner._addListener(onInnerChange);
        value = inner._read();
    };
    init();
    return {
        _read: () => value,
        _addListener: l => {
            _addListener(listeners, l);
        },
        _removeListener: l => {
            _removeListener(listeners, l);
        }
    };
};

const dynamic$slashsubscribe = (dyn, l) => {
    l(dyn._read());
    dyn._addListener(l);
};

const $plus = (a, b) => a + b;

const $minus = (a, b) => a - b;

const print = x => console.log(x);

const concat = (a, b) => a + b;

const array$slashconcat = (a, b) => a.concat(a, b);

const not = b => !b;

const int$minus$gtstring = value => "" + value;

var currentParent;

const el = (tag, props, body) => {
    const parent = currentParent;
    const e = document.createElement(tag);
    for (var p of props) {
        p(e);
    }
    parent.appendChild(e);
    currentParent = e;
    body();
    currentParent = parent;
};

const text = dyn => {
    const node = document.createTextNode(dyn._read());
    currentParent.appendChild(node);
    dyn._addListener(value => {
        node.textContent = value;
    });
};

const render$minusin$minusbody = widget => {
    currentParent = document.body;
    widget();
};

const no$minusprops = [];

const on$minusclick = handler => el => el.addEventListener("click", handler);

const on$minusinput = handler => el => el.addEventListener("input", () => handler(el.value));

const attr = (name, dyn) => el => {
    el.setAttribute(name, dyn._read());
    dyn._addListener(value => {
        el.setAttribute(name, value);
    });
};

const attr$minusif = (condD, name, valueD) => el => {
    const update = (cond, value) => {
        if (cond) {
            el.setAttribute(name, value);
        } else {
            el.removeAttribute(name);
        }
    };
    update(condD._read(), valueD._read());
    valueD._addListener(value => {
        update(condD._read(), value);
    });
    condD._addListener(cond => {
        update(cond, valueD._read());
    });
};

const if$ = (cond, then, else_) => cond ? then : else_;

var debug$minussubscribe = (name, dyn) => dynamic$slashsubscribe(dyn, x => print(concat(concat(name, ": "), int$minus$gtstring(x))));

var debug$minussubscribe$minusstr = (name, dyn) => dynamic$slashsubscribe(dyn, x => print(concat(concat(name, ": "), x)));

var main = (() => {
    debug$minussubscribe("pure 5", dynamic$slashpure(5));
    debug$minussubscribe("5 >>= (+1)", dynamic$slashbind(dynamic$slashpure(5), x => dynamic$slashpure($plus(x, 1))));
    return (() => {
        var count = ref$slashnew(0);
        var x = ref$slashnew(0);
        var y = ref$slashnew(0);
        debug$minussubscribe("count", count);
        debug$minussubscribe$minusstr("x, y", dynamic$slashbind(dynamic$slashbind(x, _$$0 => dynamic$slashpure(int$minus$gtstring(_$$0))), _$$3 => dynamic$slashbind(dynamic$slashbind(dynamic$slashbind(y, _$$1 => dynamic$slashpure(int$minus$gtstring(_$$1))), _$$2 => dynamic$slashpure(concat(", ", _$$2))), _$$4 => dynamic$slashpure(concat(_$$3, _$$4)))));
        debug$minussubscribe$minusstr("x", dynamic$slashbind(x, _$$5 => dynamic$slashpure(int$minus$gtstring(_$$5))));
        ref$slashwrite(x, dynamic$slashbind(x, _$$6 => dynamic$slashpure($plus(_$$6, 1))));
        ref$slashwrite(y, dynamic$slashpure(5));
        return ref$slashwrite(y, dynamic$slashpure(10));
    })();
})();

var test = () => (() => {
    var x = ref$slashnew(0);
    return dynamic$slashbind(dynamic$slashbind(x, _$$7 => dynamic$slashpure($plus(_$$7, 1))), _$$8 => dynamic$slashpure(ref$slashwrite$minusconstant(x, _$$8)));
})();

var text$minusinput = (props, ref) => el("input", array$slashconcat(props, [ on$minusinput(value => ref$slashwrite(ref, dynamic$slashpure(value))), attr("value", ref) ]), () => (() => {})());

var order$minusexample = () => (() => {
    var order$minusid = ref$slashnew(1755);
    var restaurant$minusname = ref$slashnew("Venezia");
    var customer$minusname = ref$slashnew("Jan Kowalski");
    var customer$minusphone = ref$slashnew("123");
    var confirmed = ref$slashnew(false);
    var details$minusrow = (label, body) => el("tr", no$minusprops, () => (() => {
        el("th", no$minusprops, () => text(dynamic$slashpure(label)));
        return el("td", no$minusprops, body);
    })());
    return render$minusin$minusbody(() => (() => {
        el("table", no$minusprops, () => (() => {
            details$minusrow("Order id", () => text(dynamic$slashbind(order$minusid, _$$9 => dynamic$slashpure(int$minus$gtstring(_$$9)))));
            details$minusrow("Restaurant", () => text(restaurant$minusname));
            details$minusrow("Customer", () => text(dynamic$slashbind(dynamic$slashbind(customer$minusname, _$$10 => dynamic$slashpure(concat(_$$10, ", "))), _$$11 => dynamic$slashbind(customer$minusphone, _$$12 => dynamic$slashpure(concat(_$$11, _$$12))))));
            return details$minusrow("Status", () => text(dynamic$slashbind(confirmed, _$$13 => dynamic$slashpure(if$(_$$13, "Confirmed", "Waiting")))));
        })());
        el("div", no$minusprops, () => (() => {
            el("label", no$minusprops, () => text(dynamic$slashpure("Customer name: ")));
            return text$minusinput(no$minusprops, customer$minusname);
        })());
        el("div", no$minusprops, () => el("button", [ on$minusclick(() => ref$slashwrite(customer$minusphone, dynamic$slashbind(customer$minusphone, _$$14 => dynamic$slashpure(concat(_$$14, "7"))))) ], () => text(dynamic$slashpure("Change phone"))));
        el("div", no$minusprops, () => (() => {
            el("label", no$minusprops, () => text(dynamic$slashpure("Restaurant: ")));
            return text$minusinput(no$minusprops, restaurant$minusname);
        })());
        el("div", no$minusprops, () => el("button", [ on$minusclick(() => ref$slashwrite(confirmed, dynamic$slashpure(true))), attr$minusif(confirmed, "disabled", dynamic$slashpure("disabled")) ], () => text(dynamic$slashpure("Confirm"))));
        return el("div", no$minusprops, () => el("button", [ on$minusclick(() => ref$slashwrite(confirmed, dynamic$slashpure(false))), attr$minusif(dynamic$slashbind(confirmed, _$$15 => dynamic$slashpure(not(_$$15))), "disabled", dynamic$slashpure("disabled")) ], () => text(dynamic$slashpure("Unconfirm"))));
    })());
})();

var counter$minusexample = () => (() => {
    var count = ref$slashnew(0);
    return render$minusin$minusbody(() => (() => {
        el("h2", no$minusprops, () => text(dynamic$slashpure("Counter")));
        el("div", no$minusprops, () => text(dynamic$slashbind(count, _$$16 => dynamic$slashpure(int$minus$gtstring(_$$16)))));
        el("div", no$minusprops, () => el("button", [ on$minusclick(() => ref$slashwrite(count, dynamic$slashbind(count, _$$17 => dynamic$slashpure($plus(_$$17, 1))))) ], () => text(dynamic$slashpure("Increment"))));
        return el("div", no$minusprops, () => el("button", [ on$minusclick(() => ref$slashwrite(count, dynamic$slashbind(count, _$$18 => dynamic$slashpure($minus(_$$18, 1))))) ], () => text(dynamic$slashpure("Decrement"))));
    })());
})();

var main = order$minusexample();
