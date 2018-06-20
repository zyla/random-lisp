// Runtime
function zero$huh(x) { return x === 0; }
function $mul(x, y) { return x * y; }
function $minus(x, y) { return x - y; }

var fac =
// Generated code
(function fac(n) { return (zero$huh(n))?(1):($mul(n,fac($minus(n,1)))); })
;

console.log(fac(5));
