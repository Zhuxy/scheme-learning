'use strict';

//无赋值的递归计算
console.log(
	(function (n) {
		return (function (fact) {
			return fact(fact, n);
		})(function (ft, k) {
			if (k === 1) {
				return 1;
			} else {
				return k * ft(ft, k - 1);
			}
		});
	})(10)
);

//上面的语法糖简化版
console.log(
	(n =>
		(fact => fact(fact, n))
		((ft, k) => k === 1 ? 1 : k * ft(ft, k - 1))
	)(10)
);


const Y = func => 
			x => 
				func(Y(func))(x);


const F2 = Y(self =>
	x => x > 1 ? self(x - 1) * x : 1
);

console.log(F2(10));


const C = 
	(Y => 
		func => 
			x => func(Y(Y)(func))(x))
	(Y => func => x => func(Y(Y)(func))(x));





