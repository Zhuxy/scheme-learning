'use strict';

//js定义的常规递归函数
let fact = 
	n => n == 1 ? 1 : n * fact(n - 1);

//如果函数在定义完成之前不能调用自己, 必须引入额外参数self来代表其自己
//这样的函数先称为伪递归, 要找到self的实现并传入来生成真正的递归函数
let F =
	self =>
		n => n == 1 ? 1 : n * self(n - 1);

//定义一个可以接受函数F, 并提取其中self参数对应的函数体
let call =
	f =>
		x => f(call(f))(x);
//当call(F)时, 参数f就是F
//要继续往下提取F的函数体, 就要用f去调用F的self
//也就是当前call函数的目的, 这里递归调用call(f)

//test
console.log(call(F)(10)); //3628800

//这里还是用到了递归call, 再加一层, 把call放入到参数中
let recur_call =
	call =>
		f =>
			x => f((call(call))(f))(x);
//要取得最后对于x的函数需要调用2次
//第一次传入call, 第二次传入F
//所以对于f里需要的参数self, 就是执行上面2步 (call(call))(f)

//最终的Y算子
let Y = 
	f =>
		(call =>
			f =>
				x => f((call(call))(f))(x))
		(call =>
			f =>
				x => f((call(call))(f))(x))
		(f);

//test
let real_fact = Y(F);
console.log(real_fact(10)); //3628800

