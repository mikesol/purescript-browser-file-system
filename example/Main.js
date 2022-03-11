exports.waitTillDone_ = function (bad) {
	return function (good) {
		return function (fr) {
			return function () {
				fr.onerror = function (e) {
					bad(e)();
				};
				fr.onloadend = function () {
					good()();
				};
			};
		};
	};
};
