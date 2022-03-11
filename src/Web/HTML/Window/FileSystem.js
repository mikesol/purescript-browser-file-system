var fx = function(f) {
	return function(i) {
		f(i)();
	}
}

exports.showOpenFilePicker_ = function (options) {
	return function (onerror) {
		return function (onsuccess) {
			return function () {
				window.showOpenFilePicker(options).then(fx(onsuccess), fx(onerror));
			};
		};
	};
};

exports.showSaveFilePickerBase_ = function (options) {
	return function (onerror) {
		return function (onsuccess) {
			return function () {
				window.showSaveFilePicker(options).then(fx(onsuccess), fx(onerror));
			};
		};
	};
};

exports.showSaveFilePickerFull_ = function (options) {
	return function (onerror) {
		return function (onsuccess) {
			return function () {
				window.showSaveFilePicker(options).then(fx(onsuccess), fx(onerror));
			};
		};
	};
};

exports.showDirectoryPicker = function (onerror) {
	return function (onsuccess) {
		return function () {
			window.showDirectoryPicker().then(fx(onsuccess), fx(onerror));
		};
	};
};

exports.kind_FileSystemFileHandle = function (fsh) {
	return function () {
		return fsh.kind;
	};
};

exports.name_FileSystemFileHandle = function (fsh) {
	return function () {
		return fsh.name;
	};
};
exports.isSameEntry_FileSystemFileHandle = function (fsh1) {
	return function (fsh2) {
		return function () {
			return fsh1.isSameEntry(fsh2);
		};
	};
};

exports.queryPermission_FileSystemFileHandle = function (fsh) {
	return function (options) {
		return function (onerror) {
			return function (onsuccess) {
				return function () {
					fsh.queryPermission(options).then(fx(onsuccess), fx(onerror));
				};
			};
		};
	};
};

exports.requestPermission_FileSystemFileHandle = function (fsh) {
	return function (options) {
		return function (onerror) {
			return function (onsuccess) {
				return function () {
					fsh.requestPermission(options).then(fx(onsuccess), fx(onerror));
				};
			};
		};
	};
};

exports.kind_FileSystemDirectoryHandle = function (fsh) {
	return function () {
		return fsh.kind;
	};
};

exports.name_FileSystemDirectoryHandle = function (fsh) {
	return function () {
		return fsh.name;
	};
};
exports.isSameEntry_FileSystemDirectoryHandle = function (fsh1) {
	return function (fsh2) {
		return function () {
			return fsh1.isSameEntry(fsh2);
		};
	};
};

exports.queryPermission_FileSystemDirectoryHandle = function (fsh) {
	return function (options) {
		return function (onerror) {
			return function (onsuccess) {
				return function () {
					fsh.queryPermission(options).then(fx(onsuccess), fx(onerror));
				};
			};
		};
	};
};

exports.requestPermission_FileSystemDirectoryHandle = function (fsh) {
	return function (options) {
		return function (onerror) {
			return function (onsuccess) {
				return function () {
					fsh.requestPermission(options).then(fx(onsuccess), fx(onerror));
				};
			};
		};
	};
};

exports.getFile = function (fsh) {
	return function (onerror) {
		return function (onsuccess) {
			return function () {
				fsh.getFile().then(fx(onsuccess), fx(onerror));
			};
		};
	};
};

exports.entries_ = function (makeFSFH) {
	return function (makeFSDH) {
		return function (makeTP) {
			return function (fsh) {
				return function (onerror) {
					return function (onsuccess) {
						return function () {
							var itr = fsh.entries();
							var loopMe = function (x, arr) {
								return x.next().then(function (res) {
									if (res.done) {
										return arr;
									} else {
										return loopMe(x, arr.concat([res.value]));
									}
								});
							};
							loopMe(itr, [])
								.then(function (res) {
									var o = [];
									for (var i = 0; i < res.length; i++) {
										o.push(
											makeTP(res[i][0])(
												res[i][1] instanceof FileSystemFileHandle
													? makeFSFH(res[i][1])
													: makeFSDH(res[i][1])
											)
										);
									}
									return Promise.resolve(o);
								}, onerror)
								.then(fx(onsuccess), fx(onerror));
						};
					};
				};
			};
		};
	};
};

exports.createWritable = function (fsh) {
	return function (onerror) {
		return function (onsuccess) {
			return function () {
				fsh.createWritable().then(fx(onsuccess), fx(onerror));
			};
		};
	};
};

exports.getFileHandle = function (fsh) {
	return function (name) {
		return function (options) {
			return function (onerror) {
				return function (onsuccess) {
					return function () {
						fsh.getFileHandle(name, options).then(fx(onsuccess), fx(onerror));
					};
				};
			};
		};
	};
};

exports.getDirectoryHandle = function (fsh) {
	return function (name) {
		return function (options) {
			return function (onerror) {
				return function (onsuccess) {
					return function () {
						fsh.getDirectoryHandle(name, options).then(fx(onsuccess), fx(onerror));
					};
				};
			};
		};
	};
};
