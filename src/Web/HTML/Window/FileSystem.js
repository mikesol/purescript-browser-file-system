exports.showOpenFilePicker_ = function (options) {
	return function () {
		return window.showOpenFilePicker(options);
	};
};

exports.showSaveFilePicker_ = function (options) {
	return function () {
		return window.showSaveFilePicker(options);
	};
};

exports.showDirectoryPicker_ = function () {
	return window.showDirectoryPicker();
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
		return function () {
			return fsh.queryPermission(options);
		};
	};
};

exports.requestPermission_FileSystemFileHandle = function (fsh) {
	return function (options) {
		return function () {
			return fsh.requestPermission(options);
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
		return function () {
			return fsh.queryPermission(options);
		};
	};
};

exports.requestPermission_FileSystemDirectoryHandle = function (fsh) {
	return function (options) {
		return function () {
			return fsh.requestPermission(options);
		};
	};
};

exports.getFile_ = function (fsh) {
	return function () {
		return fsh.getFile();
	};
};

exports.createWritable_ = function (makeFSFH) {
	return function (makeFSDH) {
		return function (makeTP) {
			return function (fsh) {
				return function () {
					return fsh.createWritable().then(function (res) {
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
					});
				};
			};
		};
	};
};

exports.entries_ = function (fsh) {
	return function () {
		return fsh.entries();
	};
};

exports.getFileHandle_ = function (fsh) {
	return function (name) {
		return function (options) {
			return function () {
				return fsh.getFileHandle(name, options);
			};
		};
	};
};

exports.getDirectoryHandle_ = function (fsh) {
	return function (name) {
		return function (options) {
			return function () {
				return fsh.getDirectoryHandle(name, options);
			};
		};
	};
};
