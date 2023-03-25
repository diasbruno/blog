function ArrayZipper(array) {
  this._left = [];
  this._at = array[0];
  this._right = array.slice(1);
}

ArrayZipper.of = (array) => new ArrayZipper(array);

const azp = ArrayZipper.prototype;

azp.right = function() {
  this._left.push(this._at);
  this._at = this._right.shift();
};

azp.left = function() {
  this._right = [this._at].concat(this._right);
  this._at = this._left.pop();
};

azp.at = function() {
  return this._at;
};

azp.edit = function(v) {
  this._at = v;
};

azp.map = function(f) {
  this._left = this._left.map(f);
  this._at = f(this._at);
  this._right = this._right.map(f);
  return this;
};

let list = [1, 2, 3],
    z = ArrayZipper.of(list);

console.log(list, z);
// ArrayZipper { _left: [], _at: 1, _right: [ 2, 3 ] }

z.right(), console.log(z);
// ArrayZipper { _left: [ 1 ], _at: 2, _right: [ 3 ] }

z.right(), console.log(z);
// ArrayZipper { _left: [ 1, 2 ], _at: 3, _right: [] }

z.edit(10), console.log(z);
// ArrayZipper { _left: [ 1, 2 ], _at: 10, _right: [] }

console.log(z);
// ArrayZipper { _left: [ 1, 2 ], _at: 10, _right: [] }

z.left(), console.log(z);
// ArrayZipper { _left: [ 1 ], _at: 2, _right: [ 10 ] }

z.left(), console.log(z);
// ArrayZipper { _left: [], _at: 1, _right: [ 2, 10 ] }

z.map(x => x + 1), console.log(z);
// ArrayZipper { _left: [], _at: 2, _right: [ 3, 11 ] }

azp.focus = function(f) {
  const tmp = this._left.concat([this._at]).concat(this._right);
  const position = tmp.findIndex(f);
  this._left = tmp.slice(0, position);
  this._at = tmp[position];
  this._right = tmp.slice(position + 1, tmp.length);
};

z.focus(v => v == 11), console.log(z);

azp.find = function(f) {
  return this._left.find(f) ||
    (f(this._at) && this._at) ||
    this._right.find(f) ||
    null;
};

console.log(z.find(v => v == 11))
