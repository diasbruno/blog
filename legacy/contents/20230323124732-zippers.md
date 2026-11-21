references:

[Functional pearl - The zipper - Gérard Huet 1997](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf)

---

zipper is way to navigate a data structure keeping
a focal point.

the main operations are: `left`, `right`, `up` and `down`.

they are very handy because when a point of interest
is found, you can edit in-place with zero cost.

let's right a simple zipper for an array (it will be a mutable
structure for convinience).

a array zipper will have 3 properties:

- left

keep the elements we walked, from start to focal point.

- at

the current focusing element.

- right

keep the elements from focal point to right.

```js
function ArrayZipper(array) {
  this._left = [];
  this._at = array[0];
  this._right = array.slice(1);
}

ArrayZipper.of = (array) => new ArrayZipper(array);

const azp = ArrayZipper.prototype;

// just get the focusing element
azp.at = function() {
  return this._at;
};

// replace the focusing element with a new value
azp.edit = function(v) {
  this._at = v;
};
```

with our zipper, we can walk right...

```js
azp.right = function() {
  this._left.push(this._at);
  this._at = this._right.shift();
};

let z = ArrayZipper.of([1, 2, 3]);

z.right(), z;
// ArrayZipper { _left: [ 1 ], _at: 2, _right: [ 3 ] }

z.right(), z;
// ArrayZipper { _left: [ 1, 2 ], _at: 3, _right: [] }
```

or walking left...

```js
azp.left = function() {
  this._right = [this._at].concat(this._right);
  this._at = this._left.pop();
};

// walking from our previous defined zipper

z.edit(10), z;
// ArrayZipper { _left: [ 1, 2 ], _at: 10, _right: [] }

z.left(), z
// ArrayZipper { _left: [ 1 ], _at: 2, _right: [ 10 ] }

z.left(), z
// ArrayZipper { _left: [], _at: 1, _right: [ 2, 10 ] }
```

but it can also conform to other interfaces.

```js
azp.map = function(f) {
  this._left = this._left.map(f);
  this._at = f(this._at);
  this._right = this._right.map(f);
  return this;
};

z
// ArrayZipper { _left: [], _at: 1, _right: [ 2, 10 ] }

z.map(x => x + 1), z;
// ArrayZipper { _left: [], _at: 2, _right: [ 3, 11 ] }
```

focus (just like `find`, but to look at an element).

```js
azp.focus = function(f) {
  const tmp = this._left.concat(this._at).concat(this._right);
  const position = tmp.findIndex(f);
  this._left = tmp.slice(0, position);
  this._at = tmp[position];
  this._right = tmp.slice(position + 1, tmp.length);
};

z
// ArrayZipper { _left: [], _at: 2, _right: [ 3, 11 ] }

z.focus(v => v == 11), console.log(z);
// ArrayZipper { _left: [ 2, 3 ], _at: 11, _right: [] }
```

or...find, find...

```js
azp.find = function(f) {
  return this._left.find(f) ||
	(f(this._at) && this._at) ||
	this._right.find(f) ||
	null;
};

z.find(v => v == 11)
// 11
```
