## Set interface

my conclusion is that Set is just a base class.

the operations available are: `has`, `add` and `delete`.

there is no `map`, or `filter`, or `reduce`.
but this one is kinda easy to fix...

```js
class AsSet extends Set {
  constructor(initial) {
	super(initial);
  }

  reduce(f, acc) {
	for (let [_, value] of this.entries()) {
	  acc = f(acc, value);
	}
	return acc;
  }

  map(f) {
	return this.reduce(
	  (acc, item) => (acc.add(f(item)), acc),
	  new this.constructor()
	);
  }

  find(f) {
	return this.reduce((acc, item) => (acc ? acc : f(item) && item), null);
  }
}
```

now we have something to work on.


[codesandbox with example](https://codesandbox.io/p/sandbox/quizzical-haze-qfyxow?file=%2Findex.js)

we could implement iterator, but this one is for the reader.

## the json problem

this is actually something good to talk about.

json doesn't have any representation for Sets,
only for array.

this is a serializer/deserializer problem and not a JSON one.

this means that if we want to retrieve a Set from a json (and to json),
we need to explicitly tell that a particular value is a Set.

this is great because we can parse directly into a class,
and not use raw objects.

```js
class User {
  permissions = new Set();
}

User.propertyToJSON = (key, value, self) => {
  return key === "permissions" ? Array.from(value) : value;
};

User.reviverJSON = (key, value) => {
  return key === "permissions" ? new Set(value) : value;
};

User.toJSON = (user) => {
  return JSON.stringify(user, User.propertyToJSON);
};

User.fromJSON = (json) => {
  const { permissions } = JSON.parse(json, User.reviverJSON);
  const user = new User();
  user.permissions = permissions;
  return user;
};

const user = new User();
user.permissions.add(1);

deepEqual(user, User.fromJSON(User.toJSON(user)));
```

which means that `User.fromJSON . User.toJSON = id`.
(it reads fromJSON composed with toJSON
is the same of doing nothing).

[codesandbox with example](https://codesandbox.io/p/sandbox/weird-ideas-with-sets-forked-5mgurt?file=%2Findex.js)


this is handmade, but probably there is some packages smart enouth
to help with this.


## use Set when you need a Set...

this is a snippet people share on social media:

```js
[...new Set(list)]
```

the idea is to remove duplicates from a list.

let's understand what is going on.

- create a initial list

this is ok. `Set` can be initiated with a list or another `Set`
(like a copy constructor).

- Set will remove duplicates

it is its job...keep only unique items.

- expand the set into a new list (?)

this operation allocates at least a new list with size `N - m`
been `N` the size of the initial list and `m` the number of duplicated
items. worst case is when `m = 0` meaning where are just cloning the list.

the operation itself is not critical if executed once or twice,
the problems starts when the list grows or executing this operation
too many times.

the question here is why?

in most cases, this operation is uneccessary. we don't really need
to change data structures. we can just start directly with a Set
and keep adding stuff to it.

```js
let x = new Set();
x.add(1);
x.add(1);
x.add(2);
// Set { 1, 2 }
```

i would guess the reason is because most people use raw objects
on their projects and Set is not easily converted to/from JSON.
maybe it is related to what tools are available to facilitate
serialize/deserialize objects.

but there are more interesting uses of Set!

### Togglable set

a nice class to work when you need to use bulk (mass) selection.

```js
const ADD = "add";
const DELETE = "delete";

class Togglable extends AsSet {
  toggle(x) {
	this[this.has(x) ? DELETE : ADD](x);
  }
}

const a = new Togglable()
a.toggle(1);
// Set { 1 }
a.toggle(1)
// Set { }
a.toggle(2)
// Set { 2 }
Array.from(a);
// [ 2 ]
```

[codesandbox with example](https://codesandbox.io/p/sandbox/togglable-set-p0vbf1?file=%2Findex.js)

### SetWith - Set, but by a specific property of an object

Set can accept objects, but to check if the object is present
it uses strict equality, so it must be the same instance.

sometimes we don't want this. we want that it behaves live a Set
on a particular property, because we know that all objects are the same
(they are just duplicated), or, if we try to add an item with the same key
we are not going to accept it (like unique constrait).

```js
class SetWith extends Set {
  constructor(initial, testFunction) {
	super();
	this.sublist = {};
	this.testFunction = testFunction;
	this._add = this.add;
	this._delete = this.delete;

	this.add = (x) => {
	  const value = this.testFunction(x);
	  if (!this.has(value)) {
		this.sublist[value] = x;
		this._add(value);
	  }
	};

	this.delete = (x) => {
	  const value = this.testFunction(x);
	  if (this.has(value)) {
		delete this.sublist[value];
		this._delete(value);
	  }
	};

	initial.forEach((item) => this.add(item));
  }

  find(f) {
	for (let x of Object.values(this.sublist)) {
	  if (f(x)) return x;
	}
	return null;
  }
}

const a = new SetWith([], item => item.id);

a.add({ id: 1 });
// Set() { 1, sublist: { 1: { id: 1 } } }

a.add({ id: 1 });
// Set() { 1, sublist: { 1: { id: 1 } } }

a.add({ id: 2 });
// Set() { 1, 2, sublist: { 1: { id: 1 }, 2: { id: 2 } } }
```

[codesandbox with example](https://codesandbox.io/p/sandbox/setwith-a-set-but-by-an-object-property-3cp1wv?file=%2Findex.js)

it makes it clear what we want to accomplish. you can use this
when people manually add items to a system and it shouldn't have
duplicated values.

```js
const a = new SetWith([], item => item.currency);

a.add({ currency: "JPY", value: 2 });
a.add({ currency: "USD", value: 1 });
a.add({ currency: "BRL", value: 0.2 }); // :(

// this next operation is impossible
// in order to change the value you need
// to get the reference from the set and update,
// or delete the current one.
a.add({ currency: "USD", value: 5 });

const currency = a.find(i => i.currency == "USD");
currency.value = 5;

// now, this is feature safe by construction.
```
