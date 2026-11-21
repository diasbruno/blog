javascript has a weird type/object system, but we can make some interesting stuff with it...

#### pattern match or a nice switch/case

We don't need the most precise/robust implementation
of pattern matching. It can be used on user-defined
types to help spliting what can be on a class and
what can be simple functions instead.

```js
/**
 * Takes an object and a table
 * dispatch according to the constructor
 * of the object.
 * `object` must be something constructed with `new`.
 */
function match(object, table) {
  const name = object.constructor.name;
  return (table[name] || table['_'])(object);
}

class A {}

function onA(object) {
  return match(object, {
    A: (a) => console.log(a),
    '_': () => console.log("like default on a switch")
  });
}

onA(new A);
// A {}

onA(null || 0 || 'string' || 1);
// like default on a switch
```

This is a great tool when you need
to return values depending on the context.

We can imagine a case where if we have an
`A`, we must return an string, or in another
case, it should be a number. So, instead of
having a bigger or many interface(s), we use
match to return accordingly.

```js
function A(value) {
  if (!(this instanceof A))
    return new A(value);
  this.value = value;
}

// Note that in this case
// we are not using the value
function asNumber(object) {
  return match(object, {
    A: () => 1,
    '_': () => 0
  });
}

// Note that in this case
// we are expecting that `A`
// holds a string.
// (in fact, it can hold any value -
// if we want)
function asString(object) {
  return match(object, {
    A: (a) => a.value,
    '_': () => ''
  });
}

function stringSize(object) {
  return match(object, {
    A: (a) => a.value.length,
    '_': () => 0
  });
}

asNumber(new A);
// 1
asNumber(null || 0 || 'string' || 1);
// 0

asString(new A('ok'));
// 'ok'
asString(null || 0 || 'string' || 1);
// ''

stringSize(new A('ok'));
// 2
stringSize(null || 0 || 'string' || 1);
// 0
```

#### moving types as values

Imagine a json like:

```json
{
  "type": "empty"
}
```

This object has no information. It holds only
the property that tells which type it is.

If we use this plain object,
everywhere on the codebase we are going
to access the `.type` property and the behavior
will be all over the place.

Instead, we can create a type for it...

```js
const Empty = new (function Empty() {});
```

Now that we have a proper type,
we can add behaviors on it.

NOTE: there is no significant cost for doing this.

NOTE: the prototype for this object will be
hidden or you will need to do a little bit
of gynastic to get it (Empty.constructor.prototype...).
But it's not necessary to use it.

```js
Empty.asString = function() {
  return '';
};

Empty.asNumber = function() {
  return 0;
};

Empty.toJSON = function() {
  return { "type": "empty" };
};
```

It's has no relation to the singleton pattern.

#### maybe monad

If you have heard about functors or monads,
and haskell, maybe is an object in the `Hask` category
that has two constructors:

- Nothing
- Just a (where `a` is any other type - Just(a))

The nothing constructor has no arguments. So
we can instantiate once and just pass it around.

The other constructor is `Just` and it has a single argument.

```js
const Nothing = new (function Nothing {});

function Just(a) {
  if (!(this instanceof Just)) {
    return new Just(a);
  }
  this.value = a;
}

Nothing.constructor.name;
// 'Nothing'
Just(1).constructor.name;
// Just'
```

Here is how we can implement `functor` for our maybe type.

It's out of scope for this article to explain
functors, but there are a lot of content out there
explaining it.

```js
const Nothing = new (function Nothing() {});

function Just(a) {
  if (!(this instanceof Just)) {
    return new Just(a);
  }
  this.value = a;
}

Nothing.map = function(_) {
  return Nothing;
};

Just.prototype.map = function(f) {
  return Just(f(this.value));
};

Just(1).map(v => v + 1).map(v => v * 2)
// Just(4)
Nothing.map(v => v + 1).map(v => v * 2)
// Nothing

// our value needs to be between 0 and 10.
//
// don't do this!!! this is just an example.
// (unless it makes sense...)

function valueToMaybe(v) {
  return v < 0 || v > 10 ? Nothing : Just(v);
}

valueToMaybe(1).map(v => v + 1).map(v => v * 2)
// Just(4)
valueToMaybe(11).map(v => v + 1).map(v => v * 2)
// Nothing

// giving a hint
[1].map(v => v + 1).map(v => v * 2)
// [4]
```
