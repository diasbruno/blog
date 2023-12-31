Before we start making amazing stuff with pipelines,
let's recap everything about promises - I mean everything!

`Promise` is an utility to execute tasks concurrently with
the main execution.

## 1 - Creating promises

There are 2 ways to create a promise:

###  new Promise

The constructor, `Promise(f)` receivers a function
that will be executed in a scheduled manner.

In other to combine more promises,
this function `f` receives 2 callbacks: `resolve`
and `reject`.

By calling one of thoses 2 callback,
it may call a continuation, if defined,
using `Promise.then(f)`, in case of `resolve` gets called
or `Promise.catch(g)` for `reject`.

#### Creating a promise that has no continuations

```js
new Promise(() => patchUser({ id: 1, updateName: "new name" }))
```

#### Creating a promise with continuation

```js
new Promise(
  (resolve, reject) =>
    (Boolean(Math.ceil(Math.random() - 0.5))
        ? resolve
        : reject)()
).then(
  () => console.log("resolved")
).catch(
  () => console.log("rejected")
)
```

### Lifting a value into promise

As the promise constructor argument function
receives 2 callbacks, they are also available
to initialize a promise.

```js
Promise.resolve(
  1
).then(
  (value) => assert(value === 1)
)

Promise.reject(
  { error: "some error" }
).catch(
  (err) => assert(error == "some error")
)
```

## 2 - Continuations

You may think you know which path the execution will take,
but you need to pay attention to every continuation.

### `Promise.then(f)`

Addind a `.then(f)` continuation, if the promise
was fulfilled and the result is `resolve`,
the continuation is triggered calling the function `f`
with the result passed to resolve. If the promise calls
`reject`, than the continuation is not triggered.

```js
Promise.resolve(
  1
).then(
  (value) => assert(value === 1)
)

Promise.reject(
  { error: "some error" }
).then(
  () => null /* function will not be called */
)
```

#### Return from `.then(f)`

You can return 3 values from `.then(f)`:

- Any object

If you return a normal object from it,
it is the same as calling `resolve(object)`.
This will trigger the next "then" continuation,
if defined.

```js
Promise.resolve(
  1
).then(
  (value) => value + 1
).then(
  (value) => assert(value === 2)
)
```

- `Promise.resolve(object)`

You can return a promise from a continuation,
in the case of returning "resolved" promise
from a "then" continuation, it has the same effect
as return a normal object.

```js
Promise.resolve(
  1
).then(
  (value) => Promise.resolve(value + 1)
).then(
  (value) => assert(value === 2)
)
```

- `Promise.reject(object)`

As you can return a promise,
it can also be a "rejected" one.

This will trigger a "catch" continuation.

```js
Promise.resolve(
  1
).then(
  (value) => Promise.reject({ error: "some error" })
).catch(
  ({ error }) => assert(error == "some error")
)
```

### `Promise.catch(g)`

Adding a `.catch(g)` continuation, whenever
a promise is fulfilled and "rejected",
the function `g` will be called with the object
of `reject(object)`.

```js
Promise.reject(
  { error: "some error" }
).then(
  () => null /* function will not be called */
).catch(
  ({ error }) => assert(error == "some error")
)
```

#### Return from `.catch(g)`

##### Return resolved promises or normal objects

Return from catch is a little different,
but interesting in some cases, and,
there are a lot of interesting stuff
to do with it.

- `Promise.resolve(object)`

By returning a "resolved" promise from a catch
continuation, the next continuation to be "resolved"
and the `.then(f)` will be triggered.

```js
Promise.reject(
  { error: "some error" }
).catch(
  () => Promise.resolve("ok")
).catch(
  () => null /* function will not be called */
).then(
  (value) => assert(value == "ok")
)
```

- Any object

Returning a normal value from a "catch"
continuation has the same behavior
as returning a `Promise.resolve(object)`.

```js
Promise.reject(
  { error: "some error" }
).catch(
  () => "ok"
).catch(
  () => null /* function will not be called */
).then(
  (value) => assert(value == "ok")
)
```

#### Use cases for return resolved promises or normal objects

In some pipelines, there are cases there it may be possible
to recover and continue the pipeline.

Example (default values):

```js
Promise.resolve(
    /* user recovered from localStorage */
  user
).then(
    /* we are going to kick out this user,
       so we are going to reject here */
  (user) => validateUserSession(user)
).catch(
    /* we can return a object that simulates
       a user but with almost no privileges
       (better then use null) */
  () => GuestUser()
).then(
  (user) => assert(user.isAuthenticated() === false)
)
```
