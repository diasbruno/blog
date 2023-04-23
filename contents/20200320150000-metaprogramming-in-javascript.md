Sometimes it's tedious to write the same pattern
or function.

In some cases, we can get away with just a closure,
but in others, what we really need is to generate
a function with a couple tweaks or to generate it from
a template.

Unfortunately, we can't do much with the language itself.
It would be nice if we had some way to work directly
with the AST (abstract syntax tree), or manipulate
fragments - something like Elixir, or, like the most
beautiful language, in my opinion, Lisp.
At least, we can always manipulate strings...

We also have `Reflect` and `Proxy`,
that are good tools for specific cases,
but we are not going to talk about them
in this article (maybe on the next one).

In this article, we are going to see some tricks
to generate specialized functions in runtime.

The examples we'll use are really simple,
but I hope you can understand and write
more interesting stuff with it.

## The Function object

We can use the Function object constructor
to generate new functions.

Its specification says that the last argument
passed will be rendered as body of the function, and,
the first ones will be the names of the arguments.
If only one argument is passed, the function
has no arguments.

Example:

```js
const fn = new Function("a", "return a");

fn(1);

// 1

fn.toString();
// 'function anonymous(a\n) {\nreturn a\n}'
```

## Generating operations

Let's generate functions for this operations: `+`, `-`, `*`, `/`.

```js
const generateOperation =
  op => new Function("a", "b", `return a ${op} b`);

const add = generateOperation("+");
const sub = generateOperation("-");
const mul = generateOperation("*");
const div = generateOperation("/");

add(1, 1);
// 2
sub(2, 1);
// 1
mul(2, 2);
// 4
div(2, 2);
// 1
```

Too easy...but we already can see
where we are going next!

## Generating functions from URI routes

Now we have a different task, to generate functions
from URI routes.

We are going to convert some route like,
`get:/a/b/{c}/d/{e}`, into a function
`function(c, e) { ... }`.

First, we need to find all variables
in the route. If we don't find any,
the function will have no arguments.

```js
// don't trust this function,
// it's incomplete...
const collectVars = route => {
   const items = [];
   for (
     let str = route, open = -1, close = 0;
     (
       open = str.indexOf('{'),
       close = str.indexOf('}'),
       open >= 0
     );
     str = str.slice(close + 1)
     ) {
     if (close < 0) throw Error("Bad route");
     items.push(str.slice(open + 1, close))
   }
  return items;
}

collectVars("/a")
// []

collectVars("/a/b/c/{d}/e/{f}/")
// [ "d", "f" ]

collectVars("/a/b/c/{d}/e/{f/")
// Thrown:
// Error: Bad route
//    at collectVars (repl:12:27)
```

Now that we have all variables,
we can start writing the generator...

All valid javascript statements are allowed,
so you can also add a `debugger` inside of it.


```js
// A simple response processor.
const processResponse = async (response) => {
  const { status } = response;
  const data = await response.json();
  return {
    'status': (status >= 200 && status <= 299 ? 'success' :
        (status >= 400 ? 'error' : 'redirect')),
    data
  }
}

const makeApi = scheme => {
  // get the method and route :P
  const [method, route] = scheme.split(':');

  // convert to a template string style.
  // template strings must be wrapped with `\``
  // otherwise, you will get compiler errors.
  const routeTemplate = `\`${route.replace(/\{/g, '${')}\``;

  let vars = collectVars(route).concat(
    method != 'get' ? ["data=null"] : []
  ).concat(
    ["headers={}"]
  );

  const fnBody = `
return fetch(
  ${routeTemplate},
  {
    method: "${method}",
    ...headers,
    body: data
  }
).then(processResponse);
`;

  const fn = new Function(...vars, fnBody);

  // to make it easy to debug...
  if (process.env.NODE_ENV != 'production') {
    fn.args = vars;
  }

  return fn;
};

const getApi = makeApi("get:/a/b/c/{d}/e/{f}/");
getApi.toString();
// 'function anonymous(d,f\n' +
//   ') {\n' +
//   '\n' +
//   'return fetch(\n' +
//   '  `/a/b/c/${d}/e/${f}/`,\n' +
//   '  {\n' +
//   '    method: "get",\n' +
//   '  }\n' +
//   ').then(processResponse);\n' +
//   '\n' +
//   '}'
```

### Conclusion

Now you can start writing something
more sophisticated than this. You can
even use any user-defined functions
as base for your new functions
(try it with any fn.toString()),
you can generate code from DSL
(Domain Specific Language)...

That are a lot more in this topic, and now,
it's up to you to use your creativity!

<iframe frameborder="0" width="100%" height="500px" src="https://replit.com/@diasbruno/metaprogramming-in-js?embed=true"></iframe>
