We know the `router` component more as a web application
thing. We use it on backend and frontend, sometimes mobile,
to route according to a URL or some sorta of text schema.

This is an example of how a router looks like:

[ ![router diagram](/images/router-diagram.png "router diagram") ](/images/router-diagram.png)

The components are:

## dispatcher

Something that knows how to trigger a route when it's matched.

## matcher

The strategy that will be used to perform the match.

## router

A component that glues everything togheter.

## route

The implementation of the route to execute
or return a product.

Each route can also have a lifecycle,
generally `enter`, `exit`...but lifecycle
may not be required on your application.

Don't ship code you don't need.

[ ![route lifecycle](/images/route-state.png "router state") ](/images/route-state.png)

## enter state

In this stage, the application can decide
if it's possible to enter the route,
otherwise it redirects to another one.

Or do some preparation work before the route
is executed.

## exit state

At this point, the application may decide
that it's not possible to leave this route
and must stay for some reason.

Or do some tear down routine before entering
the next route.

This is the most basic router you can write:

```js
// dispatchers

function IndexPage() {
  return "index";
}

function AboutPage() {
  return "about";
}

function NotFoundPage() {
  return "not found";
}

// routes

const routes = {
  "/": IndexPage,
  "/about": AboutPage
};

// router

let currentRoute;

const router = (subject, context) => {
  if (subject === currentRoute) return;
  return (routes[subject] || NotFoundPage)(context);
};

// usage

route("/", {})
// index

route("/about", {})
// about

route("/meh", {})
// not found
```

Note that this is totally fine! You don't need
to add a full package or whatever.
It has everything we need:

The matcher

```js
routes[subject] || NotFoundPage
```

The dispatcher

```js
(routes[subject] || NotFoundPage)(context);
```

The router

```js
let currentRoute;

const router = (subject, context) => {
  if (subject === currentRoute) return;
  return (routes[subject] || NotFoundPage)(context);
};
```

Stick to your use case first!

Also note that this component doesn't know
when it will execute. How it's going to be triggered
depends on your needs or environment.

Here is a more general version of the router:

```js
// association list is a list of pairs
// [["/", f(){}], ["/about", f(){}]]
//
// different from an object, in this case,
// we don't have to use Object.entries()
// to iterate over the pairs...
const KEY = 0;
const DISPATCHER = 1;

function AssocListMatcher(spec, subject) {
  return spec.find(s => s[KEY] === subject);
}

// basic function dispatcher...
// if the matcher returns a function,
// we just execute passing the context.
function FunctionDispatcher(route, context) {
  return route[DISPATCHER](context);
}

// this Router glues everything...
// `matcher` and `dispatcher` are just interfaces
// `route`, `spec`, `context` and `defaultRoute`
// are the dependencies we pass to the implementations
// to execute.
function Router(spec, matcher, dispatcher, defaultRoute) {
  let currentRouter;

  return function(subject, context) {
    const route = matcher(spec, subject) || defaultRoute;
    if (route === currentRouter) return;
    currentRouter = route;
    return dispatcher(currentRouter, context);
  };
};
```

And can be used like this:

```js
function IndexPage() {
  return "index";
}

function AboutPage() {
  return "about";
}

function NotFoundPage() {
  return "not found";
}

const routes = [
  ["/", IndexPage],
  ["/about", AboutPage]
];

const router = Router(
  routes,
  AssocListMatcher,
  FunctionDispatcher,
  NotFoundPage
);

const context = {};

router("/", context);
// index

router("/about", context);
// about

router("/meh", context);
// not found
```

Also, routers vary on style. They can be:

## one-to-one router

Generally, one level router.

## nested router

Router that can be executed interpreting a spec like:

```js
{
  "/create": {
    route: CreateRoute,
    children: {
      "/user": {
        route: CreateUserRoute
      },
      "/admin": {
        route: CreateAdminRoute
      }
    }
  }
}
```

Or match in some sorta continuation-style:

```js
const firstlevelroutes = {
  "/create": CreateRoute
};

const createroutes = {
  "/user": CreateUserRoute,
  "/admin": CreateAdminRoute
}

function CreateUserRoute(routeState, context) {
  return form({ id: "user-form" });
}

function CreateAdminRoute(routeState, context) {
  return form({ id: "admin-form" });
}

function CreateRoute(routeState, context) {
  return div([
    h1("Create user"),
    // `routestack` was popped, here we have ["/user"]
    // on stack.
    createUserRouter(routeState, context)
  ]);
}

const routerState = {
  fullroute: "/create/user",
  routestack: pathSplit("/create/user") // ["/create", "/user"]
};

createRouter(routerState, context)
```

