we all get excited when we see an interesting trick.

some are "one-liners", or an exotic optimization...

but everytime there is something that looks convenient,
it will probably collect your soul later...

some people don't like when their files starts with a
huge list of imports. I can agree that is not that aesthetic.

to avoid huge lists of imports, we can use a file to reexport
all the modules into a single entry.


```js
import {
    a,
    b,
    c,
    d,
    e,
    f,
    g,
    h,
    i,
    j,
    k,
    l,
    m,
    n,
    o,
    p,
    q,
    r,
    s,
    t,
    u,
    v,
    x,
    z }
    from "@/components";
```

well...that doesn't look good either.

here is a list of problems you may found when reexporting
modules:

- circular dependencies
- loading more resource than necessary
- import modules that instantiate objects on top-level
- test pollution and unnecessary mocking


### circular dependencies

If you have a file that exports everything,
you better use it!

```js
// Component A

import { B, C } from "@/components";
```

```js
// Component C
import { A } from "@/components";
```

```js
// Components (syntax of this file is not important...)

/* correct import order */
export { default as C } from "./ComponentC";
export { default as B } from "./ComponentB";
export { default as A } from "./ComponentA";

/* incorrect import order */
export { default as A } from "./ComponentA";
export { default as B } from "./ComponentB";
export { default as C } from "./ComponentC";
```

depending on the order of the imports,
you may not see the problem...

but when it's wrong, you will probably spend hours trying
to remember what is going on.

some will say that a simple solution to fix this is that
"components should not use the file that reexports everything",
or basically getting back from where we started.

this is the first rule we need to make the entire team to follow
just to have a single file.

### loading more resource than necessary

getting the tools to properly do a "tree-shake"
or to better bundle the application is the last thing
I want to spend time.

this makes it harder for the compiler to decide what to do.
so we are starting to introduce problems to create tools
(we don't need), just to have a file to group modules.

### import modules that instantiate objects on top-level

once you import a module, if it evaluates an expressions on the top-level,
this work can be unncessary an slow down specially tests.

it is almost impossible to mock or to avoid running the code,
or, you will end up writing a lot of unnecessary code
to "skip this code when testing".

### test pollution and unnecessary mocking

as you may have modules that instantiates objects on the top-level,
they are kinda unpredictable or you may know, and now, you have
to create a general mocking file that every test must import.

this creates all sorts of problems:

- people assuming the mock has "correct" data
- incomplete use cases testing because "it just works"

so, just because of this trick, we are risking to introduce
a lot of unnecessary code and all kinds of problems...

this doesn't look like a good idea...

[codesandbox with some problems](https://codesandbox.io/s/javascript-reexporting-problems-vzmw5h)
