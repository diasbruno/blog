Consider this snippet to filter stuff...

```
function filter(
  by_name, by_price, by_category, list
) {
  const filter_name =
    value_or_default(
      by_name,
      obj => locale_compare(obj.name, by_name),
      const_true
    );

  const filter_price =
    value_or_default(
      by_price,
      obj => eq(obj.price, by_price),
      const_true
     );

  const filter_category =
    value_or_default(
      by_category,
      obj => locale_compare(obj.category, by_category),
      const_true
    );

  return list.filter(
    obj => filter_name(obj) && filter_price(obj) && filter_category(obj)
  );
}

const list = [
  { name: "computer", price: 1000, category: "technology" },
  { name: "laptop", price: 1500, category: "technology" },
  { name: "papers", price: 10, category: "supplies" },
  { name: "pen", price: 1, category: "supplies" }
];

filter(null, null, null, list).length === list.length
filter("Computer", null, null, list).length === 1
filter(null, 10, null, list).length === 1
filter(null, 10, "technology", list).length === 0
```

It's kinda ok if those are the only filters
the application needs.

In this case we can only deal with equality
and we can't filter using ranges or any other criteria
and that is really sad.

Another problem is that in order to add more
features to this code, we need to add a lot more state
to handle each option and than join them togheter
or use some kinda of object to hold the specification
of each filter.

```
[
  {
    spec: "name",
    type: "locale_compare",
    value: "computer"
  },
  {
    spec: "name",
    type: "locale_compare",
    value: "computer"
  }
]
```

This is an improviment, but dealing with this JSON-like objects
are really boring and generally people tend to write them by hand,
so they start to appear all over the place.
Also, this is an anti-pattern - primitive obsession.

This is where the specification pattern saves the day.

Instead of separating the data and the object, we can create
a class to deal with it.

```
class AbstractSpecification {
  is_satisfied(obj) {
     throw new Error("not implemented");
  }
}

class Equal extends AbstractSpecification {
  constructor(key, value) {
    super();
    this.key = key;
    this.value = value;
  }

  is_satisfied(obj) {
    const { key, value } = this;
    return obj[key] === value;
  }
}

eq = new Equal('price', 10);
eq.is_satisfied({ price: 10 }) === true
```

This is the base of this pattern (to be able to verify
if some stuff satifies the specification). To allow
the composition of more than one rule,
we use the composite pattern to be able to chain
(or build a tree) of rules.

```
class AbstractSpecification {
  is_satisfied(obj) {
     throw new Error("not implemented");
  }

  and(b) {
    return new AndSpec(this, b);
  }
}

class AndSpec {
  constructor(a, b) { this.a = a; this.b = b; }

  is_satisfied(obj) {
    return this.a.is_satisfied(obj) && this.a.is_satisfied(obj);
  }
}
```

We added `AbstractSpecification::and()` to handle the logic `&&`
for every specification we create.

This is how to use it:

```
bt = new Between("price", 1000, 2000);
lq = new LocaleCompare("name", "computer");
filter = bt.and(lq);

list.filter(filter.is_satisfied).length === 1
```

We can create all the logical operations
and create as many specification as we want.

The downside of this approach is that
it is a lot of machinary to maintain.

We can separate the work and the structure.

```
function And(a, b) { this.a = a; this.b = b; }

function Or(a, b) { this.a = a; this.b = b; }

function Spec(f) { this.f = f; }
```

`Spec` in our data structure is the "leaf"
and `And` and `Or` are the "nodes".

This way we can write the following structure:

```
// omitting `new` to remove noise...

spec = Or(
         Spec(notEmpty),
         And(
           Spec(minLen(3)),
           Spec(maxLen(5))
         )
       );
```

`notEmpty`, `minLen` and `maxLen` are just any kinda of function.

This way the only thing we need is to teach
how can to `reduce` this structure.

```
And.prototype.reduce = function(f, state) {
  const { a, b } = this;
  return a.reduce(f, state) && b.reduce(f, state);
};

Or.prototype.reduce = function(f, state) {
  const { a, b } = this;
  return a.reduce(f, state) || b.reduce(f, state);
};

Spec.prototype.reduce = function(g, state) {
  const { f } = this;
  return g(f, state);
};
```

This is great 'cause we can create a structure
and later decide what operation we want to do.

If we decide to use functions:

```
// omitting `new` to remove noise...
s = Or(
      Spec(isEmpty),
      And(
        Spec(minLen(3)),
        Spec(maxLen(10))));

function filter(spec, subject) {
  return s.reduce(
    (str, f) => f(str),
    subject
  );
}

filter(s, "a string") === true
```

Nice! Now we have very small pieces
and we just need to put them togheter.

Going further in this idea, instead
of using functions, we can use objects.

This is a process called defunctionalization
and it's super interesting. This technic allow us
to create a specification and have different
interpretation.

```
// omitting `new` to remove noise...

function NotEmpty() {}
function Between(min, max) { this.min = min; this.max = max; }
function StartsWith(str) { this.str = str; }

spec = Or(
         Spec(NotEmpty),
         And(
           Spec(Between(3, 10)),
           StartsWith("inter")
         )
       );

const interpreter = {
  [NotEmpty]: (_, subject) => subject.length !== 0,
  [Between]: (self, subject) => {
    const l = subject.length;
    return l >= self.min && l <= self.max;
  },
  [Startswith]: (self, subject) => subject.startsWith(self.str)
}

function intepretAsFilter(w, subject) {
  const c = w.constructor.name;
  return interpreters[c](w, subject);
}

function filter(spec, subject) {
  return spec.reduce(
    (subject, spec) => intepretAsFilter(spec, subject),
    subject
  );
}

filter(s, "a string") === true
```
