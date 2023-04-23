more interesing stuff with types
and javascript. let's see `equality`, `ordering` and `enumeration`.

every application has some types that
doesn't have much behaviors. they mostly just
represent something. for example: currencies.

the accepted currencies are: `USD`, `BRL` and `JPY`.

we could use an `enum` to make our lives easier.

```js
enum Currency {
  USD = 'USD',
  BRL = 'BRL',
  JPY = 'JPY'
}

{
  "currency": Currency.JPY,
  "amount": 10
}
```

this is better than using raw strings all over the place
to avoid typos and forgetting if a currency is valid.

these is no much we can do with them, they only help
if we are doing equality.

```js
object.currency === Currency.JPY
```

...and nothing more.

let's say we want to put them in order by currency.
we would need to do it by hand and it's terrible.

#### equality

this is the easy one.

```js
const USD = new (function USD() {});
const BRL = new (function BRL() {});
const JPY = new (function JPY() {});
```

this will allow us to attach behaviors to the objects.

```js
USD === USD;
// true

USD === BRL;
// false

// if we want to follow fantasy land specification.

function currencyEq(b) {
  return this === b;
};

USD.equals = currencyEq;
BRL.equals = currencyEq;
JPY.equals = currencyEq;

USD.equals(JPY);
// false

USD.equals(USD);
// true
```

Still didn't change much, but we now have the hability
to improve your simple types.

#### enumeration

we don't get really enumeration from our `enum` types,
but now the can implement ourself!

```js
USD.toEnum = () => 0;
BRL.toEnum = () => 1;
JPY.toEnum = () => 2;

const Currency = {
  fromEnum(x) {
    switch(x) {
    case 0: return USD;
    case 1: return BRL;
    case 2: return JPY;
    default: { throw new Error(`no current for ${x}.`); }
    }
  }
};

// nice properties!
Currency.fromEnum(currency.toEnum()) === currency;
// true

// now can do weird stuff...

function cycle(n, fn) {
  let x = 0;
  return () => fn(x++ % n);
}

const currenciesCycle = cycle(3, Currency.fromEnum);

currenciesCycle();
// USD
currenciesCycle();
// BRL
currenciesCycle();
// JPY
currenciesCycle();
// USD
currenciesCycle();
// BRL
currenciesCycle();
// JPY
...
```

#### ordering

ordering is also an interesting operation that people
don't think about.

```js
// in this example, we are going to
// use the inverse of enum.
// so, the order will be: USD, BRL, JPY
function currencyOrd(b) {
  return this.toEnum() > b.toEnum();
}

USD.lte = currencyOrd;
BRL.lte = currencyOrd;
JPY.lte = currencyOrd;

const Currency = {
  // ...
  sortAsc(a, b) {
    return -b.lte(a);
  },
  sortDesc(a, b) {
    return -a.lte(b);
  }
};

// sorting descendent
[JPY, USD, BRL, JPY, BRL].sort(Currency.sortDesc);
// [JPY, JPY, BRL, BRL, USD]

// sorting ascendent
[JPY, USD, BRL, JPY, BRL].sort(Currency.sortAsc);
// [USD, BRL, BRL, JPY, JPY]
```

the complete example can be found at [diasbruno/equality-enum-order-with-javascript at github](https://github.com/diasbruno/equality-enum-order-with-javascript).
