Things get so much easier when you try
to understand how the types work together
to accomplish some work.

In this article, we are going to model
a system that apply discounts using many
strategies.

I'm going to use Haskell and javascript.
haskell (or ocaml if you like) is great to visualize
what's going on the type level...

We can starting by trying to think
on the simplest way to accomplish the job.

We can start by using a simple subtraction `(-)`.

```hs
applyDiscount :: Float -> Float -> Float
applyDiscount price discount = price - discount

let price = 10.0
    discounts = [1.0, 4.0]
    finalPrice = 5.0
in foldl applyDiscount price discounts == finalPrice
```

```js
const applyDiscount = (price, discount) => price - discount;

const price = 10.0;
const discounts = [1.0, 4.0];
const finalPrice = 5.0;

discounts.reduce(applyDiscount, price) == finalPrice;
```

Then we can clarify what which `Float` means
on the type specifiction...

```hs
type Price = Float
type Discount = Float

applyDiscount :: Price -> Discount -> Price
applyDiscount price discount = price - discount
```

Now we have `Price -> Discount -> Price` which means
"given a Price and a Discount, it returns back the price
after the discount"...and that means, in this case,
that the Price acts like an accumulator!

With this information, we can now improve `applyDiscount`
to be more generic.

```hs
class ApplyDiscount a where
  applyDiscount :: a -> Discount -> a

applyDiscounts :: ApplyDiscount a => a -> [Discount] -> a
applyDiscounts = foldl applyDiscount
```

In this case, in typescript, it should be the same as...

```ts
interface ApplyDiscount {
  applyDiscount(discount: Discount): ApplyDiscount;
}
```

Now, we just need to create instances for each `a`!


```hs
newtype ApplyAll = ApplyAll Price
  deriving (Show, Eq)

instance ApplyDiscount a where
  applyDiscount (ApplyAll p) d = ApplyAll (p - d)

let discounts = [1.0, 4.0]
    strategy = ApplyAll 10.0
    finalPrice = ApplyAll 5.0
in foldl applyDiscount strategy discounts == finalPrice
```

```js
class ApplyAll {
  constructor(x) { this.price = x; }
  applyDiscount(d) { return new ApplyAll(this.price - d); }
}

const discounts = [1.0, 4.0];
const strategy = new ApplyAll(10.0);
const finalPrice = 5.0;

const applyDiscount =
  (strategy, discount) => strategy.applyDiscount(discount);

discounts.reduce(applyDiscount, strategy).price == finalPrice;
```

Now we have our first strategy! `ApplyAll`
just apply all discounts available,
and this is a great first strategy.

A new requirement has arived!

Now, we have too apply all discounts,
but we are not going to let the final price
goes negative.

```hs
newtype ApplyAllNoNegative = ApplyAllNoNegative Price
  deriving (Show, Eq)

instance ApplyDiscount a where
  applyDiscount (ApplyAllNoNegative p) d =
    ApplyAllNoNegative (max 0 (p - d))

let discounts = [5.0, 5.0]
    strategy = ApplyAllNoNegative 8.0
    finalPrice = ApplyAllNoNegative 0.0
in foldl applyDiscount strategy discounts == finalPrice
```

```js
class ApplyAllNoNegative {
  constructor(x) { this.price = x; }
  applyDiscount(d) {
    const price = Math.max(0, this.price - d);
    return new ApplyAllNoNegative(price);
  }
}

const discounts = [1.0, 4.0];
const strategy = new ApplyAllNoNegative(10.0);
const finalPrice = 5.0;

discounts.reduce(applyDiscount, strategy).price == finalPrice;
```

Too easy!

Our next task is to:

"If it goes negative, just collect
all discounts that we couldn't apply,
otherwise return the final price."

```hs
newtype CollectDiscountWhenGoesNegative =
  Applying Price |
  NotApplied [Discount]
  deriving (Show, Eq)

instance ApplyDiscount a where
  applyDiscount (Applying p) d =
    let x = p - d
    in if x >= 0 then
         Applying x
       else
         NotApplied [d]
  applyDiscount (NotApplied ls) c =
    NotApplied (c : ls)

let discounts = [5.0, 5.0]
    strategy = Applying 10.0
    finalPrice = Applying 0.0
in foldl applyDiscount strategy discounts == finalPrice

let discounts = [5.0, 8.0]
    strategy = Applying 10.0
    finalPrice = NotApplied [8.0]
in foldl applyDiscount strategy discounts == finalPrice
```

```js
// Don't do this! It's just to help visualize what's going on.
class CollectDiscountWhenGoesNegative {}

class NotApplied extends CollectDiscountWhenGoesNegative {
  constructor(x) { this.list = x; }
  applyDiscount(d) { retun new this(this.list.concat([d])); }
}

class Applying extends CollectDiscountWhenGoesNegative {
  constructor(x) { this.price = x; }
  applyDiscount(d) {
    const price = this.price - d;
    if (price >= 0) {
      return new Applying(price);
    } else {
      return new NotApplied([d]);
    }
  }
}

const discounts = [1.0, 4.0];
const strategy = new Applying(10.0);
const finalPrice = 5.0;

discounts.reduce(applyDiscount, strategy).price == finalPrice;

const discounts = [1.0, 4.0];
const strategy = new Applying(1.0);

discounts.reduce(applyDiscount, strategy).list == [4.0];
```

It's really easy to work with this. If
the discount goes negative, we know exactly
which discounts were not applied to mark on
the frontend as feedback.

Our final task is:

"It must not apply duplicate discounts,
 and it must keep the duplicated to send as feedback
 to the user. Also, it must use the previous strategies
 so we can allow it goes negative or not."


 ```hs

type AlreadyAppliedDiscounts = S.Set Discount
type DuplicatedDiscounts = [Discount]

data DontApplyDuplicate a =
  DontApplyDuplicate AlreadyAppliedDiscounts DuplicatedDiscounts a
  deriving (Show, Eq)

dadup = DontApplyDuplicate

instance (ApplyDiscount a) => ApplyDiscount (DontApplyDuplicate a) where
  applyDiscount (DontApplyDuplicate ls xs s) d =
    if S.member d ls then
      dadup ls (d : xs) s
    else
      dadup (S.insert d ls) xs $ applyDiscount s d

-- could be a `Default` instance
new = dadup S.empty []

let discounts = [5.0, 5.0]
    strategy = new (Applying 10.0)
    finalPrice = dadup (S.fromList [5.0]) [5.0] (Applying 5)
in foldl applyDiscount strategy discounts == finalPrice

let discounts = [5.0, 8.0]
    strategy = Applying 10.0
    finalPrice = NotApplied [8.0]
in foldl applyDiscount strategy discounts == finalPrice
```

```js
class DontApplyDuplicate {
  constructor(strategy) {
    this.usedDiscounts = new Set();
    this.duplicated = [];
    this.strategy = strategy;
  }
  applyDiscount(d) {
    if (this.usedDiscount.has(d)) {
     this.duplicated.push(d);
     return this;
    }
    this.usedDiscount.add(d);
    this.strategy = this.strategy.applyDiscount(d)
    return this;
  }
}

const discounts = [1.0, 4.0];
const strategy = new DontApplyDuplicate(new Applying(10.0));
const finalPrice = 5.0;

discounts.reduce(
  (p, d) => p.applyDiscount(d),
  strategy
).price == finalPrice;

const discounts = [1.0, 1.0, 4.0, 6.0];
const strategy = new DontApplyDuplicate(new Applying(10.0));

const {
  usedDiscounts,
  duplicated,
  strategy: { list },
} = discounts.reduce(applyDiscount, strategy);

usedDiscount = [1.0, 4.0];
duplicated = [1.0];
list = [6.0];
```
