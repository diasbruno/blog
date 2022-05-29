Favorite last words:

    "The feature is fully tested. EVERY POSSIBLE CASE!"

It's very hard to think about every possible test case
for a feature.

Let's see an example:

```rb
describe 'filling the bucket' do
  it 'should not overflow' do
    # args are (capacity, quantity)
    b = described_class.filled(2, 0)
    c = Cup.filled(2, 2)

    expect(b.fill(c).quantity).to be <= b.capacity
  end
end
```

What can go wrong?

Problem is that we know how to write
the easy solution.

What would happen if we change the capacity and quantity
to be random numbers?

```rb
describe 'filling the bucket' do
  it 'should not overflow' do
    # args are (capacity, quantity)
    b = described_class.filled(rand, rand)
    c = Cup.filled(rand, rand)

    expect(b.fill(c).quantity).to be <= b.capacity
  end
end
```

Eventually it fails. One of the possibilities
is that the `Cup` can be bigger than the `Bucket`, so
it will overflow.

So, let's improve the testing to check some properties
about the problem.

```rb
describe 'filling the bucket' do
  it 'should not overflow' do
    b = Bucket.filled(rand, rand)
    c = Cup.filled(rand, rand)

    # if the cup is not empty, bucket must not be full
    # there is the case where the cup is empty
    if c.quantity > 0
        expect(b.quantity).to be < b.capacity
    else
        expect(b.quantity).to be <= b.capacity
    end
    # cup must not overflow
    expect(c.quantity).to be <= c.capacity
    # filling the bucket must not overflow
    expect(b.fill(c).quantity).to be <= b.capacity
  end
end
```

Now we can understand a little more about the problem.

It will fail randomily on one of the assertions and
part of the problem is that there is no validation
when we construct both objects.

```rb
# both classes extends this one
class Recipient
  def self.validate(capacity, quantity)
    assert(capacity > 0, CAPACITY_GREATER_THAN_ZERO)
    assert(quantity >= 0 && quantity <= capacity, QUATITY_BETWEEN_ZERO_AND_CAPACITY)

    true
  end

  def self.filled(capacity, quantity)
    validate(capacity, quantity)
    new(capacity, quantity)
  end
end
```

Now we know that we can only instantiate a valid recipient...
time to write some tests for the `Recipient` class, but instead
of thinking on the valid ones, we are going to stress out
the failing case.

```rb
describe 'instantiate' do
  it 'must fail for every case' do
    # we create a pair of integers (capacity, quantity)
    # and they have a special property...
    # if the test fails, this kind of test
    # knows how to "shrink" the result
    # to find the smallest possible failing case.
    # by default it will run 100 times with different
    # values.
    p = property_of { [integer, integer] }
    p.check do |opts|
      capacity, quantity = opts
      expect {
        Recipient.filled(capacity, quantity)
      }.to raise_error(StandardError)
    end
  end
end
```

This is how you can write a property-based testing, but it's
not done yet.

Every pair of values must be invalid in this case and if we
got it right we don't need to write the other case.

```rb
describe 'instantiate' do
  it 'must fail for every case' do
    p = property_of do
      capacity = integer
      quantity = integer
      guard(capacity < 0 || quantity < 0 || quantity > capacity)
      [capacity, quantity]
    end
    p.check do |opts|
      capacity, quantity = opts
      expect {
        Recipient.filled(capacity, quantity)
      }.to raise_error(StandardError)
    end
  end
end
```

Now we can be confident that we have cover a lot of spots when
instantiating any subclass of Recipient. If we want, we can
use `.check(t)`, where `t` is how many tests we want, to stress
even further.

We can now go back to the broken test and try to fix it.

So, it should be possible to instantiate a Bucket
and an as many Cups as we want and it should always end up
on our goal state (the bucket been full).

It would be nice if we could implement "shrink"
on our classes so we could right this test instead.

```rb
it 'should not overflow' do
  p = property_of do
    b = bucket
    cs = array { cup }
    [b, cs]
  end

  p.check do |opts|
    bucket, cups = opts

    cups.reduce(bucket) do |b, cup|
      b.fill(cup)
    end

    expect(b.quantity).to be <= b.capacity
  end
end
```

To implement "shrink" for our classes they must implement
2 methods (`shrinkable?` and `shrink`).

```rb
module RecipientShrinkable
  def shrink
    copy = new(@capacity, @quantity)
    copy.capacity.shrink if @capacity.shrinkable?
    copy.quantity.shrink if @quantity.shrinkable?
    copy
  end

  def shrinkable?
    @capacity.shrinkable? || @quantity.shrinkable?
  end
end

class BucketGen < Bucket
  include RecipientShrinkable
end

class CupGen < Cup
  include RecipientShrinkable
end

# creates the data to create a valid recipient
def valid_recipient
  capacity = range(1, 10)
  guard(capacity > 0)
  quantity = range(0, capacity)
  [capacity, quantity]
end

# always creates a valid bucket
def bucket
  BucketGen.filled(*valid_recipient)
end

# always creates a valid cup
def cup
  CupGen.filled(*valid_recipient)
end
```

With this we have a small DSL (domain specific language)
to write our tests.

Our previous test, will also fails, because we can have
a cup bigger that the bucket, maybe there are more cups than
what the bucket can handle...so we need to improve the preconditions
of our test.

First let's make the precondition for just a single cup.

```rb
it 'should not overflow' do
  p = property_of do
    b = bucket
    c = cup

    # only test if it won't overflow
    guard((c.quantity + b.quantity) <= b.capacity)

    [b, c]
  end

  p.check(1000) do |opts|
    b, c = opts

    b.fill(c)

    # we expect in this case that the bucket
    # maybe full or not.
    expect(b.quantity).to be <= b.capacity
  end
end
```

Now let's make for any number of cups...


```rb
it 'should not overflow' do
  p = property_of do
    b = bucket
    cs = array(20) { cup }

    # check if all the cups can overflow
    # and reduce the list if necessary
    cs, _, check = cs.reduce([[], b.capacity, b.quantity]) do |acc, cup|
      cs, max, quantity = acc

      q = quantity + cup.quantity

      if q <= max
        cs.push(cup)
      else
        q = quantity
      end

      [cs, max, q]
    end

    # if the bucket is full
    # check == b.capacity
    # so the array will still be empty
    # so it's covered
    if check < b.capacity
      remainder = b.capacity - check
      c = CupGen.filled(remainder, remainder)
      cs.push(c)
    end

    [b, cs]
  end

  p.check do |opts|
    b, cs = opts

    cs.reduce(b) do |b, c|
      b.fill(c)
    end

    expect(b.full?).to be(true)
  end
end
```
