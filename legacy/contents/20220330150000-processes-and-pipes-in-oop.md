This article is partially based on John Hughes'
(Arrows)[https://www.haskell.org/arrows/index.html]
and the idea of unix processes on the OOP land.

The idea of Arrows is a way to compose functions
in such a way that it look like a circuit. This is
great, because we can encode a pipeline process
just like we would write a diagram.

(Add arrow's diagram picture)

Also, we know how to compose processes on the terminal,
and they are pretty simple.

Each process has 4 components:

- arguments
- stdin
- stdout
- stderr

We can imagine a pipeline that may looks like this:

```sh
echo "1" | add --value 4 - | sub --value 1
// 4
```

Looking at `add`, it has an argument `-s 2`,
it use `stdin` with the `-`syntax, might use `stderr`
and the result is piped to `sub` through `stdout`,
and received as `stdin` on sub.

Let's start with a simple function composition...

(we are going to use `compose = flip (.)` for the entire article)

```rb
def compose(*fs)
  ->(arg) { fs.reduce(arg) do |acc, f| f.call(acc); end }
end

compose(->(x) { x + 1 }, ->(y) { y + 2 }).call(2) == 5
```

This will give use the simpliest composition we know...

Some processes might need some configuration so it can
use later on when we run the computation. We can
use a closure for this.

```rb
adder = ->(y) { ->(x) { x + y } }

compose(adder.call(2), ->(y) { y + 2 }).call(2) == 6
```

Now that we know how to configure our function,
next step is improve it using a `class`.

```rb
class Adder
  def self.make(x); self.new(2).method(:calc); end

  attr_accessor :value

  def initialize(i); @value = i; end

  def calc(x); @value + x; end
end

compose(Adder.make(2), Adder.make(1)).call(2).value == 5
```

Given a bounded methods to the compose function
it still works!!

The return type can be anything on this simple
pipeline, but it doesn't help when we need to deal with
error and exceptions.

We are going to add 3 classes to simulate the unix pipeline:

- `Result` (stdin - stdout)

Can be used to initialize the pipeline
and hold the result of the computation.

- `Failure` (stdin - stdout)

Indicates that we failed, but no exception was thrown.

- `Except` (stderr)

We failed miserable...


```rb
class BaseValue
  attr_accessor :value
  def initialize(value); @value = value; end
end

class Result < BaseValue; end
class Failure < BaseValue; end
class Except < BaseValue; end

class Adder
  def self.make(x); self.new(2).method(:calc); end
  def initialize(i); @i = i; end

  def calc(x)
	Result.new(@i + x.value)
  end
end

class Double
  def self.make; self.new.method(:calc); end
  def initialize; end

  def calc(x)
	Result.new(2 * x.value)
  end
end

compose(Adder.make(2), Double.make, Result.new(2)).value == 8
```

This will help us to propagate the value across
the entire pipeline.

We can use than to run the continuation
when we match the correct result type.

```rb
class Result < BaseValue
  def on_success(f); f.call(value); end
  def on_failure(f); self; end
  def on_exception(f); self; end
end

class Failure < BaseValue
  def on_success(f); self; end
  def on_failure(f); f.call(value); end
  def on_exception(f); self; end
end

class Except < BaseValue
  def on_success(f); self; end
  def on_failure(f); self; end
  def on_exception(f); f.call(value); end
end

Result.new(2).on_success(-> (x) { Result.new(x + 1) }).value == 3
Failure.new(2).on_success(-> {}).value == 2
```

The only thing missing is the pipe.

We can write a class that can glue everything togheter.

```rb
class Pipe
  attr_accessor :work
  def initialize(initial_work); @work = initial_work; end

  def id(*x); x; end

  def pipe(work, on_failure = nil, on_exception = nil)
	pw = @work
	handle_failure = (on_failure or self.id)
	handle_exception = (on_exception or self.id)

	@work = ->(*input) {
	  pw.call(
		*input
	  ).on_success(
		work
	  ).on_failure(
		handle_failure
	  ).on_exception(
		handle_exception
	  )
	}

	self
  end

  def run(*input)
	@work.call(*input)
  end
end
```

Here is a simple usage of this lib (non-sense)...

```rb
class CreateUser
  def self.make(service)
    self.new(service).method(:execute)
  end

  def initialize(service)
    @service = service
  end

  def execute(data)
    Result.new(@service.create_user(data))
  rescue => e
    Except.new(e)
  end
end

class SendWelcomeEmail
  def self.make(mailerService)
    self.new(mailerService).method(:execute)
  end

  def initialize(service)
    @service = service
  end

  def execute(user)
    mailer = @service.prepare_email(user)
    if mailer.send
      Result.new(user)
    else
      Failure.new({ error: 'failed_to_send_email', 'user': user })
  rescue => e
    Except.new(e)
  end
end

class EnqueueEmail
  ...
  def execute(data) do
    if @mailer_job.enqueue_mail(data)
      Result.new(data.user)
    else
      Failure.new({ error: 'failed_to_enqueue_email', user: data.user })
  end
  ... 
end

Pipe.new(CreateUser.make(UserService.new))
	.pipe(SendWelcomeEmail.make(MailerService.new))
	.pipe(->(user) { user }, ReenqueueEmail.make())
	.run(user_input)
```
