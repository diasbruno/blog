Step components are great, the problem is that
they can get messy when we have
a complex pipeline.

If you can find a package/library/module
that implements some kind of state machine
in your programming language, that is fine.

Here is one idea of how we can archive this
with our bear hands...

## The idea

Each step knows to which step it should go next, but
they don't know how to get there. We can
`delegate` this responsibility to a `controller`.

### Step controller

The controller will be responsible
to instantiate/render the initial step,
and, will give to the step all the necessary
functions to move around.

Here is an interface to illustrate.

```java
interface StepController {
  void goToStep(step);
  // T represents additional stuff you can pass
  // if you need extra functionality like submit function...
  void forward(T env);
  void back(T env);
}
```

### Step component

Every `step` must implement `back` and `next`
and they must return a `Step`, or, return
a step in an asynchronous structure that
your language supports, like `async/await`, `Promise`,
`Future<Step>`...

```java
interface Step {
  Step | Async<Step> back();
  Step | Async<Step> next();
}
```

<iframe frameborder="0" width="100%" height="500px" src="https://replit.com/@diasbruno/step-component-java?embed=true"></iframe>

## With React

Managing the state is not a problem,
you can use either `useState`, `useReducer`...

<iframe src="https://codesandbox.io/embed/step-component-react-8ietx?fontsize=14&hidenavigation=1&theme=dark"
	 style="width:100%; height:500px; border:0; border-radius: 4px; overflow:hidden;"
	 title="step-component-react"
	 allow="accelerometer; ambient-light-sensor; camera; encrypted-media; geolocation; gyroscope; hid; microphone; midi; payment; usb; vr; xr-spatial-tracking"
	 sandbox="allow-forms allow-modals allow-popups allow-presentation allow-same-origin allow-scripts"
   ></iframe>
