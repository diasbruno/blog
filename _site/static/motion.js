const reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
const easeIn = "cubic-bezier(.55,.085,.68,.53)";
const easeInOut = "cubic-bezier(.455,.03,.515,.955)";

function canAnimate() {
  return !reducedMotion.matches && !document.hidden && typeof Element.prototype.animate === "function";
}

export function prepareLanding(signal) {
  const menu = document.querySelector(".landing-menu");
  if (!menu || !canAnimate() || signal?.aborted) return;

  const signature = menu.querySelector(".landing-mark");
  const divider = menu.querySelector(".landing-divider");
  const links = menu.querySelectorAll("nav a");
  const frame = signature.getBoundingClientRect();
  const offset = (document.documentElement.clientWidth - frame.width) / 2 - frame.left;
  const centered = `translateX(${offset}px)`;
  const strokes = Array.from(signature.querySelectorAll("path"), path => ({
    path,
    length: path.getTotalLength(),
  }));
  const animations = [];
  let frameRequest;
  let started;

  function solidStrokes() {
    for (const { path } of strokes) {
      path.style.removeProperty("stroke-dasharray");
      path.style.removeProperty("stroke-dashoffset");
    }
  }

  function stop() {
    cancelAnimationFrame(frameRequest);
    for (const animation of animations) animation.cancel();
    solidStrokes();
    signature.style.removeProperty("transform");
    menu.classList.remove("is-revealing");
    menu.removeEventListener("focusin", stop);
    reducedMotion.removeEventListener("change", stop);
    document.removeEventListener("visibilitychange", stop);
    signal?.removeEventListener("abort", stop);
  }

  function slide() {
    animations.push(signature.animate(
      [{ transform: centered }, { transform: "translateX(0)" }],
      { duration: 350, easing: easeInOut, fill: "both" },
    ));
    animations.push(divider.animate(
      [{ transform: "scaleY(0)" }, { transform: "scaleY(1)" }],
      { duration: 350, easing: easeInOut, fill: "both" },
    ));
    links.forEach((link, index) => {
      animations.push(link.animate([
        { opacity: 0, transform: "translateX(-2rem)" },
        { opacity: 1, transform: "translateX(.15rem)", offset: .87 },
        { opacity: 1, transform: "translateX(0)" },
      ], { delay: 350 + index * 100, duration: 400 + index * 100, easing: easeIn, fill: "both" }));
    });
    Promise.allSettled(animations.map(animation => animation.finished)).then(stop);
  }

  function draw(timestamp) {
    started ??= timestamp;
    const progress = Math.min((timestamp - started) / 2000, 1);
    if (progress === 1) {
      // Restore solid strokes before translating the signature.
      solidStrokes();
      frameRequest = requestAnimationFrame(slide);
      return;
    }

    const easedProgress = progress < .5 ? 2 * progress ** 2 : 1 - 2 * (1 - progress) ** 2;
    for (const { path, length } of strokes) {
      path.style.strokeDashoffset = length * (1 - easedProgress);
    }
    frameRequest = requestAnimationFrame(draw);
  }

  menu.classList.add("is-revealing");
  signature.style.transform = centered;
  for (const { path, length } of strokes) {
    path.style.strokeDasharray = `${length} ${length}`;
    path.style.strokeDashoffset = length;
  }
  menu.addEventListener("focusin", stop, { once: true });
  reducedMotion.addEventListener("change", stop, { once: true });
  document.addEventListener("visibilitychange", stop, { once: true });
  signal?.addEventListener("abort", stop, { once: true });

  return () => {
    if (menu.classList.contains("is-revealing")) frameRequest = requestAnimationFrame(draw);
  };
}

async function waitForFade(animations, signal) {
  function finish() {
    for (const animation of animations) animation.finish();
  }

  function cancel() {
    for (const animation of animations) animation.cancel();
  }

  signal.addEventListener("abort", cancel, { once: true });
  reducedMotion.addEventListener("change", finish, { once: true });
  document.addEventListener("visibilitychange", finish, { once: true });
  try {
    await Promise.allSettled(animations.map(animation => animation.finished));
  } finally {
    signal.removeEventListener("abort", cancel);
    reducedMotion.removeEventListener("change", finish);
    document.removeEventListener("visibilitychange", finish);
  }
}

export async function transitionPage(page, render, signal) {
  if (signal.aborted) return;
  if (!canAnimate()) {
    render();
    return;
  }

  const body = document.body;
  const oldBackground = getComputedStyle(body).backgroundColor;
  const outgoing = page.animate([{ opacity: 1 }, { opacity: 0 }], {
    duration: 120,
    easing: "ease-in",
    fill: "forwards",
  });
  const incoming = [];
  try {
    await waitForFade([outgoing], signal);
    if (signal.aborted) return;
    render();
    if (!canAnimate()) return;

    const options = { duration: 180, easing: "ease-out", fill: "backwards" };
    incoming.push(page.animate([{ opacity: 0 }, { opacity: 1 }], options));
    const newBackground = getComputedStyle(body).backgroundColor;
    if (oldBackground !== newBackground) {
      incoming.push(body.animate(
        [{ backgroundColor: oldBackground }, { backgroundColor: newBackground }],
        options,
      ));
    }
    outgoing.cancel();
    await waitForFade(incoming, signal);
  } finally {
    outgoing.cancel();
    for (const animation of incoming) animation.cancel();
  }
}
