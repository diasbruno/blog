const reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)");
const colors = ["#64b4dc", "#e67896", "#5ac88c", "#fac85a", "#f08c5a", "#dc4646", "#aa78e6"];
const easeIn = "cubic-bezier(.55,.085,.68,.53)";
const easeInOut = "cubic-bezier(.455,.03,.515,.955)";

const tileColumns = 5;
const tileRows = 3;
const transitions = [
  {
    name: "columns",
    columns: colors.length,
    rows: 1,
    scale: "scaleY",
    duration: 1000,
    easing: easeIn,
    origin: (_, reveal) => reveal ? "bottom" : "top",
    delay: index => index * 50,
  },
  {
    name: "bands",
    columns: 1,
    rows: colors.length,
    scale: "scaleX",
    duration: 720,
    easing: easeInOut,
    origin: (index, reveal) => (index % 2 === 0) !== reveal ? "left" : "right",
    delay: index => index * 45,
  },
  {
    name: "tiles",
    columns: tileColumns,
    rows: tileRows,
    scale: "scale",
    duration: 420,
    easing: easeInOut,
    origin: () => "center",
    delay: (index, reveal) => {
      const diagonal = Math.floor(index / tileColumns) + index % tileColumns;
      return (reveal ? tileColumns + tileRows - 2 - diagonal : diagonal) * 55;
    },
  },
];

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

async function movePanels(panels, transition, reveal, signal) {
  if (!canAnimate() || signal.aborted) return;

  const start = `${transition.scale}(${reveal ? 1 : 0})`;
  const end = `${transition.scale}(${reveal ? 0 : 1})`;
  const animations = panels.map((panel, index) => {
    panel.style.transformOrigin = transition.origin(index, reveal);
    return panel.animate([{ transform: start }, { transform: end }], {
      duration: transition.duration,
      delay: transition.delay(index, reveal),
      easing: transition.easing,
      fill: "both",
    });
  });
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
    for (const panel of panels) panel.style.transform = end;
  } finally {
    cancel();
    signal.removeEventListener("abort", cancel);
    reducedMotion.removeEventListener("change", finish);
    document.removeEventListener("visibilitychange", finish);
  }
}

export async function transitionPage(render, signal) {
  if (!canAnimate()) {
    if (!signal.aborted) render();
    return;
  }

  const transition = transitions[Math.floor(Math.random() * transitions.length)];
  const curtain = document.createElement("div");
  curtain.className = "page-transition";
  curtain.dataset.transition = transition.name;
  curtain.setAttribute("aria-hidden", "true");
  curtain.style.setProperty("--columns", transition.columns);
  curtain.style.setProperty("--rows", transition.rows);
  const panels = Array.from({ length: transition.columns * transition.rows }, (_, index) => {
    const panel = document.createElement("span");
    panel.style.backgroundColor = colors[index % colors.length];
    return panel;
  });
  curtain.append(...panels);
  document.body.append(curtain);

  try {
    await movePanels(panels, transition, false, signal);
    if (signal.aborted) return;
    render();
    await movePanels(panels, transition, true, signal);
  } finally {
    curtain.remove();
  }
}
