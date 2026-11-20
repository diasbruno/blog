import "./legacy-routes.js";
import { prepareLanding, transitionPage } from "./motion.js";

const documentPage = document.querySelector("#site-page");
const description = document.querySelector('meta[name="description"]');
const alternate = document.querySelector('link[type="application/json"]');
const scrollPositions = new Map();
let activeRequest;
let currentPath = pagePath(location);
const knownPaths = new Set([currentPath, "/", ...navigationPaths(documentPage)]);

function pagePath(url) {
  return url.pathname.replace(/\/index\.html$/, "/");
}

function navigationPaths(page) {
  return Array.from(page.querySelectorAll("a[data-site-navigation]"), link => pagePath(new URL(link.href)));
}

async function fetchPage(path, signal) {
  const address = path + "content.json";
  const response = await fetch(address, { signal, headers: { Accept: "application/json" } });
  if (!response.ok) throw new Error(`Could not load ${address}`);
  const data = await response.json();
  if (data.path !== path || typeof data.html !== "string") {
    throw new Error(`Invalid content for ${path}`);
  }

  const page = new DOMParser().parseFromString(data.html, "text/html");
  const content = page.querySelector("#site-page");
  if (!content?.querySelector("main") || !page.title) {
    throw new Error(`Missing page content for ${path}`);
  }
  return { page, content };
}

async function navigate(url, { traverse = false } = {}) {
  activeRequest?.abort();
  const request = new AbortController();
  activeRequest = request;
  const path = pagePath(url);
  scrollPositions.set(currentPath, window.scrollY);
  let timer = window.setTimeout(() => request.abort(), 4000);

  try {
    const { page, content } = await fetchPage(path, request.signal);
    window.clearTimeout(timer);
    timer = undefined;
    let startLanding;
    await transitionPage(documentPage, () => {
      if (!traverse) window.history.pushState(null, "", url);
      currentPath = path;
      document.title = page.title;
      document.documentElement.lang = page.documentElement.lang;
      document.body.className = page.body.className;
      description.content = page.querySelector('meta[name="description"]')?.content || "";
      alternate.href = path + "content.json";
      documentPage.replaceChildren(...content.childNodes);
      for (const link of navigationPaths(documentPage)) knownPaths.add(link);
      window.scrollTo(0, traverse ? scrollPositions.get(path) || 0 : 0);
      startLanding = prepareLanding(request.signal);
    }, request.signal);
    if (!request.signal.aborted) {
      documentPage.querySelector("main").focus({ preventScroll: true });
      startLanding?.();
    }
  } catch {
    if (activeRequest === request) {
      if (traverse) window.location.reload();
      else window.location.assign(url);
    }
  } finally {
    window.clearTimeout(timer);
    if (activeRequest === request) activeRequest = undefined;
  }
}

document.addEventListener("click", event => {
  if (event.defaultPrevented || event.button !== 0 || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) {
    return;
  }
  const link = event.target.closest?.("a[data-site-navigation]");
  if (!link || (link.target && link.target !== "_self") || link.hasAttribute("download")) return;
  const url = new URL(link.href);
  if (url.origin !== location.origin || url.search || url.hash) return;

  event.preventDefault();
  if (url.pathname !== location.pathname || location.hash || location.search) {
    navigate(url);
  }
});

window.addEventListener("popstate", () => {
  const url = new URL(location.href);
  const path = pagePath(url);
  if (path === currentPath) {
    const pending = activeRequest;
    activeRequest = undefined;
    pending?.abort();
    return;
  }
  if (knownPaths.has(path)) navigate(url, { traverse: true });
  else window.location.reload();
});

window.history.scrollRestoration = "manual";
try {
  prepareLanding()?.();
} finally {
  document.documentElement.classList.remove("landing-pending");
}
