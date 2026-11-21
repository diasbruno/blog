function followLegacyRoute() {
  if (location.pathname !== "/" && location.pathname !== "/index.html") return;
  const hash = window.location.hash;
  if (/^#\/blog\/?$/.test(hash)) {
    window.location.replace("/posts/");
    return;
  }

  const article = /^#\/blog\/(\d{4}\/\d{2}\/\d{2}\/[a-z0-9-]+)\/?$/.exec(hash);
  if (article) window.location.replace(`/articles/${article[1]}/`);
}

window.addEventListener("hashchange", followLegacyRoute);
followLegacyRoute();
