/*
 * BlockR: the interactive layer of the app (the canvas itself: blockr_canvas.js).
 *
 * Everything here is an enhancement over teal's own behaviour; without the
 * script the app works as before. No dependencies: plain DOM, Bootstrap's
 * Tab API when present, and Shiny's jQuery events.
 *
 *  1. Colour mode: saved choice > app default > operating system; switching
 *     is a circular reveal from the toggle (View Transitions, if supported).
 *  2. Appearance: text size, spacing, accent, corners, contrast, canvas,
 *     motion and the "What is this?" tips of the canvas; remembered.
 *  3. Status: while the server computes, a progress line and an amber dot;
 *     afterwards the duration of the last update.
 *  4. Where am I: a breadcrumb of the current module, the page title, deep
 *     links (#/safety-analysis/adverse-events/ae-summary, also for TLG
 *     outputs), browser back / forward between views, recent views.
 *  5. Rails: top-level tab bars get a sliding indicator.
 *  6. Command bar (Ctrl/Cmd+K or /): fuzzy search over every module, every
 *     TLG Catalog output and every action; scopes, prefixes (> actions,
 *     # outputs), open-in-new-tab.
 *  7. Keyboard shortcuts and their cheat sheet (?).
 *  8. Hover tools on plots (expand, pin, compare, copy, download) and tables
 *     (copy for Excel, CSV, pin).
 *  9. Pins: snapshots of plots and tables kept for this session, shown in a
 *     tray (P); two of them, or a pin and the live plot, can be compared.
 * 10. Viewer: full screen, zoom (wheel, keys), pan (drag); comparison by
 *     swipe (a divider over two plots), side by side, or difference (what
 *     changed between the two keeps its colour, the rest turns white).
 * 11. Focus mode (F), filter panel (\), "Show R code" (C), previous / next
 *     module ([ and ]).
 * 12. DataTables search fields get a placeholder.
 */
(function () {
  "use strict";

  var doc = document;
  var root = doc.documentElement;
  root.classList.add("tn-js");

  /* ---- Utilities ------------------------------------------------------- */

  function $(sel, ctx) { return (ctx || doc).querySelector(sel); }
  function $$(sel, ctx) { return Array.prototype.slice.call((ctx || doc).querySelectorAll(sel)); }
  function h(tag, attrs, children) {
    var node = doc.createElement(tag);
    if (attrs) {
      Object.keys(attrs).forEach(function (k) {
        var v = attrs[k];
        if (v === null || v === undefined || v === false) return;
        if (k === "class") node.className = v;
        else if (k === "html") node.innerHTML = v;
        else if (k === "text") node.textContent = v;
        else if (k.slice(0, 2) === "on") node.addEventListener(k.slice(2), v);
        else node.setAttribute(k, v === true ? "" : v);
      });
    }
    (children || []).forEach(function (c) { if (c) node.appendChild(typeof c === "string" ? doc.createTextNode(c) : c); });
    return node;
  }
  function label(node) { return node ? (node.textContent || "").replace(/\s+/g, " ").trim() : ""; }
  function visible(node) { return !!(node && (node.offsetWidth || node.offsetHeight || node.getClientRects().length)); }
  function esc(s) { return String(s).replace(/[&<>"']/g, function (c) { return { "&": "&amp;", "<": "&lt;", ">": "&gt;", "\"": "&quot;", "'": "&#39;" }[c]; }); }
  function slug(s) { return String(s).toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, ""); }
  // The main categories each have their own tint (see blockr.css)
  function sectionOf(name) {
    var n = String(name || "");
    if (/explor/i.test(n)) return "explore";
    if (/safety/i.test(n)) return "safety";
    if (/tlg|catalog/i.test(n)) return "catalog";
    return "home";
  }
  function load(key) { try { return window.localStorage.getItem("blockr:" + key); } catch (e) { return null; } }
  function save(key, value) { try { window.localStorage.setItem("blockr:" + key, value); } catch (e) { /* storage unavailable */ } }
  function reducedMotion() { return !!(window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches) || root.classList.contains("tn-still"); }
  function typing(t) { return !!(t && (t.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName))); }
  function clamp(x, a, b) { return Math.max(a, Math.min(b, x)); }
  var isMac = /Mac|iPhone|iPad/.test(navigator.platform || navigator.userAgent);
  var MOD = isMac ? "\u2318" : "Ctrl ";
  function timeNow() { return new Date().toTimeString().slice(0, 8); }

  var ICON = {
    search: '<circle cx="7" cy="7" r="4.5"/><path d="m10.5 10.5 3 3"/>',
    table: '<rect x="2" y="3" width="12" height="10" rx="2"/><path d="M2 6.5h12M6.5 6.5V13"/>',
    listing: '<path d="M6 4.5h8M6 8h8M6 11.5h8"/><circle cx="2.8" cy="4.5" r=".6"/><circle cx="2.8" cy="8" r=".6"/><circle cx="2.8" cy="11.5" r=".6"/>',
    graph: '<path d="M2.5 13.5h11"/><path d="M4.5 11V8M8 11V4.5M11.5 11V6.5"/>',
    data: '<ellipse cx="8" cy="4" rx="5" ry="2"/><path d="M3 4v8c0 1.1 2.2 2 5 2s5-.9 5-2V4M3 8c0 1.1 2.2 2 5 2s5-.9 5-2"/>',
    info: '<circle cx="8" cy="8" r="6"/><path d="M8 7.2V11M8 5h.01"/>',
    catalog: '<path d="M3 3.5h4a1.5 1.5 0 0 1 1.5 1.5v8A1.5 1.5 0 0 0 7 11.5H3zM13 3.5H9.5A1.5 1.5 0 0 0 8 5v8a1.5 1.5 0 0 1 1.5-1.5H13z"/>',
    group: '<path d="M2 5.5 8 2.5l6 3-6 3z"/><path d="m2 9 6 3 6-3"/>',
    module: '<rect x="2.5" y="2.5" width="4.5" height="4.5" rx="1"/><rect x="9" y="2.5" width="4.5" height="4.5" rx="1"/><rect x="2.5" y="9" width="4.5" height="4.5" rx="1"/><rect x="9" y="9" width="4.5" height="4.5" rx="1"/>',
    bolt: '<path d="M9 2 3.5 9H8l-1 5 5.5-7H8z"/>',
    moon: '<path d="M13 9.5A5.5 5.5 0 0 1 6.5 3a5.5 5.5 0 1 0 6.5 6.5z"/>',
    density: '<path d="M2.5 4h11M2.5 8h11M2.5 12h11"/><path d="M5 2.5v3M11 6.5v3M7 10.5v3"/>',
    focus: '<path d="M2.5 6V3.5a1 1 0 0 1 1-1H6M10 2.5h2.5a1 1 0 0 1 1 1V6M13.5 10v2.5a1 1 0 0 1-1 1H10M6 13.5H3.5a1 1 0 0 1-1-1V10"/>',
    filter: '<path d="M2.5 3.5h11L9.5 8.5v4l-3 1.5v-5.5z"/>',
    code: '<path d="M5.5 4.5 2 8l3.5 3.5M10.5 4.5 14 8l-3.5 3.5"/>',
    prev: '<path d="M10 3.5 5.5 8l4.5 4.5"/>',
    next: '<path d="M6 3.5 10.5 8 6 12.5"/>',
    link: '<path d="M6.5 9.5 9.5 6.5"/><path d="M7 4.5 8.3 3.2a2.5 2.5 0 0 1 3.5 3.5L10.5 8M9 11.5l-1.3 1.3a2.5 2.5 0 0 1-3.5-3.5L5.5 8"/>',
    keyboard: '<rect x="1.5" y="4" width="13" height="8" rx="1.5"/><path d="M4 6.5h.01M6.5 6.5h.01M9 6.5h.01M11.5 6.5h.01M5 9.5h6"/>',
    clock: '<circle cx="8" cy="8" r="6"/><path d="M8 4.5V8l2.5 1.5"/>',
    copy: '<rect x="5" y="5" width="8.5" height="8.5" rx="1.5"/><path d="M3.5 10.5h-.5a1 1 0 0 1-1-1V3a1 1 0 0 1 1-1h6.5a1 1 0 0 1 1 1v.5"/>',
    download: '<path d="M8 2.5v8M4.5 7 8 10.5 11.5 7M3 13.5h10"/>',
    expand: '<path d="M9.5 2.5h4v4M6.5 13.5h-4v-4M13.5 2.5 9 7M2.5 13.5 7 9"/>',
    pin: '<path d="M9.5 2.5 13.5 6.5 11 7.5 8.5 10l-.5 3L3 8l3-.5L8.5 5z"/><path d="m5.5 10.5-3 3"/>',
    compare: '<rect x="2" y="3" width="12" height="10" rx="1.5"/><path d="M8 1.5v13"/>',
    close: '<path d="m4 4 8 8M12 4l-8 8"/>',
    check: '<path d="m3 8.5 3 3 7-7"/>',
    plus: '<path d="M8 3v10M3 8h10"/>',
    minus: '<path d="M3 8h10"/>',
    fit: '<path d="M2.5 6V2.5H6M10 2.5h3.5V6M13.5 10v3.5H10M6 13.5H2.5V10"/><rect x="5.5" y="5.5" width="5" height="5" rx="1"/>',
    actual: '<rect x="2.5" y="2.5" width="11" height="11" rx="1.5"/><path d="M6 6.5 7 6v4.5M9.5 6.5l1-.5v4.5"/>',
    side: '<rect x="1.5" y="3" width="5.5" height="10" rx="1"/><rect x="9" y="3" width="5.5" height="10" rx="1"/>',
    diff: '<circle cx="6" cy="8" r="4"/><circle cx="10" cy="8" r="4"/>',
    trash: '<path d="M3 4.5h10M6.5 4.5V3h3v1.5M4.5 4.5l.5 9h6l.5-9"/>',
    arrow: '<path d="M3 8h10M9 4l4 4-4 4"/>',
    canvas: '<rect x="2.5" y="2.5" width="11" height="3.5" rx="1"/><rect x="2.5" y="7" width="11" height="3" rx="1"/><rect x="2.5" y="11" width="7" height="2.5" rx="1"/>',
    tutorial: '<path d="M1.5 6 8 3l6.5 3L8 9z"/><path d="M4 7.3v3.2c0 .9 1.8 2 4 2s4-1.1 4-2V7.3M14.5 6v4"/>'
  };
  function icon(name, size) {
    var s = size || 16;
    return '<svg class="tn-icon" viewBox="0 0 16 16" width="' + s + '" height="' + s + '" fill="none" stroke="currentColor" ' +
      'stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" focusable="false">' + (ICON[name] || "") + "</svg>";
  }

  /* ---- Toasts ----------------------------------------------------------- */

  var toastBox = null;
  function toast(message, iconName) {
    if (!toastBox) {
      toastBox = h("div", { class: "tn-toasts", role: "status", "aria-live": "polite" });
      doc.body.appendChild(toastBox);
    }
    var t = h("div", { class: "tn-toast", html: icon(iconName || "check") + "<span>" + esc(message) + "</span>" });
    toastBox.appendChild(t);
    window.setTimeout(function () { t.classList.add("tn-out"); }, 1900);
    window.setTimeout(function () { t.remove(); }, 2200);
  }

  function copyText(text, html) {
    if (navigator.clipboard && window.ClipboardItem && html && window.isSecureContext) {
      return navigator.clipboard.write([new window.ClipboardItem({
        "text/plain": new Blob([text], { type: "text/plain" }),
        "text/html": new Blob([html], { type: "text/html" })
      })]);
    }
    if (navigator.clipboard && window.isSecureContext) return navigator.clipboard.writeText(text);
    return new Promise(function (resolve, reject) {
      var ta = h("textarea", { style: "position:fixed;left:-9999px;top:0", "aria-hidden": "true" });
      ta.value = text;
      doc.body.appendChild(ta);
      ta.select();
      var ok = false;
      try { ok = doc.execCommand("copy"); } catch (e) { ok = false; }
      ta.remove();
      if (ok) resolve(); else reject(new Error("copy"));
    });
  }

  function download(href, name) {
    var a = h("a", { href: href, download: name, style: "display:none" });
    doc.body.appendChild(a);
    a.click();
    a.remove();
  }

  /* ---- 1. Colour mode ------------------------------------------------------ */

  function osDark() { return !!(window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches); }
  function savedMode() { var v = load("color-mode"); return v === "dark" || v === "light" || v === "auto" ? v : null; }
  function wantedMode() {
    var cfg = (window.blockr && window.blockr.colorMode) || "light";
    var v = savedMode() || cfg;
    if (v === "auto") return osDark() ? "dark" : "light";
    return v === "dark" || v === "light" ? v : null;
  }
  if (window.matchMedia) {
    var mq = window.matchMedia("(prefers-color-scheme: dark)");
    var follow = function () { if (savedMode() === "auto" || (!savedMode() && (window.blockr || {}).colorMode === "auto")) applyMode(); };
    if (mq.addEventListener) mq.addEventListener("change", follow);
  }
  function applyMode() {
    var mode = wantedMode();
    var current = root.getAttribute("data-bs-theme");
    if (mode && current !== mode) {
      // bslib's toggle observes this attribute and updates itself (and its
      // Shiny input value) accordingly.
      root.setAttribute("data-bs-theme", mode);
    } else if (current !== "dark" && current !== "light") {
      // Repair a missing or "undefined" value left by the toggle (a bslib bug
      // when the page starts in light mode).
      var os = window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches;
      root.setAttribute("data-bs-theme", os ? "dark" : "light");
    }
  }
  function rememberMode() {
    var mode = root.getAttribute("data-bs-theme");
    if (mode === "dark" || mode === "light") save("color-mode", mode);
  }
  function switchMode(origin) {
    var next = root.getAttribute("data-bs-theme") === "dark" ? "light" : "dark";
    var set = function () { root.setAttribute("data-bs-theme", next); rememberMode(); };
    if (!doc.startViewTransition || reducedMotion()) { set(); return; }
    var box = origin ? origin.getBoundingClientRect() : { left: window.innerWidth - 40, top: 30, width: 0, height: 0 };
    var x = box.left + box.width / 2;
    var y = box.top + box.height / 2;
    var r = Math.hypot(Math.max(x, window.innerWidth - x), Math.max(y, window.innerHeight - y));
    root.classList.add("tn-vt");
    var transition = doc.startViewTransition(set);
    transition.ready.then(function () {
      root.animate(
        { clipPath: ["circle(0px at " + x + "px " + y + "px)", "circle(" + r + "px at " + x + "px " + y + "px)"] },
        { duration: 420, easing: "cubic-bezier(.2, .8, .2, 1)", pseudoElement: "::view-transition-new(root)" }
      );
    }).catch(function () {});
    transition.finished.then(function () { root.classList.remove("tn-vt"); }, function () { root.classList.remove("tn-vt"); });
  }
  // Clicks on the toggle: handled here (capture phase, before the toggle's
  // own handler) so the switch can be revealed; the toggle follows the
  // attribute. Only explicit clicks are remembered.
  doc.addEventListener("click", function (event) {
    var toggle = event.target && event.target.closest && event.target.closest("bslib-input-dark-mode");
    if (!toggle) return;
    event.stopPropagation();
    event.preventDefault();
    switchMode(toggle);
  }, true);
  if (window.MutationObserver) {
    new MutationObserver(function () {
      var mode = root.getAttribute("data-bs-theme");
      if (mode !== "dark" && mode !== "light") applyMode();
    }).observe(root, { attributes: true, attributeFilter: ["data-bs-theme"] });
  }

  /* ---- 2. Appearance ----------------------------------------------------------
     Colour mode, text size, spacing, accent, corners, contrast, canvas and
     motion; applied as attributes on <html> (see blockr.css), saved in the
     browser and applied before the first paint by the inline <head> script. */

  var APPEARANCE = [
    { id: "mode", label: "Mode", options: [["light", "Light"], ["dark", "Dark"], ["auto", "System"]] },
    { id: "text", label: "Text size", attr: "data-tn-text", def: "m", options: [["s", "S"], ["m", "M"], ["l", "L"], ["xl", "XL"]] },
    { id: "density", label: "Spacing", attr: "data-tn-density", def: "comfortable", options: [["compact", "Compact"], ["comfortable", "Comfortable"], ["roomy", "Roomy"]] },
    { id: "accent", label: "Accent", attr: "data-tn-accent", def: "section", swatches: true, options: [["section", "By section"], ["teal", "Teal"], ["ocean", "Ocean"], ["iris", "Iris"], ["graphite", "Graphite"]] },
    { id: "corners", label: "Corners", attr: "data-tn-corners", def: "soft", options: [["soft", "Soft"], ["crisp", "Crisp"]] },
    { id: "contrast", label: "Contrast", attr: "data-tn-contrast", def: "standard", options: [["standard", "Standard"], ["high", "High"]] },
    { id: "canvas", label: "Canvas", attr: "data-tn-canvas", def: "dots", options: [["dots", "Dot grid"], ["plain", "Plain"]] },
    { id: "motion", label: "Motion", def: "on", options: [["on", "On"], ["off", "Off"]] },
    { id: "tips", label: "\u201cWhat is this?\u201d tips", attr: "data-tn-tips", def: "on", options: [["on", "Show"], ["off", "Hide"]] }
  ];
  function appearance() {
    try { return JSON.parse(load("appearance") || "{}") || {}; } catch (e) { return {}; }
  }
  function currentSetting(id) {
    if (id === "mode") return savedMode() || "auto-default";
    if (id === "motion") return root.classList.contains("tn-still") ? "off" : "on";
    var spec = APPEARANCE.filter(function (a) { return a.id === id; })[0];
    return root.getAttribute(spec.attr) || spec.def;
  }
  function setSetting(id, value) {
    if (id === "mode") {
      save("color-mode", value);
      if (value === "auto") {
        var os = window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches;
        if ((root.getAttribute("data-bs-theme") === "dark") !== os) switchMode(null);
        save("color-mode", "auto");
      } else if (root.getAttribute("data-bs-theme") !== value) {
        switchMode(null);
      }
      return;
    }
    var st = appearance();
    var spec = APPEARANCE.filter(function (a) { return a.id === id; })[0];
    if (id === "motion") {
      root.classList.toggle("tn-still", value === "off");
    } else if (value === spec.def) {
      root.removeAttribute(spec.attr);
    } else {
      root.setAttribute(spec.attr, value);
    }
    if (value === spec.def) delete st[id]; else st[id] = value;
    save("appearance", JSON.stringify(st));
    placeRails();
  }
  function resetAppearance() {
    APPEARANCE.forEach(function (a) { if (a.id !== "mode") setSetting(a.id, a.def); });
    save("appearance", "{}");
  }
  function cycleDensity() {
    var order = ["compact", "comfortable", "roomy"];
    var next = order[(order.indexOf(currentSetting("density")) + 1) % order.length];
    setSetting("density", next);
    syncAppearance();
    toast(next.charAt(0).toUpperCase() + next.slice(1) + " spacing", "density");
  }

  var appearPanel = null;
  var appearBack = null;
  function placeAppearance() {
    var btn = $('[data-tn-action="appearance"]');
    var r = btn ? btn.getBoundingClientRect() : { right: window.innerWidth - 16, bottom: 60 };
    appearPanel.style.top = (r.bottom + window.scrollY + 8) + "px";
    appearPanel.style.left = Math.max(8, r.right + window.scrollX - appearPanel.offsetWidth) + "px";
  }
  function syncAppearance() {
    if (!appearPanel) return;
    // the header may have moved (text size, spacing): stay under the button
    window.requestAnimationFrame(placeAppearance);
    placeAppearance();
    $$(".tn-seg button", appearPanel).forEach(function (b) {
      var id = b.getAttribute("data-set");
      var cur = currentSetting(id);
      if (id === "mode" && cur === "auto-default") cur = root.getAttribute("data-bs-theme") || "light";
      b.setAttribute("aria-checked", b.getAttribute("data-value") === cur ? "true" : "false");
    });
  }
  function buildAppearance() {
    appearPanel = h("div", { class: "tn-appear", role: "dialog", "aria-label": "Appearance", hidden: true });
    appearPanel.appendChild(h("div", { class: "tn-appear-head", html: '<span class="tn-eyebrow">Appearance</span>' }));
    APPEARANCE.forEach(function (a) {
      var seg = h("div", { class: "tn-seg" + (a.swatches ? " tn-seg-swatches" : ""), role: "radiogroup", "aria-label": a.label });
      a.options.forEach(function (o) {
        var b = h("button", {
          type: "button", role: "radio", "data-set": a.id, "data-value": o[0], "aria-checked": "false", title: o[1],
          html: (a.swatches ? '<span class="tn-swatch" data-swatch="' + o[0] + '"></span>' : "") + "<span>" + esc(o[1]) + "</span>"
        });
        b.addEventListener("click", function () { setSetting(a.id, o[0]); syncAppearance(); });
        seg.appendChild(b);
      });
      appearPanel.appendChild(h("div", { class: "tn-appear-row" }, [h("span", { class: "tn-appear-label", text: a.label }), seg]));
    });
    var reset = h("button", { type: "button", class: "tn-appear-reset", text: "Reset to defaults" });
    reset.addEventListener("click", function () { resetAppearance(); syncAppearance(); toast("Appearance reset", "check"); });
    appearPanel.appendChild(h("div", { class: "tn-appear-foot" }, [h("span", { html: "<kbd>A</kbd> opens this panel \u00b7 <kbd>D</kbd> cycles spacing" }), reset]));
    appearPanel.addEventListener("keydown", function (e) {
      if (e.key === "Escape") { e.preventDefault(); closeAppearance(); }
      var b = e.target.closest && e.target.closest(".tn-seg button");
      if (b && (e.key === "ArrowRight" || e.key === "ArrowLeft")) {
        e.preventDefault();
        var sibs = $$("button", b.parentElement);
        var k = (sibs.indexOf(b) + (e.key === "ArrowRight" ? 1 : -1) + sibs.length) % sibs.length;
        sibs[k].focus();
        sibs[k].click();
      }
    });
    doc.body.appendChild(appearPanel);
    doc.addEventListener("mousedown", function (e) {
      if (!appearPanel.hidden && !appearPanel.contains(e.target) && !e.target.closest('[data-tn-action="appearance"]')) closeAppearance();
    });
  }
  function openAppearance() {
    if (!appearPanel) buildAppearance();
    var btn = $('[data-tn-action="appearance"]');
    appearBack = doc.activeElement;
    appearPanel.hidden = false;
    syncAppearance();
    if (btn) btn.setAttribute("aria-expanded", "true");
    var first = $('.tn-seg button[aria-checked="true"]', appearPanel) || $(".tn-seg button", appearPanel);
    if (first) first.focus();
  }
  function closeAppearance() {
    if (!appearPanel || appearPanel.hidden) return;
    appearPanel.hidden = true;
    var btn = $('[data-tn-action="appearance"]');
    if (btn) btn.setAttribute("aria-expanded", "false");
    if (appearBack && appearBack.focus) appearBack.focus();
  }
  function toggleAppearance() { if (appearPanel && !appearPanel.hidden) closeAppearance(); else openAppearance(); }

  /* ---- 3. Status ------------------------------------------------------------- */

  var progress = null;
  var busySince = 0;
  var busyTimer = 0;
  function onBusy() {
    if (!busySince) busySince = window.performance.now();
    root.classList.add("tn-busy");
    window.clearTimeout(busyTimer);
    busyTimer = window.setTimeout(function () { root.classList.add("tn-busy-visible"); }, 180);
  }
  var readyShown = false;
  function onIdle() {
    window.clearTimeout(busyTimer);
    root.classList.remove("tn-busy", "tn-busy-visible");
    if (!busySince) return;
    var secs = (window.performance.now() - busySince) / 1000;
    busySince = 0;
    var out = $(".tn-status-time");
    if (!out) return;
    if (!readyShown) {
      // the first time: how long the app took to be ready, from the moment
      // the page was requested
      readyShown = true;
      var ready = window.performance.now() / 1000;
      out.textContent = "ready " + (ready < 10 ? ready.toFixed(1) : ready.toFixed(0)) + " s";
      out.title = "The app was ready " + ready.toFixed(2) + " seconds after the page was requested";
      return;
    }
    out.textContent = (secs < 10 ? secs.toFixed(2) : secs.toFixed(1)) + " s";
    out.title = "The last update took " + secs.toFixed(2) + " seconds (" + timeNow() + ")";
  }

  /* ---- 4. Where am I: tabs, chains, breadcrumb, history ------------------------ */

  var TRIGGERS = '[data-bs-toggle="tab"], [data-bs-toggle="pill"], [data-toggle="tab"], [data-toggle="pill"]';

  function targetId(t) {
    var s = t.getAttribute("data-bs-target") || t.getAttribute("data-target") || t.getAttribute("href") || "";
    return s.charAt(0) === "#" && s.length > 1 ? s.slice(1) : null;
  }
  function triggers() {
    return $$(TRIGGERS).filter(function (t) { return !t.closest(".tn-overlay, .tn-viewer, .tn-tray"); });
  }
  function triggerMap() {
    var map = {};
    triggers().forEach(function (t) { var id = targetId(t); if (id && !map[id]) map[id] = t; });
    return map;
  }
  // The path of labels from the outermost tab to `t`, with the labels of the
  // menus it sits in (a dropdown's toggle). Entries with `el` can be shown.
  function chain(t, map) {
    var out = [];
    var node = t;
    var guard = 0;
    while (node && guard++ < 12) {
      out.unshift({ label: label(node), el: node });
      var menu = node.closest(".dropdown-menu");
      if (menu) {
        var toggle = menu.parentElement && menu.parentElement.querySelector(".dropdown-toggle");
        if (toggle) out.unshift({ label: label(toggle), el: null });
      }
      var pane = node.parentElement && node.parentElement.closest(".tab-pane");
      node = pane && pane.id ? map[pane.id] : null;
    }
    return out;
  }
  function showTab(t) {
    try {
      if (window.bootstrap && window.bootstrap.Tab) { window.bootstrap.Tab.getOrCreateInstance(t).show(); return; }
      if (window.jQuery && window.jQuery.fn && window.jQuery.fn.tab) { window.jQuery(t).tab("show"); return; }
    } catch (e) { /* fall back to a click */ }
    t.click();
  }
  function openChain(t, map) {
    chain(t, map || triggerMap()).forEach(function (c) { if (c.el) showTab(c.el); });
  }
  // Fallback for navigation that is not made of Bootstrap tabs: click the
  // visible elements whose text is each label of the path, in turn.
  function clickPath(path) {
    var i = 0;
    (function step() {
      if (i >= path.length) return;
      var want = path[i++];
      var hit = $$("a, button, [role='tab'], [role='menuitem'], summary").filter(function (n) {
        return label(n) === want && visible(n) && !n.closest(".tn-overlay, .tn-header");
      })[0];
      if (hit) hit.click();
      window.setTimeout(step, hit ? 140 : 0);
    })();
  }
  function leafPane() {
    var panes = $$(".tab-pane.active").filter(function (p) {
      return visible(p) && !$$(".tab-pane.active", p).some(visible);
    });
    var map = triggerMap();
    for (var i = 0; i < panes.length; i++) if (panes[i].id && map[panes[i].id]) return { pane: panes[i], map: map };
    return null;
  }
  // The current view: the chain of the deepest visible tab, plus the TLG
  // output selected on a TLG Catalog page.
  function currentView() {
    var leaf = leafPane();
    if (!leaf) return [];
    var c = chain(leaf.map[leaf.pane.id], leaf.map);
    var item = $(".tn-tlg-item.active", leaf.pane);
    if (item) c.push({ label: item.getAttribute("data-value"), el: item, tlg: true });
    return c;
  }
  function viewHash(c) {
    return c.length ? "#/" + c.map(function (x) { return slug(x.label); }).join("/") : "";
  }

  // Navigate to a "#/a/b/c" path (deep links, back / forward)
  function navigateHash(hash) {
    if (!hash || hash.indexOf("#/") !== 0) return false;
    var want = decodeURIComponent(hash.slice(2)).split("/").filter(Boolean).join("/");
    var map = triggerMap();
    var exact = null;
    var parent = null;
    triggers().forEach(function (t) {
      var s = chain(t, map).map(function (x) { return slug(x.label); }).join("/");
      if (s === want) exact = t;
      else if (want.indexOf(s + "/") === 0 && (!parent || s.length > parent.s.length)) parent = { t: t, s: s };
    });
    if (exact) { openChain(exact, map); return true; }
    if (parent) {
      var rest = want.slice(parent.s.length + 1);
      var id = targetId(parent.t);
      var pane = id && doc.getElementById(id);
      var item = pane && $$(".tn-tlg-item", pane).filter(function (x) { return slug(x.getAttribute("data-value")) === rest; })[0];
      if (item) item.click();
      openChain(parent.t, map);
      return true;
    }
    return false;
  }

  var baseTitle = doc.title;
  var lastKey = null;
  var routerReady = false;
  var viewTimer = 0;

  function renderCrumbs(c) {
    var nav = $(".tn-crumbs");
    if (!nav) return;
    nav.innerHTML = "";
    c.forEach(function (x, i) {
      if (i) nav.appendChild(h("span", { class: "tn-crumb-sep", "aria-hidden": "true", text: "/" }));
      var b = h("button", {
        type: "button",
        "data-tn-sec": i === 0 ? sectionOf(x.label) : null,
        class: "tn-crumb" + (x.tlg ? " tn-num" : ""),
        title: i < c.length - 1 ? "Everything in " + x.label : "Jump to another view",
        text: x.label,
        "aria-current": i === c.length - 1 ? "page" : null
      });
      b.addEventListener("click", function () {
        // a group crumb lists what is in that group; the last crumb lists
        // its siblings
        var upto = i === c.length - 1 ? i : i + 1;
        palette.open("", { prefix: c.slice(0, upto).filter(function (y) { return !y.tlg; }).map(function (y) { return y.label; }) });
      });
      nav.appendChild(b);
    });
  }

  function recents() {
    try { return JSON.parse(load("recent") || "[]"); } catch (e) { return []; }
  }
  function addRecent(labels) {
    if (!labels.length) return;
    var key = labels.join("\u0001");
    var list = recents().filter(function (r) { return r.join("\u0001") !== key; });
    list.unshift(labels);
    save("recent", JSON.stringify(list.slice(0, 8)));
  }

  function updateView() {
    var c = currentView();
    root.setAttribute("data-tn-section", c.length ? sectionOf(c[0].label) : "home");
    renderCrumbs(c);
    placeRails();
    var labels = c.map(function (x) { return x.label; });
    var key = labels.join("\u0001");
    if (key === lastKey) return;
    lastKey = key;
    doc.title = labels.length ? labels[labels.length - 1] + (baseTitle ? " \u00b7 " + baseTitle : "") : baseTitle;
    if (!routerReady || !labels.length) return;
    var hash = viewHash(c);
    if (location.hash !== hash) {
      var url = location.pathname + location.search + hash;
      if (location.hash.indexOf("#/") === 0) window.history.pushState(null, "", url);
      else window.history.replaceState(null, "", url);
    }
    addRecent(labels);
  }
  function viewChanged() {
    window.clearTimeout(viewTimer);
    viewTimer = window.setTimeout(updateView, 40);
  }

  function startRouter() {
    var tries = 0;
    (function wait() {
      if (!triggers().length && tries++ < 150) { window.setTimeout(wait, 100); return; }
      if (location.hash.indexOf("#/") === 0) navigateHash(location.hash);
      window.setTimeout(function () { routerReady = true; lastKey = null; updateView(); }, 120);
    })();
  }
  window.addEventListener("popstate", function () {
    if (location.hash.indexOf("#/") === 0) navigateHash(location.hash);
  });

  /* ---- 5. Rails: sliding indicator under top-level tab bars ------------------- */

  var RAILS = ".nav-tabs:not(.card-header-tabs), .nav-underline";
  function activeTop(nav) {
    var links = nav.querySelectorAll(".nav-link.active, li.active > a, .nav-item.show > .nav-link");
    for (var i = 0; i < links.length; i++) {
      if (!links[i].closest(".dropdown-menu") && links[i].closest(".nav") === nav) return links[i];
    }
    return null;
  }
  function placeRail(nav, animate) {
    var link = activeTop(nav);
    if (!link || !link.offsetWidth) { nav.style.setProperty("--tn-ind-o", "0"); return; }
    var nb = nav.getBoundingClientRect();
    var lb = link.getBoundingClientRect();
    nav.style.setProperty("--tn-ind-x", (lb.left - nb.left + 10) + "px");
    nav.style.setProperty("--tn-ind-w", Math.max(0, lb.width - 20) + "px");
    nav.style.setProperty("--tn-ind-o", "1");
  }
  function rail(nav) {
    if (nav._tnRail || nav.closest(".dropdown-menu, .tn-overlay")) return;
    nav._tnRail = true;
    nav.classList.add("tn-rail");
    placeRail(nav, false);
    window.requestAnimationFrame(function () { window.requestAnimationFrame(function () { nav.classList.add("tn-rail-ready"); }); });
    // Only the tabs' classes are watched (not the bar's own, which this
    // code changes), so the observer never triggers itself.
    new MutationObserver(function (records) {
      if (records.some(function (r) { return r.target !== nav; })) placeRail(nav, true);
    }).observe(nav, { attributes: true, attributeFilter: ["class"], subtree: true });
    if (window.ResizeObserver) new window.ResizeObserver(function () { placeRail(nav, true); }).observe(nav);
  }
  function placeRails() {
    $$(RAILS).forEach(function (nav) { if (!nav._tnRail) rail(nav); else placeRail(nav, true); });
  }

  /* ---- Index: modules, TLG outputs, actions ------------------------------------ */

  var indexData = null;
  function index() {
    if (indexData) return indexData;
    indexData = { modules: [], tlg: [] };
    var node = doc.getElementById("tn-index");
    if (node) { try { indexData = JSON.parse(node.textContent); } catch (e) { /* keep empty */ } }
    indexData.modules = indexData.modules || [];
    indexData.tlg = indexData.tlg || [];
    return indexData;
  }

  function moduleEntries() {
    var idx = index();
    var map = triggerMap();
    var byPath = {};
    var byLeaf = {};
    idx.modules.forEach(function (m, i) {
      byPath[m.path.join("\u0001")] = { kind: m.kind, order: i };
      byLeaf[m.path[m.path.length - 1]] = { kind: m.kind, order: i };
    });
    var out = [];
    var seen = {};
    triggers().forEach(function (t, i) {
      if (t.closest(".dropdown-menu") && !t.closest(".nav")) return;
      var path = chain(t, map).map(function (x) { return x.label; }).filter(Boolean);
      if (!path.length) return;
      var key = path.join("\u0001");
      if (seen[key]) return;
      var leaf = path[path.length - 1];
      var known = byPath[key] || byLeaf[leaf];
      if (idx.modules.length && !known && t.parentElement.closest(".tab-pane")) return; // a tab inside a module
      seen[key] = true;
      var id = targetId(t);
      var pane = id && doc.getElementById(id);
      var isGroup = !!(pane && $(TRIGGERS, pane));
      var order = known ? known.order : 1000 + i;
      if (isGroup) {
        // a group sits where its first module is
        idx.modules.some(function (m, k) {
          var inside = path.every(function (l, j) { return m.path[j] === l; }) && m.path.length > path.length;
          if (inside) order = k - .5;
          return inside;
        });
      }
      out.push({
        type: "module", path: path, title: leaf, trigger: t,
        kind: isGroup ? "group" : (known ? known.kind : "module"),
        order: order
      });
    });
    // modules of the index that have no tab on the page: reached by text
    idx.modules.forEach(function (m, i) {
      var key = m.path.join("\u0001");
      var leaf = m.path[m.path.length - 1];
      var found = out.some(function (e) { return e.path.join("\u0001") === key || e.title === leaf; });
      if (!found) out.push({ type: "module", path: m.path.slice(), title: leaf, trigger: null, kind: m.kind, order: i });
    });
    out.sort(function (a, b) { return a.order - b.order; });
    return out;
  }

  function tlgEntries() {
    var list = index().tlg;
    if (!list.length) {
      list = $$(".tn-tlg-item").map(function (b) {
        var pane = b.closest(".tab-pane");
        return {
          id: b.getAttribute("data-value"),
          title: label($(".tn-tlg-item-title", b)),
          type: b.getAttribute("data-type") || "",
          topic: pane ? label($(".tn-tlg-heading", pane)) : ""
        };
      });
    }
    return list.map(function (e) {
      return { type: "tlg", id: e.id, title: e.title, ttype: e.type, topic: e.topic, path: ["TLG Catalog"] };
    });
  }

  function goModule(entry) {
    if (entry.trigger && doc.contains(entry.trigger)) openChain(entry.trigger);
    else clickPath(entry.path);
  }
  function goTlg(id) {
    var item = $$(".tn-tlg-item").filter(function (b) { return b.getAttribute("data-value") === id; })[0];
    if (!item) { toast("Output " + id + " is not on this page", "info"); return; }
    // select the output first, then show its page: one step in the history
    item.click();
    var pane = item.closest(".tab-pane");
    var map = triggerMap();
    if (pane && pane.id && map[pane.id]) openChain(map[pane.id], map);
    window.setTimeout(function () { item.scrollIntoView({ block: "nearest" }); viewChanged(); }, 60);
  }
  function go(entry) {
    if (entry.type === "module") goModule(entry);
    else if (entry.type === "tlg") goTlg(entry.id);
    else if (entry.type === "recent") navigateHash("#/" + entry.labels.map(slug).join("/"));
    else if (entry.type === "action") entry.run();
  }
  function entryHash(entry) {
    if (entry.type === "module") return "#/" + entry.path.map(slug).join("/");
    if (entry.type === "tlg") return "#/" + entry.path.concat(entry.id).map(slug).join("/");
    if (entry.type === "recent") return "#/" + entry.labels.map(slug).join("/");
    return null;
  }

  /* Actions */

  function currentScope() { var leaf = leafPane(); return leaf ? leaf.pane : doc.body; }
  function findButton(re) {
    var scope = currentScope();
    var pick = function (ctx) {
      return $$("button, a.btn, .action-button", ctx).filter(function (b) {
        var text = label(b) + " " + (b.getAttribute("title") || "") + " " + (b.getAttribute("aria-label") || "");
        return re.test(text) && visible(b) && !b.closest(".tn-header, .tn-overlay, .tn-viewer, .tn-tray");
      })[0];
    };
    return pick(scope) || pick(doc.body);
  }
  function showCode() {
    var b = findButton(/show r code/i);
    if (b) b.click(); else toast("No \u201cShow R code\u201d here", "info");
  }
  function toggleFilters() {
    var scope = currentScope();
    var t = $$(".bslib-sidebar-layout > .collapse-toggle", scope).filter(visible)[0] ||
      $$(".bslib-sidebar-layout > .collapse-toggle").filter(visible)[0];
    if (t) t.click(); else toast("No filter panel here", "info");
  }
  function toggleFocus() {
    root.classList.toggle("tn-focus");
    if (!$(".tn-focus-exit")) {
      doc.body.appendChild(h("button", {
        type: "button", class: "tn-focus-exit", html: icon("focus") + "<span>Exit focus</span><kbd>F</kbd>",
        onclick: toggleFocus
      }));
    }
    placeRails();
    if (root.classList.contains("tn-focus")) toast("Focus mode \u00b7 press F to leave", "focus");
  }
  function step(delta) {
    var list = moduleEntries().filter(function (e) { return e.kind !== "group"; });
    if (!list.length) return;
    var labels = currentView().filter(function (x) { return !x.tlg; }).map(function (x) { return x.label; });
    var at = -1;
    list.forEach(function (e, i) {
      var p = e.path;
      var ok = p.length <= labels.length && p.every(function (l, k) { return labels[k] === l; });
      if (ok && (at < 0 || p.length >= list[at].path.length)) at = i;
    });
    var next = list[(at + delta + list.length) % list.length];
    go(next);
    toast(next.path.join(" / "), delta > 0 ? "next" : "prev");
  }
  function goHome() {
    var list = moduleEntries();
    var home = list.filter(function (e) { return e.kind === "canvas"; })[0] ||
      list.filter(function (e) { return /^(canvas|home|start)$/i.test(e.title); })[0] || list[0];
    if (home) go(home);
    window.scrollTo({ top: 0, behavior: reducedMotion() ? "auto" : "smooth" });
  }
  // the Tutorial: a view of the canvas page (R/canvas.R, blockr_canvas.js)
  function goTutorial() {
    goHome();
    window.setTimeout(function () {
      if (window.BlockR && window.BlockR.showTutorial) window.BlockR.showTutorial();
      else toast("No tutorial on this page", "info");
    }, 60);
  }
  function copyLink() {
    copyText(location.href).then(function () { toast("Link to this view copied", "link"); }, function () { toast("Copy is not allowed here", "info"); });
  }

  var ACTIONS = [
    { id: "home", title: "Canvas", sub: "Build tables, listings and graphs from blocks", keys: ["H"], icon: "canvas", run: goHome },
    { id: "tutorial", title: "Tutorial", sub: "Eight short lessons, on a canvas of their own", keys: [], icon: "tutorial", run: goTutorial },
    { id: "theme", title: "Switch light / dark", sub: "Colour mode", keys: ["T"], icon: "moon", run: function () { switchMode($("bslib-input-dark-mode")); } },
    { id: "appearance", title: "Appearance", sub: "Text size, spacing, accent, corners, contrast, motion", keys: ["A"], icon: "density", run: function () { openAppearance(); } },
    { id: "density", title: "Cycle spacing", sub: "Compact, comfortable, roomy", keys: ["D"], icon: "density", run: cycleDensity },
    { id: "focus", title: "Focus mode", sub: "Hide the header and footer", keys: ["F"], icon: "focus", run: toggleFocus },
    { id: "filters", title: "Show / hide the filter panel", sub: "Of the current module", keys: ["\\"], icon: "filter", run: toggleFilters },
    { id: "code", title: "Show R code", sub: "Of the current module", keys: ["C"], icon: "code", run: showCode },
    { id: "pins", title: "Open pins", sub: "Plots and tables kept for comparison", keys: ["P"], icon: "pin", run: function () { tray.toggle(true); } },
    { id: "next", title: "Next module", sub: "In menu order", keys: ["]"], icon: "next", run: function () { step(1); } },
    { id: "prev", title: "Previous module", sub: "In menu order", keys: ["["], icon: "prev", run: function () { step(-1); } },
    { id: "link", title: "Copy link to this view", sub: "Opens this module (and output) directly", keys: ["L"], icon: "link", run: copyLink },
    { id: "keys", title: "Keyboard shortcuts", sub: "Everything you can do from the keyboard", keys: ["?"], icon: "keyboard", run: function () { sheet.open(); } }
  ].map(function (a) { a.type = "action"; return a; });

  /* ---- 6. Command bar ------------------------------------------------------------ */

  // Fuzzy match of one token in a string: contiguous matches score higher,
  // so do matches at word starts and at the start.
  function match(token, s) {
    var sl = s.toLowerCase();
    var at = sl.indexOf(token);
    if (at >= 0) {
      var hits = [];
      for (var k = 0; k < token.length; k++) hits.push(at + k);
      var start = at === 0 || /[\s\-_/().\u00b7:]/.test(s.charAt(at - 1));
      return { score: 40 + token.length * 6 + (start ? 30 : 0) - Math.min(at, 20) * .5, hits: hits };
    }
    var j = 0;
    var score = 0;
    var prev = -2;
    var out = [];
    for (var i = 0; i < sl.length && j < token.length; i++) {
      if (sl.charAt(i) === token.charAt(j)) {
        score += (i === prev + 1 ? 5 : 1) + (i === 0 || /[\s\-_/().\u00b7:]/.test(s.charAt(i - 1)) ? 6 : 0);
        out.push(i);
        prev = i;
        j++;
      }
    }
    return j === token.length ? { score: score, hits: out } : null;
  }
  function highlight(s, hits) {
    if (!hits || !hits.length) return esc(s);
    var set = {};
    hits.forEach(function (i) { set[i] = true; });
    var out = "";
    var open = false;
    for (var i = 0; i < s.length; i++) {
      if (set[i] && !open) { out += "<mark>"; open = true; }
      if (!set[i] && open) { out += "</mark>"; open = false; }
      out += esc(s.charAt(i));
    }
    return out + (open ? "</mark>" : "");
  }
  // Score an entry for a multi-word query: every token must match one field
  function scoreEntry(fields, tokens) {
    var total = 0;
    var hits = fields.map(function () { return []; });
    for (var t = 0; t < tokens.length; t++) {
      var best = null;
      var where = -1;
      for (var f = 0; f < fields.length; f++) {
        if (!fields[f]) continue;
        var m = match(tokens[t], fields[f]);
        if (m && (!best || m.score * (f === 0 ? 1.4 : 1) > best.score)) { best = { score: m.score * (f === 0 ? 1.4 : 1), hits: m.hits }; where = f; }
      }
      if (!best) return null;
      total += best.score;
      hits[where] = hits[where].concat(best.hits);
    }
    return { score: total, hits: hits };
  }

  var palette = (function () {
    var overlay, input, list, scopesBox, prefixBox, totalBox;
    var state = { scope: "all", prefix: [], items: [], active: 0, returnFocus: null };
    var SCOPES = [
      { id: "all", label: "All" },
      { id: "module", label: "Where to?" },
      { id: "tlg", label: "TLG outputs" },
      { id: "action", label: "Actions" }
    ];

    function build() {
      input = h("input", {
        class: "tn-pal-input", type: "text", role: "combobox", "aria-expanded": "true", "aria-controls": "tn-pal-list",
        "aria-autocomplete": "list", autocomplete: "off", spellcheck: "false",
        placeholder: "Jump to a module or TLG output, or type > for actions",
        "aria-label": "Search modules, TLG outputs and actions"
      });
      prefixBox = h("span", { class: "tn-pal-prefix" });
      list = h("ul", { class: "tn-pal-list", id: "tn-pal-list", role: "listbox", "aria-label": "Results" });
      scopesBox = h("div", { class: "tn-pal-scopes", role: "toolbar", "aria-label": "Scope" });
      totalBox = h("span", { class: "tn-pal-total" });
      var dialog = h("div", { class: "tn-dialog tn-pal", role: "dialog", "aria-modal": "true", "aria-label": "Command bar" }, [
        h("div", { class: "tn-pal-search", html: icon("search", 18) }, [prefixBox, input, h("kbd", { text: "esc" })]),
        scopesBox,
        list,
        h("div", { class: "tn-pal-foot", html:
          "<span><kbd>\u2191</kbd><kbd>\u2193</kbd> move</span><span><kbd>\u21b5</kbd> open</span>" +
          "<span><kbd>" + MOD.trim() + "</kbd><kbd>\u21b5</kbd> new tab</span><span><kbd>Tab</kbd> scope</span>" +
          "<span><kbd>&gt;</kbd> actions <kbd>#</kbd> outputs</span>" }, [totalBox])
      ]);
      overlay = h("div", { class: "tn-overlay", hidden: true }, [dialog]);
      overlay.addEventListener("mousedown", function (e) { if (e.target === overlay) close(); });
      input.addEventListener("input", function () { state.active = 0; render(); });
      input.addEventListener("keydown", onKey);
      list.addEventListener("mousemove", function (e) {
        var li = e.target.closest(".tn-pal-item");
        if (li && +li.getAttribute("data-i") !== state.active) { state.active = +li.getAttribute("data-i"); mark(false); }
      });
      list.addEventListener("click", function (e) {
        var li = e.target.closest(".tn-pal-item");
        if (li) run(state.items[+li.getAttribute("data-i")], e.metaKey || e.ctrlKey);
      });
      doc.body.appendChild(overlay);
    }

    function renderScopes(counts) {
      scopesBox.innerHTML = "";
      SCOPES.forEach(function (s) {
        var b = h("button", {
          type: "button", class: "tn-pal-scope", "aria-pressed": state.scope === s.id ? "true" : "false",
          html: esc(s.label) + (counts ? ' <span class="tn-pal-count">' + (counts[s.id] || 0) + "</span>" : "")
        });
        b.addEventListener("click", function () { state.scope = s.id; state.active = 0; render(); input.focus(); });
        scopesBox.appendChild(b);
      });
    }

    function entryFields(e) {
      if (e.type === "module") return [e.title, e.path.slice(0, -1).join(" / ")];
      if (e.type === "tlg") return [e.title, e.topic + " \u00b7 " + e.ttype, e.id];
      if (e.type === "recent") return [e.labels[e.labels.length - 1], e.labels.slice(0, -1).join(" / ")];
      return [e.title, e.sub];
    }

    function collect() {
      var raw = input.value;
      var scope = state.scope;
      var q = raw.trim();
      if (q.charAt(0) === ">") { scope = "action"; q = q.slice(1).trim(); }
      else if (q.charAt(0) === "#") { scope = "tlg"; q = q.slice(1).trim(); }
      var tokens = q.toLowerCase().split(/\s+/).filter(Boolean);
      var prefix = state.prefix;
      var inPrefix = function (path) { return prefix.every(function (p, i) { return path[i] === p; }); };

      var modules = moduleEntries().filter(function (e) { return inPrefix(e.path); });
      var tlg = tlgEntries().filter(function (e) { return inPrefix(e.path); });
      var actions = prefix.length ? [] : ACTIONS;
      var counts = { all: modules.length + tlg.length + actions.length, module: modules.length, tlg: tlg.length, action: actions.length };

      var groups = [];
      var add = function (name, entries) { if (entries.length) groups.push({ name: name, entries: entries }); };
      var scored = function (entries) {
        if (!tokens.length) return entries.map(function (e) { return { e: e, hits: [] }; });
        return entries.map(function (e) {
          var s = scoreEntry(entryFields(e), tokens);
          return s ? { e: e, score: s.score, hits: s.hits } : null;
        }).filter(Boolean).sort(function (a, b) { return b.score - a.score; });
      };

      if (!tokens.length && !prefix.length && scope === "all") {
        var rec = recents().slice(0, 5).map(function (labels) { return { type: "recent", labels: labels }; });
        add("Recent", scored(rec));
      }
      if (scope === "all" || scope === "module") {
        if (!tokens.length && !prefix.length) {
          // browsing: every module in its category and sub-group, as cards
          var where = modules.filter(function (e) { return e.kind !== "group"; });
          if (where.length) groups.push({ name: "Where to?", entries: where.map(function (e) { return { e: e, hits: [] }; }), where: true });
        } else {
          add(prefix.length ? prefix.join(" / ") : "Where to?", scored(modules));
        }
      }
      if (scope === "all" || scope === "tlg") add("TLG Catalog outputs", scored(tlg).slice(0, tokens.length || scope === "tlg" || prefix.length ? 400 : 12));
      if (scope === "all" || scope === "action") add("Actions", scored(actions));
      if (tokens.length && scope === "all") {
        // best group first
        groups.sort(function (a, b) { return (b.entries[0].score || 0) - (a.entries[0].score || 0); });
      }
      return { groups: groups, counts: counts };
    }

    function itemHtml(x) {
      var e = x.e;
      var f = entryFields(e);
      var iconName, meta = "";
      if (e.type === "module") iconName = e.kind === "catalog" ? "catalog" : (ICON[e.kind] ? e.kind : "module");
      else if (e.type === "tlg") { iconName = e.ttype === "Graph" ? "graph" : e.ttype === "Listing" ? "listing" : "table"; meta = '<span class="tn-tag">' + highlight(e.id, x.hits[2]) + "</span>"; }
      else if (e.type === "recent") iconName = "clock";
      else { iconName = e.icon || "bolt"; meta = (e.keys || []).map(function (k) { return "<kbd>" + esc(k) + "</kbd>"; }).join(""); }
      return '<span class="tn-pal-icon">' + icon(iconName) + "</span>" +
        '<span class="tn-pal-text"><span class="tn-pal-title">' + highlight(f[0], x.hits[0]) + "</span>" +
        (f[1] ? '<span class="tn-pal-sub">' + highlight(f[1], x.hits[1]) + "</span>" : "") + "</span>" +
        '<span class="tn-pal-meta">' + meta + (e.type !== "action" ? icon("arrow", 14) : "") + "</span>";
    }

    function render() {
      var res = collect();
      renderScopes(res.counts);
      prefixBox.innerHTML = state.prefix.length
        ? '<span class="tn-pal-chip">' + esc(state.prefix.join(" / ")) + ' <button type="button" aria-label="Clear">' + icon("close", 12) + "</button></span>"
        : "";
      var clear = $("button", prefixBox);
      if (clear) clear.addEventListener("click", function () { state.prefix = []; render(); input.focus(); });
      list.innerHTML = "";
      state.items = [];
      res.groups.forEach(function (g) {
        list.appendChild(h("li", { class: "tn-pal-group", role: "presentation", text: g.name }));
        if (g.where) { list.appendChild(whereGrid(g.entries)); return; }
        g.entries.forEach(function (x) {
          var i = state.items.length;
          state.items.push(x.e);
          list.appendChild(h("li", { class: "tn-pal-item", role: "option", id: "tn-pal-" + i, "data-i": i, "aria-selected": "false", html: itemHtml(x) }));
        });
      });
      if (!state.items.length) {
        list.appendChild(h("li", { class: "tn-pal-empty", role: "presentation", html: "<strong>Nothing matches \u201c" + esc(input.value) + "\u201d</strong>Try fewer letters, an output ID such as AET02, or &gt; for actions." }));
      }
      totalBox.textContent = state.items.length + " result" + (state.items.length === 1 ? "" : "s");
      state.active = clamp(state.active, 0, Math.max(0, state.items.length - 1));
      mark(true);
    }

    // "Where to?": categories as cards, sub-groups as headings, modules as
    // options (still one list for the keyboard)
    function whereGrid(entries) {
      var cards = [];
      var byName = {};
      entries.forEach(function (x) {
        var e = x.e;
        var name = e.path.length > 1 ? e.path[0] : "Start here";
        if (!byName[name]) { byName[name] = { name: name, items: [] }; cards.push(byName[name]); }
        byName[name].items.push(e);
      });
      var counts = {};
      tlgEntries().forEach(function (t) { counts[t.topic] = (counts[t.topic] || 0) + 1; counts["TLG Catalog"] = (counts["TLG Catalog"] || 0) + 1; });
      var grid = h("div", { class: "tn-where" });
      cards.forEach(function (c) {
        var card = h("div", { class: "tn-where-card", role: "group", "aria-label": c.name, "data-tn-sec": sectionOf(c.name === "Start here" ? "" : c.name) }, [
          h("div", { class: "tn-eyebrow tn-where-title", text: c.name + " \u00b7 " + c.items.length })
        ]);
        var sub = null;
        c.items.forEach(function (e) {
          var section = e.path.length > 2 ? e.path.slice(1, -1).join(" / ") : null;
          if (section !== sub) {
            sub = section;
            if (section) card.appendChild(h("div", { class: "tn-where-sub", text: section }));
          }
          var i = state.items.length;
          state.items.push(e);
          var iconName = e.kind === "catalog" ? "catalog" : (ICON[e.kind] ? e.kind : "module");
          card.appendChild(h("div", {
            class: "tn-pal-item tn-where-item", role: "option", id: "tn-pal-" + i, "data-i": i, "aria-selected": "false",
            html: icon(iconName) + "<span>" + esc(e.title) + "</span>" +
              (e.kind === "catalog" && counts[e.title] ? '<span class="tn-where-count tn-num">' + counts[e.title] + "</span>" : "")
          }));
        });
        grid.appendChild(card);
      });
      return h("li", { class: "tn-where-wrap", role: "presentation" }, [grid]);
    }

    function mark(scroll) {
      $$(".tn-pal-item", list).forEach(function (li) {
        var on = +li.getAttribute("data-i") === state.active;
        li.setAttribute("aria-selected", on ? "true" : "false");
        if (on && scroll) li.scrollIntoView({ block: "nearest" });
      });
      input.setAttribute("aria-activedescendant", state.items.length ? "tn-pal-" + state.active : "");
    }

    function run(entry, newTab) {
      if (!entry) return;
      if (newTab) {
        var hash = entryHash(entry);
        if (hash) { window.open(location.pathname + location.search + hash, "_blank", "noopener"); close(); return; }
      }
      close(true);
      window.setTimeout(function () { go(entry); }, 0);
    }

    function onKey(e) {
      var n = state.items.length;
      if (e.key === "ArrowDown") { e.preventDefault(); state.active = n ? (state.active + 1) % n : 0; mark(true); }
      else if (e.key === "ArrowUp") { e.preventDefault(); state.active = n ? (state.active - 1 + n) % n : 0; mark(true); }
      else if (e.key === "PageDown") { e.preventDefault(); state.active = clamp(state.active + 8, 0, n - 1); mark(true); }
      else if (e.key === "PageUp") { e.preventDefault(); state.active = clamp(state.active - 8, 0, n - 1); mark(true); }
      else if (e.key === "Enter") { e.preventDefault(); run(state.items[state.active], e.metaKey || e.ctrlKey); }
      else if (e.key === "Escape") { e.preventDefault(); close(); }
      else if (e.key === "Tab") {
        e.preventDefault();
        var i = SCOPES.map(function (s) { return s.id; }).indexOf(state.scope);
        state.scope = SCOPES[(i + (e.shiftKey ? -1 : 1) + SCOPES.length) % SCOPES.length].id;
        state.active = 0;
        render();
      } else if (e.key === "Backspace" && !input.value && state.prefix.length) {
        e.preventDefault();
        state.prefix = state.prefix.slice(0, -1);
        render();
      }
    }

    function open(query, opts) {
      if (!overlay) build();
      opts = opts || {};
      state.returnFocus = doc.activeElement;
      state.scope = opts.scope || "all";
      state.prefix = opts.prefix || [];
      state.active = 0;
      input.value = query || "";
      overlay.hidden = false;
      root.classList.add("tn-overlay-open");
      render();
      input.focus();
      input.select();
    }
    function close(keepFocus) {
      if (!overlay || overlay.hidden) return;
      overlay.hidden = true;
      root.classList.remove("tn-overlay-open");
      if (!keepFocus && state.returnFocus && state.returnFocus.focus) state.returnFocus.focus();
    }
    function isOpen() { return !!(overlay && !overlay.hidden); }
    return { open: open, close: close, isOpen: isOpen, toggle: function () { if (isOpen()) close(); else open(); } };
  })();

  /* ---- 7. Shortcut sheet ---------------------------------------------------------- */

  var sheet = (function () {
    var overlay = null;
    var back = null;
    function row(keys, what) {
      return "<dt>" + keys.map(function (k) { return "<kbd>" + esc(k) + "</kbd>"; }).join("") + "</dt><dd>" + what + "</dd>";
    }
    function build() {
      var html =
        '<div class="tn-keys-head"><h2>Keyboard shortcuts</h2><button type="button" class="tn-tool" data-close aria-label="Close">' + icon("close") + "</button></div>" +
        '<div class="tn-keys">' +
        '<h3 class="tn-eyebrow">Move around</h3><dl>' +
        row([MOD.trim(), "K"], "Command bar: jump to any module, TLG output or action") +
        row(["H"], "Home: the canvas (or click the logo)") +
        row(["/"], "Command bar") +
        row(["["], "Previous module") + row(["]"], "Next module") +
        row(["Alt", "\u2190"], "Back to the previous view (browser history)") +
        row(["L"], "Copy a link to this view") +
        "</dl>" +
        '<h3 class="tn-eyebrow">View</h3><dl>' +
        row(["T"], "Light / dark") + row(["A"], "Appearance: text size, spacing, accent, corners, contrast, motion") +
        row(["D"], "Cycle spacing: compact, comfortable, roomy") +
        row(["F"], "Focus mode") + row(["\\"], "Filter panel") +
        "</dl>" +
        '<h3 class="tn-eyebrow">Outputs</h3><dl>' +
        row(["C"], "Show R code of the current module") +
        row(["P"], "Pins: snapshots kept for comparison") +
        row(["hover"], "On a plot: expand, pin, compare, copy, download. On a table: copy for Excel, CSV, pin") +
        "</dl>" +
        '<h3 class="tn-eyebrow">Canvas</h3><dl>' +
        row(["del"], "Delete the selected block") +
        row([MOD.trim(), "Z"], "Undo") + row([MOD.trim(), "\u21e7", "Z"], "Redo") +
        row([MOD.trim(), "D"], "Duplicate the selected block") +
        row([MOD.trim(), "wheel"], "Zoom") + row(["esc"], "Deselect") +
        "</dl>" +
        '<h3 class="tn-eyebrow">Viewer</h3><dl>' +
        row(["wheel"], "Zoom at the pointer") + row(["drag"], "Pan") +
        row(["0"], "Fit") + row(["1"], "Actual size") + row(["+", "\u2212"], "Zoom") +
        row(["S"], "Comparison: swipe, side by side, difference") + row(["esc"], "Close") +
        "</dl></div>";
      var dialog = h("div", { class: "tn-dialog", role: "dialog", "aria-modal": "true", "aria-label": "Keyboard shortcuts", tabindex: "-1", html: html });
      overlay = h("div", { class: "tn-overlay", hidden: true }, [dialog]);
      overlay.addEventListener("mousedown", function (e) { if (e.target === overlay || e.target.closest("[data-close]")) close(); });
      overlay.addEventListener("keydown", function (e) { if (e.key === "Escape" || e.key === "?") { e.preventDefault(); close(); } });
      doc.body.appendChild(overlay);
    }
    function open() {
      if (!overlay) build();
      back = doc.activeElement;
      overlay.hidden = false;
      root.classList.add("tn-overlay-open");
      $(".tn-dialog", overlay).focus();
    }
    function close() {
      if (!overlay || overlay.hidden) return;
      overlay.hidden = true;
      root.classList.remove("tn-overlay-open");
      if (back && back.focus) back.focus();
    }
    return { open: open, close: close, isOpen: function () { return !!(overlay && !overlay.hidden); } };
  })();

  /* ---- 8-10. Pins, viewer, hover tools ---------------------------------------------- */

  function viewTitle() {
    var c = currentView();
    return c.length ? c.map(function (x) { return x.label; }).join(" / ") : baseTitle;
  }
  function imageData(img) {
    var src = img.currentSrc || img.src;
    if (/^data:/.test(src)) return Promise.resolve(src);
    return window.fetch(src).then(function (r) { return r.blob(); }).then(function (blob) {
      return new Promise(function (resolve, reject) {
        var fr = new window.FileReader();
        fr.onload = function () { resolve(fr.result); };
        fr.onerror = reject;
        fr.readAsDataURL(blob);
      });
    });
  }
  function copyImage(src) {
    if (!(navigator.clipboard && window.ClipboardItem && window.isSecureContext)) return Promise.reject(new Error("clipboard"));
    return window.fetch(src).then(function (r) { return r.blob(); }).then(function (blob) {
      var item = {};
      item[blob.type || "image/png"] = blob;
      return navigator.clipboard.write([new window.ClipboardItem(item)]);
    });
  }
  function tableMatrix(table) {
    return Array.prototype.map.call(table.rows, function (row) {
      var cells = [];
      Array.prototype.forEach.call(row.cells, function (cell) {
        cells.push((cell.innerText || cell.textContent || "").replace(/\s*\n\s*/g, " ").trim());
        for (var k = 1; k < (cell.colSpan || 1); k++) cells.push("");
      });
      return cells;
    }).filter(function (r) { return r.some(Boolean); });
  }
  function toTsv(m) { return m.map(function (r) { return r.map(function (c) { return c.replace(/\t/g, " "); }).join("\t"); }).join("\n"); }
  function toCsv(m) {
    return m.map(function (r) {
      return r.map(function (c) { return /[",\n]/.test(c) ? "\"" + c.replace(/"/g, "\"\"") + "\"" : c; }).join(",");
    }).join("\r\n");
  }
  function fileName(ext) { return (slug(viewTitle()) || "blockr") + "-" + timeNow().replace(/:/g, "") + "." + ext; }

  var pins = [];
  var pinSeq = 0;
  function pinPlot(img) {
    var title = viewTitle();
    imageData(img).then(function (src) {
      pins.unshift({ id: ++pinSeq, kind: "plot", src: src, title: title, time: timeNow(), w: img.naturalWidth, h: img.naturalHeight });
      pins = pins.slice(0, 16);
      tray.update();
      toast("Pinned \u00b7 P to see pins", "pin");
    }, function () { toast("This plot cannot be pinned", "info"); });
  }
  function pinTable(table) {
    pins.unshift({ id: ++pinSeq, kind: "table", html: table.outerHTML, title: viewTitle(), time: timeNow(), rows: table.rows.length });
    pins = pins.slice(0, 16);
    tray.update();
    toast("Table pinned \u00b7 P to see pins", "pin");
  }

  /* Viewer: one image, or two (swipe / side by side); tables as a sheet */
  var viewer = (function () {
    var box, bar, titleBox, zoomBox, stage, modeBtn, copyBtn, dlBtn, zoomTools;
    var st = { items: [], mode: "single", scale: 1, x: 0, y: 0, split: .5, back: null };

    function btn(name, text, title, fn) {
      var b = h("button", { type: "button", class: "tn-tool", title: title, "aria-label": title, html: icon(name) + (text ? "<span>" + esc(text) + "</span>" : "") });
      b.addEventListener("click", fn);
      return b;
    }
    function build() {
      titleBox = h("div", { class: "tn-viewer-title" });
      zoomBox = h("span", { class: "tn-viewer-zoom tn-num" });
      modeBtn = h("span", { class: "tn-viewer-modes", role: "group", "aria-label": "Comparison" });
      [["swipe", "compare", "Swipe"], ["side", "side", "Side by side"], ["diff", "diff", "Difference"]].forEach(function (m) {
        var b = btn(m[1], m[2], m[2] + " (S cycles)", function () { setMode(m[0]); });
        b.setAttribute("data-mode", m[0]);
        modeBtn.appendChild(b);
      });
      copyBtn = btn("copy", "Copy", "Copy image", function () {
        copyImage(st.items[0].src).then(function () { toast("Image copied", "copy"); }, function () { toast("Copy is not allowed here \u00b7 use Download", "info"); });
      });
      dlBtn = btn("download", "Download", "Download PNG", function () { download(st.items[0].src, fileName("png")); });
      zoomTools = h("span", { class: "tn-viewer-zoomtools" }, [
        btn("minus", "", "Zoom out (\u2212)", function () { zoomCenter(1 / 1.25); }),
        zoomBox,
        btn("plus", "", "Zoom in (+)", function () { zoomCenter(1.25); }),
        btn("fit", "", "Fit (0)", fit),
        btn("actual", "", "Actual size (1)", actual)
      ]);
      bar = h("div", { class: "tn-viewer-bar" }, [titleBox, modeBtn, zoomTools, copyBtn, dlBtn, btn("close", "", "Close (Esc)", close)]);
      stage = h("div", { class: "tn-viewer-stage" });
      box = h("div", { class: "tn-viewer", role: "dialog", "aria-modal": "true", "aria-label": "Viewer", tabindex: "-1", hidden: true }, [bar, stage]);
      doc.body.appendChild(box);

      stage.addEventListener("wheel", function (e) {
        if (st.items[0].kind !== "plot") return;
        e.preventDefault();
        var r = paneRect(e.clientX);
        zoomAt(e.clientX - r.left, e.clientY - r.top, Math.exp(-e.deltaY * 0.0015));
      }, { passive: false });
      var drag = null;
      stage.addEventListener("pointerdown", function (e) {
        if (st.items[0].kind !== "plot" || e.button !== 0) return;
        if (e.target.closest(".tn-swipe-handle")) {
          drag = { swipe: true };
        } else {
          drag = { x: e.clientX, y: e.clientY, ox: st.x, oy: st.y };
          stage.classList.add("tn-dragging");
        }
        stage.setPointerCapture(e.pointerId);
      });
      stage.addEventListener("pointermove", function (e) {
        if (!drag) return;
        if (drag.swipe) {
          var r = stage.getBoundingClientRect();
          st.split = clamp((e.clientX - r.left) / r.width, 0, 1);
          apply();
        } else {
          st.x = drag.ox + e.clientX - drag.x;
          st.y = drag.oy + e.clientY - drag.y;
          apply();
        }
      });
      var end = function () { drag = null; stage.classList.remove("tn-dragging"); };
      stage.addEventListener("pointerup", end);
      stage.addEventListener("pointercancel", end);
      stage.addEventListener("dblclick", function (e) {
        if (st.items[0].kind !== "plot") return;
        var r = paneRect(e.clientX);
        if (Math.abs(st.scale - 1) < .01) fit(); else { var k = 1 / st.scale; zoomAt(e.clientX - r.left, e.clientY - r.top, k); }
      });
      // keys work wherever the focus is while the viewer is open (the focused
      // element may be replaced when the comparison mode changes)
      doc.addEventListener("keydown", function (e) {
        if (box.hidden || e.metaKey || e.ctrlKey || e.altKey) return;
        if (e.key === "Escape") { e.preventDefault(); close(); }
        else if (e.key === "+" || e.key === "=") zoomCenter(1.25);
        else if (e.key === "-" || e.key === "_") zoomCenter(1 / 1.25);
        else if (e.key === "0") fit();
        else if (e.key === "1") actual();
        else if ((e.key === "s" || e.key === "S") && st.items.length === 2 && st.items[0].kind === "plot") setMode({ swipe: "side", side: "diff", diff: "swipe" }[st.mode] || "swipe");
        else if (st.mode === "swipe" && (e.key === "ArrowLeft" || e.key === "ArrowRight")) { st.split = clamp(st.split + (e.key === "ArrowLeft" ? -.05 : .05), 0, 1); apply(); }
      });
      window.addEventListener("resize", function () { if (!box.hidden) fit(); });
    }

    function panes() { return $$(".tn-viewer-pane", stage); }
    function paneRect(clientX) {
      var ps = panes();
      for (var i = 0; i < ps.length; i++) { var r = ps[i].getBoundingClientRect(); if (clientX >= r.left && clientX <= r.right) return r; }
      return (ps[0] || stage).getBoundingClientRect();
    }
    function apply() {
      var t = "translate(" + st.x + "px," + st.y + "px) scale(" + st.scale + ")";
      $$("img.tn-viewer-img", stage).forEach(function (img) { img.style.transform = t; });
      zoomBox.textContent = Math.round(st.scale * 100) + "%";
      var clip = $(".tn-layer-b", stage);
      var handle = $(".tn-swipe-handle", stage);
      if (clip) clip.style.clipPath = "inset(0 0 0 " + (st.split * 100) + "%)";
      if (handle) { handle.style.left = (st.split * 100) + "%"; handle.setAttribute("aria-valuenow", Math.round(st.split * 100)); }
    }
    function zoomAt(px, py, k) {
      var ns = clamp(st.scale * k, .1, 12);
      k = ns / st.scale;
      st.x = px - (px - st.x) * k;
      st.y = py - (py - st.y) * k;
      st.scale = ns;
      apply();
    }
    function zoomCenter(k) {
      var r = (panes()[0] || stage).getBoundingClientRect();
      zoomAt(r.width / 2, r.height / 2, k);
    }
    function fit() {
      var it = st.items[0];
      if (!it || it.kind !== "plot") return;
      var r = (panes()[0] || stage).getBoundingClientRect();
      var w = it.w || 800;
      var hh = it.h || 600;
      st.scale = clamp(Math.min((r.width - 48) / w, (r.height - 48) / hh), .1, 3);
      st.x = (r.width - w * st.scale) / 2;
      st.y = (r.height - hh * st.scale) / 2;
      apply();
    }
    function actual() {
      var it = st.items[0];
      var r = (panes()[0] || stage).getBoundingClientRect();
      st.scale = 1;
      st.x = (r.width - (it.w || 800)) / 2;
      st.y = Math.max(24, (r.height - (it.h || 600)) / 2);
      apply();
    }
    function img(item) {
      var i = h("img", { class: "tn-viewer-img", src: item.src, alt: item.title, draggable: "false" });
      i.addEventListener("load", function () {
        if (!item.w) { item.w = i.naturalWidth; item.h = i.naturalHeight; fit(); }
      });
      return i;
    }
    function caption(item, which) {
      return h("span", { class: "tn-viewer-cap tn-viewer-cap-" + which, text: (which === "a" ? "A" : "B") + " \u00b7 " + item.title + " \u00b7 " + item.time });
    }
    function setMode(mode) {
      st.mode = mode;
      if (stage.contains(doc.activeElement)) box.focus();
      stage.innerHTML = "";
      stage.className = "tn-viewer-stage tn-viewer-" + mode;
      var a = st.items[0];
      var b = st.items[1];
      if (a.kind === "table") {
        st.items.forEach(function (it, i) {
          stage.appendChild(h("div", { class: "tn-viewer-pane" }, [
            st.items.length > 1 ? caption(it, i ? "b" : "a") : null,
            h("div", { class: "tn-viewer-doc", html: it.html })
          ]));
        });
      } else if (mode === "single") {
        stage.appendChild(h("div", { class: "tn-viewer-pane" }, [img(a)]));
        stage.appendChild(h("div", { class: "tn-viewer-hint", text: "Scroll to zoom \u00b7 drag to pan \u00b7 double-click for 100%" }));
      } else if (mode === "swipe") {
        var handle = h("div", {
          class: "tn-swipe-handle", role: "slider", tabindex: "0", "aria-label": "Comparison divider",
          "aria-valuemin": "0", "aria-valuemax": "100", html: '<span class="tn-swipe-grip">' + icon("compare", 14) + "</span>"
        });
        stage.appendChild(h("div", { class: "tn-viewer-pane" }, [
          img(a),
          h("div", { class: "tn-layer-b" }, [img(b)]),
          handle,
          caption(a, "a"),
          caption(b, "b")
        ]));
      } else if (mode === "diff") {
        // pixels that are the same in both plots turn white; what changed
        // keeps its colour
        stage.appendChild(h("div", { class: "tn-viewer-pane" }, [
          h("div", { class: "tn-diff" }, [img(a), img(b)]),
          h("span", { class: "tn-viewer-cap tn-viewer-cap-a", text: "Difference \u00b7 A " + a.time + " vs B " + b.time + " \u00b7 unchanged = white" })
        ]));
      } else {
        stage.appendChild(h("div", { class: "tn-viewer-pane" }, [img(a), caption(a, "a")]));
        stage.appendChild(h("div", { class: "tn-viewer-pane" }, [img(b), caption(b, "b")]));
      }
      modeBtn.style.display = st.items.length === 2 && a.kind === "plot" ? "" : "none";
      $$("[data-mode]", modeBtn).forEach(function (x) { x.setAttribute("aria-pressed", x.getAttribute("data-mode") === mode ? "true" : "false"); });
      var plot = a.kind === "plot";
      zoomTools.style.display = plot ? "" : "none";
      copyBtn.style.display = plot && st.items.length === 1 ? "" : "none";
      dlBtn.style.display = plot && st.items.length === 1 ? "" : "none";
      window.requestAnimationFrame(fit);
    }
    function open(items, mode) {
      if (!box) build();
      st.items = items;
      st.split = .5;
      st.back = doc.activeElement;
      titleBox.textContent = items.length === 2 ? "Compare \u00b7 " + items[0].title + (items[0].title === items[1].title ? "" : " \u2194 " + items[1].title) : items[0].title;
      box.hidden = false;
      root.classList.add("tn-overlay-open");
      setMode(mode || (items.length === 2 ? "swipe" : "single"));
      box.focus();
    }
    function close() {
      if (!box || box.hidden) return;
      box.hidden = true;
      stage.innerHTML = "";
      root.classList.remove("tn-overlay-open");
      if (st.back && st.back.focus) st.back.focus();
    }
    return { open: open, close: close, isOpen: function () { return !!(box && !box.hidden); } };
  })();

  function livePlot(img) {
    return { kind: "plot", src: img.currentSrc || img.src, title: viewTitle(), time: "now", w: img.naturalWidth, h: img.naturalHeight };
  }

  /* Tray of pins */
  var tray = (function () {
    var box, row, countBox, compareBtn, launcher;
    var selected = [];
    function build() {
      countBox = h("span", { class: "tn-tray-count tn-num" });
      compareBtn = h("button", { type: "button", class: "btn btn-primary btn-sm", html: icon("compare") + " Compare", disabled: true });
      compareBtn.addEventListener("click", function () {
        var items = pins.filter(function (p) { return selected.indexOf(p.id) >= 0; });
        if (items.length === 2) viewer.open(items.reverse());
      });
      var clear = h("button", { type: "button", class: "btn btn-default btn-sm", html: icon("trash") + " Clear" });
      clear.addEventListener("click", function () { pins = []; selected = []; update(); toggle(false); });
      var close = h("button", { type: "button", class: "tn-tool", "aria-label": "Close pins", html: icon("close") });
      close.addEventListener("click", function () { toggle(false); });
      row = h("div", { class: "tn-tray-row" });
      box = h("section", { class: "tn-tray", "aria-label": "Pins", hidden: true }, [
        h("div", { class: "tn-tray-head" }, [
          h("span", { class: "tn-eyebrow", text: "Pins" }), countBox,
          h("span", { class: "tn-tray-hint", text: "Select two to compare \u00b7 double-click to open" }),
          compareBtn, clear, close
        ]),
        row
      ]);
      doc.body.appendChild(box);
      box.addEventListener("keydown", function (e) { if (e.key === "Escape") toggle(false); });
    }
    function card(p) {
      var sel = selected.indexOf(p.id) >= 0;
      var thumb = p.kind === "plot"
        ? h("img", { src: p.src, alt: "", draggable: "false" })
        : h("div", { class: "tn-tray-table", html: icon("table", 22) + "<span class='tn-num'>" + p.rows + " rows</span>" });
      var rm = h("button", { type: "button", class: "tn-tray-rm", "aria-label": "Remove pin", html: icon("close", 12) });
      rm.addEventListener("click", function (e) {
        e.stopPropagation();
        pins = pins.filter(function (x) { return x.id !== p.id; });
        selected = selected.filter(function (x) { return x !== p.id; });
        update();
      });
      var c = h("button", { type: "button", class: "tn-tray-card" + (sel ? " tn-sel" : ""), "aria-pressed": sel ? "true" : "false", title: p.title }, [
        h("span", { class: "tn-tray-thumb" }, [thumb]),
        h("span", { class: "tn-tray-meta" }, [
          h("span", { class: "tn-tray-title", text: p.title.split(" / ").pop() }),
          h("span", { class: "tn-tray-time tn-num", text: p.time + " \u00b7 " + p.kind })
        ]),
        rm
      ]);
      c.addEventListener("click", function () {
        var at = selected.indexOf(p.id);
        if (at >= 0) selected.splice(at, 1);
        else {
          var same = pins.filter(function (x) { return selected.indexOf(x.id) >= 0; });
          if (same.length && same[0].kind !== p.kind) selected = [];
          selected.push(p.id);
          if (selected.length > 2) selected.shift();
        }
        update();
      });
      c.addEventListener("dblclick", function () { viewer.open([p]); });
      return c;
    }
    function update() {
      if (!box) build();
      row.innerHTML = "";
      selected = selected.filter(function (id) { return pins.some(function (p) { return p.id === id; }); });
      pins.forEach(function (p) { row.appendChild(card(p)); });
      if (!pins.length) row.appendChild(h("div", { class: "tn-tray-empty", html: "No pins yet. Hover a plot or a table and choose " + icon("pin", 14) + " Pin." }));
      countBox.textContent = pins.length;
      compareBtn.disabled = selected.length !== 2;
      // launcher in the header
      if (!launcher) {
        var actions = $(".tn-actions");
        if (actions) {
          launcher = h("button", { type: "button", class: "tn-iconbtn tn-pins-btn", title: "Pins (P)", "aria-label": "Pins", html: icon("pin") + '<span class="tn-pins-count tn-num"></span>' });
          launcher.addEventListener("click", function () { toggle(); });
          actions.insertBefore(launcher, actions.querySelector('[data-tn-action="density"]'));
        }
      }
      if (launcher) {
        launcher.hidden = !pins.length;
        $(".tn-pins-count", launcher).textContent = pins.length || "";
      }
    }
    function toggle(force) {
      if (!box) build();
      var show = force === undefined ? box.hidden : force;
      if (show) update();
      box.hidden = !show;
      root.classList.toggle("tn-tray-open", show);
      if (show) { var first = $(".tn-tray-card", box); if (first) first.focus(); }
    }
    return { update: update, toggle: toggle };
  })();

  /* Hover tools over plots and tables */
  var tools = (function () {
    var box = null;
    var target = null;
    var hideTimer = 0;
    function build() {
      box = h("div", { class: "tn-tools", role: "toolbar", "aria-label": "Output tools" });
      box.addEventListener("mouseenter", function () { window.clearTimeout(hideTimer); });
      box.addEventListener("mouseleave", scheduleHide);
      doc.body.appendChild(box);
    }
    function tool(name, text, title, fn) {
      var b = h("button", { type: "button", class: "tn-tool", title: title, html: icon(name) + "<span>" + esc(text) + "</span>" });
      b.addEventListener("click", function (e) { e.preventDefault(); e.stopPropagation(); fn(); });
      return b;
    }
    function show(t) {
      if (!box) build();
      window.clearTimeout(hideTimer);
      if (target === t && box.classList.contains("tn-on")) return;
      target = t;
      box.innerHTML = "";
      if (t.tagName === "IMG") {
        box.appendChild(tool("expand", "Expand", "Open in the viewer", function () { viewer.open([livePlot(t)]); }));
        box.appendChild(tool("pin", "Pin", "Keep a snapshot to compare later", function () { pinPlot(t); }));
        var lastPlot = pins.filter(function (p) { return p.kind === "plot"; })[0];
        if (lastPlot) box.appendChild(tool("compare", "Compare", "Compare with the last pinned plot (" + lastPlot.time + ")", function () { viewer.open([lastPlot, livePlot(t)]); }));
        box.appendChild(tool("copy", "Copy", "Copy the image", function () {
          copyImage(t.currentSrc || t.src).then(function () { toast("Image copied", "copy"); }, function () { toast("Copy is not allowed here \u00b7 use Download", "info"); });
        }));
        box.appendChild(tool("download", "PNG", "Download as PNG", function () { download(t.currentSrc || t.src, fileName("png")); }));
      } else {
        box.appendChild(tool("copy", "Copy", "Copy for Excel (keeps the layout)", function () {
          var m = tableMatrix(t);
          copyText(toTsv(m), t.outerHTML).then(function () { toast("Copied " + m.length + " rows", "copy"); }, function () { toast("Copy is not allowed here", "info"); });
        }));
        box.appendChild(tool("download", "CSV", "Download as CSV", function () {
          var csv = "\ufeff" + toCsv(tableMatrix(t));
          download(URL.createObjectURL(new Blob([csv], { type: "text/csv" })), fileName("csv"));
        }));
        box.appendChild(tool("pin", "Pin", "Keep a snapshot to compare later", function () { pinTable(t); }));
      }
      place();
      box.classList.add("tn-on");
    }
    function place() {
      if (!target || !box) return;
      var r = target.getBoundingClientRect();
      var w = box.offsetWidth;
      var bh = box.offsetHeight;
      // plots: inside the top-right corner; tables: just above their
      // top-right corner, so the header row stays readable
      var top = target.tagName === "IMG" || r.top < bh + 8 ? r.top + 8 : r.top - bh - 4;
      box.style.top = Math.max(4, top + window.scrollY) + "px";
      box.style.left = Math.max(4, r.right + window.scrollX - w - (target.tagName === "IMG" ? 8 : 0)) + "px";
    }
    function scheduleHide() {
      window.clearTimeout(hideTimer);
      hideTimer = window.setTimeout(function () { if (box) box.classList.remove("tn-on"); target = null; }, 220);
    }
    function candidate(node) {
      if (!node || !node.closest || node.closest(".tn-header, .tn-overlay, .tn-viewer, .tn-tray, .tn-tools, .modal, .dropdown-menu, .selectize-dropdown")) return null;
      var out = node.closest(".shiny-plot-output, .shiny-image-output");
      if (out) { var img = $("img", out); return img && img.naturalWidth ? img : null; }
      var table = node.closest("table");
      if (table && !table.closest(".dataTables_wrapper, .dt-container") && !table.classList.contains("dataTable") && table.rows.length > 1) return table;
      return null;
    }
    doc.addEventListener("mouseover", function (e) {
      if (box && box.contains(e.target)) return;
      var t = candidate(e.target);
      if (t) show(t); else if (target) scheduleHide();
    });
    window.addEventListener("scroll", function () { if (target) place(); }, { passive: true, capture: true });
    return { hide: scheduleHide };
  })();

  // Clicking a plot opens it in the viewer, unless the plot itself reacts to
  // clicks (brushing or clicking to select points)
  var INTERACTIVE = "[data-click-id], [data-dblclick-id], [data-brush-id]";
  doc.addEventListener("click", function (e) {
    var img = e.target.closest && e.target.closest(".shiny-plot-output img");
    if (!img || e.defaultPrevented || e.button !== 0 || !img.naturalWidth) return;
    if (img.closest(INTERACTIVE)) return;
    viewer.open([livePlot(img)]);
  });

  /* ---- 11. Global keys ----------------------------------------------------------------- */

  function anyOverlay() { return palette.isOpen() || sheet.isOpen() || viewer.isOpen(); }
  doc.addEventListener("keydown", function (e) {
    if ((e.metaKey || e.ctrlKey) && !e.altKey && !e.shiftKey && (e.key === "k" || e.key === "K")) {
      e.preventDefault();
      if (viewer.isOpen()) viewer.close();
      if (sheet.isOpen()) sheet.close();
      palette.toggle();
      return;
    }
    if (anyOverlay() || e.defaultPrevented || e.metaKey || e.ctrlKey || e.altKey) return;
    if (typing(e.target) || $(".modal.show")) return;
    var k = e.key;
    var map = {
      "/": function () { palette.open(); },
      h: goHome,
      "?": function () { sheet.open(); },
      t: function () { switchMode($("bslib-input-dark-mode")); },
      d: cycleDensity,
      a: toggleAppearance,
      f: toggleFocus,
      "\\": toggleFilters,
      c: showCode,
      p: function () { tray.toggle(); },
      l: copyLink,
      "[": function () { step(-1); },
      "]": function () { step(1); }
    };
    if (k === "Escape" && root.classList.contains("tn-tray-open")) { tray.toggle(false); return; }
    var fn = map[k] || (k.length === 1 ? map[k.toLowerCase()] : null);
    if (fn) {
      e.preventDefault();
      fn();
    }
  });

  doc.addEventListener("click", function (e) {
    var a = e.target.closest && e.target.closest("[data-tn-action]");
    if (!a) return;
    var what = a.getAttribute("data-tn-action");
    if (what === "home") { e.preventDefault(); goHome(); }
    else if (what === "palette") palette.open();
    else if (what === "appearance") toggleAppearance();
  });

  /* ---- Start ------------------------------------------------------------------------- */

  function start() {
    // a small API for tests and power users (set here: the inline <head>
    // script that carries the settings may run after this file)
    window.blockr = window.blockr || {};
    window.blockr.palette = palette;
    window.blockr.navigate = navigateHash;
    window.blockr.view = function () { return currentView().map(function (x) { return x.label; }); };
    // change an appearance setting from elsewhere (e.g. the canvas hides its tips)
    window.blockr.setting = function (id, value) { setSetting(id, value); if (appearPanel) syncAppearance(); };
    applyMode();
    if (!progress) { progress = h("div", { class: "tn-progress", "aria-hidden": "true" }); doc.body.appendChild(progress); }
    // platform-specific key labels
    $$(".tn-cmd-key, .tn-footer-keys kbd").forEach(function (k) { if (/Ctrl K/.test(k.textContent)) k.textContent = MOD + "K"; });
    placeRails();
    startRouter();
    if (window.MutationObserver) {
      var queued = false;
      new MutationObserver(function () {
        if (queued) return;
        queued = true;
        window.requestAnimationFrame(function () { queued = false; placeRails(); });
      }).observe(doc.body, { childList: true, subtree: true });
    }
    doc.addEventListener("shown.bs.tab", viewChanged);
    doc.addEventListener("click", function (e) {
      if (e.target.closest && e.target.closest(TRIGGERS + ", .tn-tlg-item")) viewChanged();
    });
    window.addEventListener("resize", function () { placeRails(); }, { passive: true });
    if (doc.fonts && doc.fonts.ready) doc.fonts.ready.then(placeRails);
  }

  if (doc.readyState === "loading") doc.addEventListener("DOMContentLoaded", start);
  else start();

  if (window.jQuery) {
    var jq = window.jQuery;
    jq(doc).one("shiny:sessioninitialized", function () { applyMode(); viewChanged(); });
    jq(doc).on("shiny:busy", onBusy);
    jq(doc).on("shiny:idle", onIdle);
    jq(doc).on("shown.bs.tab", viewChanged);
    // DataTables: the search field shows a placeholder (its "Search:" label
    // is only read by screen readers)
    jq(doc).on("init.dt", function (event) {
      jq(event.target).closest(".dataTables_wrapper, .dt-container").find(".dataTables_filter input, .dt-search input").each(function () {
        if (!this.getAttribute("placeholder")) this.setAttribute("placeholder", "Search");
      });
    });
  }

})();
