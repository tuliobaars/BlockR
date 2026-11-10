/*
 * BlockR canvas: a drag-and-drop editor of block graphs (see R/canvas.R and
 * R/blocks_graph.R for the graph).
 *
 * The graph lives here, in the browser; R compiles and runs it after every
 * change and answers with the status of every block (rows, columns, a
 * table, a plot, an error). Plain DOM, no dependencies.
 *
 *  - Stacks: a block whose input is the block above it (a pipe step) is
 *    drawn snapped under it; a pipeline is a stack of blocks.
 *  - Wires: a block's result used by another block as an argument (e.g. the
 *    rows and the layout of "Build table") is a wire from the bottom of its
 *    stack to a socket of the other block. Dashed wires: a block's code
 *    mentions another block's variable.
 *  - The output of a stack is its last block: blocks added at the bottom
 *    take over its wires and its variable.
 *  - Drag a block to move it (the gap closes), shift-drag to take the blocks
 *    below it too, drag the first block of a stack to move the stack. Drop
 *    blocks between or below blocks to insert them. Drag from the round
 *    port under a stack to a socket to wire it.
 *  - Pan: drag the background or scroll; zoom: Ctrl/Cmd + scroll or the
 *    buttons. Undo/redo: Ctrl/Cmd+Z, Shift+Ctrl/Cmd+Z. Delete: Delete key.
 */
(function () {
  "use strict";

  var doc = document;
  var BLOCK_W = 264;
  var COL_GAP = 96;
  var ROW_GAP = 44;
  var SNAP_DX = 150;
  var SNAP_DY = 56;

  /* ---- Utilities ---------------------------------------------------------- */

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
    (children || []).forEach(function (c) {
      if (c === null || c === undefined || c === false) return;
      node.appendChild(typeof c === "string" ? doc.createTextNode(c) : c);
    });
    return node;
  }
  function esc(s) {
    return String(s).replace(/[&<>"']/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", "\"": "&quot;", "'": "&#39;" }[c];
    });
  }
  function clamp(x, a, b) { return Math.max(a, Math.min(b, x)); }
  // on screen (offsetParent is null for fixed elements, e.g. full screen)
  function isShown(el) { return !!(el && el.getClientRects().length); }
  function arr(x) { return x === null || x === undefined ? [] : Array.isArray(x) ? x : [x]; }
  function clone(x) { return JSON.parse(JSON.stringify(x)); }
  function fmtInt(n) { return Number(n).toLocaleString("en-US"); }
  function typing(t) { return !!(t && (t.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName))); }
  function debounce(fn, ms) {
    var timer = 0;
    return function () {
      var args = arguments;
      var self = this;
      window.clearTimeout(timer);
      timer = window.setTimeout(function () { fn.apply(self, args); }, ms);
    };
  }
  var isMac = /Mac|iPhone|iPad/.test(navigator.platform || navigator.userAgent);

  var ICON = {
    data: '<ellipse cx="8" cy="4" rx="5" ry="2"/><path d="M3 4v8c0 1.1 2.2 2 5 2s5-.9 5-2V4M3 8c0 1.1 2.2 2 5 2s5-.9 5-2"/>',
    prep: '<path d="M2.5 3.5h11L9.5 8.5v4l-3 1.5v-5.5z"/>',
    layout: '<rect x="2" y="3" width="12" height="10" rx="2"/><path d="M2 6.5h12M6.5 6.5V13"/>',
    analyze: '<path d="M3 13V9M6.5 13V5M10 13V7.5M13.5 13V3"/>',
    output: '<path d="M4 2.5h5.5L12.5 5.5V13a.5.5 0 0 1-.5.5H4a.5.5 0 0 1-.5-.5V3a.5.5 0 0 1 .5-.5z"/><path d="M9.5 2.5v3h3M6 9h4M6 11.5h4"/>',
    plot: '<path d="M2.5 13.5h11"/><path d="M3.5 11 6.5 7l2.5 2.5L13 4.5"/>',
    value: '<path d="M5 3C3.5 3 3.5 4.5 3.5 5.5S3 8 2 8c1 0 1.5 1.5 1.5 2.5S3.5 13 5 13M11 3c1.5 0 1.5 1.5 1.5 2.5S13 8 14 8c-1 0-1.5 1.5-1.5 2.5S12.5 13 11 13"/>',
    code: '<path d="M5.5 4.5 2 8l3.5 3.5M10.5 4.5 14 8l-3.5 3.5"/>',
    search: '<circle cx="7" cy="7" r="4.5"/><path d="m10.5 10.5 3 3"/>',
    undo: '<path d="M5.5 3.5 2.5 6.5l3 3"/><path d="M2.5 6.5h7a4 4 0 0 1 0 8H7"/>',
    redo: '<path d="m10.5 3.5 3 3-3 3"/><path d="M13.5 6.5h-7a4 4 0 0 0 0 8H9"/>',
    arrange: '<rect x="2" y="2.5" width="4.5" height="4" rx="1"/><rect x="9.5" y="2.5" width="4.5" height="4" rx="1"/><rect x="9.5" y="9.5" width="4.5" height="4" rx="1"/><path d="M6.5 4.5h3M11.75 6.5v3"/>',
    fit: '<path d="M2.5 6V2.5H6M10 2.5h3.5V6M13.5 10v3.5H10M6 13.5H2.5V10"/>',
    plus: '<path d="M8 3v10M3 8h10"/>',
    minus: '<path d="M3 8h10"/>',
    play: '<path d="M5 3.5v9l7-4.5z"/>',
    check: '<path d="m3 8.5 3 3 7-7"/>',
    warning: '<path d="M8 2.5 14 13H2L8 2.5Z"/><path d="M8 6.5v3"/><circle cx="8" cy="11.2" r=".5"/>',
    close: '<path d="m4 4 8 8M12 4l-8 8"/>',
    trash: '<path d="M3 4.5h10M6.5 4.5V3h3v1.5M4.5 4.5l.5 9h6l.5-9"/>',
    copy: '<rect x="5" y="5" width="8.5" height="8.5" rx="1.5"/><path d="M3.5 10.5h-.5a1 1 0 0 1-1-1V3a1 1 0 0 1 1-1h6.5a1 1 0 0 1 1 1v.5"/>',
    bypass: '<path d="M2.5 8h3M10.5 8h3"/><path d="M5.5 8c0-3 5-3 5 0"/><circle cx="8" cy="11" r="1"/>',
    star: '<path d="m8 2.5 1.7 3.5 3.8.5-2.8 2.7.7 3.8L8 11.2 4.6 13l.7-3.8L2.5 6.5l3.8-.5z"/>',
    template: '<rect x="2.5" y="2.5" width="4.5" height="4.5" rx="1"/><rect x="9" y="2.5" width="4.5" height="4.5" rx="1"/><rect x="2.5" y="9" width="4.5" height="4.5" rx="1"/><path d="M11.25 9.5v3.5M9.5 11.25H13"/>',
    blocks: '<rect x="2.5" y="2.5" width="11" height="3.5" rx="1"/><rect x="2.5" y="7" width="11" height="3" rx="1"/><rect x="2.5" y="11" width="7" height="2.5" rx="1"/>',
    more: '<circle cx="4" cy="8" r=".8"/><circle cx="8" cy="8" r=".8"/><circle cx="12" cy="8" r=".8"/>',
    help: '<circle cx="8" cy="8" r="6"/><path d="M6.3 6.2a1.8 1.8 0 0 1 3.5.6c0 1.2-1.8 1.5-1.8 2.6M8 11.4h.01"/>',
    expand: '<path d="M9.5 2.5h4v4M6.5 13.5h-4v-4M13.5 2.5 9 7M2.5 13.5 7 9"/>',
    shrink: '<path d="M13.5 6.5h-4v-4M2.5 9.5h4v4M9.5 6.5l4-4M6.5 9.5l-4 4"/>',
    grad: '<path d="M1.5 6 8 3l6.5 3L8 9z"/><path d="M4 7.3v3.2c0 .9 1.8 2 4 2s4-1.1 4-2V7.3M14.5 6v4"/>',
    panel: '<rect x="2" y="2.5" width="12" height="11" rx="1.5"/><path d="M9.5 2.5v11"/>',
    file: '<path d="M4 2.5h5.5L12.5 5.5V13a.5.5 0 0 1-.5.5H4a.5.5 0 0 1-.5-.5V3a.5.5 0 0 1 .5-.5z"/><path d="M9.5 2.5v3h3"/>',
    edit: '<path d="M10.5 2.5 13.5 5.5 6 13H3v-3z"/><path d="M9 4l3 3"/>',
    back: '<path d="M6.5 3.5 2 8l4.5 4.5M2.5 8h11"/>',
    link: '<path d="M6.5 9.5 9.5 6.5"/><path d="M7 4.5 8.3 3.2a2.5 2.5 0 0 1 3.5 3.5L10.5 8M9 11.5l-1.3 1.3a2.5 2.5 0 0 1-3.5-3.5L5.5 8"/>',
    Table: '<rect x="2" y="3" width="12" height="10" rx="2"/><path d="M2 6.5h12M6.5 6.5V13"/>',
    Listing: '<path d="M6 4.5h8M6 8h8M6 11.5h8"/><circle cx="2.8" cy="4.5" r=".6"/><circle cx="2.8" cy="8" r=".6"/><circle cx="2.8" cy="11.5" r=".6"/>',
    Graph: '<path d="M2.5 13.5h11"/><path d="M4.5 11V8M8 11V4.5M11.5 11V6.5"/>'
  };
  function icon(name, size) {
    var s = size || 16;
    return '<svg class="bk-i" viewBox="0 0 16 16" width="' + s + '" height="' + s + '" fill="none" stroke="currentColor" ' +
      'stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" focusable="false">' + (ICON[name] || "") + "</svg>";
  }

  /* ---- R literals ------------------------------------------------------------
     The graph stores every argument as R code. Editors read simple R values
     (strings, numbers, vectors of strings, conditions) from it and write R
     code back; anything they cannot read is edited as code. */

  var R = {
    str: function (s) { return JSON.stringify(String(s)); },
    // a column name in code: AGE, or `Age group` when not syntactic
    sym: function (s) { return /^([A-Za-z]|[.][A-Za-z._])[A-Za-z0-9._]*$/.test(s) || s === "." ? s : "`" + String(s).replace(/`/g, "\\`") + "`"; },
    // a string literal -> its value (null if `code` is not a string literal)
    unstr: function (code) {
      if (code === null || code === undefined) return null;
      var m = /^\s*"((?:[^"\\]|\\.)*)"\s*$/.exec(code) || /^\s*'((?:[^'\\]|\\.)*)'\s*$/.exec(code);
      if (!m) return null;
      try { return JSON.parse('"' + m[1].replace(/\\'/g, "'").replace(/\\`/g, "`") + '"'); } catch (e) { return m[1]; }
    },
    unsym: function (code) {
      if (code === null || code === undefined) return null;
      var t = String(code).trim();
      if (/^([A-Za-z]|[.][A-Za-z._])[A-Za-z0-9._]*$/.test(t)) return t;
      var m = /^`((?:[^`\\]|\\.)+)`$/.exec(t);
      return m ? m[1].replace(/\\`/g, "`") : null;
    },
    // split the arguments of c(...) / list(...) / a call at top-level commas
    splitTop: function (s) {
      var out = [];
      var depth = 0;
      var q = null;
      var cur = "";
      for (var i = 0; i < s.length; i++) {
        var ch = s[i];
        if (q) {
          cur += ch;
          if (ch === "\\") { cur += s[++i] || ""; continue; }
          if (ch === q) q = null;
          continue;
        }
        if (ch === '"' || ch === "'" || ch === "`") { q = ch; cur += ch; continue; }
        if (ch === "(" || ch === "[" || ch === "{") depth++;
        if (ch === ")" || ch === "]" || ch === "}") depth--;
        if (ch === "," && depth === 0) { out.push(cur.trim()); cur = ""; continue; }
        cur += ch;
      }
      if (cur.trim() !== "" || out.length) out.push(cur.trim());
      return out;
    },
    // `name = value` -> [name, value]; positional -> ["", value]
    splitNamed: function (item) {
      var m = /^\s*(`(?:[^`\\]|\\.)+`|"(?:[^"\\]|\\.)*"|[A-Za-z.][A-Za-z0-9._]*)\s*=(?!=)\s*([\s\S]*)$/.exec(item);
      if (!m) return ["", item.trim()];
      var n = m[1];
      if (n[0] === "`") n = n.slice(1, -1);
      else if (n[0] === '"') n = R.unstr(n);
      return [n, m[2].trim()];
    },
    // the items of c(...) (or a single value)
    items: function (code, fn) {
      var t = String(code || "").trim();
      var m = new RegExp("^" + (fn || "c") + "\\(([\\s\\S]*)\\)$").exec(t);
      if (!m) return null;
      return R.splitTop(m[1]);
    },
    // c("a", "b") or "a" -> ["a", "b"]; null if not only strings
    strings: function (code) {
      if (code === null || code === undefined || String(code).trim() === "") return [];
      var one = R.unstr(code);
      if (one !== null) return [one];
      var it = R.items(code);
      if (!it) return null;
      var out = [];
      for (var i = 0; i < it.length; i++) {
        var v = R.unstr(it[i]);
        if (v === null) return null;
        out.push(v);
      }
      return out;
    },
    vec: function (values) {
      if (!values.length) return null;
      return values.length === 1 ? R.str(values[0]) : "c(" + values.map(R.str).join(", ") + ")";
    },
    // c(a = "x", b = "y") -> [["a", "x"], ["b", "y"]]; null if not of that form
    named: function (code) {
      if (code === null || code === undefined || String(code).trim() === "") return [];
      var it = R.items(code);
      if (!it) return null;
      var out = [];
      for (var i = 0; i < it.length; i++) {
        var p = R.splitNamed(it[i]);
        var v = R.unstr(p[1]);
        if (v === null) return null;
        out.push([p[0], v]);
      }
      return out;
    },
    namedVec: function (pairs) {
      pairs = pairs.filter(function (p) { return p[0] !== "" || p[1] !== ""; });
      if (!pairs.length) return null;
      return "c(" + pairs.map(function (p) { return (p[0] ? R.argName(p[0]) + " = " : "") + R.str(p[1]); }).join(", ") + ")";
    },
    argName: function (n) { return /^[A-Za-z.][A-Za-z0-9._]*$/.test(n) ? n : "`" + n.replace(/`/g, "\\`") + "`"; },
    num: function (code) {
      var t = String(code === null || code === undefined ? "" : code).trim();
      return /^-?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?L?$/.test(t) ? parseFloat(t) : null;
    },
    bool: function (code) {
      var t = String(code === null || code === undefined ? "" : code).trim();
      if (t === "TRUE" || t === "T") return true;
      if (t === "FALSE" || t === "F") return false;
      return null;
    },
    // short display of R code
    show: function (code, max) {
      if (code === null || code === undefined) return "";
      var s = R.unstr(code);
      if (s === null) {
        var v = R.strings(code);
        s = v && v.length ? v.join(", ") : String(code).replace(/\s+/g, " ");
      }
      max = max || 48;
      return s.length > max ? s.slice(0, max - 1) + "\u2026" : s;
    }
  };

  /* Filter conditions <-> rows of (column, operator, values) */
  var OPS = [
    ["==", "is"], ["!=", "is not"], ["in", "is one of"], ["notin", "is not one of"],
    [">", ">"], [">=", "\u2265"], ["<", "<"], ["<=", "\u2264"],
    ["na", "is missing"], ["notna", "is not missing"], ["true", "is true"], ["false", "is false"]
  ];
  var OP_LABEL = {};
  OPS.forEach(function (o) { OP_LABEL[o[0]] = o[1]; });
  var COL = "(`(?:[^`\\\\]|\\\\.)+`|[A-Za-z.][A-Za-z0-9._]*)";
  function literal(code) {
    var s = R.unstr(code);
    if (s !== null) return { v: s, q: true };
    var n = R.num(code);
    if (n !== null) return { v: String(code).trim().replace(/L$/, ""), q: false };
    var b = R.bool(code);
    if (b !== null) return { v: b ? "TRUE" : "FALSE", q: false };
    return null;
  }
  function parseCondition(code) {
    var t = String(code || "").trim();
    var m;
    if ((m = new RegExp("^" + COL + "\\s*(==|!=|>=|<=|>|<)\\s*(.+)$").exec(t))) {
      var lit = literal(m[3]);
      if (lit) return { col: R.unsym(m[1]), op: m[2], values: [lit.v], quoted: lit.q };
    }
    if ((m = new RegExp("^(!\\s*)?\\(?\\s*" + COL + "\\s*%in%\\s*(c\\([\\s\\S]*\\)|\"(?:[^\"\\\\]|\\\\.)*\")\\s*\\)?$").exec(t))) {
      var items = R.unstr(m[3]) !== null ? [m[3]] : R.items(m[3]);
      var vals = [];
      var quoted = true;
      for (var i = 0; items && i < items.length; i++) {
        var l = literal(items[i]);
        if (!l) { items = null; break; }
        vals.push(l.v);
        quoted = l.q;
      }
      if (items) return { col: R.unsym(m[2]), op: m[1] ? "notin" : "in", values: vals, quoted: quoted };
    }
    if ((m = new RegExp("^(!\\s*)?is\\.na\\(\\s*" + COL + "\\s*\\)$").exec(t))) return { col: R.unsym(m[2]), op: m[1] ? "notna" : "na", values: [] };
    if ((m = new RegExp("^(!\\s*)?" + COL + "$").exec(t))) return { col: R.unsym(m[2]), op: m[1] ? "false" : "true", values: [] };
    return null;
  }
  function writeCondition(c) {
    if (!c.col) return null;
    var col = R.sym(c.col);
    var val = function (v) { return c.quoted === false && (R.num(v) !== null || v === "TRUE" || v === "FALSE") ? String(v) : R.str(v); };
    switch (c.op) {
      case "na": return "is.na(" + col + ")";
      case "notna": return "!is.na(" + col + ")";
      case "true": return col;
      case "false": return "!" + col;
      case "in":
      case "notin": {
        if (!c.values.length) return null;
        var v = "c(" + c.values.map(val).join(", ") + ")";
        return (c.op === "notin" ? "!" : "") + col + " %in% " + v;
      }
      default:
        if (!c.values.length || c.values[0] === "") return null;
        return col + " " + c.op + " " + val(c.values[0]);
    }
  }
  function describeCondition(code) {
    var c = parseCondition(code);
    if (!c) return String(code).replace(/\s+/g, " ");
    var vals = c.values.length > 3 ? c.values.slice(0, 3).join(", ") + ", \u2026" : c.values.join(", ");
    if (c.op === "na" || c.op === "notna" || c.op === "true" || c.op === "false") return c.col + " " + OP_LABEL[c.op];
    return c.col + " " + OP_LABEL[c.op] + " " + vals;
  }

  /* ---- Graph helpers ------------------------------------------------------------ */

  function byId(g) {
    var m = {};
    g.nodes.forEach(function (n) { m[n.id] = n; });
    return m;
  }
  // who uses each node, and how: pipe (input), ref (argument), implicit
  function consumersOf(g, implicit) {
    var out = {};
    g.nodes.forEach(function (n) { out[n.id] = []; });
    g.nodes.forEach(function (n) {
      if (n.input && out[n.input]) out[n.input].push({ id: n.id, via: "pipe" });
      (n.args || []).forEach(function (a, i) {
        if (a.ref && out[a.ref]) out[a.ref].push({ id: n.id, via: "ref", arg: i });
      });
      arr(implicit && implicit[n.id]).forEach(function (src) {
        if (out[src]) out[src].push({ id: n.id, via: "implicit" });
      });
    });
    return out;
  }
  function nextId(g) {
    var max = 0;
    g.nodes.forEach(function (n) {
      var m = /^n(\d+)$/.exec(n.id);
      if (m) max = Math.max(max, parseInt(m[1], 10));
    });
    return "n" + (max + 1);
  }
  function nextOrder(g) {
    var max = 0;
    g.nodes.forEach(function (n) { if (typeof n.order === "number") max = Math.max(max, n.order); });
    return max + 1;
  }
  // would wiring `from` into `to` create a loop?
  function reaches(g, from, to) {
    var nodes = byId(g);
    var seen = {};
    var stack = [from];
    while (stack.length) {
      var id = stack.pop();
      if (id === to) return true;
      if (seen[id] || !nodes[id]) continue;
      seen[id] = true;
      var n = nodes[id];
      if (n.input) stack.push(n.input);
      (n.args || []).forEach(function (a) { if (a.ref) stack.push(a.ref); });
    }
    return false;
  }

  // Arguments of a call matched to the function's formal arguments, like R:
  // by name first, then by position (the piped input takes the first one).
  function matchArgs(node, spec) {
    var formals = arr(spec && spec.formals).slice();
    if (node.input && spec && spec.op !== "+" && formals.length && formals[0] !== "...") formals = formals.slice(1);
    // names the block knows (its editors), e.g. labs(title = ...)
    var known = formals.concat(arr(spec && spec.args).map(function (x) { return x.name; }));
    var map = {};
    var dots = [];
    var extra = [];
    var used = {};
    (node.args || []).forEach(function (a, i) {
      if (a.name && known.indexOf(a.name) >= 0 && a.name !== "..." && map[a.name] === undefined) { map[a.name] = i; used[i] = true; }
    });
    var fi = 0;
    (node.args || []).forEach(function (a, i) {
      if (used[i]) return;
      if (a.name) {
        if (formals.indexOf("...") >= 0) dots.push(i); else extra.push(i);
        return;
      }
      while (fi < formals.length && formals[fi] !== "..." && map[formals[fi]] !== undefined) fi++;
      if (fi < formals.length && formals[fi] !== "...") { map[formals[fi]] = i; fi++; } else if (formals.indexOf("...") >= 0) dots.push(i); else extra.push(i);
    });
    return { map: map, dots: dots, extra: extra, formals: formals };
  }

  /* ---- Editor ------------------------------------------------------------------- */

  function Editor(root) {
    var self = this;
    this.root = root;
    this.ns = root.getAttribute("data-ns") || "";
    var cfg = {};
    try { cfg = JSON.parse($(".bk-config", root).textContent); } catch (e) { cfg = {}; }
    this.cfg = cfg;
    this.reg = {};
    arr(cfg.registry && cfg.registry.blocks).forEach(function (b) { self.reg[b.fn] = b; });
    this.cats = arr(cfg.registry && cfg.registry.categories);
    this.dsLabels = (cfg.registry && cfg.registry.datasets) || {};
    this.templates = arr(cfg.templates);
    this.datasets = [];
    this.status = { nodes: {}, rev: -1 };
    this.schemas = {};
    this.view = { x: 40, y: 40, k: 1 };
    this.selected = null;
    this.undoStack = [];
    this.redoStack = [];
    this.sentJson = null;
    this.rev = 0;
    this.heights = {};
    this.pos = {};
    this.hooks = {};
    this.insets = { left: 0 };
    this.tutorial = !!cfg.tutorial;
    this.g = this.emptyGraph();
    this.build();
    this.restore();
    this.render();
    // the Tutorial page: lessons on this canvas (blockr_tutorial.js)
    if (this.tutorial && window.BlockRTutorial) this.coach = window.BlockRTutorial.attach(this);
  }

  // A tiny event hub (the tutorial listens): change, status, graph, select, pane, tip
  Editor.prototype.on = function (what, fn) { (this.hooks[what] = this.hooks[what] || []).push(fn); };
  Editor.prototype.emit = function (what, arg) {
    var self = this;
    (this.hooks[what] || []).forEach(function (fn) { try { fn(arg); } catch (e) { if (window.console) window.console.error(e); } });
    void self;
  };

  Editor.prototype.emptyGraph = function () {
    return { version: 1, packages: [], nodes: [], result: null, template: null, title: null };
  };
  Editor.prototype.input = function (name, value, event) {
    if (window.Shiny && window.Shiny.setInputValue) {
      window.Shiny.setInputValue(this.ns + name, value, event ? { priority: "event" } : undefined);
    }
  };
  Editor.prototype.spec = function (n) { return n && n.type === "call" ? this.reg[n.fn] || null : null; };
  Editor.prototype.node = function (id) {
    for (var i = 0; i < this.g.nodes.length; i++) if (this.g.nodes[i].id === id) return this.g.nodes[i];
    return null;
  };
  Editor.prototype.implicit = function () {
    var out = {};
    var st = this.status.nodes || {};
    Object.keys(st).forEach(function (id) { out[id] = arr(st[id].implicit); });
    return out;
  };
  Editor.prototype.consumers = function () { return consumersOf(this.g, this.implicit()); };

  /* ---- Building the interface -------------------------------------------------- */

  Editor.prototype.build = function () {
    var self = this;
    var root = this.root;
    this.top = $(".bk-top", root);
    this.palette = $(".bk-palette", root);
    this.stage = $(".bk-stage", root);
    this.panel = $(".bk-panel", root);
    var boot = $(".bk-boot", this.stage);
    if (boot) boot.remove();

    // canvas: a world (panned and zoomed) holding the wires and the blocks
    this.world = h("div", { class: "bk-world", html: '<svg class="bk-wires" aria-hidden="true"></svg>' });
    this.svg = this.world.firstChild;
    this.stage.appendChild(this.world);
    this.els = {};
    this.stage.appendChild(h("div", { class: "bk-zoom", role: "group", "aria-label": "Zoom" }, [
      h("button", { type: "button", title: "Zoom out", "aria-label": "Zoom out", html: icon("minus"), onclick: function () { self.zoomBy(1 / 1.2); } }),
      h("button", { type: "button", class: "bk-zoom-level", title: "Actual size", "aria-label": "Actual size", text: "100%", onclick: function () { self.zoomTo(1); } }),
      h("button", { type: "button", title: "Zoom in", "aria-label": "Zoom in", html: icon("plus"), onclick: function () { self.zoomBy(1.2); } }),
      h("button", { type: "button", title: "Fit the blocks in view", "aria-label": "Fit the blocks in view", html: icon("fit"), onclick: function () { self.fit(true); } }),
      h("button", { type: "button", title: "Mouse and keys", "aria-label": "Mouse and keys", html: icon("help"), onclick: function () { self.openHelp(); } })
    ]));
    this.stage.appendChild(h("div", { class: "bk-stage-tip" }, [this.tipButton("canvas", "the canvas")]));
    this.emptyEl = h("div", { class: "bk-empty-state", hidden: true });
    this.stage.appendChild(this.emptyEl);
    this.hintEl = h("div", { class: "bk-hint-bar", hidden: true });
    this.stage.appendChild(this.hintEl);

    var layout = this.root.closest(".bslib-sidebar-layout");
    if (layout) layout.classList.add("bk-host-layout");
    var pane = this.root.closest(".tab-pane");
    if (pane) pane.classList.add("bk-host-pane");
    this.buildTop();
    this.buildPalette();
    this.buildPanel();
    this.bindStage();
    this.bindKeys();
    this.applyView();
  };

  Editor.prototype.buildTop = function () {
    var self = this;
    var btn = function (name, title, fn, cls) {
      return h("button", { type: "button", class: "bk-tbtn" + (cls ? " " + cls : ""), title: title, "aria-label": title, html: icon(name), onclick: fn });
    };
    this.titleEl = h("div", { class: "bk-title" });
    this.statusEl = h("span", { class: "bk-run", role: "status", "aria-live": "polite" });
    this.undoBtn = btn("undo", "Undo (" + (isMac ? "\u2318" : "Ctrl+") + "Z)", function () { self.undo(); });
    this.redoBtn = btn("redo", "Redo (" + (isMac ? "\u21e7\u2318" : "Ctrl+Shift+") + "Z)", function () { self.redo(); });
    this.top.appendChild(h("div", { class: "bk-top-left" }, [
      btn("blocks", "Show / hide the blocks and templates", function () { self.root.classList.toggle("bk-no-palette"); self.afterResize(); }, "bk-tbtn-palette"),
      this.titleEl
    ]));
    this.top.appendChild(h("div", { class: "bk-top-mid" }, [
      this.undoBtn, this.redoBtn,
      h("span", { class: "bk-sep" }),
      btn("arrange", "Tidy up: arrange the blocks", function () { self.arrange(true, true); self.fit(true); }),
      h("span", { class: "bk-sep" }),
      this.statusEl,
      this.tipButton("toolbar", "the toolbar")
    ]));
    this.titleEl.addEventListener("click", function (e) { if (e.target.closest(".bk-title-text")) self.editTitle(); });
    this.top.appendChild(h("div", { class: "bk-top-right" }, [
      btn("more", "More: paste R code, the code in R or SAS, export, import, help", function (e) {
        var r = e.currentTarget.getBoundingClientRect();
        self.openMoreMenu(r.right - 240, r.bottom + 6);
      }),
      h("button", { type: "button", class: "bk-btn", html: icon("template") + "<span>Templates</span>", onclick: function () { self.showPalette("templates"); } }),
      h("button", { type: "button", class: "bk-btn", html: icon("file") + "<span>New</span>", title: "Start an empty canvas", onclick: function () { self.newCanvas(); } }),
      h("button", { type: "button", class: "bk-btn bk-btn-primary", html: icon("code") + "<span>Show R code</span>", onclick: function () { self.input("show_code", Date.now(), true); } }),
      btn("panel", "Show / hide the result panel", function () { self.root.classList.toggle("bk-no-panel"); self.afterResize(); }, "bk-tbtn-panel"),
      btn("expand", "Full screen (Esc to leave)", function () { self.toggleFull(); }, "bk-tbtn-full")
    ]));
  };

  // The canvas over the whole window
  Editor.prototype.toggleFull = function (on) {
    var full = on === undefined ? !this.root.classList.contains("bk-full") : on;
    this.root.classList.toggle("bk-full", full);
    doc.documentElement.classList.toggle("bk-has-full", full);
    var b = $(".bk-tbtn-full", this.top);
    if (b) { b.innerHTML = icon(full ? "shrink" : "expand"); b.title = full ? "Leave full screen (Esc)" : "Full screen (Esc to leave)"; }
    if (!full) this.fitHeight();
    this.afterResize();
  };

  Editor.prototype.buildPalette = function () {
    var self = this;
    var tabs = h("div", { class: "bk-pal-tabs", role: "tablist" });
    this.palTab = "blocks";
    ["blocks", "templates"].forEach(function (t) {
      tabs.appendChild(h("button", {
        type: "button", role: "tab", class: "bk-pal-tab", "data-tab": t,
        html: icon(t === "blocks" ? "blocks" : "template") + "<span>" + (t === "blocks" ? "Blocks" : "Templates") + "</span>",
        onclick: function () { self.showPalette(t); }
      }));
    });
    this.palSearch = h("input", { type: "search", class: "bk-pal-search", placeholder: "Search", "aria-label": "Search blocks and templates", autocomplete: "off", spellcheck: "false" });
    this.palSearch.addEventListener("input", function () { self.filterPalette(); });
    this.palList = h("div", { class: "bk-pal-list" });
    tabs.appendChild(this.tipButton("palette", "the palette"));
    this.palette.appendChild(tabs);
    this.palette.appendChild(h("div", { class: "bk-pal-search-wrap", html: icon("search") }, [this.palSearch]));
    this.palette.appendChild(this.palList);
    this.palette.appendChild(h("div", { class: "bk-pal-foot", text: "Drag a block onto the canvas, or click it to add it below the selected block." }));
    this.fillPalette();
  };

  Editor.prototype.showPalette = function (tab) {
    this.root.classList.remove("bk-no-palette");
    this.palTab = tab;
    this.palSearch.value = "";
    this.fillPalette();
    this.afterResize();
    if (tab === "templates") this.palSearch.focus();
  };

  Editor.prototype.fillPalette = function () {
    var self = this;
    var list = this.palList;
    list.innerHTML = "";
    $$(".bk-pal-tab", this.palette).forEach(function (b) {
      var on = b.getAttribute("data-tab") === self.palTab;
      b.classList.toggle("active", on);
      b.setAttribute("aria-selected", on ? "true" : "false");
    });
    var section = function (title, sub, cat) {
      return h("div", { class: "bk-pal-sec", "data-cat": cat || "" }, [
        h("div", { class: "bk-pal-sec-head" }, [h("span", { text: title }), sub ? h("span", { class: "bk-pal-sec-sub", text: sub }) : null])
      ]);
    };
    if (this.palTab === "templates") {
      var featured = arr(this.cfg.featured);
      var byTopic = {};
      this.templates.forEach(function (t) { (byTopic[t.topic] = byTopic[t.topic] || []).push(t); });
      var item = function (t) {
        return h("button", {
          type: "button", class: "bk-pal-item bk-pal-template", "data-template": t.id, title: t.id + ": " + t.title,
          "data-search": (t.id + " " + t.title + " " + t.topic).toLowerCase()
        }, [
          h("span", { class: "bk-pal-ico bk-ttype-" + String(t.type).toLowerCase(), html: icon(t.type) }),
          h("span", { class: "bk-pal-text" }, [h("span", { class: "bk-pal-id", text: t.id }), h("span", { class: "bk-pal-label", text: t.title })])
        ]);
      };
      if (featured.length) {
        var fs = section("Start here", "", "featured");
        featured.forEach(function (id) {
          var t = self.templates.filter(function (x) { return x.id === id; })[0];
          if (t) fs.appendChild(item(t));
        });
        list.appendChild(fs);
      }
      arr(this.cfg.topics).forEach(function (topic) {
        var items = byTopic[topic] || [];
        if (!items.length) return;
        var s = section(topic, String(items.length));
        items.forEach(function (t) { s.appendChild(item(t)); });
        list.appendChild(s);
      });
      return;
    }
    // blocks
    var blocks = {};
    Object.keys(this.reg).forEach(function (fn) {
      var b = self.reg[fn];
      if (b.palette) (blocks[b.cat] = blocks[b.cat] || []).push(b);
    });
    this.cats.forEach(function (c) {
      var s = section(c.label, "", c.id);
      var add = function (attrs, label, sub) {
        s.appendChild(h("div", Object.assign({
          class: "bk-pal-item bk-cat-" + c.id, tabindex: "0", role: "button",
          "data-search": (label + " " + (sub || "") + " " + (attrs["data-fn"] || attrs["data-dataset"] || "")).toLowerCase()
        }, attrs), [
          h("span", { class: "bk-pal-ico", html: icon(c.id) }),
          h("span", { class: "bk-pal-text" }, [h("span", { class: "bk-pal-label", text: label }), sub ? h("span", { class: "bk-pal-sub", text: sub }) : null])
        ]));
      };
      if (c.id === "data") {
        var ds = self.datasets.length ? self.datasets : Object.keys(self.dsLabels).slice(0, 12).map(function (n) { return { name: n }; });
        ds.forEach(function (d) {
          add({ "data-dataset": d.name, title: (self.dsLabels[d.name] || d.name) + (d.rows ? " \u00b7 " + fmtInt(d.rows) + " rows" : "") },
            d.name, (self.dsLabels[d.name] || "Dataset") + (d.rows ? " \u00b7 " + fmtInt(d.rows) + " rows" : ""));
        });
      }
      (blocks[c.id] || []).forEach(function (b) { add({ "data-fn": b.fn, title: b.help }, b.label, b.help); });
      if (c.id === "code") {
        add({ "data-code": "expr", title: "A value computed with any R expression" }, "R expression", "Any R expression, e.g. a vector or a function call");
        add({ "data-code": "code", title: "Any R statement" }, "R statement", "Code run as is, e.g. if (...) x <- y");
      }
      if (s.children.length > 1) list.appendChild(s);
    });
    this.filterPalette();
  };

  Editor.prototype.filterPalette = function () {
    var q = this.palSearch.value.trim().toLowerCase();
    $$(".bk-pal-sec", this.palList).forEach(function (sec) {
      var any = false;
      $$(".bk-pal-item", sec).forEach(function (it) {
        var ok = !q || q.split(/\s+/).every(function (w) { return (it.getAttribute("data-search") || "").indexOf(w) >= 0; });
        it.hidden = !ok;
        any = any || ok;
      });
      sec.hidden = !any;
    });
  };

  Editor.prototype.buildPanel = function () {
    var self = this;
    var head = $(".bk-panel-head", this.panel);
    this.panes = {};
    $$(".bk-pane", this.panel).forEach(function (p) { self.panes[p.getAttribute("data-pane")] = p; });
    var tabs = h("div", { class: "bk-panel-tabs", role: "tablist" });
    [["result", "Result"], ["block", "Block"], ["code", "Code"]].forEach(function (t) {
      tabs.appendChild(h("button", {
        type: "button", role: "tab", class: "bk-panel-tab", "data-pane": t[0], text: t[1],
        onclick: function () { self.showPane(t[0]); }
      }));
    });
    head.appendChild(tabs);
    head.appendChild(this.tipButton("pane", "this panel"));
    this.codeEl = $(".bk-code code", this.panel);
    this.codePre = $(".bk-code", this.panel);
    this.lang = "r";
    this.sas = { rev: -1, code: "" };
    if (this.panes.code) this.buildCodePane();
    this.inspector = $(".bk-inspector", this.panel);
    this.resultHead = h("div", { class: "bk-result-head" });
    if (this.panes.result) this.panes.result.insertBefore(this.resultHead, this.panes.result.firstChild);
    // resize the panel by dragging its left edge
    var grip = h("div", { class: "bk-panel-grip", title: "Drag to resize", "aria-hidden": "true" });
    this.panel.appendChild(grip);
    grip.addEventListener("pointerdown", function (e) {
      e.preventDefault();
      var startX = e.clientX;
      var startW = self.panel.offsetWidth;
      grip.setPointerCapture(e.pointerId);
      var move = function (ev) {
        var w = clamp(startW + (startX - ev.clientX), 300, Math.max(320, self.root.offsetWidth - 420));
        self.root.style.setProperty("--bk-panel-w", w + "px");
      };
      var up = function () {
        grip.removeEventListener("pointermove", move);
        grip.removeEventListener("pointerup", up);
        try { window.localStorage.setItem("blockr:panel-w", self.panel.offsetWidth); } catch (err) { /* ignore */ }
        self.afterResize();
      };
      grip.addEventListener("pointermove", move);
      grip.addEventListener("pointerup", up);
    });
    try {
      var w = parseInt(window.localStorage.getItem("blockr:panel-w"), 10);
      if (w > 200) this.root.style.setProperty("--bk-panel-w", w + "px");
    } catch (err) { /* ignore */ }
    this.showPane("result");
  };

  Editor.prototype.showPane = function (name) {
    var self = this;
    this.pane = name;
    Object.keys(this.panes).forEach(function (k) { self.panes[k].hidden = k !== name; });
    $$(".bk-panel-tab", this.panel).forEach(function (b) {
      var on = b.getAttribute("data-pane") === name;
      b.classList.toggle("active", on);
      b.setAttribute("aria-selected", on ? "true" : "false");
    });
    this.root.classList.remove("bk-no-panel");
    // Shiny outputs in a pane that was hidden: let them know they are visible
    if (window.jQuery) {
      window.jQuery(this.panes[name]).trigger("shown");
      window.jQuery(window).trigger("resize");
    }
    if (name === "block") this.renderInspector();
    if (this.emit) this.emit("pane", name);
  };

  // The canvas is at least `height` of the window (option blockr.canvas_height,
  // 85% by default), and fills the window below its top edge when that is more
  Editor.prototype.fitHeight = function () {
    if (this.root.classList.contains("bk-full")) return;
    var r = this.root.getBoundingClientRect();
    if (!r.width) return;
    var share = typeof this.cfg.height === "number" ? clamp(this.cfg.height, 0.4, 1) : 0.85;
    var below = window.innerHeight - (r.top + window.scrollY) - 14;
    var hgt = Math.round(Math.max(480, share * window.innerHeight, below));
    if (Math.abs(this.root.offsetHeight - hgt) > 2) this.root.style.height = hgt + "px";
  };

  // Controls of the page that float over the canvas's edges (e.g. teal's
  // buttons to reopen its sidebars, once folded): the canvas moves aside by
  // just that much, whatever the page's markup.
  Editor.prototype.avoidOverlays = function () {
    var root = this.root;
    root.style.marginLeft = "";
    root.style.marginTop = "";
    if (root.classList.contains("bk-full") || !isShown(root) || !doc.elementsFromPoint) return;
    var r = root.getBoundingClientRect();
    var ours = ".bk-pop, .bk-menu, .bk-ghost, .bk-toast, .tn-overlay, .tn-toasts, .tn-tools, .modal, .dropdown-menu";
    var seen = [];
    var left = [];
    var top = [];
    var probe = function (x, y, into) {
      var stack = doc.elementsFromPoint(x, y);
      for (var i = 0; i < stack.length; i++) {
        var el = stack[i];
        if (el === root || root.contains(el) || el.contains(root)) return; // the rest is under the canvas
        if (el.closest(ours)) return;
        var b = el.closest("button, a, [role='button'], .collapse-toggle") || el;
        if (seen.indexOf(b) >= 0) continue;
        seen.push(b);
        var br = b.getBoundingClientRect();
        if (!br.width || br.width > 160 || br.height > 240) continue; // a control, not a container
        into.push(br);
      }
    };
    for (var y = r.top + 3; y < Math.min(r.bottom, r.top + 360); y += 9) {
      for (var x = r.left + 2; x < r.left + 60; x += 9) probe(x, y, left);
    }
    for (var x2 = r.left + 60; x2 < r.right - 2; x2 += 24) {
      for (var y2 = r.top + 2; y2 < r.top + 40; y2 += 12) probe(x2, y2, top);
    }
    if (left.length) {
      var right = Math.max.apply(null, left.map(function (b) { return b.right; }));
      root.style.marginLeft = Math.ceil(right - r.left + 8) + "px";
    }
    if (top.length) {
      var bottom = Math.max.apply(null, top.map(function (b) { return b.bottom; }));
      root.style.marginTop = Math.ceil(bottom - r.top + 8) + "px";
    }
    if (left.length || top.length) this.afterResize();
  };

  // First time the canvas is shown: fold teal's filter panel, the canvas
  // needs the room (its own Filter blocks do the filtering); the panel stays
  // one click (or \) away.
  Editor.prototype.foldFilterPanel = function () {
    if (this.folded) return;
    this.folded = true;
    var layout = this.root.closest(".bslib-sidebar-layout");
    if (!layout || layout.classList.contains("sidebar-collapsed")) return;
    var toggle = null;
    for (var i = 0; i < layout.children.length; i++) {
      if (layout.children[i].classList.contains("collapse-toggle")) toggle = layout.children[i];
    }
    if (toggle) toggle.click();
  };

  Editor.prototype.afterResize = function () {
    if (window.jQuery) window.jQuery(window).trigger("resize");
    this.drawWires();
  };

  /* ---- Code, export, import, help, title ------------------------------------------------------ */

  // The Code pane: the canvas as R or SAS code; the R code can be edited and
  // drawn again as blocks
  Editor.prototype.buildCodePane = function () {
    var self = this;
    var seg = h("div", { class: "bk-seg", role: "group", "aria-label": "Language of the code" });
    [["r", "R"], ["sas", "SAS"]].forEach(function (l) {
      seg.appendChild(h("button", {
        type: "button", class: "bk-seg-btn", "data-lang": l[0], text: l[1], "aria-pressed": l[0] === "r" ? "true" : "false",
        title: l[0] === "r" ? "The R code of the canvas" : "The canvas translated to SAS: DATA steps, PROC SQL, MEANS, REPORT, SGPLOT",
        onclick: function () { self.setLang(l[0]); }
      }));
    });
    this.codeHint = h("span", { class: "bk-hint" });
    this.editBtn = h("button", {
      type: "button", class: "bk-btn bk-btn-sm", html: icon("edit") + "<span>Edit</span>",
      title: "Change the code, or paste your own: it becomes blocks",
      onclick: function () { self.editCode(); }
    });
    this.panes.code.insertBefore(h("div", { class: "bk-code-head" }, [
      seg,
      this.codeHint,
      this.editBtn,
      h("button", { type: "button", class: "bk-btn bk-btn-sm", html: icon("copy") + "<span>Copy</span>", onclick: function () { self.copyCode(); } }),
      h("button", { type: "button", class: "bk-btn bk-btn-sm", html: icon("file") + "<span>Download</span>", onclick: function () { self.downloadCode(); } })
    ]), this.panes.code.firstChild);
    this.renderCode();
  };
  Editor.prototype.setLang = function (lang) {
    this.lang = lang === "sas" ? "sas" : "r";
    if (this.lang === "sas") this.stopEditCode();
    this.input("lang", this.lang);
    this.renderCode();
    this.emit("lang", this.lang);
  };
  Editor.prototype.renderCode = function () {
    if (!this.codeEl) return;
    var sas = this.lang === "sas";
    $$(".bk-seg-btn", this.panes.code).forEach(function (b) {
      var on = b.getAttribute("data-lang") === (sas ? "sas" : "r");
      b.classList.toggle("active", on);
      b.setAttribute("aria-pressed", on ? "true" : "false");
    });
    if (this.codeHint) {
      this.codeHint.textContent = sas
        ? "A translation for SAS users: check it against the R results; TODO marks what has no SAS equivalent."
        : "The code of the canvas, updated as you go. Edit it, or paste your own: it becomes blocks.";
    }
    if (this.editBtn) this.editBtn.hidden = sas;
    this.panes.code.classList.toggle("bk-code-sas", sas);
    var stale = sas && this.sas.rev !== this.status.rev;
    this.codeEl.textContent = sas ? (this.sas.code || "/* Writing the SAS code\u2026 */") : (this.status.code || "");
    if (this.codePre) this.codePre.classList.toggle("bk-code-stale", !!stale && !!this.sas.code);
  };
  Editor.prototype.onSas = function (msg) {
    this.sas = { rev: msg.rev, code: msg.code || "" };
    this.renderCode();
    this.emit("sas");
  };

  // Edit the R code in place; "Draw the blocks" sends it to R, which sends
  // back the graph (or what is wrong with the code)
  Editor.prototype.editCode = function () {
    var self = this;
    if (this.codeEditor) { this.codeEditor.area.focus(); return; }
    var area = h("textarea", { class: "bk-code-edit", spellcheck: "false", autocomplete: "off", "aria-label": "R code of the canvas" });
    area.value = this.status.code || "";
    var err = h("div", { class: "bk-code-error", role: "alert", hidden: true });
    var draw = h("button", { type: "button", class: "bk-btn bk-btn-primary bk-btn-sm", html: icon("blocks") + "<span>Draw the blocks</span>", onclick: function () { self.submitCode(area.value, "replace", err); } });
    var box = h("div", { class: "bk-code-editor" }, [
      area, err,
      h("div", { class: "bk-code-actions" }, [
        h("span", { class: "bk-hint", text: (isMac ? "\u2318" : "Ctrl+") + "Enter: draw \u00b7 Esc: cancel" }),
        h("button", { type: "button", class: "bk-btn bk-btn-sm", text: "Cancel", onclick: function () { self.stopEditCode(); } }),
        draw
      ])
    ]);
    area.addEventListener("keydown", function (e) {
      if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) { e.preventDefault(); self.submitCode(area.value, "replace", err); }
      if (e.key === "Escape") { e.preventDefault(); e.stopPropagation(); self.stopEditCode(); }
    });
    this.codeEditor = { box: box, area: area, err: err };
    this.codePre.hidden = true;
    this.codePre.parentNode.insertBefore(box, this.codePre);
    this.panes.code.classList.add("bk-code-editing");
    area.focus();
  };
  Editor.prototype.stopEditCode = function () {
    if (!this.codeEditor) return;
    this.codeEditor.box.remove();
    this.codeEditor = null;
    if (this.codePre) this.codePre.hidden = false;
    this.panes.code.classList.remove("bk-code-editing");
  };
  // send R code to be drawn: mode "replace" (the canvas) or "add" (to it)
  Editor.prototype.submitCode = function (code, mode, errEl) {
    if (!String(code || "").trim()) { this.codeError("There is no R code to draw here.", errEl); return; }
    this.codeErrEl = errEl || null;
    if (errEl) errEl.hidden = true;
    this.pendingCode = { mode: mode };
    this.input("code", JSON.stringify({ code: code, title: mode === "replace" ? (this.g.title || null) : null, mode: mode }), true);
  };
  Editor.prototype.codeError = function (message, errEl) {
    var el = errEl || this.codeErrEl;
    if (el && doc.contains(el)) { el.textContent = message; el.hidden = false; }
    else this.toast(message);
  };
  Editor.prototype.onCodeError = function (msg) {
    this.pendingCode = null;
    this.codeError(msg.message || "This code could not be read.");
    this.emit("code-error", msg.message);
  };

  // "Paste R code": a dialog to draw code as blocks, replacing the canvas or
  // added to it
  Editor.prototype.openPaste = function (text) {
    var self = this;
    this.closePaste();
    var empty = !this.g.nodes.length;
    var area = h("textarea", {
      class: "bk-code-edit", spellcheck: "false", autocomplete: "off", "aria-label": "R code",
      placeholder: "adsl <- ADSL %>% filter(SAFFL == \"Y\")\nadae <- ADAE %>% filter(AESER == \"Y\")\n\nresult <- adae %>% count(AEBODSYS, sort = TRUE)"
    });
    area.value = text || "";
    var err = h("div", { class: "bk-code-error", role: "alert", hidden: true });
    var mode = empty ? "replace" : "add";
    var choice = h("div", { class: "bk-seg", role: "radiogroup", "aria-label": "Where the blocks go" });
    var modes = [["add", "Add to the canvas"], ["replace", "Replace the canvas"]];
    var paint = function () {
      $$(".bk-seg-btn", choice).forEach(function (b) {
        var on = b.getAttribute("data-mode") === mode;
        b.classList.toggle("active", on);
        b.setAttribute("aria-checked", on ? "true" : "false");
      });
    };
    modes.forEach(function (m) {
      choice.appendChild(h("button", { type: "button", role: "radio", class: "bk-seg-btn", "data-mode": m[0], text: m[1], disabled: empty && m[0] === "add" ? true : null, onclick: function () { mode = m[0]; paint(); } }));
    });
    paint();
    var go = function () { self.submitCode(area.value, mode, err); };
    var box = h("div", { class: "bk-dialog", role: "dialog", "aria-modal": "true", "aria-label": "Paste R code" }, [
      h("div", { class: "bk-dialog-card" }, [
        h("div", { class: "bk-help-head" }, [
          h("strong", { html: icon("code") + "<span>Paste R code: it becomes blocks</span>" }),
          h("button", { type: "button", class: "bk-x", "aria-label": "Close", html: icon("close", 14), onclick: function () { self.closePaste(); } })
        ]),
        h("p", { class: "bk-hint", text: "dplyr steps (filter, mutate, count, ...), rtables layouts, ggplot2 layers and any other R code. The datasets are ADSL, ADAE, ... as in the app; each pipe chain becomes a stack of blocks." }),
        area,
        err,
        h("div", { class: "bk-code-actions" }, [
          choice,
          h("span", { class: "bk-grow" }),
          h("button", { type: "button", class: "bk-btn bk-btn-sm", text: "Cancel", onclick: function () { self.closePaste(); } }),
          h("button", { type: "button", class: "bk-btn bk-btn-primary bk-btn-sm", html: icon("blocks") + "<span>Draw the blocks</span>", onclick: go })
        ])
      ])
    ]);
    box.addEventListener("keydown", function (e) {
      if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) { e.preventDefault(); go(); }
      if (e.key === "Escape") { e.preventDefault(); e.stopPropagation(); self.closePaste(); }
    });
    box.addEventListener("pointerdown", function (e) { if (e.target === box) self.closePaste(); });
    this.pasteBox = box;
    this.root.appendChild(box);
    area.focus();
    if (text) area.setSelectionRange(0, 0);
  };
  Editor.prototype.closePaste = function () {
    if (this.pasteBox) { this.pasteBox.remove(); this.pasteBox = null; }
  };

  Editor.prototype.codeText = function () {
    if (this.lang === "sas") return this.sas.code || "";
    var title = this.g.title || "Untitled canvas";
    return "# BlockR canvas: " + (this.g.template ? this.g.template + " \u00b7 " : "") + title + "\n" +
      "# The datasets (ADSL, ADAE, ...) come from the app: \"Show R code\" gives the code that loads them.\n\n" +
      (this.status.code || "");
  };
  Editor.prototype.copyCode = function () {
    var self = this;
    var text = this.codeText();
    var done = function () { self.toast(self.lang === "sas" ? "SAS code copied" : "Code copied"); };
    if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(text).then(done, function () { self.toast("Copy is not allowed here"); });
    else self.toast("Copy is not allowed here");
  };
  function download(name, text, type) {
    var url = URL.createObjectURL(new Blob([text], { type: type }));
    var a = h("a", { href: url, download: name });
    doc.body.appendChild(a);
    a.click();
    a.remove();
    window.setTimeout(function () { URL.revokeObjectURL(url); }, 1000);
  }
  Editor.prototype.fileName = function (ext) {
    var base = (this.g.template ? this.g.template + "-" : "") + (this.g.title || "canvas");
    return base.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "").slice(0, 60) + ext;
  };
  Editor.prototype.downloadCode = function () { download(this.fileName(this.lang === "sas" ? ".sas" : ".R"), this.codeText(), "text/plain"); };
  Editor.prototype.exportCanvas = function () {
    download(this.fileName(".blockr.json"), JSON.stringify({ blockr: 1, graph: this.g, view: this.view }, null, 1), "application/json");
  };
  Editor.prototype.importCanvas = function () {
    var self = this;
    var input = h("input", { type: "file", accept: ".json,application/json", hidden: true });
    doc.body.appendChild(input);
    input.addEventListener("change", function () {
      var f = input.files && input.files[0];
      input.remove();
      if (!f) return;
      var reader = new FileReader();
      reader.onload = function () {
        var data = null;
        try { data = JSON.parse(reader.result); } catch (e) { data = null; }
        var g = data && (data.graph || data);
        if (!g || !Array.isArray(g.nodes)) { self.toast("This file is not a BlockR canvas."); return; }
        self.remember();
        self.g = { version: 1, packages: arr(g.packages), nodes: g.nodes, result: g.result || null, template: g.template || null, title: g.title || null };
        self.baseJson = null;
        self.select(null);
        self.changed();
        self.fit(true);
        self.toast("Canvas imported: " + (g.title || f.name));
      };
      reader.readAsText(f);
    });
    input.click();
  };
  Editor.prototype.openMoreMenu = function (cx, cy) {
    var self = this;
    this.openMenu([
      { label: "Paste R code\u2026", sub: "Your code becomes blocks (" + (isMac ? "\u2318" : "Ctrl+") + "V on the canvas)", icon: "code", run: function () { self.openPaste(""); } },
      { label: "Edit the R code", sub: "Change the code: the blocks follow", icon: "edit", run: function () { self.showPane("code"); self.setLang("r"); self.editCode(); } },
      "-",
      { label: "Copy the R code", icon: "copy", run: function () { self.setLang("r"); self.copyCode(); } },
      { label: "Download the R code", sub: "A .R script of the canvas", icon: "file", run: function () { self.setLang("r"); self.downloadCode(); } },
      { label: "Show the SAS code", sub: "The canvas translated to SAS", icon: "code", run: function () { self.showPane("code"); self.setLang("sas"); } },
      "-",
      { label: "Export the canvas", sub: "A file you can share or open later", icon: "blocks", run: function () { self.exportCanvas(); } },
      { label: "Import a canvas\u2026", sub: "Open an exported canvas", icon: "template", run: function () { self.importCanvas(); } },
      "-",
      { label: "Help: mouse and keys", icon: "help", run: function () { self.openHelp(); } },
      { label: tipsOn() ? "Hide the \u201cWhat is this?\u201d buttons" : "Show the \u201cWhat is this?\u201d buttons", sub: "Also in Appearance (the sliders in the header)", icon: "help", run: function () { setTips(!tipsOn()); } }
    ], cx, cy);
  };
  Editor.prototype.openHelp = function () {
    var k = isMac ? "\u2318" : "Ctrl+";
    var rows = [
      ["Add a block", "Drag it from the left, or click it: it goes below the selected block"],
      ["Add the next step", "The + under a stack suggests what to do with its result"],
      ["Move a block", "Drag it; the blocks around it close the gap"],
      ["Move a block and those below it", "Shift + drag"],
      ["Move a stack", "Drag its first block"],
      ["Connect a result", "Drag from the round port under a stack to a socket"],
      ["Change a block", "Click it: its settings open on the right"],
      ["Skip a block", "Right-click it \u2192 Bypass"],
      ["Delete a block", "Select it, then Delete"],
      ["Undo / redo", k + "Z / " + (isMac ? "\u21e7\u2318Z" : "Ctrl+Shift+Z")],
      ["Pan / zoom", "Drag the background or scroll / " + k + "scroll"]
    ];
    var self = this;
    var box = h("div", { class: "bk-help", role: "dialog", "aria-label": "Mouse and keys" }, [
      h("div", { class: "bk-help-head" }, [
        h("strong", { text: "Mouse and keys" }),
        h("button", { type: "button", class: "bk-x", "aria-label": "Close", html: icon("close", 14), onclick: function () { box.remove(); } })
      ]),
      h("dl", {}, [].concat.apply([], rows.map(function (r) { return [h("dt", { text: r[0] }), h("dd", { text: r[1] })]; })))
    ]);
    $$(".bk-help", this.stage).forEach(function (x) { x.remove(); });
    this.stage.appendChild(box);
    void self;
  };
  Editor.prototype.editTitle = function () {
    var self = this;
    var span = $(".bk-title-text", this.titleEl);
    if (!span) return;
    var input = h("input", { type: "text", class: "bk-title-input", value: this.g.title || "", placeholder: "Name this canvas", "aria-label": "Name of the canvas" });
    span.replaceWith(input);
    input.focus();
    input.select();
    var done = false;
    var commit = function (keep) {
      if (done) return;
      done = true;
      var v = input.value.trim();
      if (keep && v !== (self.g.title || "")) {
        self.remember();
        self.g.title = v || null;
        self.save();
        self.sendGraph();
      }
      self.renderTop();
    };
    input.addEventListener("keydown", function (e) {
      if (e.key === "Enter") { e.preventDefault(); commit(true); }
      if (e.key === "Escape") { e.preventDefault(); commit(false); }
    });
    input.addEventListener("blur", function () { commit(true); });
  };

  /* ---- "What is this?": explanations of each part and of each block ----------------------------
     Small "?" buttons; hidden with the Tips setting (Appearance panel, or
     the ··· menu), stored with the appearance: <html data-tn-tips="off">. */

  function tipsOn() { return doc.documentElement.getAttribute("data-tn-tips") !== "off"; }
  function setTips(on) {
    if (window.blockr && typeof window.blockr.setting === "function") {
      window.blockr.setting("tips", on ? "on" : "off");
    } else {
      if (on) doc.documentElement.removeAttribute("data-tn-tips"); else doc.documentElement.setAttribute("data-tn-tips", "off");
      try {
        var a = JSON.parse(window.localStorage.getItem("blockr:appearance") || "{}") || {};
        if (on) delete a.tips; else a.tips = "off";
        window.localStorage.setItem("blockr:appearance", JSON.stringify(a));
      } catch (e) { /* ignore */ }
    }
  }

  Editor.prototype.tipButton = function (what, label) {
    var self = this;
    return h("button", {
      type: "button", class: "bk-tip", "data-tip": what, title: "What is this?", "aria-label": "What is " + label + "?", text: "?",
      onclick: function (e) { e.stopPropagation(); self.openPartTip(what, e.currentTarget); }
    });
  };

  var PART_TIPS = {
    toolbar: ["The toolbar", [
      "<b>Title</b>: click it to rename the canvas. A template shows its ID (e.g. AET02) and <i>edited</i> once you changed it.",
      "<b>Undo / redo</b> every change (Ctrl/Cmd+Z). <b>Tidy up</b> arranges the blocks.",
      "<b>Status</b>: <i>Running\u2026</i> while R computes, <i>Up to date</i> (and how long it took), or how many blocks need attention.",
      "<b>Templates</b> opens the list of TLG templates; <b>New</b> empties the canvas; <b>Show R code</b> gives the full script, data loading included.",
      "<b>\u00b7\u00b7\u00b7</b>: paste R code as blocks, edit, copy or download the code (R or SAS), export or import a canvas, help. The last buttons hide the result panel and show the canvas full screen."
    ]],
    canvas: ["The canvas", [
      "Your analysis, as <b>blocks</b>. A <b>stack</b> of blocks is a pipeline read from top to bottom: each block works on the result of the block above it.",
      "The footer of a block says what it produces: rows and columns (and <b>\u2212</b> how many rows it removed), a table, a plot.",
      "<b>Wires</b> bring the result of a stack to a block that uses it; the number on a wire is the number of rows travelling on it. Dashed wires: a block's code mentions another block's result.",
      "The <b>\u2605 Result</b> block is the one shown in the Result panel.",
      "The <b>+</b> under a stack suggests what to do next. Drag a block to move it; right-click it for more. Drag the background to move around, Ctrl/Cmd + scroll to zoom.",
      "Every block has a <b>?</b> that explains what it does here."
    ]],
    palette: [null, null],
    pane: [null, null]
  };
  Editor.prototype.partTip = function (what) {
    if (what === "palette") {
      return this.palTab === "templates"
        ? ["Templates", [
          "Every output of the TLG Catalog, as a starting point. Click one: its R code becomes blocks on the canvas, ready to change.",
          "Search by ID (<i>aet02</i>), title or topic. Your current canvas comes back with Undo."
        ]]
        : ["Blocks", [
          "Everything you can add to the canvas, by kind: <b>Data</b> (where the rows come from), <b>Prepare</b> (filter, derive, order, count), <b>Table layout</b> and <b>Statistics</b> (the columns, rows and figures of a table), <b>Output</b> (build a table, a listing), <b>Plot layers</b>, <b>Values</b> and <b>R code</b>.",
          "<b>Drag</b> a block onto the canvas: between or below the blocks of a stack to insert it, or anywhere to start a new stack.",
          "Or <b>click</b> it: it goes below the selected block.",
          "The search field finds a block by name or by what it does."
        ]];
    }
    if (what === "pane") {
      if (this.pane === "block") {
        return ["Block", [
          "The settings of the selected block, in plain words: conditions as <i>column \u00b7 is \u00b7 value</i>, columns as tags, labels, statistics. Changes apply at once.",
          "<b>More settings</b> holds the rarely used ones; anything the editors cannot show is R code.",
          "<b>Bypass</b> skips the block without deleting it; <b>Show as result</b> puts its output in the Result panel.",
          "Below: what the block produces (its rows, its table, its plot)."
        ]];
      }
      if (this.pane === "code") {
        return ["Code", [
          "The R code of the canvas, written for you as you build it: a stack of blocks is one pipe chain.",
          "<b>Edit</b> it, or paste your own R code (\u00b7\u00b7\u00b7 \u2192 Paste R code, or " + (isMac ? "\u2318" : "Ctrl+") + "V on the canvas): <b>Draw the blocks</b> turns it back into blocks.",
          "<b>SAS</b> shows the same steps as SAS code (DATA steps, PROC SQL, MEANS, REPORT, SGPLOT), for SAS users; what has no SAS equivalent is marked TODO.",
          "Copy or download it. <b>Show R code</b> (top right) gives the complete script, including the code that loads the data and the app's filters."
        ]];
      }
      return ["Result", [
        "The output of the <b>\u2605 Result</b> block: a table, a listing or a plot, computed on the app's data as you edit.",
        "By default it is the template's result, or the last table, listing or plot. To show another block's output: right-click it \u2192 <i>Show as the result</i>.",
        "Hover a table or a plot for its tools (copy, download, pin)."
      ]];
    }
    return PART_TIPS[what];
  };
  Editor.prototype.openPartTip = function (what, anchor) {
    var tip = this.partTip(what);
    if (!tip) return;
    this.openPop(anchor, tip[0], tip[1].map(function (p) { return h("p", { html: p }); }));
    this.emit("tip", what);
  };

  // What a block does, here: its help, what it works on and what it gives, who uses it
  Editor.prototype.explainBlock = function (id) {
    var self = this;
    var n = this.node(id);
    var info = this.info(n);
    var st = this.status.nodes[id] || {};
    var spec = info.spec;
    var out = [];
    var plain = function (html) { var d = doc.createElement("div"); d.innerHTML = html; return d.textContent.replace(/\s+/g, " ").trim(); };
    var what = spec ? spec.help
      : n.type === "dataset" ? "The rows of the dataset " + n.dataset + " (" + (this.dsLabels[n.dataset] || "a dataset") + "), after the app's filters."
        : n.type === "code" ? "R code run as it is. It can create variables that other blocks use."
          : n.type === "expr" ? "A value computed with R code, that other blocks can use."
            : "The R function " + n.fn + "(): its settings are R code.";
    out.push(h("p", { text: what }));
    var here = [];
    if (n.type !== "dataset") here.push(["Here", plain(this.summary(n))]);
    var src = this.inputOf(n);
    if (src && this.node(src)) {
      var sst = this.status.nodes[src] || {};
      here.push(["Works on", this.sourceLabel(src) + (sst.kind === "data" ? " (" + fmtInt(sst.rows) + " rows)" : sst.kind ? " (" + sst.kind + ")" : "")]);
    }
    this.sockets(n).forEach(function (s) {
      if (s.ref) here.push([s.label, self.sourceLabel(s.ref)]);
    });
    if (st.state) here.push(["Gives", plain(this.statusLine(n).html)]);
    var users = [];
    var cons = this.consumers()[id] || [];
    cons.forEach(function (c) {
      var u = self.node(c.id);
      if (!u) return;
      var label = self.info(u).label;
      if (c.via === "ref") {
        var sk = self.sockets(u).filter(function (s) { return s.key === "i:" + c.arg; })[0];
        label += sk ? " (" + sk.label + ")" : "";
      } else if (c.via === "implicit") label += " (in its code)";
      users.push(label);
    });
    if (users.length) here.push(["Used by", users.join(", ")]);
    if (this.status.result === id) here.push(["Result", "its output is shown in the Result panel"]);
    if (n.off) here.push(["Bypassed", "skipped: its input passes through unchanged"]);
    if (here.length) {
      out.push(h("dl", { class: "bk-pop-facts" }, [].concat.apply([], here.map(function (r) { return [h("dt", { text: r[0] }), h("dd", { text: r[1] })]; }))));
    }
    if (n.note) out.push(h("p", { class: "bk-pop-note", text: "From the template: " + n.note }));
    if (st.state === "error" || st.state === "missing") out.push(h("p", { class: "bk-pop-error", text: st.error }));
    var how = n.type === "dataset" ? "Click it to see its rows, or to choose another dataset."
      : n.type === "code" || n.type === "expr" ? "Click it to edit its R code."
        : "Click it to change its settings; right-click it to bypass, duplicate or delete it.";
    out.push(h("p", { class: "bk-pop-how", text: how }));
    return { title: info.label, sub: info.fn ? info.fn + "()" : n.type === "dataset" ? "dataset" : "R", body: out, cat: info.cat };
  };
  Editor.prototype.openBlockTip = function (id, anchor) {
    var x = this.explainBlock(id);
    this.openPop(anchor, x.title, x.body, x.sub, x.cat);
    this.emit("tip", "block:" + id);
  };

  // a small card next to `anchor`; closes on a click elsewhere or Escape
  Editor.prototype.openPop = function (anchor, title, body, sub, cat) {
    var self = this;
    this.closePop();
    var pop = h("div", { class: "bk-pop" + (cat ? " bk-cat-" + cat : ""), role: "dialog", "aria-label": title }, [
      h("div", { class: "bk-pop-head" }, [
        h("span", { class: "bk-pop-q", text: "?" }),
        h("div", { class: "bk-pop-titles" }, [h("strong", { text: title }), sub ? h("code", { text: sub }) : null]),
        h("button", { type: "button", class: "bk-x", "aria-label": "Close", html: icon("close", 14), onclick: function () { self.closePop(); } })
      ]),
      h("div", { class: "bk-pop-body" }, body),
      h("div", { class: "bk-pop-foot", html: "Hide these <b>?</b> buttons: \u00b7\u00b7\u00b7 menu or Appearance." })
    ]);
    doc.body.appendChild(pop);
    var r = anchor.getBoundingClientRect();
    var w = pop.offsetWidth;
    var hh = pop.offsetHeight;
    var left = r.right + 10 + w < window.innerWidth ? r.right + 10 : r.left - 10 - w;
    pop.style.left = clamp(left, 8, window.innerWidth - w - 8) + "px";
    pop.style.top = clamp(r.top - 8, 8, window.innerHeight - hh - 8) + "px";
    this.pop = pop;
    var away = function (ev) {
      if (!self.pop) { doc.removeEventListener("pointerdown", away, true); return; }
      if (!self.pop.contains(ev.target) && !ev.target.closest(".bk-tip")) { self.closePop(); doc.removeEventListener("pointerdown", away, true); }
    };
    doc.addEventListener("pointerdown", away, true);
  };
  Editor.prototype.closePop = function () {
    if (!this.pop) return false;
    this.pop.remove();
    this.pop = null;
    return true;
  };

  /* ---- Stacks ------------------------------------------------------------------------ */

  // A block is snapped under its input when it is the only one using it.
  Editor.prototype.computeStacks = function () {
    var g = this.g;
    var nodes = byId(g);
    var cons = consumersOf(g, null);
    var snapped = {};
    var child = {};
    g.nodes.forEach(function (n) {
      if (!n.input || !nodes[n.input]) return;
      var c = cons[n.input];
      if (c.length === 1 && c[0].id === n.id && c[0].via === "pipe") { snapped[n.id] = true; child[n.input] = n.id; }
    });
    var stacks = [];
    var stackOf = {};
    var seen = {};
    var walk = function (head) {
      var s = { head: head, members: [] };
      var cur = head;
      while (cur && !seen[cur]) {
        seen[cur] = true;
        s.members.push(cur);
        stackOf[cur] = s;
        cur = child[cur];
      }
      stacks.push(s);
    };
    g.nodes.forEach(function (n) { if (!snapped[n.id]) walk(n.id); });
    g.nodes.forEach(function (n) { if (!seen[n.id]) { delete snapped[n.id]; walk(n.id); } }); // loops
    this.stacks = stacks;
    this.stackOf = stackOf;
    this.snapped = snapped;
    this.childOf = child;
  };
  Editor.prototype.isHead = function (id) { return !this.snapped[id]; };
  Editor.prototype.stackEnd = function (id) { var s = this.stackOf[id]; return s ? s.members[s.members.length - 1] : id; };

  /* ---- Rendering ----------------------------------------------------------------------- */

  Editor.prototype.render = function () {
    var self = this;
    var g = this.g;
    this.computeStacks();
    var keep = {};
    g.nodes.forEach(function (n) {
      keep[n.id] = true;
      var el = self.els[n.id];
      if (!el) {
        el = h("div", { class: "bk-block", "data-id": n.id });
        self.els[n.id] = el;
        self.world.appendChild(el);
      }
      self.fillBlock(el, n);
    });
    Object.keys(this.els).forEach(function (id) {
      if (!keep[id]) { self.els[id].remove(); delete self.els[id]; }
    });
    g.nodes.forEach(function (n) { self.heights[n.id] = self.els[n.id].offsetHeight || 60; });
    var unplaced = this.stacks.some(function (s) { var n = self.node(s.head); return typeof n.x !== "number" || typeof n.y !== "number"; });
    if (unplaced) this.arrange(false);
    this.place();
    this.drawWires();
    this.renderEmpty();
    this.renderTop();
    if (this.pane === "block") this.renderInspector(true);
  };

  // positions: the first block of each stack has x, y; the others follow
  Editor.prototype.place = function () {
    var self = this;
    this.pos = {};
    this.stacks.forEach(function (s) {
      var head = self.node(s.head);
      var y = head.y;
      s.members.forEach(function (id) {
        self.pos[id] = { x: head.x, y: y };
        var el = self.els[id];
        el.style.transform = "translate(" + head.x + "px," + y + "px)";
        y += self.heights[id];
      });
    });
  };

  Editor.prototype.info = function (n) {
    if (n.type === "dataset") {
      return { label: n.dataset || "Dataset", cat: "data", fn: "", sub: this.dsLabels[n.dataset] || "Dataset" };
    }
    if (n.type === "code") return { label: "R code", cat: "code", fn: "", code: n.code };
    if (n.type === "expr") return { label: "R expression", cat: "code", fn: "", code: n.code };
    var spec = this.spec(n);
    if (spec) return { label: spec.label, cat: spec.cat, fn: n.fn, spec: spec };
    var fn = String(n.fn || "");
    var base = fn.replace(/^.*::/, "");
    var cat = /^(geom_|stat_|theme|labs$|scale_|facet_|coord_|guides$|ggtitle$)/.test(base) ? "plot"
      : /^split_|^add_|^append_|^basic_table$/.test(base) ? "layout"
        : /^(summarize|summarise|analyze|analyse|count_|estimate_|surv_|coxph_|test_|tabulate_|compare_)/.test(base) ? "analyze"
          : /^(g_|build_|as_listing|fit_|extract_|h_)/.test(base) ? "output"
            : n.input ? "prep" : "value";
    var label = base.replace(/_/g, " ");
    label = label.charAt(0).toUpperCase() + label.slice(1);
    return { label: label, cat: cat, fn: fn, spec: null };
  };

  // Display of one argument's value, for summaries
  Editor.prototype.showArg = function (n, spec, formal, argIdx) {
    var a = argIdx === undefined || argIdx === null ? null : n.args[argIdx];
    var sa = spec && arr(spec.args).filter(function (x) { return x.name === formal; })[0];
    if (!a) return null;
    if (a.ref) return this.sourceLabel(a.ref);
    if (a.value === null || a.value === undefined) return null;
    var ed = sa ? sa.editor : "code";
    if (ed === "bool") { var b = R.bool(a.value); return b === null ? R.show(a.value) : (b ? "yes" : "no"); }
    if (ed === "aes") {
      var it = R.items(a.value, "aes");
      if (it) return it.map(function (x) { var p = R.splitNamed(x); return (p[0] || "x") + " = " + p[1]; }).join(", ");
    }
    return R.show(a.value);
  };
  Editor.prototype.showDots = function (n, spec, idx) {
    var sa = spec && arr(spec.args).filter(function (x) { return x.name === "..."; })[0];
    var ed = sa ? sa.editor : "code";
    var self = this;
    var parts = idx.map(function (i) {
      var a = n.args[i];
      if (a.ref) return self.sourceLabel(a.ref);
      if (ed === "conditions") return describeCondition(a.value);
      if (ed === "assign") return a.name || R.show(a.value, 24);
      if (ed === "labels") return a.name || R.show(a.value, 24);
      if (ed === "symbols") {
        var m = /^desc\((.+)\)$/.exec(String(a.value).trim());
        return m ? (R.unsym(m[1]) || m[1]) + " \u2193" : R.unsym(a.value) || R.show(a.value, 24);
      }
      return (a.name ? a.name + " = " : "") + R.show(a.value, 28);
    });
    if (!parts.length) return null;
    var joiner = ed === "conditions" ? " and " : ", ";
    var max = ed === "conditions" ? 3 : 4;
    if (parts.length > max) return parts.slice(0, max).join(joiner) + " +" + (parts.length - max) + " more";
    return parts.join(joiner);
  };

  Editor.prototype.summary = function (n) {
    var self = this;
    var info = this.info(n);
    if (n.type === "dataset") return esc(info.sub);
    if (n.type === "code" || n.type === "expr") {
      var first = String(n.code || "").split("\n")[0];
      return '<code class="bk-sum-code">' + esc(first.length > 80 ? first.slice(0, 79) + "\u2026" : first) + "</code>";
    }
    var spec = info.spec;
    var m = matchArgs(n, spec);
    if (spec && spec.summary) {
      var missing = false;
      var text = String(spec.summary).replace(/\{(\??)([^{}?]+)(\?(!?)([^{}]*))?\}/g, function (all, optional, key, cond, neg, txt) {
        var val;
        if (key === "...") val = self.showDots(n, spec, m.dots);
        else if (/^\d+$/.test(key)) { var f = m.formals[parseInt(key, 10) - 1]; val = f === "..." ? self.showDots(n, spec, m.dots) : self.showArg(n, spec, f, m.map[f]); } else val = self.showArg(n, spec, key, m.map[key]);
        if (cond) {
          var has = val !== null && val !== "" && val !== "no";
          return (neg ? !has : has) ? esc(txt) : "";
        }
        if (val === null || val === "") {
          if (optional) return "";
          missing = true;
          return '<b class="bk-chip bk-chip-missing">\u2026</b>';
        }
        return '<b class="bk-chip">' + esc(val) + "</b>";
      });
      return text;
    }
    // unknown function: its first arguments
    var shown = (n.args || []).slice(0, 3).map(function (a) {
      var v = a.ref ? self.sourceLabel(a.ref) : R.show(a.value, 30);
      return (a.name ? '<span class="bk-sum-arg">' + esc(a.name) + "</span> " : "") + '<b class="bk-chip">' + esc(v) + "</b>";
    });
    if ((n.args || []).length > 3) shown.push('<span class="bk-sum-more">+' + (n.args.length - 3) + "</span>");
    return shown.join(" ") || '<span class="bk-sum-more">' + (spec ? "with its default settings" : "no settings") + "</span>";
  };

  // What a block's result is called: its variable, else its dataset / label
  Editor.prototype.sourceLabel = function (id) {
    var n = this.node(id);
    if (!n) return "\u2026";
    if (n.type === "dataset") return n.dataset;
    var st = this.status.nodes[id];
    if (n.name) return n.name;
    if (st && st.var) return st.var;
    var end = this.node(this.stackEnd(id));
    return end && end.name ? end.name : this.info(n).label;
  };

  Editor.prototype.statusLine = function (n) {
    var st = this.status.nodes[n.id];
    if (!st) return { cls: "bk-st-wait", html: '<span class="bk-dot"></span>not run yet' };
    if (st.state === "error") return { cls: "bk-st-error", html: icon("warning", 13) + "<span>" + esc(String(st.error || "Problem").split("\n")[0]) + "</span>" };
    if (st.state === "blocked") return { cls: "bk-st-blocked", html: '<span class="bk-dot"></span>waiting for a block above' };
    if (st.state === "missing") return { cls: "bk-st-error", html: icon("warning", 13) + "<span>" + esc(st.error || "package needed") + "</span>" };
    var k = st.kind;
    var txt;
    if (k === "data" || k === "listing") {
      txt = '<b class="bk-num">' + fmtInt(st.rows) + "</b> rows \u00b7 " + fmtInt(st.cols) + " cols";
      var inp = n.input && this.status.nodes[n.input];
      if (inp && (inp.kind === "data" || inp.kind === "listing") && typeof inp.rows === "number" && inp.rows !== st.rows) {
        var d = st.rows - inp.rows;
        txt += ' <span class="bk-delta ' + (d < 0 ? "bk-down" : "bk-up") + '">' + (d < 0 ? "\u2212" : "+") + fmtInt(Math.abs(d)) + "</span>";
      }
      if (k === "listing") txt = "listing \u00b7 " + txt;
    } else if (k === "table") txt = typeof st.rows === "number" ? "table \u00b7 <b class=\"bk-num\">" + fmtInt(st.rows) + "</b> rows \u00d7 " + (st.cols || 0) + " cols" : "table";
    else if (k === "layout") txt = "table layout";
    else if (k === "plot") txt = "plot";
    else txt = esc(st.info || "value");
    if (st.warnings) txt += ' <span class="bk-warn" title="' + esc(arr(st.warnings).join("\n")) + '">' + icon("warning", 12) + "</span>";
    return { cls: "bk-st-ok bk-k-" + k, html: txt };
  };

  // Blocks that work on rows (spec.input) but get them as their first
  // argument, e.g. as_listing(out, ...): the index of that argument, else -1.
  // It is drawn as the block's input, like a pipe.
  Editor.prototype.dataArg = function (n) {
    var spec = this.spec(n);
    if (n.input || !spec || !spec.input || spec.op === "+" || !n.args || !n.args.length) return -1;
    var first = arr(spec.formals)[0];
    for (var i = 0; i < n.args.length; i++) {
      var a = n.args[i];
      if ((i === 0 && !a.name && a.ref !== undefined) || (a.name && a.name === first && a.ref !== undefined)) return a.ref ? i : -1;
    }
    return -1;
  };
  Editor.prototype.inputOf = function (n) {
    if (n.input) return n.input;
    var i = this.dataArg(n);
    return i >= 0 ? n.args[i].ref : null;
  };

  // data arguments shown as sockets (wire targets)
  Editor.prototype.sockets = function (n) {
    if (n.type !== "call") return [];
    var self = this;
    var spec = this.spec(n);
    var m = matchArgs(n, spec);
    var skip = this.dataArg(n);
    var out = [];
    arr(spec && spec.args).forEach(function (sa) {
      if (sa.editor !== "data") return;
      if (sa.name === "...") {
        m.dots.forEach(function (i) { out.push({ key: "i:" + i, label: sa.label, ref: n.args[i].ref, value: n.args[i].value }); });
        out.push({ key: "f:...", label: "+ " + sa.label, ref: null, add: true });
      } else {
        var i = m.map[sa.name];
        out.push({ key: i === undefined ? "f:" + sa.name : "i:" + i, label: sa.label, ref: i === undefined ? null : n.args[i].ref, value: i === undefined ? null : n.args[i].value, optional: !sa.main });
      }
    });
    (n.args || []).forEach(function (a, i) {
      if (!a.ref || i === skip) return;
      if (out.some(function (s) { return s.key === "i:" + i; })) return;
      var f = null;
      Object.keys(m.map).forEach(function (k) { if (m.map[k] === i) f = k; });
      var sa = spec && arr(spec.args).filter(function (x) { return x.name === f; })[0];
      out.push({ key: "i:" + i, label: sa ? sa.label : a.name || f || "input", ref: a.ref });
    });
    return out;
  };

  Editor.prototype.fillBlock = function (el, n) {
    var self = this;
    var info = this.info(n);
    var st = this.status.nodes[n.id] || {};
    var head = this.isHead(n.id);
    var end = this.stackEnd(n.id) === n.id;
    var spec = info.spec;
    var isResult = this.status.result === n.id;
    var cls = ["bk-block", "bk-cat-" + info.cat, "bk-state-" + (st.state || "none")];
    if (head) cls.push("bk-head");
    if (end) cls.push("bk-end");
    if (n.type === "dataset") cls.push("bk-dataset");
    if (n.off) cls.push("bk-off");
    if (isResult) cls.push("bk-result");
    if (this.selected === n.id) cls.push("bk-selected");
    el.className = cls.join(" ");
    var line = this.statusLine(n);
    var quiet = st.state === "ok" && st.kind === "layout" && !end;
    var sockets = this.sockets(n);
    var html = "";
    var src = this.inputOf(n);
    if (head && (src || (spec && spec.input && spec.op !== "+") || (spec && spec.op === "+"))) {
      html += '<div class="bk-port-in' + (src ? " bk-connected" : "") + '" title="' + (src ? "Input: " + esc(this.sourceLabel(src)) : "Needs an input: drag it below a block, or wire a block into it") + '"></div>';
    }
    html += '<div class="bk-block-head">' +
      '<span class="bk-block-ico">' + icon(info.cat) + "</span>" +
      '<span class="bk-block-title" title="' + esc(info.fn ? info.label + " · " + info.fn + "()" : info.label) + '">' + esc(info.label) + "</span>" +
      (n.name && !(isResult && n.name === "result") ? '<span class="bk-var" title="Saved as ' + esc(n.name) + ' in the code">' + esc(n.name) + "</span>" : "") +
      (isResult ? '<span class="bk-badge-result" title="Shown in the Result panel">' + icon("star", 11) + "Result</span>" : "") +
      '<button type="button" class="bk-tip bk-tip-block" title="What is this block?" aria-label="What is this block?">?</button>' +
      "</div>";
    html += '<div class="bk-block-sum">' + (n.off ? '<span class="bk-off-tag">bypassed</span> ' : "") + this.summary(n) + "</div>";
    if (n.note) html += '<div class="bk-note" title="' + esc(n.note) + '">' + esc(n.note) + "</div>";
    if (sockets.length) {
      html += '<div class="bk-sockets">' + sockets.map(function (s) {
        var src = s.ref ? self.sourceLabel(s.ref) : s.value ? R.show(s.value, 22) : s.add ? "" : s.optional ? "optional" : "connect a block";
        return '<div class="bk-socket' + (s.ref ? " bk-connected" : s.value ? " bk-valued" : s.add || s.optional ? " bk-optional" : " bk-open") + '" data-key="' + esc(s.key) + '">' +
          '<span class="bk-socket-dot"></span><span class="bk-socket-label">' + esc(s.label) + "</span>" +
          '<span class="bk-socket-src">' + esc(src) + "</span></div>";
      }).join("") + "</div>";
    }
    if (!quiet) html += '<div class="bk-block-foot ' + line.cls + '">' + line.html + "</div>";
    if (end && n.type !== "code") {
      html += '<div class="bk-port-out" title="Drag to connect this result to another block"></div>';
      html += '<button type="button" class="bk-add-next" title="Add a block below" aria-label="Add a block below">' + icon("plus", 14) + "</button>";
    }
    el.innerHTML = html;
    el.setAttribute("title", "");
    el.setAttribute("tabindex", "0");
    el.setAttribute("role", "button");
    el.setAttribute("aria-label", info.label + ". " + (el.querySelector(".bk-block-sum") || {}).textContent + ". " + (el.querySelector(".bk-block-foot") || {}).textContent);
  };

  /* ---- Wires ---------------------------------------------------------------------------- */

  Editor.prototype.portOut = function (id) {
    var end = this.stackEnd(id);
    var p = this.pos[end];
    if (!p) return null;
    return { x: p.x + BLOCK_W / 2, y: p.y + this.heights[end] };
  };
  Editor.prototype.portIn = function (id) {
    var p = this.pos[id];
    return p ? { x: p.x + BLOCK_W / 2, y: p.y } : null;
  };
  // a socket's dot: on the side of the block facing its source
  Editor.prototype.socketPoint = function (id, key, from) {
    var el = this.els[id];
    var p = this.pos[id];
    if (!el || !p) return null;
    var s = $('.bk-socket[data-key="' + key + '"]', el);
    if (!s) return null;
    var right = !!from && from.x > p.x + BLOCK_W * 0.75;
    s.classList.toggle("bk-socket-right", right);
    return { x: right ? p.x + BLOCK_W : p.x, y: p.y + s.offsetTop + s.offsetHeight / 2, right: right };
  };

  Editor.prototype.drawWires = function () {
    var self = this;
    var svg = this.svg;
    if (!svg) return;
    var paths = [];
    var labels = [];
    var kindOf = function (id) { var st = self.status.nodes[id]; return st && st.state === "ok" ? st.kind : "none"; };
    var rows = function (id) {
      var st = self.status.nodes[id];
      return st && st.state === "ok" && (st.kind === "data" || st.kind === "listing") ? fmtInt(st.rows) : null;
    };
    var sel = this.selected;
    this.g.nodes.forEach(function (n) {
      // pipe (or first-argument data) from another stack into the first block of a stack
      var src = self.inputOf(n);
      if (src && self.isHead(n.id) && self.node(src)) {
        var a = self.portOut(src);
        var b = self.portIn(n.id);
        if (a && b) {
          var dy = Math.max(50, Math.abs(b.y - a.y) / 2);
          paths.push({ d: "M" + a.x + "," + a.y + " C" + a.x + "," + (a.y + dy) + " " + b.x + "," + (b.y - dy) + " " + b.x + "," + b.y, cls: "bk-wire bk-wire-pipe bk-wk-" + kindOf(src), hot: sel === n.id || sel === src, label: rows(src) });
        }
      }
      // arguments wired to other blocks
      self.sockets(n).forEach(function (s) {
        if (!s.ref || !self.node(s.ref)) return;
        var a = self.portOut(s.ref);
        var b = a && self.socketPoint(n.id, s.key, a);
        if (!a || !b) return;
        var side = b.right ? 1 : -1;
        // a source on the far side of the socket: a tight hook around the block's edge
        var far = side < 0 ? a.x > b.x : a.x < b.x;
        var dx = far ? 26 : Math.max(40, Math.min(140, Math.abs(b.x - a.x) / 2));
        var down = Math.max(40, Math.min(160, (b.y - a.y) / 2));
        var end = b.x + side * 6;
        paths.push({ d: "M" + a.x + "," + a.y + " C" + a.x + "," + (a.y + down) + " " + (b.x + side * dx) + "," + b.y + " " + end + "," + b.y, cls: "bk-wire bk-wire-ref bk-wk-" + kindOf(s.ref), hot: sel === n.id || sel === s.ref, label: rows(s.ref) });
      });
      // dashed: the block's code mentions another block's variable
      arr((self.status.nodes[n.id] || {}).implicit).forEach(function (src) {
        var pt = self.pos[n.id];
        if (!self.pos[src] || !pt || src === n.input) return;
        var end = self.stackEnd(src);
        var sameStack = self.stackOf[src] === self.stackOf[n.id];
        if (sameStack) return;
        var a = end === src ? self.portOut(src) : { x: self.pos[src].x + BLOCK_W, y: self.pos[src].y + 20 };
        var right = a.x > pt.x + BLOCK_W * 0.75;
        var b = { x: right ? pt.x + BLOCK_W : pt.x, y: pt.y + 18 };
        var side = right ? 1 : -1;
        var far = side < 0 ? a.x > b.x : a.x < b.x;
        var dx = far ? 26 : Math.max(40, Math.min(140, Math.abs(b.x - a.x) / 2));
        var down = Math.max(30, Math.min(160, (b.y - a.y) / 2));
        paths.push({ d: "M" + a.x + "," + a.y + " C" + a.x + "," + (a.y + down) + " " + (b.x + side * dx) + "," + b.y + " " + b.x + "," + b.y, cls: "bk-wire bk-wire-implicit", hot: sel === n.id || sel === src });
      });
    });
    if (this.pending) paths.push({ d: this.pending, cls: "bk-wire bk-wire-pending", hot: true });
    svg.innerHTML = paths.map(function (p) {
      return '<path class="' + p.cls + (p.hot ? " bk-hot" : "") + '" d="' + p.d + '"/>';
    }).join("");
    // row counts travelling on the wires
    $$("path", svg).forEach(function (el, i) {
      var p = paths[i];
      if (!p || !p.label) return;
      var len = el.getTotalLength();
      if (!len) return;
      var pt = el.getPointAtLength(len * 0.5);
      labels.push('<g class="bk-wire-label" transform="translate(' + pt.x + "," + pt.y + ')"><rect x="' + (-(p.label.length * 3.6 + 8)) + '" y="-9" rx="9" width="' + (p.label.length * 7.2 + 16) + '" height="18"/><text text-anchor="middle" dy="4">' + esc(p.label) + "</text></g>");
    });
    if (labels.length) svg.insertAdjacentHTML("beforeend", labels.join(""));
  };

  /* ---- View: pan, zoom, fit ----------------------------------------------------------- */

  Editor.prototype.applyView = function () {
    var v = this.view;
    this.world.style.transform = "translate(" + v.x + "px," + v.y + "px) scale(" + v.k + ")";
    this.stage.style.setProperty("--bk-k", v.k);
    this.stage.style.backgroundPosition = v.x + "px " + v.y + "px";
    this.stage.style.backgroundSize = (22 * v.k) + "px " + (22 * v.k) + "px";
    var lvl = $(".bk-zoom-level", this.stage);
    if (lvl) lvl.textContent = Math.round(v.k * 100) + "%";
  };
  Editor.prototype.toWorld = function (cx, cy) {
    var r = this.stage.getBoundingClientRect();
    return { x: (cx - r.left - this.view.x) / this.view.k, y: (cy - r.top - this.view.y) / this.view.k };
  };
  Editor.prototype.zoomAt = function (k, cx, cy) {
    var r = this.stage.getBoundingClientRect();
    var px = cx === undefined ? r.width / 2 : cx - r.left;
    var py = cy === undefined ? r.height / 2 : cy - r.top;
    k = clamp(k, 0.25, 2);
    var v = this.view;
    v.x = px - (px - v.x) * (k / v.k);
    v.y = py - (py - v.y) * (k / v.k);
    v.k = k;
    this.applyView();
  };
  Editor.prototype.zoomBy = function (f) { this.zoomAt(this.view.k * f); };
  Editor.prototype.zoomTo = function (k) { this.zoomAt(k); };
  Editor.prototype.bounds = function () {
    var self = this;
    var b = null;
    Object.keys(this.pos).forEach(function (id) {
      var p = self.pos[id];
      var x2 = p.x + BLOCK_W;
      var y2 = p.y + (self.heights[id] || 60);
      if (!b) b = { x1: p.x, y1: p.y, x2: x2, y2: y2 };
      else { b.x1 = Math.min(b.x1, p.x); b.y1 = Math.min(b.y1, p.y); b.x2 = Math.max(b.x2, x2); b.y2 = Math.max(b.y2, y2); }
    });
    return b;
  };
  Editor.prototype.fit = function (animate) {
    var b = this.bounds();
    var r = this.stage.getBoundingClientRect();
    if (!b || !r.width || !r.height) return;
    var pad = 48;
    var left = (this.insets && this.insets.left) || 0;
    var width = r.width - left;
    var k = clamp(Math.min((width - 2 * pad) / (b.x2 - b.x1), (r.height - 2 * pad - 40) / (b.y2 - b.y1)), 0.3, 1);
    var v = this.view;
    v.k = k;
    v.x = left + (width - (b.x2 - b.x1) * k) / 2 - b.x1 * k;
    v.y = Math.max(pad, (r.height - (b.y2 - b.y1) * k) / 2) - b.y1 * k;
    if (animate && !window.matchMedia("(prefers-reduced-motion: reduce)").matches) {
      this.world.classList.add("bk-animate");
      var w = this.world;
      window.setTimeout(function () { w.classList.remove("bk-animate"); }, 320);
    }
    this.applyView();
  };

  /* ---- Layout: stacks in columns, by dependency -------------------------------------------- */

  // Stacks flow downwards, so the layout does too: the stacks a stack uses
  // sit in the row above it, side by side. all = true: arrange every stack
  // (undoable when `undoable`); false: only place the stacks without a position.
  Editor.prototype.arrange = function (all, undoable) {
    var self = this;
    if (all && undoable) this.remember();
    var stacks = this.stacks;
    var idx = {};
    stacks.forEach(function (s, i) { s.members.forEach(function (m) { idx[m] = i; }); });
    var implicit = this.implicit();
    var deps = stacks.map(function () { return {}; });
    var users = stacks.map(function () { return {}; });
    this.g.nodes.forEach(function (n) {
      var i = idx[n.id];
      var add = function (src) {
        if (src && idx[src] !== undefined && idx[src] !== i) { deps[i][idx[src]] = true; users[idx[src]][i] = true; }
      };
      if (n.input && self.isHead(n.id)) add(n.input);
      (n.args || []).forEach(function (a) { add(a.ref); });
      arr(implicit[n.id]).forEach(add);
    });
    var heightOf = function (s) { return s.members.reduce(function (t, m) { return t + (self.heights[m] || 60); }, 0); };
    var W = BLOCK_W + COL_GAP;

    if (!all) {
      var placed = {};
      stacks.forEach(function (s, i) {
        var n = self.node(s.head);
        if (typeof n.x === "number" && typeof n.y === "number") placed[i] = true;
      });
      if (Object.keys(placed).length) {
        // new stacks go below what they use, else below everything; never
        // on top of another stack (moved right until the place is free)
        var b = this.bounds() || { x1: 0, y1: 0, x2: 0, y2: 0 };
        var boxes = [];
        stacks.forEach(function (s, i) {
          if (!placed[i]) return;
          var hn = self.node(s.head);
          boxes.push({ x: hn.x, y: hn.y, h: heightOf(s) });
        });
        var free = function (x, y, hgt) {
          return !boxes.some(function (o) { return x < o.x + BLOCK_W + 8 && o.x < x + BLOCK_W + 8 && y < o.y + o.h + 8 && o.y < y + hgt + 8; });
        };
        stacks.forEach(function (s, i) {
          if (placed[i]) return;
          var n = self.node(s.head);
          var src = Object.keys(deps[i]).filter(function (j) { return placed[j]; })[0];
          if (src !== undefined) {
            var ss = stacks[src];
            var sh = self.node(ss.head);
            n.x = sh.x + W;
            n.y = sh.y + heightOf(ss) + ROW_GAP;
          } else {
            n.x = b.x1;
            n.y = b.y2 + ROW_GAP;
            b.y2 += heightOf(s) + ROW_GAP;
          }
          var guard = 0;
          while (!free(n.x, n.y, heightOf(s)) && guard++ < 40) n.x += W;
          boxes.push({ x: n.x, y: n.y, h: heightOf(s) });
          placed[i] = true;
        });
        return;
      }
    }

    // rank: the longest chain of stacks above
    var rank = stacks.map(function () { return -1; });
    var rankOf = function (i, guard) {
      if (rank[i] >= 0) return rank[i];
      if (guard > stacks.length) return 0;
      var r = 0;
      Object.keys(deps[i]).forEach(function (j) { r = Math.max(r, rankOf(+j, guard + 1) + 1); });
      rank[i] = r;
      return r;
    };
    stacks.forEach(function (s, i) { rankOf(i, 0); });
    // a stack used only further down moves down, next to its user
    stacks.forEach(function (s, i) {
      var us = Object.keys(users[i]).map(Number);
      if (!us.length) return;
      var low = Math.min.apply(null, us.map(function (j) { return rank[j]; }));
      if (low - 1 > rank[i]) rank[i] = low - 1;
    });
    var byRank = [];
    stacks.forEach(function (s, i) { (byRank[rank[i]] = byRank[rank[i]] || []).push(i); });
    byRank = byRank.filter(Boolean);
    var ordOf = function (i) { return self.node(stacks[i].head).order || 0; };

    // Order the stacks of each rank to avoid crossings: by the mean position
    // of the stacks they use (going down), then of their users (going up).
    var orderRanks = function (ranks) {
      var where = {};
      var pack = function (list) { list.forEach(function (i, k) { where[i] = k; }); };
      ranks.forEach(function (list) { list.sort(function (a, b) { return ordOf(a) - ordOf(b); }); pack(list); });
      var sweep = function (list, near) {
        var key = {};
        list.forEach(function (i) {
          var ns = Object.keys(near[i]).map(Number);
          key[i] = ns.length ? ns.reduce(function (t, j) { return t + where[j]; }, 0) / ns.length : where[i];
        });
        list.sort(function (a, b) { return key[a] - key[b] || ordOf(a) - ordOf(b); });
        pack(list);
      };
      for (var pass = 0; pass < 2; pass++) {
        for (var r = 1; r < ranks.length; r++) sweep(ranks[r], deps);
        for (var r2 = ranks.length - 2; r2 >= 0; r2--) sweep(ranks[r2], users);
      }
      return ranks;
    };
    orderRanks(byRank);

    // Two ways to place them: ranks as rows (top to bottom, at most 4 stacks
    // side by side) or as columns (left to right). The one that fits the
    // canvas at the larger zoom wins.
    var asRows = function () {
      var pos = {};
      var rows = [];
      byRank.forEach(function (list) { for (var k = 0; k < list.length; k += 4) rows.push(list.slice(k, k + 4)); });
      var widest = Math.max.apply(null, rows.map(function (row) { return row.length; }));
      var y = 0;
      rows.forEach(function (row) {
        var shift = (widest - row.length) * W / 2;
        var hmax = 0;
        row.forEach(function (i, k) {
          pos[i] = { x: Math.round(shift + k * W), y: Math.round(y) };
          hmax = Math.max(hmax, heightOf(stacks[i]));
        });
        y += hmax + ROW_GAP * 2;
      });
      return { pos: pos, w: widest * W - COL_GAP, h: y - ROW_GAP * 2 };
    };
    // ranks as columns, `per` columns per band; bands go down like lines of text
    var asColumns = function (per) {
      var pos = {};
      var y0 = 0;
      var wmax = 0;
      for (var b = 0; b * per < byRank.length; b++) {
        var band = byRank.slice(b * per, b * per + per);
        var hband = 0;
        band.forEach(function (list) {
          var hh = list.reduce(function (t, i) { return t + heightOf(stacks[i]) + ROW_GAP; }, -ROW_GAP);
          hband = Math.max(hband, hh);
        });
        band.forEach(function (list, c) {
          var hh = list.reduce(function (t, i) { return t + heightOf(stacks[i]) + ROW_GAP; }, -ROW_GAP);
          var y = y0 + (hband - hh) / 2;
          list.forEach(function (i) {
            pos[i] = { x: c * (W + COL_GAP / 2), y: Math.round(y) };
            y += heightOf(stacks[i]) + ROW_GAP;
          });
        });
        wmax = Math.max(wmax, band.length * (W + COL_GAP / 2) - COL_GAP / 2);
        y0 += hband + ROW_GAP * 2;
      }
      return { pos: pos, w: wmax, h: y0 - ROW_GAP * 2 };
    };
    var r = this.stage.getBoundingClientRect();
    var sw = r.width || 900;
    var sh = r.height || 700;
    var fitOf = function (l) { return Math.min(1, (sw - 96) / Math.max(1, l.w), (sh - 136) / Math.max(1, l.h)); };
    var best = asRows();
    var bestFit = fitOf(best);
    if (byRank.length > 2) {
      for (var per = 2; per <= byRank.length; per++) {
        var l = asColumns(per);
        var f = fitOf(l);
        if (f > bestFit * 1.15) { best = l; bestFit = f; }
      }
    }
    stacks.forEach(function (s, i) {
      var n = self.node(s.head);
      n.x = best.pos[i].x;
      n.y = best.pos[i].y;
    });
    if (all) { this.place(); this.drawWires(); this.save(); }
  };

  /* ---- Changing the graph ------------------------------------------------------------------ */

  Editor.prototype.remember = function () {
    this.undoStack.push(JSON.stringify(this.g));
    if (this.undoStack.length > 100) this.undoStack.shift();
    this.redoStack = [];
  };
  Editor.prototype.undo = function () {
    if (!this.undoStack.length) return;
    this.redoStack.push(JSON.stringify(this.g));
    this.g = JSON.parse(this.undoStack.pop());
    this.changed();
  };
  Editor.prototype.redo = function () {
    if (!this.redoStack.length) return;
    this.undoStack.push(JSON.stringify(this.g));
    this.g = JSON.parse(this.redoStack.pop());
    this.changed();
  };
  // after every change: draw, save, and send to R
  Editor.prototype.changed = function () {
    if (this.selected && !this.node(this.selected)) this.select(null);
    this.render();
    this.save();
    this.sendGraph();
    this.emit("change");
  };

  // the graph R needs (no positions)
  Editor.prototype.semantic = function () {
    var g = this.g;
    return {
      version: 1, packages: g.packages || [], result: g.result || null, template: g.template || null, title: g.title || null,
      nodes: g.nodes.map(function (n) {
        var o = {};
        Object.keys(n).forEach(function (k) { if (k !== "x" && k !== "y") o[k] = n[k]; });
        return o;
      })
    };
  };
  Editor.prototype.sendGraph = function () {
    var json = JSON.stringify(this.semantic());
    if (json === this.sentJson) return;
    this.sentJson = json;
    this.rev += 1;
    var payload = JSON.parse(json);
    payload.rev = this.rev;
    this.running = true;
    this.renderTop();
    this.input("graph", JSON.stringify(payload));
  };

  // Transfer what uses `from` (wires, the result, the variable) to `to`
  Editor.prototype.transfer = function (from, to, opts) {
    var g = this.g;
    var toNode = this.node(to);
    g.nodes.forEach(function (n) {
      if (n.id === to) return;
      if (n.input === from && !(opts && opts.keepPipe === n.id)) n.input = to;
      (n.args || []).forEach(function (a) { if (a.ref === from) a.ref = to; });
    });
    if (g.result === from) g.result = to;
    var fromNode = this.node(from);
    if (fromNode && fromNode.name && toNode && !toNode.name && toNode.type !== "dataset") {
      toNode.name = fromNode.name;
      delete fromNode.name;
    }
  };

  // Take one block out of its stack; the blocks around it close the gap
  Editor.prototype.detach = function (id) {
    var n = this.node(id);
    var x = n.input;
    var c = this.childOf[id];
    var p = this.pos[id] || { x: 0, y: 0 };
    if (c) {
      this.node(c).input = x || null;
      if (!x) { var cn = this.node(c); cn.x = p.x; cn.y = p.y; }
    } else if (x) {
      this.transfer(id, x);
    }
    n.input = null;
    n.x = p.x;
    n.y = p.y;
  };

  // Take a block and the blocks below it out of their stack
  Editor.prototype.detachTail = function (id) {
    var n = this.node(id);
    var p = this.pos[id] || { x: 0, y: 0 };
    n.input = null;
    n.x = p.x;
    n.y = p.y;
  };

  // Put the stack segment head..tail right below block `after`
  Editor.prototype.insertAfter = function (headId, tailId, after) {
    var head = this.node(headId);
    var c = this.childOf[after];
    if (c && c !== headId) {
      this.node(c).input = tailId;
    } else if (!c) {
      // below the last block: the segment becomes the output of the stack
      var users = this.g.nodes.filter(function (n) { return n.id !== headId && (n.input === after || (n.args || []).some(function (a) { return a.ref === after; })); });
      if (users.length || this.g.result === after || this.node(after).name) {
        users.forEach(function (n) {
          if (n.input === after) n.input = tailId;
          (n.args || []).forEach(function (a) { if (a.ref === after) a.ref = tailId; });
        });
        if (this.g.result === after) this.g.result = tailId;
        var an = this.node(after);
        var tn = this.node(tailId);
        if (an.name && !tn.name && an.type !== "dataset") { tn.name = an.name; delete an.name; }
      }
      if (!this.node(tailId).name && this.node(after).type === "dataset") {
        this.node(tailId).name = this.freshName(this.node(after).dataset.toLowerCase());
      }
    }
    head.input = after;
    var spec = this.spec(head);
    head.op = spec && spec.op === "+" ? "+" : "%>%";
    delete head.x;
    delete head.y;
  };

  Editor.prototype.freshName = function (base) {
    var taken = {};
    this.g.nodes.forEach(function (n) { if (n.name) taken[n.name] = true; if (n.dataset) taken[n.dataset] = true; });
    base = String(base).replace(/[^A-Za-z0-9_.]/g, "_").replace(/^([^A-Za-z.])/, "x$1") || "x";
    if (!taken[base]) return base;
    for (var i = 2; ; i++) if (!taken[base + "_" + i]) return base + "_" + i;
  };

  Editor.prototype.deleteNode = function (id, wholeStack) {
    var self = this;
    var ids = wholeStack && this.stackOf[id] ? this.stackOf[id].members.slice() : [id];
    this.remember();
    ids.slice().reverse().forEach(function (nid) {
      var n = self.node(nid);
      if (!n) return;
      var x = n.input && self.node(n.input) ? n.input : null;
      if (x) {
        self.transfer(nid, x);
      } else {
        var p = self.pos[nid] || { x: 0, y: 0 };
        self.g.nodes.forEach(function (m) {
          if (m.input === nid) { m.input = null; if (self.childOf[nid] === m.id) { m.x = p.x; m.y = p.y; } }
          (m.args || []).forEach(function (a) { if (a.ref === nid) { a.ref = null; } });
        });
        if (self.g.result === nid) self.g.result = null;
      }
      self.g.nodes = self.g.nodes.filter(function (m) { return m.id !== nid; });
      self.computeStacks();
    });
    if (ids.indexOf(this.selected) >= 0) this.select(null);
    this.changed();
  };

  Editor.prototype.duplicate = function (id) {
    var n = this.node(id);
    if (!n) return;
    this.remember();
    var copy = clone(n);
    copy.id = nextId(this.g);
    copy.order = nextOrder(this.g);
    delete copy.name;
    if (this.isHead(id)) {
      var p = this.pos[id] || { x: 0, y: 0 };
      copy.x = p.x + 40;
      copy.y = p.y + 40;
      this.g.nodes.push(copy);
    } else {
      this.g.nodes.push(copy);
      copy.input = null;
      this.computeStacks();
      this.insertAfter(copy.id, copy.id, id);
    }
    this.changed();
    this.select(copy.id);
  };

  // a new block from the palette
  Editor.prototype.newNode = function (kind, what) {
    var g = this.g;
    var n = { id: nextId(g), order: nextOrder(g) };
    if (kind === "dataset") { n.type = "dataset"; n.dataset = what; return n; }
    if (kind === "code") {
      n.type = what;
      n.code = what === "expr" ? 'c("A", "B")' : "# any R code\n";
      if (what === "expr") n.name = this.freshName("x");
      return n;
    }
    var spec = this.reg[what];
    n.type = "call";
    n.fn = what;
    n.args = clone(arr(spec && spec.insert)).map(function (a) {
      var o = { name: a.name || "" };
      if (Object.prototype.hasOwnProperty.call(a, "ref")) o.ref = null; else o.value = a.value;
      return o;
    });
    if (spec && spec.hint && !spec.input) n.name = this.freshName(spec.hint);
    return n;
  };

  // add a palette block: below `after` if given, else at a point of the canvas
  Editor.prototype.addNode = function (n, at, after) {
    this.remember();
    this.g.nodes.push(n);
    var spec = this.spec(n);
    if (after && n.type === "call") {
      this.computeStacks();
      this.insertAfter(n.id, n.id, after);
    } else {
      n.x = Math.round(at.x);
      n.y = Math.round(at.y);
      if (spec && spec.input) n.op = spec.op === "+" ? "+" : "%>%";
    }
    this.changed();
    this.select(n.id);
  };

  // wire the result of `src` into a socket (key "i:<index>" or "f:<formal>") of `id`
  Editor.prototype.connect = function (src, id, key) {
    var n = this.node(id);
    if (!n || !src) return false;
    src = this.stackEnd(src);
    if (src === id || reaches(this.g, src, id)) { this.toast("That would make a loop: a block cannot use its own result."); return false; }
    this.remember();
    if (key === "in" && this.dataArg(n) >= 0) {
      n.args[this.dataArg(n)].ref = src;
    } else if (key === "in") {
      n.input = src;
      var spec = this.spec(n);
      n.op = spec && spec.op === "+" ? "+" : (n.op || "%>%");
    } else if (key.indexOf("i:") === 0) {
      var a = n.args[parseInt(key.slice(2), 10)];
      delete a.value;
      a.ref = src;
    } else {
      var formal = key.slice(2);
      n.args = n.args || [];
      n.args.push({ name: formal === "..." ? "" : formal, ref: src });
    }
    this.changed();
    return true;
  };

  Editor.prototype.toast = function (msg) {
    $$(".bk-toast", this.stage).forEach(function (x) { x.remove(); });
    var t = h("div", { class: "bk-toast", role: "status", text: msg });
    this.stage.appendChild(t);
    window.setTimeout(function () { t.classList.add("bk-out"); }, 2600);
    window.setTimeout(function () { t.remove(); }, 3000);
  };

  /* ---- Interaction: canvas ----------------------------------------------------------------- */

  Editor.prototype.bindStage = function () {
    var self = this;
    var stage = this.stage;
    stage.addEventListener("wheel", function (e) {
      if (e.target.closest && e.target.closest(".bk-empty-state, .bk-menu")) return;
      e.preventDefault();
      if (e.ctrlKey || e.metaKey) {
        self.zoomAt(self.view.k * Math.exp(-e.deltaY * 0.0025), e.clientX, e.clientY);
      } else {
        self.view.x -= e.shiftKey && !e.deltaX ? e.deltaY : e.deltaX;
        self.view.y -= e.shiftKey && !e.deltaX ? 0 : e.deltaY;
        self.applyView();
      }
    }, { passive: false });

    stage.addEventListener("pointerdown", function (e) {
      if (e.button !== 0) return;
      var t = e.target;
      if (t.closest(".bk-zoom, .bk-empty-state, .bk-toast, .bk-menu, .bk-hint-bar, .bk-stage-tip, .bk-coach, .bk-pop")) return;
      self.closeMenu();
      var blockEl = t.closest(".bk-block");
      var id = blockEl && blockEl.getAttribute("data-id");
      if (t.closest(".bk-tip") && id) {
        e.preventDefault();
        e.stopPropagation();
        self.openBlockTip(id, t.closest(".bk-tip"));
        return;
      }
      if (t.closest(".bk-port-out") && id) { e.preventDefault(); self.startWire(e, id); return; }
      if (t.closest(".bk-add-next") && id) {
        e.preventDefault();
        var b = t.closest(".bk-add-next").getBoundingClientRect();
        self.select(id);
        self.openNextMenu(id, b.left, b.bottom + 6);
        return;
      }
      var socket = t.closest(".bk-socket");
      if (socket && id) { e.preventDefault(); self.startSocket(e, id, socket); return; }
      if (id) { e.preventDefault(); self.startDrag(e, id); return; }
      self.startPan(e);
    });
    stage.addEventListener("dblclick", function (e) {
      var blockEl = e.target.closest && e.target.closest(".bk-block");
      if (blockEl) { self.select(blockEl.getAttribute("data-id")); self.showPane("block"); }
    });
    stage.addEventListener("contextmenu", function (e) {
      var blockEl = e.target.closest && e.target.closest(".bk-block");
      if (!blockEl) return;
      e.preventDefault();
      var id = blockEl.getAttribute("data-id");
      self.select(id);
      self.openBlockMenu(id, e.clientX, e.clientY);
    });
    this.bindPalette();
    if (window.ResizeObserver) {
      new window.ResizeObserver(function () { self.drawWires(); }).observe(stage);
    }
  };

  // Track a pointer from pointerdown: onMove(ev, dx, dy) once it moved more
  // than 4 px, then onUp(ev, moved). The pointer is captured by `captureEl`,
  // which must outlive the gesture (blocks are redrawn while dragging).
  function track(e, onMove, onUp, captureEl) {
    var sx = e.clientX;
    var sy = e.clientY;
    var moved = false;
    var el = captureEl || e.target;
    try { el.setPointerCapture(e.pointerId); } catch (err) { /* ignore */ }
    var move = function (ev) {
      if (!moved && Math.abs(ev.clientX - sx) + Math.abs(ev.clientY - sy) < 5) return;
      moved = true;
      onMove(ev, ev.clientX - sx, ev.clientY - sy);
    };
    var up = function (ev) {
      el.removeEventListener("pointermove", move);
      el.removeEventListener("pointerup", up);
      el.removeEventListener("pointercancel", up);
      onUp(ev, moved);
    };
    el.addEventListener("pointermove", move);
    el.addEventListener("pointerup", up);
    el.addEventListener("pointercancel", up);
  }

  Editor.prototype.startPan = function (e) {
    var self = this;
    var v0 = { x: this.view.x, y: this.view.y };
    this.stage.classList.add("bk-panning");
    track(e, function (ev, dx, dy) {
      self.view.x = v0.x + dx;
      self.view.y = v0.y + dy;
      self.applyView();
    }, function (ev, moved) {
      self.stage.classList.remove("bk-panning");
      if (!moved) { self.select(null); self.showPane("result"); }
    }, this.stage);
  };

  // Where would a block dropped at world point (x, y) go? Below a block of
  // another stack, if close enough.
  Editor.prototype.findSlot = function (x, y, exclude) {
    var self = this;
    var best = null;
    this.stacks.forEach(function (s) {
      if (exclude && s.members.indexOf(exclude.head) >= 0) return;
      s.members.forEach(function (id) {
        var n = self.node(id);
        if (n.type === "code") return;
        var p = self.pos[id];
        if (!p) return;
        var by = p.y + self.heights[id];
        var dx = Math.abs(x - p.x);
        var dy = Math.abs(y - by);
        if (dx > SNAP_DX || dy > SNAP_DY) return;
        if (exclude && exclude.members.some(function (m) { return reaches(self.g, id, m); })) return;
        var score = dy + dx / 3;
        if (!best || score < best.score) best = { after: id, x: p.x, y: by, score: score };
      });
    });
    return best;
  };
  Editor.prototype.showSlot = function (slot) {
    if (!this.slotEl) {
      this.slotEl = h("div", { class: "bk-slot" });
      this.world.appendChild(this.slotEl);
    }
    this.slotEl.hidden = !slot;
    if (slot) this.slotEl.style.transform = "translate(" + slot.x + "px," + (slot.y - 3) + "px)";
  };

  Editor.prototype.startDrag = function (e, id) {
    var self = this;
    var n = this.node(id);
    var shift = e.shiftKey;
    var seg = null;
    var off = null;
    var slot = null;
    track(e, function (ev) {
      var w = self.toWorld(ev.clientX, ev.clientY);
      if (!seg) {
        self.remember();
        if (!self.isHead(id)) {
          if (shift) self.detachTail(id); else self.detach(id);
          self.render();
        }
        seg = self.stackOf[id];
        var p = self.pos[id];
        off = { x: w.x - p.x, y: w.y - p.y };
        seg.members.forEach(function (m) { self.els[m].classList.add("bk-dragging"); });
      }
      n.x = Math.round(w.x - off.x);
      n.y = Math.round(w.y - off.y);
      var head = self.node(seg.head);
      // datasets and R code blocks stand on their own: they do not go into stacks
      slot = head.type === "call" ? self.findSlot(n.x, n.y, seg) : null;
      self.showSlot(slot);
      self.place();
      self.drawWires();
    }, function (ev, moved) {
      self.showSlot(null);
      if (!moved) { self.select(id); self.showPane("block"); return; }
      seg.members.forEach(function (m) { if (self.els[m]) self.els[m].classList.remove("bk-dragging"); });
      if (slot) self.insertAfter(seg.head, seg.members[seg.members.length - 1], slot.after);
      self.changed();
    }, this.stage);
  };

  Editor.prototype.startWire = function (e, id) {
    var self = this;
    var src = this.stackEnd(id);
    var a = this.portOut(src);
    var target = null;
    var clear = function () { $$(".bk-drop-target", self.world).forEach(function (x) { x.classList.remove("bk-drop-target"); }); };
    track(e, function (ev) {
      var w = self.toWorld(ev.clientX, ev.clientY);
      var dy = Math.max(40, Math.abs(w.y - a.y) / 2);
      self.pending = "M" + a.x + "," + a.y + " C" + a.x + "," + (a.y + dy) + " " + w.x + "," + (w.y - dy) + " " + w.x + "," + w.y;
      clear();
      target = self.wireTarget(ev.clientX, ev.clientY, src);
      if (target) target.el.classList.add("bk-drop-target");
      self.drawWires();
    }, function () {
      self.pending = null;
      clear();
      if (target) self.connect(src, target.id, target.key); else self.drawWires();
    }, this.stage);
  };

  // what is under the pointer that a wire can connect to
  Editor.prototype.wireTarget = function (cx, cy, src) {
    var el = doc.elementFromPoint(cx, cy);
    if (!el) return null;
    var blockEl = el.closest(".bk-block");
    if (!blockEl) return null;
    var id = blockEl.getAttribute("data-id");
    if (id === src || this.stackOf[src] === this.stackOf[id]) return null;
    var socket = el.closest(".bk-socket");
    if (socket) return { id: id, key: socket.getAttribute("data-key"), el: socket };
    var n = this.node(id);
    var spec = this.spec(n);
    var open = this.sockets(n).filter(function (s) { return !s.ref; });
    if (open.length) {
      var key = open[0].key;
      return { id: id, key: key, el: $('.bk-socket[data-key="' + key + '"]', blockEl) || blockEl };
    }
    if (this.isHead(id) && (spec && spec.input || n.input || this.dataArg(n) >= 0)) return { id: id, key: "in", el: $(".bk-port-in", blockEl) || blockEl };
    return null;
  };

  // click on a socket: choose what to connect; drag from it: wire from a block
  Editor.prototype.startSocket = function (e, id, socketEl) {
    var self = this;
    var key = socketEl.getAttribute("data-key");
    track(e, function () { /* no drag from sockets */ }, function (ev, moved) {
      if (moved) return;
      self.select(id);
      self.openSourceMenu(id, key, ev.clientX, ev.clientY);
    }, this.stage);
  };

  // candidate sources: the last block of every stack, and datasets
  Editor.prototype.sources = function (exceptId) {
    var self = this;
    var out = [];
    this.stacks.forEach(function (s) {
      var end = s.members[s.members.length - 1];
      var n = self.node(end);
      if (n.type === "code" || s.members.indexOf(exceptId) >= 0) return;
      if (exceptId && reaches(self.g, end, exceptId)) return;
      var st = self.status.nodes[end] || {};
      out.push({ id: end, label: self.sourceLabel(end), sub: self.info(n).label + (st.kind ? " \u00b7 " + (st.kind === "data" ? fmtInt(st.rows) + " rows" : st.kind) : "") });
    });
    return out;
  };

  /* ---- Menus ---------------------------------------------------------------------------------- */

  Editor.prototype.closeMenu = function () {
    if (this.menu) { this.menu.remove(); this.menu = null; }
  };
  Editor.prototype.openMenu = function (items, cx, cy) {
    var self = this;
    this.closeMenu();
    var m = h("div", { class: "bk-menu", role: "menu" });
    items.forEach(function (it) {
      if (it === "-") { m.appendChild(h("div", { class: "bk-menu-sep" })); return; }
      if (it.head) { m.appendChild(h("div", { class: "bk-menu-head", text: it.head })); return; }
      m.appendChild(h("button", {
        type: "button", role: "menuitem", class: "bk-menu-item" + (it.danger ? " bk-danger" : "") + (it.on ? " bk-on" : ""),
        html: (it.icon ? icon(it.icon) : "") + '<span class="bk-menu-label">' + esc(it.label) + "</span>" + (it.sub ? '<span class="bk-menu-sub">' + esc(it.sub) + "</span>" : "") + (it.key ? "<kbd>" + esc(it.key) + "</kbd>" : ""),
        onclick: function () { self.closeMenu(); it.run(); }
      }));
    });
    doc.body.appendChild(m);
    var r = m.getBoundingClientRect();
    m.style.left = clamp(cx, 8, window.innerWidth - r.width - 8) + "px";
    m.style.top = clamp(cy, 8, window.innerHeight - r.height - 8) + "px";
    this.menu = m;
    var first = $(".bk-menu-item", m);
    if (first) first.focus();
    var away = function (ev) {
      if (self.menu && !self.menu.contains(ev.target)) { self.closeMenu(); doc.removeEventListener("pointerdown", away, true); }
    };
    doc.addEventListener("pointerdown", away, true);
    m.addEventListener("keydown", function (ev) {
      var items2 = $$(".bk-menu-item", m);
      var i = items2.indexOf(doc.activeElement);
      if (ev.key === "Escape") { self.closeMenu(); ev.stopPropagation(); }
      if (ev.key === "ArrowDown") { ev.preventDefault(); (items2[i + 1] || items2[0]).focus(); }
      if (ev.key === "ArrowUp") { ev.preventDefault(); (items2[i - 1] || items2[items2.length - 1]).focus(); }
    });
  };
  Editor.prototype.openSourceMenu = function (id, key, cx, cy) {
    var self = this;
    var n = this.node(id);
    var items = [{ head: "Connect" }];
    this.sources(id).forEach(function (s) {
      items.push({ label: s.label, sub: s.sub, icon: "link", run: function () { self.connect(s.id, id, key); } });
    });
    if (key.indexOf("i:") === 0) {
      items.push("-");
      items.push({
        label: "Disconnect", icon: "close", danger: true, run: function () {
          self.remember();
          var i = parseInt(key.slice(2), 10);
          var sa = self.argSpec(n, i);
          if (sa && sa.main) { n.args[i].ref = null; delete n.args[i].value; } else n.args.splice(i, 1);
          self.changed();
        }
      });
    }
    this.openMenu(items, cx, cy);
  };
  Editor.prototype.openBlockMenu = function (id, cx, cy) {
    var self = this;
    var n = this.node(id);
    var head = this.isHead(id);
    var items = [];
    items.push({ label: "Settings", icon: "panel", run: function () { self.select(id); self.showPane("block"); } });
    items.push({ label: "Show as the result", icon: "star", on: this.status.result === id, run: function () { self.setResult(id); } });
    if (n.input) items.push({ label: n.off ? "Turn back on" : "Bypass (skip this block)", icon: "bypass", on: !!n.off, run: function () { self.toggleOff(id); } });
    items.push({ label: "Duplicate", icon: "copy", key: (isMac ? "\u2318" : "Ctrl+") + "D", run: function () { self.duplicate(id); } });
    items.push("-");
    items.push({ label: "Delete block", icon: "trash", danger: true, key: "Del", run: function () { self.deleteNode(id); } });
    if (head && this.stackOf[id].members.length > 1) items.push({ label: "Delete the whole stack", icon: "trash", danger: true, run: function () { self.deleteNode(id, true); } });
    this.openMenu(items, cx, cy);
  };

  Editor.prototype.setResult = function (id) {
    this.remember();
    this.g.result = id;
    this.changed();
    this.showPane("result");
  };
  Editor.prototype.toggleOff = function (id) {
    var n = this.node(id);
    this.remember();
    if (n.off) delete n.off; else n.off = true;
    this.changed();
  };

  /* ---- Suggestions: what to add below a stack ------------------------------------------------------
     By what the stack produces: rows, a table layout, a table, a plot.
     Recipes add several blocks at once, already wired (e.g. a summary
     table: its layout stack and the Build table block). */

  var NEXT = {
    data: ["filter", "mutate", "select", "arrange", "distinct", "count", "var_relabel", "df_explicit_na"],
    layout: ["split_cols_by", "add_overall_col", "split_rows_by", "analyze_vars", "count_occurrences", "summarize_num_patients", "count_patients_with_flags", "append_topleft"],
    table: ["prune_table", "sort_at_path"],
    plot: ["geom_point", "geom_line", "geom_col", "geom_boxplot", "geom_histogram", "labs", "facet_wrap", "theme_minimal"]
  };

  Editor.prototype.openNextMenu = function (id, cx, cy) {
    var self = this;
    var end = this.stackEnd(id);
    var n = this.node(end);
    var st = this.status.nodes[end] || {};
    var kind = n.type === "dataset" ? "data" : st.kind || (this.spec(n) && this.spec(n).cat === "layout" ? "layout" : null);
    if (kind === "listing") kind = "data";
    var items = [];
    var step = function (fn) {
      var spec = self.reg[fn];
      if (!spec) return;
      items.push({ label: spec.label, sub: spec.help, icon: spec.cat, run: function () { self.addNode(self.newNode("call", fn), null, end); } });
    };
    if (kind === "data") {
      items.push({ head: "Make something from these rows" });
      items.push({ label: "Summary table by arm", sub: "Statistics of a few columns, one column per treatment arm", icon: "layout", run: function () { self.recipe("summary", end); } });
      items.push({ label: "Patients per value, by arm", sub: "How many patients have each value (e.g. each adverse event)", icon: "analyze", run: function () { self.recipe("occurrences", end); } });
      items.push({ label: "Listing", sub: "The rows as a listing", icon: "output", run: function () { self.addNode(self.newNode("call", "as_listing"), null, end); } });
      items.push({ label: "Plot", sub: "A graph of these rows", icon: "plot", run: function () { self.recipe("plot", end); } });
      items.push({ head: "Change the rows" });
    } else if (kind === "layout") {
      items.push({ head: "Make the table" });
      items.push({ label: "Build the table", sub: "Apply this layout to rows", icon: "output", run: function () { self.recipe("build", end); } });
      items.push({ head: "Add to the layout" });
    } else if (kind === "table") {
      items.push({ head: "Tidy the table" });
    } else if (kind === "plot") {
      items.push({ head: "Add a layer" });
    }
    (NEXT[kind] || []).forEach(step);
    items.push("-");
    items.push({ label: "More blocks\u2026", icon: "blocks", run: function () { self.showPalette("blocks"); self.palSearch.focus(); } });
    this.openMenu(items, cx, cy);
  };

  // column of a schema: the first of `want` that exists, else the first matching `test`
  function pickCol(cols, want, test) {
    for (var i = 0; i < want.length; i++) if (cols.some(function (c) { return c.n === want[i]; })) return want[i];
    var hit = cols.filter(test || function () { return true; })[0];
    return hit ? hit.n : null;
  }

  Editor.prototype.recipe = function (what, end) {
    var self = this;
    var g = this.g;
    var st = this.status.nodes[end] || {};
    var cols = st.schema && this.schemas[st.schema] ? arr(this.schemas[st.schema]) : this.columnsFor(end);
    var arm = pickCol(cols, ["ACTARM", "ARM", "TRT01A", "TRT01P", "ARMCD"], function (c) { return c.t === "fct" && c.v && c.v.length <= 8; });
    var p = this.pos[end] || { x: 0, y: 0 };
    var below = p.y + (this.heights[end] || 60) + ROW_GAP * 2;
    var call = function (fn, args, extra) {
      var n = { id: nextId(g), order: nextOrder(g), type: "call", fn: fn, args: args || [] };
      Object.keys(extra || {}).forEach(function (k) { n[k] = extra[k]; });
      g.nodes.push(n);
      return n;
    };
    var arg = function (name, value) { return { name: name, value: value }; };
    this.remember();
    var focus = null;
    if (what === "summary" || what === "occurrences") {
      var l1 = call("basic_table", [arg("show_colcounts", "TRUE")], { x: p.x + BLOCK_W + COL_GAP, y: p.y });
      var last = l1;
      if (arm) last = call("split_cols_by", [arg("var", R.str(arm))], { input: last.id, op: "%>%" });
      last = call("add_overall_col", [arg("label", '"All Patients"')], { input: last.id, op: "%>%" });
      if (what === "summary") {
        var vars = ["AGE", "SEX", "RACE"].filter(function (v) { return cols.some(function (c) { return c.n === v; }); });
        if (!vars.length) vars = cols.filter(function (c) { return c.n !== arm && !/ID$/.test(c.n); }).slice(0, 2).map(function (c) { return c.n; });
        last = call("analyze_vars", [arg("vars", R.vec(vars) || 'c("")')], { input: last.id, op: "%>%" });
      } else {
        var term = pickCol(cols, ["AEDECOD", "CMDECOD", "PARAM", "AEBODSYS"], function (c) { return (c.t === "fct" || c.t === "chr") && c.n !== arm; });
        last = call("count_occurrences", [arg("vars", R.str(term || ""))], { input: last.id, op: "%>%" });
      }
      last.name = this.freshName("lyt");
      var tbl = call("build_table", [{ name: "", ref: last.id }, { name: "df", ref: end }], { x: p.x + (BLOCK_W + COL_GAP) / 2, y: below + 220, name: this.freshName("tbl") });
      g.result = tbl.id;
      focus = what === "summary" ? last.id : last.id;
    } else if (what === "plot") {
      var num = pickCol(cols, ["AGE", "AVAL", "BMRKR1"], function (c) { return c.t === "num" || c.t === "int"; });
      var gg = call("ggplot", [arg("", "aes(x = " + R.sym(num || (cols[0] && cols[0].n) || "x") + ")")], {});
      this.computeStacks();
      this.insertAfter(gg.id, gg.id, end);
      var layer = call(num ? "geom_histogram" : "geom_bar", num ? [arg("bins", "30")] : [], {});
      this.computeStacks();
      this.insertAfter(layer.id, layer.id, gg.id);
      g.result = layer.id;
      focus = gg.id;
    } else if (what === "build") {
      var rows = this.sources(end).filter(function (s) { var k = (self.status.nodes[s.id] || {}).kind; return k === "data"; })[0];
      var bt = call("build_table", [{ name: "", ref: end }, { name: "df", ref: rows ? rows.id : null }], { x: p.x, y: below, name: this.freshName("tbl") });
      g.result = bt.id;
      focus = bt.id;
    }
    this.changed();
    if (this.g.nodes.length <= 16) this.fit(true);
    if (focus) { this.select(focus); this.showPane("block"); this.reveal(focus); }
    this.toast(what === "build" ? "Connect the rows to build the table from (the \u201cRows\u201d socket)." : "Added: change any block to adjust it.");
  };

  /* ---- Interaction: palette -------------------------------------------------------------------- */

  Editor.prototype.bindPalette = function () {
    var self = this;
    this.palList.addEventListener("pointerdown", function (e) {
      if (e.button !== 0) return;
      var item = e.target.closest(".bk-pal-item");
      if (!item) return;
      var tpl = item.getAttribute("data-template");
      if (tpl) return; // templates: click only
      e.preventDefault();
      var ghost = null;
      var slot = null;
      var over = false;
      track(e, function (ev) {
        if (!ghost) {
          ghost = h("div", { class: "bk-ghost " + (item.className.match(/bk-cat-\w+/) || [""])[0] }, [
            h("span", { class: "bk-pal-ico", html: $(".bk-pal-ico", item).innerHTML }),
            h("span", { text: $(".bk-pal-label", item).textContent })
          ]);
          doc.body.appendChild(ghost);
        }
        ghost.style.transform = "translate(" + (ev.clientX + 8) + "px," + (ev.clientY + 8) + "px)";
        var r = self.stage.getBoundingClientRect();
        over = ev.clientX > r.left && ev.clientX < r.right && ev.clientY > r.top && ev.clientY < r.bottom;
        var w = self.toWorld(ev.clientX, ev.clientY);
        slot = over && item.hasAttribute("data-fn") ? self.findSlot(w.x - 20, w.y - 10, null) : null;
        self.showSlot(slot);
        ghost.classList.toggle("bk-ghost-off", !over);
      }, function (ev, moved) {
        if (ghost) ghost.remove();
        self.showSlot(null);
        var n = self.paletteNode(item);
        if (!n) return;
        if (!moved) { self.addFromClick(n); return; }
        if (!over) return;
        var w = self.toWorld(ev.clientX, ev.clientY);
        self.addNode(n, { x: w.x - 20, y: w.y - 10 }, slot && slot.after);
      }, item);
    });
    this.palList.addEventListener("click", function (e) {
      var item = e.target.closest(".bk-pal-item");
      if (item && item.getAttribute("data-template")) self.loadTemplate(item.getAttribute("data-template"));
    });
    this.palList.addEventListener("keydown", function (e) {
      var item = e.target.closest(".bk-pal-item");
      if (!item || (e.key !== "Enter" && e.key !== " ")) return;
      e.preventDefault();
      if (item.getAttribute("data-template")) { self.loadTemplate(item.getAttribute("data-template")); return; }
      var n = self.paletteNode(item);
      if (n) self.addFromClick(n);
    });
  };
  Editor.prototype.paletteNode = function (item) {
    if (item.hasAttribute("data-dataset")) return this.newNode("dataset", item.getAttribute("data-dataset"));
    if (item.hasAttribute("data-code")) return this.newNode("code", item.getAttribute("data-code"));
    if (item.hasAttribute("data-fn")) return this.newNode("call", item.getAttribute("data-fn"));
    return null;
  };
  // a click on a palette block: below the selected block, else in view
  Editor.prototype.addFromClick = function (n) {
    var sel = this.selected && this.node(this.selected);
    var spec = this.spec(n);
    // steps (filter, a plot layer, ...) go below the selected block
    if (sel && sel.type !== "code" && spec && spec.input) {
      this.addNode(n, null, this.selected);
      return;
    }
    var r = this.stage.getBoundingClientRect();
    var w = this.toWorld(r.left + r.width / 2 - BLOCK_W / 2 * this.view.k, r.top + r.height / 3);
    var b = this.bounds();
    if (b) w = { x: b.x1, y: b.y2 + ROW_GAP };
    this.addNode(n, w, null);
    this.reveal(n.id);
  };
  // scroll a block into view
  Editor.prototype.reveal = function (id) {
    var p = this.pos[id];
    if (!p) return;
    var r = this.stage.getBoundingClientRect();
    var v = this.view;
    var sx = p.x * v.k + v.x;
    var sy = p.y * v.k + v.y;
    if (sx < 20 || sy < 20 || sx + BLOCK_W * v.k > r.width - 20 || sy + 80 > r.height - 20) {
      v.x = r.width / 2 - (p.x + BLOCK_W / 2) * v.k;
      v.y = r.height / 2 - (p.y + 40) * v.k;
      this.applyView();
    }
  };

  /* ---- Selection and keys ---------------------------------------------------------------------- */

  Editor.prototype.select = function (id) {
    if (id && !this.node(id)) id = null;
    this.selected = id;
    var self = this;
    Object.keys(this.els).forEach(function (k) { self.els[k].classList.toggle("bk-selected", k === id); });
    this.drawWires();
    this.input("selected", id || "");
    this.inspected = null;
    this.renderInspector();
    this.emit("select", id);
  };

  Editor.prototype.bindKeys = function () {
    var self = this;
    // Ctrl/Cmd+V on the canvas (not in a field): R code becomes blocks
    doc.addEventListener("paste", function (e) {
      if (!isShown(self.root) || doc.querySelector(".modal.show") || self.pasteBox) return;
      if (typing(e.target)) return;
      var inApp = self.root.contains(e.target) || e.target === doc.body;
      if (!inApp) return;
      var text = e.clipboardData && e.clipboardData.getData("text/plain");
      if (!text || !String(text).trim()) return;
      e.preventDefault();
      self.openPaste(String(text));
    });
    doc.addEventListener("keydown", function (e) {
      if (!isShown(self.root) || doc.querySelector(".modal.show")) return;
      if (typing(e.target) || (e.target.closest && e.target.closest(".bk-menu"))) return;
      var inApp = self.root.contains(e.target) || e.target === doc.body;
      if (!inApp) return;
      var mod = e.metaKey || e.ctrlKey;
      var k = e.key;
      if (mod && (k === "z" || k === "Z") && !e.shiftKey) { e.preventDefault(); e.stopPropagation(); self.undo(); return; }
      if (mod && ((k === "z" || k === "Z") && e.shiftKey || k === "y" || k === "Y")) { e.preventDefault(); e.stopPropagation(); self.redo(); return; }
      if (mod && (k === "d" || k === "D") && self.selected) { e.preventDefault(); e.stopPropagation(); self.duplicate(self.selected); return; }
      if (mod || e.altKey) return;
      if ((k === "Delete" || k === "Backspace") && self.selected) { e.preventDefault(); e.stopPropagation(); self.deleteNode(self.selected); return; }
      // keyboard: Enter opens the focused block, arrows move along its stack
      var focused = e.target.closest && e.target.closest(".bk-block");
      if (k === "Enter" && focused) { e.preventDefault(); self.select(focused.getAttribute("data-id")); self.showPane("block"); return; }
      if ((k === "ArrowUp" || k === "ArrowDown") && self.selected) {
        var cur = self.node(self.selected);
        var next = k === "ArrowUp" ? (self.snapped[cur.id] ? cur.input : null) : self.childOf[cur.id];
        if (next) {
          e.preventDefault();
          self.select(next);
          if (self.els[next]) self.els[next].focus({ preventScroll: true });
          self.reveal(next);
        }
        return;
      }
      if (k === "Escape" && self.closePop()) { e.stopPropagation(); return; }
      if (k === "Escape" && self.selected) { e.stopPropagation(); self.select(null); self.showPane("result"); return; }
      if (k === "Escape" && self.root.classList.contains("bk-full")) { e.stopPropagation(); self.toggleFull(false); }
    }, true);
  };

  /* ---- Inspector: the settings of the selected block ------------------------------------------ */

  Editor.prototype.argSpec = function (n, i) {
    var spec = this.spec(n);
    if (!spec) return null;
    var m = matchArgs(n, spec);
    var f = null;
    Object.keys(m.map).forEach(function (k) { if (m.map[k] === i) f = k; });
    if (f === null && m.dots.indexOf(i) >= 0) f = "...";
    return arr(spec.args).filter(function (x) { return x.name === f; })[0] || null;
  };

  // Columns the block can refer to: its input's columns; for table layouts,
  // the rows of the Build table block they go to; else every dataset's
  Editor.prototype.columnsFor = function (id) {
    var self = this;
    var seen = {};
    var schemaOf = function (nid) {
      var st = self.status.nodes[nid];
      return st && st.schema && self.schemas[st.schema] ? arr(self.schemas[st.schema]) : null;
    };
    var fromNode = function (nid, depth) {
      if (!nid || seen[nid] || depth > 30) return null;
      seen[nid] = true;
      var n = self.node(nid);
      if (!n) return null;
      var s = schemaOf(nid);
      if (s && n.id !== id) return s;
      // a ggplot2 layer or a layout step: look further up / down
      if (n.input) {
        var up = fromNode(n.input, depth + 1);
        if (up) return up;
      }
      var spec = self.spec(n);
      var m = matchArgs(n, spec);
      var dataArg = ["df", "data", "x", ".data", "dataset"].map(function (f) { return m.map[f]; }).filter(function (i) { return i !== undefined; })[0];
      if (dataArg === undefined && n.args && n.args[0] && n.args[0].ref && !n.args[0].name) dataArg = 0;
      if (dataArg !== undefined && n.args[dataArg].ref) {
        var s2 = schemaOf(n.args[dataArg].ref) || fromNode(n.args[dataArg].ref, depth + 1);
        if (s2) return s2;
      }
      // layouts: the rows of the tables built from them
      var users = self.g.nodes.filter(function (u) { return (u.args || []).some(function (a) { return a.ref === nid; }) || u.input === nid; });
      for (var i = 0; i < users.length; i++) {
        var u = users[i];
        if (u.fn === "build_table") {
          var mu = matchArgs(u, self.spec(u));
          var di = mu.map.df;
          if (di !== undefined && u.args[di].ref) { var s3 = schemaOf(u.args[di].ref); if (s3) return s3; }
        } else if (u.input === nid) {
          var s4 = fromNode(u.id, depth + 1);
          if (s4) return s4;
        }
      }
      return null;
    };
    var cols = fromNode(id, 0);
    if (cols) return cols;
    // every dataset's columns
    var all = {};
    var out = [];
    this.g.nodes.forEach(function (n) {
      if (n.type !== "dataset") return;
      arr(schemaOf(n.id)).forEach(function (c) { if (!all[c.n]) { all[c.n] = true; out.push(c); } });
    });
    return out;
  };

  Editor.prototype.renderInspector = function (soft) {
    var box = this.inspector;
    if (!box) return;
    if (soft && box.contains(doc.activeElement) && this.inspected === this.selected) return;
    if (this.fromInspector && this.inspected === this.selected) return;
    this.inspected = this.selected;
    box.innerHTML = "";
    var n = this.selected && this.node(this.selected);
    if (!n) {
      box.appendChild(h("div", { class: "bk-insp-empty" }, [
        h("div", { class: "bk-insp-empty-ico", html: icon("blocks", 28) }),
        h("p", { text: "Select a block to change its settings and see what it produces." })
      ]));
      return;
    }
    var self = this;
    var info = this.info(n);
    var st = this.status.nodes[n.id] || {};
    var head = h("div", { class: "bk-insp-head bk-cat-" + info.cat }, [
      h("span", { class: "bk-insp-ico", html: icon(info.cat, 18) }),
      h("div", { class: "bk-insp-titles" }, [
        h("div", { class: "bk-insp-title", text: info.label }),
        info.fn ? h("code", { class: "bk-insp-fn", text: info.fn + "()" }) : null
      ]),
      n.name ? h("span", { class: "bk-var", title: "Saved as " + n.name + " in the code", text: n.name }) : null
    ]);
    box.appendChild(head);
    var help = info.spec ? info.spec.help : n.type === "dataset" ? "The rows of " + (this.dsLabels[n.dataset] || n.dataset) + ", after the app's filters." : n.type === "code" ? "R code run as it is. It can create variables that other blocks use." : n.type === "expr" ? "A value computed with R code." : "A function call: its settings are R code.";
    box.appendChild(h("p", { class: "bk-insp-help", text: help }));
    if (n.note) box.appendChild(h("p", { class: "bk-insp-note", text: n.note }));

    var form = h("div", { class: "bk-form" });
    box.appendChild(form);
    if (n.type === "dataset") this.formDataset(form, n);
    else if (n.type === "code" || n.type === "expr") this.formCode(form, n);
    else this.formCall(form, n);

    // actions
    var acts = h("div", { class: "bk-insp-actions" });
    var act = function (ic, label, fn, cls) { acts.appendChild(h("button", { type: "button", class: "bk-btn bk-btn-sm" + (cls ? " " + cls : ""), html: icon(ic) + "<span>" + label + "</span>", onclick: fn })); };
    if (n.input) act("bypass", n.off ? "Turn on" : "Bypass", function () { self.toggleOff(n.id); self.inspected = null; self.renderInspector(); }, n.off ? "bk-on" : "");
    act("star", this.status.result === n.id ? "Is the result" : "Show as result", function () { self.setResult(n.id); }, this.status.result === n.id ? "bk-on" : "");
    act("copy", "Duplicate", function () { self.duplicate(n.id); });
    act("trash", "Delete", function () { self.deleteNode(n.id); }, "bk-danger");
    box.appendChild(acts);

    box.appendChild(h("div", { class: "bk-insp-status" }));
    this.renderInspectorStatus();
  };

  // the status part of the inspector (updated on its own while editing)
  Editor.prototype.renderInspectorStatus = function () {
    var box = this.inspector && $(".bk-insp-status", this.inspector);
    var n = this.selected && this.node(this.selected);
    if (!box || !n) return;
    var st = this.status.nodes[n.id] || {};
    var line = this.statusLine(n);
    box.innerHTML = "";
    box.appendChild(h("div", { class: "bk-insp-out" }, [
      h("span", { class: "bk-insp-out-label", text: "Output of this block" }),
      h("span", { class: "bk-insp-out-status " + line.cls, html: line.html }),
      st.ms !== undefined ? h("span", { class: "bk-insp-ms", text: st.ms + " ms" }) : null
    ]));
    if (st.state === "error" || st.state === "missing") box.appendChild(h("pre", { class: "bk-insp-error", text: st.error }));
  };

  // run a change made in the inspector (without rebuilding it)
  Editor.prototype.edit = function (fn) {
    this.remember();
    fn();
    this.fromInspector = true;
    try { this.changed(); } finally { this.fromInspector = false; }
  };
  Editor.prototype.setFormal = function (id, formal, value) {
    var self = this;
    this.edit(function () {
      var n = self.node(id);
      var m = matchArgs(n, self.spec(n));
      var i = m.map[formal];
      n.args = n.args || [];
      if (value === null || value === undefined || value === "") {
        if (i !== undefined) n.args.splice(i, 1);
      } else if (i !== undefined) {
        n.args[i].value = value;
        delete n.args[i].ref;
      } else {
        n.args.push({ name: formal, value: value });
      }
    });
  };
  Editor.prototype.setDots = function (id, items) {
    var self = this;
    this.edit(function () {
      var n = self.node(id);
      var m = matchArgs(n, self.spec(n));
      var args = n.args || [];
      var keep = args.filter(function (a, i) { return m.dots.indexOf(i) < 0; });
      var before = m.dots.length ? args.slice(0, m.dots[0]).filter(function (a, i) { return m.dots.indexOf(i) < 0; }).length : keep.length;
      n.args = keep.slice(0, before).concat(items.map(function (it) { return { name: it.name || "", value: it.value }; }), keep.slice(before));
    });
  };

  function field(label, control, help, cls) {
    return h("div", { class: "bk-field" + (cls ? " " + cls : "") }, [
      h("label", { class: "bk-field-label", text: label }),
      control,
      help ? h("div", { class: "bk-field-help", text: help }) : null
    ]);
  }
  function colOption(c) {
    var label = c.n + (c.l && c.l !== c.n ? " \u2014 " + c.l : "");
    return h("option", { value: c.n, text: label.length > 70 ? label.slice(0, 69) + "\u2026" : label, title: c.l || c.n });
  }
  function colSelect(cols, current, onchange, placeholder) {
    var s = h("select", { class: "bk-input bk-select" });
    s.appendChild(h("option", { value: "", text: placeholder || "Choose a column\u2026" }));
    var found = false;
    cols.forEach(function (c) { var o = colOption(c); if (c.n === current) { o.selected = true; found = true; } s.appendChild(o); });
    if (current && !found) s.appendChild(h("option", { value: current, text: current + " (not in the data)", selected: true }));
    s.addEventListener("change", function () { onchange(s.value); });
    return s;
  }
  function codeInput(value, onchange, multiline, placeholder) {
    var el = h(multiline ? "textarea" : "input", { class: "bk-input bk-code-input", spellcheck: "false", autocomplete: "off", placeholder: placeholder || "R code" });
    if (!multiline) el.type = "text";
    el.value = value === null || value === undefined ? "" : value;
    if (multiline) el.rows = Math.min(12, Math.max(2, String(el.value).split("\n").length + 1));
    el.addEventListener("change", function () { onchange(el.value); });
    el.addEventListener("keydown", function (e) {
      if (e.key === "Enter" && (e.metaKey || e.ctrlKey || !multiline)) { e.preventDefault(); el.blur(); }
    });
    return el;
  }
  function textInput(value, onchange, placeholder) {
    var el = h("input", { type: "text", class: "bk-input", placeholder: placeholder || "" });
    el.value = value || "";
    el.addEventListener("change", function () { onchange(el.value); });
    el.addEventListener("keydown", function (e) { if (e.key === "Enter") { e.preventDefault(); el.blur(); } });
    return el;
  }
  function removeBtn(fn, title) {
    return h("button", { type: "button", class: "bk-x", title: title || "Remove", "aria-label": title || "Remove", html: icon("close", 14), onclick: fn });
  }
  function addBtn(label, fn) {
    return h("button", { type: "button", class: "bk-add", html: icon("plus", 14) + "<span>" + esc(label) + "</span>", onclick: fn });
  }
  function chip(label, onremove, extra) {
    return h("span", { class: "bk-tag" }, [h("span", { text: label }), extra || null, onremove ? removeBtn(onremove) : null]);
  }

  Editor.prototype.formDataset = function (form, n) {
    var self = this;
    var s = h("select", { class: "bk-input bk-select" });
    var names = this.datasets.length ? this.datasets.map(function (d) { return d.name; }) : Object.keys(this.dsLabels);
    if (names.indexOf(n.dataset) < 0) names.unshift(n.dataset);
    names.forEach(function (nm) { s.appendChild(h("option", { value: nm, text: nm + " \u2014 " + (self.dsLabels[nm] || ""), selected: nm === n.dataset })); });
    s.addEventListener("change", function () { self.edit(function () { self.node(n.id).dataset = s.value; }); });
    form.appendChild(field("Dataset", s));
  };

  Editor.prototype.formCode = function (form, n) {
    var self = this;
    form.appendChild(field(n.type === "code" ? "R code" : "R expression", codeInput(n.code, function (v) {
      self.edit(function () { self.node(n.id).code = v; });
    }, true), n.type === "expr" ? "Its value is saved as " + (n.name || "a variable") + "." : "Runs as it is, in order."));
  };

  Editor.prototype.formCall = function (form, n) {
    var self = this;
    var spec = this.spec(n);
    var m = matchArgs(n, spec);
    var shown = {};
    var more = h("div", { class: "bk-more-body" });
    var specArgs = arr(spec && spec.args);
    specArgs.forEach(function (sa) {
      var target = sa.advanced ? more : form;
      if (sa.name === "...") {
        m.dots.forEach(function (i) { shown[i] = true; });
        target.appendChild(self.dotsEditor(n, sa, m));
        return;
      }
      var i = m.map[sa.name];
      if (i !== undefined) shown[i] = true;
      var el = self.argEditor(n, sa, i === undefined ? null : n.args[i], function (v) { self.setFormal(n.id, sa.name, v); });
      target.appendChild(el);
    });
    // everything else: R code, one row per argument
    var others = (n.args || []).map(function (a, i) { return i; }).filter(function (i) { return !shown[i]; });
    var generic = spec ? more : form;
    others.forEach(function (i) {
      var a = n.args[i];
      var f = null;
      Object.keys(m.map).forEach(function (k) { if (m.map[k] === i) f = k; });
      var row = h("div", { class: "bk-generic" }, [
        h("input", {
          type: "text", class: "bk-input bk-generic-name", value: a.name || "", placeholder: f ? f : "(unnamed)", title: "Name of the setting",
          onchange: function (e) { var v = e.target.value.trim(); self.edit(function () { self.node(n.id).args[i].name = v; }); }
        }),
        h("span", { class: "bk-eq", text: "=" }),
        a.ref ? h("span", { class: "bk-generic-ref", text: "\u2190 " + self.sourceLabel(a.ref) }) : codeInput(a.value, function (v) { self.edit(function () { self.node(n.id).args[i].value = v; }); }, String(a.value || "").length > 40),
        removeBtn(function () { self.edit(function () { self.node(n.id).args.splice(i, 1); }); self.inspected = null; self.renderInspector(); })
      ]);
      generic.appendChild(row);
    });
    generic.appendChild(addBtn("Add a setting", function () {
      self.edit(function () { var nn = self.node(n.id); nn.args = nn.args || []; nn.args.push({ name: "name", value: "NULL" }); });
      self.inspected = null;
      self.renderInspector();
    }));
    if (spec) {
      var det = h("details", { class: "bk-more" }, [h("summary", { text: "More settings" }), more]);
      if (others.length || this.openMore) det.open = true;
      det.addEventListener("toggle", function () { self.openMore = det.open; });
      form.appendChild(det);
    }
  };

  // editor of one argument
  Editor.prototype.argEditor = function (n, sa, a, commit) {
    var self = this;
    var value = a ? a.value : null;
    var ed = sa.editor;
    var label = sa.label;
    if (a && a.ref && ed !== "data") ed = "data";
    switch (ed) {
      case "column": {
        var s = R.unstr(value);
        if (value !== null && s === null) break;
        return field(label, colSelect(this.columnsFor(n.id), s, function (v) { commit(v ? R.str(v) : null); }), sa.help);
      }
      case "columns": {
        var vals = R.strings(value);
        if (vals === null) break;
        return field(label, this.multiColumns(n, vals, function (list) { commit(R.vec(list)); }), sa.help);
      }
      case "strings": {
        var sv = R.strings(value);
        if (sv === null) break;
        return field(label, this.stringsEditor(sv, arr(sa.choices), function (list) { commit(R.vec(list)); }), sa.help);
      }
      case "named": {
        var pairs = R.named(value);
        if (pairs === null) break;
        return field(label, this.pairsEditor(pairs, function (p) { commit(R.namedVec(p)); }, "name", "text"), sa.help);
      }
      case "text": {
        var t = R.unstr(value);
        if (value !== null && t === null) break;
        return field(label, textInput(t, function (v) { commit(v === "" ? null : R.str(v)); }, sa.placeholder), sa.help);
      }
      case "number": {
        var num = R.num(value);
        if (value !== null && num === null) break;
        var ni = h("input", { type: "number", class: "bk-input bk-input-num", step: "any" });
        ni.value = value === null ? "" : String(value).replace(/L$/, "");
        ni.addEventListener("change", function () { commit(ni.value === "" ? null : String(value || "").match(/L$/) ? String(Math.round(+ni.value)) + "L" : ni.value); });
        return field(label, ni, sa.help);
      }
      case "bool": {
        var b = R.bool(value);
        if (value !== null && b === null) break;
        var seg = h("div", { class: "bk-seg", role: "group" });
        [[null, "Default"], [true, "Yes"], [false, "No"]].forEach(function (o) {
          seg.appendChild(h("button", {
            type: "button", class: o[0] === b ? "active" : "", text: o[1],
            onclick: function () { commit(o[0] === null ? null : o[0] ? "TRUE" : "FALSE"); $$("button", seg).forEach(function (x) { x.classList.remove("active"); }); this.classList.add("active"); }
          }));
        });
        return field(label, seg, sa.help);
      }
      case "choice": {
        var cs = h("select", { class: "bk-input bk-select" });
        cs.appendChild(h("option", { value: "", text: "Default" }));
        var choices = arr(sa.choices);
        var found = false;
        choices.forEach(function (c) { var o = h("option", { value: c, text: R.show(c) }); if (c === value) { o.selected = true; found = true; } cs.appendChild(o); });
        if (value !== null && !found) cs.appendChild(h("option", { value: value, text: R.show(value), selected: true }));
        cs.appendChild(h("option", { value: "\u0001code", text: "R code\u2026" }));
        cs.addEventListener("change", function () {
          if (cs.value === "\u0001code") {
            var c = codeInput(value, function (v) { commit(v.trim() || null); });
            cs.replaceWith(c);
            c.focus();
            return;
          }
          commit(cs.value || null);
        });
        return field(label, cs, sa.help);
      }
      case "aes": {
        var it = value === null ? [] : R.items(value, "aes");
        if (it === null) break;
        var pairs2 = it.map(function (x, k) { var p = R.splitNamed(x); return [p[0] || (k === 0 ? "x" : k === 1 ? "y" : ""), p[1]]; });
        return field(label, this.aesEditor(n, pairs2, function (p) {
          var items = p.filter(function (q) { return q[0] && q[1]; }).map(function (q) { return q[0] + " = " + q[1]; });
          commit("aes(" + items.join(", ") + ")");
        }), sa.help);
      }
      case "data": {
        if (a && !a.ref && a.value !== null && a.value !== undefined) {
          // an R expression (e.g. count(adsl, ARM)): edit it as code
          return field(label, codeInput(value, function (v) { commit(v.trim() || null); }, String(value || "").length > 44), "R code. Or drag a wire onto its socket to use a block instead.", "bk-field-code");
        }
        var ds = h("select", { class: "bk-input bk-select" });
        ds.appendChild(h("option", { value: "", text: "\u2014 not connected \u2014" }));
        var cur = a && a.ref ? this.stackEnd(a.ref) : null;
        this.sources(n.id).forEach(function (src) { ds.appendChild(h("option", { value: src.id, text: src.label + "  \u00b7  " + src.sub, selected: src.id === cur })); });
        ds.addEventListener("change", function () {
          var m = matchArgs(n, self.spec(n));
          var i = m.map[sa.name];
          if (ds.value) { self.connect(ds.value, n.id, i === undefined ? "f:" + sa.name : "i:" + i); return; }
          if (i !== undefined) self.edit(function () { var nn = self.node(n.id); if (sa.main) { nn.args[i].ref = null; } else nn.args.splice(i, 1); });
        });
        return field(label, ds, sa.help || "Or drag a wire from the round port under a stack.");
      }
      default:
        break;
    }
    // R code
    return field(label, codeInput(value, function (v) { commit(v.trim() || null); }, String(value || "").length > 44), sa.help, "bk-field-code");
  };

  // editors of `...`: conditions, new columns, labels, columns
  Editor.prototype.dotsEditor = function (n, sa, m) {
    var self = this;
    var items = m.dots.map(function (i) { return { name: n.args[i].name || "", value: n.args[i].value, ref: n.args[i].ref }; });
    var box = h("div", { class: "bk-dots" });
    var commit = function (list) { self.setDots(n.id, list.filter(function (x) { return x.value !== null && x.value !== undefined && x.value !== ""; })); };
    var cols = this.columnsFor(n.id);
    var colInfo = function (name) { return cols.filter(function (c) { return c.n === name; })[0]; };

    if (sa.editor === "conditions") {
      var rows = items.map(function (it) { return { raw: it.value, c: parseCondition(it.value) }; });
      var list = h("div", { class: "bk-rows" });
      var write = function () {
        commit(rows.map(function (r) { return { name: "", value: r.c ? writeCondition(r.c) : r.raw }; }));
      };
      var drawRow = function (r, idx) {
        var row = h("div", { class: "bk-cond" });
        if (!r.c) {
          row.appendChild(codeInput(r.raw, function (v) { r.raw = v; r.c = parseCondition(v); write(); redraw(); }, false, 'e.g. AGE >= 65'));
          row.appendChild(removeBtn(function () { rows.splice(idx, 1); write(); redraw(); }));
          return row;
        }
        var c = r.c;
        var info = colInfo(c.col);
        var numeric = info ? (info.t === "num" || info.t === "int") : c.quoted === false;
        var valueCtl = function () {
          if (c.op === "na" || c.op === "notna" || c.op === "true" || c.op === "false") return null;
          var levels = info && info.v ? arr(info.v) : null;
          if (c.op === "in" || c.op === "notin") {
            return self.stringsEditor(c.values, levels || [], function (vals) { c.values = vals; c.quoted = !numeric; write(); }, true);
          }
          if (levels && levels.length) {
            var s = h("select", { class: "bk-input bk-select" });
            s.appendChild(h("option", { value: "", text: "Choose\u2026" }));
            var found = false;
            levels.forEach(function (l) { var o = h("option", { value: l, text: l === "" ? "(empty)" : l }); if (l === c.values[0]) { o.selected = true; found = true; } s.appendChild(o); });
            if (c.values[0] !== undefined && !found) s.appendChild(h("option", { value: c.values[0], text: c.values[0], selected: true }));
            s.addEventListener("change", function () { c.values = [s.value]; c.quoted = true; write(); });
            return s;
          }
          var t = h("input", { type: numeric ? "number" : "text", class: "bk-input", placeholder: "value", step: "any" });
          if (info && info.r && numeric) t.placeholder = arr(info.r).join(" to ");
          t.value = c.values[0] || "";
          t.addEventListener("change", function () { c.values = [t.value]; c.quoted = !numeric; write(); });
          return t;
        };
        var colSel = colSelect(cols, c.col, function (v) {
          c.col = v;
          info = colInfo(v);
          numeric = info ? (info.t === "num" || info.t === "int") : false;
          c.quoted = !numeric;
          if (info && info.t === "lgl" && (c.op === "==" || c.op === "!=")) c.op = "true";
          c.values = [];
          write();
          redraw();
        });
        var opSel = h("select", { class: "bk-input bk-select bk-op" });
        OPS.forEach(function (o) { opSel.appendChild(h("option", { value: o[0], text: o[1], selected: o[0] === c.op })); });
        opSel.addEventListener("change", function () {
          var multi = function (op) { return op === "in" || op === "notin"; };
          if (multi(opSel.value) !== multi(c.op)) c.values = c.values.slice(0, 1);
          c.op = opSel.value;
          write();
          redraw();
        });
        row.appendChild(colSel);
        row.appendChild(opSel);
        var vc = valueCtl();
        if (vc) { vc.classList.add("bk-cond-value"); row.appendChild(vc); }
        row.appendChild(removeBtn(function () { rows.splice(idx, 1); write(); redraw(); }));
        return row;
      };
      var redraw = function () {
        list.innerHTML = "";
        if (!rows.length) list.appendChild(h("div", { class: "bk-rows-empty", text: "No condition: every row is kept." }));
        rows.forEach(function (r, idx) {
          if (idx) list.appendChild(h("div", { class: "bk-and", text: "and" }));
          list.appendChild(drawRow(r, idx));
        });
      };
      redraw();
      box.appendChild(field(sa.label, list));
      box.appendChild(h("div", { class: "bk-add-row" }, [
        addBtn("Add a condition", function () { rows.push({ c: { col: "", op: "==", values: [], quoted: true } }); redraw(); }),
        addBtn("Custom (R code)", function () { rows.push({ raw: "", c: null }); redraw(); })
      ]));
      return box;
    }

    if (sa.editor === "assign" || sa.editor === "labels") {
      var labels = sa.editor === "labels";
      var pairs = items.map(function (it) { return [it.name, labels ? (R.unstr(it.value) !== null ? R.unstr(it.value) : it.value) : it.value]; });
      var editor = labels
        ? this.pairsEditor(pairs, function (p) {
          commit(p.filter(function (q) { return q[0]; }).map(function (q) { return { name: q[0], value: R.str(q[1]) }; }));
        }, "column", "label", cols)
        : this.pairsEditor(pairs, function (p) {
          commit(p.filter(function (q) { return q[0] && q[1]; }).map(function (q) { return { name: q[0], value: q[1] }; }));
        }, "new column", "R code, e.g. AESER == \"Y\"", null, true);
      box.appendChild(field(sa.label, editor, labels ? "" : "Each line adds (or replaces) a column; its value is R code computed on every row."));
      return box;
    }

    if (sa.editor === "symbols") {
      var arrange = n.fn === "arrange";
      var syms = items.map(function (it) {
        var d = /^desc\((.+)\)$/.exec(String(it.value).trim());
        var name = R.unsym(d ? d[1] : it.value);
        return name ? { col: name, desc: !!d } : { raw: it.value, name: it.name };
      });
      var wrap = h("div", { class: "bk-tags" });
      var write2 = function () {
        commit(syms.map(function (s) {
          if (s.raw !== undefined) return { name: s.name || "", value: s.raw };
          return { name: "", value: s.desc ? "desc(" + R.sym(s.col) + ")" : R.sym(s.col) };
        }));
      };
      var redraw2 = function () {
        wrap.innerHTML = "";
        syms.forEach(function (s, k) {
          var extra = arrange && s.raw === undefined ? h("button", {
            type: "button", class: "bk-tag-dir", title: s.desc ? "Largest first (click for smallest first)" : "Smallest first (click for largest first)",
            text: s.desc ? "\u2193" : "\u2191", onclick: function () { s.desc = !s.desc; write2(); redraw2(); }
          }) : null;
          wrap.appendChild(chip(s.raw !== undefined ? s.raw : s.col, function () { syms.splice(k, 1); write2(); redraw2(); }, extra));
        });
        var add = colSelect(cols, "", function (v) { if (v) { syms.push({ col: v, desc: false }); write2(); redraw2(); } }, "+ Add a column");
        add.classList.add("bk-tag-add");
        wrap.appendChild(add);
      };
      redraw2();
      box.appendChild(field(sa.label, wrap));
      return box;
    }

    if (sa.editor === "data") {
      box.appendChild(field(sa.label, h("div", { class: "bk-field-help", text: "Connected on the canvas: drag a wire from a stack to the \u201c+ " + sa.label + "\u201d socket." })));
      return box;
    }

    // code, one line per item
    var lines = h("div", { class: "bk-rows" });
    items.forEach(function (it, k) {
      lines.appendChild(h("div", { class: "bk-generic" }, [
        it.name ? h("span", { class: "bk-generic-name-ro", text: it.name + " =" }) : null,
        it.ref ? h("span", { class: "bk-generic-ref", text: "\u2190 " + self.sourceLabel(it.ref) }) : codeInput(it.value, function (v) { items[k].value = v; commit(items); }),
        removeBtn(function () { items.splice(k, 1); commit(items); self.inspected = null; self.renderInspector(); })
      ]));
    });
    box.appendChild(field(sa.label, lines));
    box.appendChild(addBtn("Add", function () { items.push({ name: "", value: "NULL" }); commit(items); self.inspected = null; self.renderInspector(); }));
    return box;
  };

  // several columns, as tags
  Editor.prototype.multiColumns = function (n, values, commit) {
    var cols = this.columnsFor(n.id);
    var vals = values.slice();
    var wrap = h("div", { class: "bk-tags" });
    var redraw = function () {
      wrap.innerHTML = "";
      vals.forEach(function (v, k) { wrap.appendChild(chip(v, function () { vals.splice(k, 1); commit(vals); redraw(); })); });
      var add = colSelect(cols.filter(function (c) { return vals.indexOf(c.n) < 0; }), "", function (v) { if (v) { vals.push(v); commit(vals); redraw(); } }, "+ Add a column");
      add.classList.add("bk-tag-add");
      wrap.appendChild(add);
    };
    redraw();
    return wrap;
  };
  // several strings, as tags, with suggestions
  Editor.prototype.stringsEditor = function (values, suggestions, commit, valuesOnly) {
    var vals = values.slice();
    var wrap = h("div", { class: "bk-tags" });
    var redraw = function () {
      wrap.innerHTML = "";
      vals.forEach(function (v, k) { wrap.appendChild(chip(v === "" ? "(empty)" : v, function () { vals.splice(k, 1); commit(vals); redraw(); })); });
      var rest = suggestions.filter(function (s) { return vals.indexOf(s) < 0; });
      if (rest.length && rest.length <= 300) {
        var s = h("select", { class: "bk-input bk-select bk-tag-add" });
        s.appendChild(h("option", { value: "\u0001", text: valuesOnly ? "+ Add a value" : "+ Add" }));
        rest.forEach(function (x) { s.appendChild(h("option", { value: x, text: x === "" ? "(empty)" : x })); });
        if (!valuesOnly) s.appendChild(h("option", { value: "\u0002", text: "Other\u2026" }));
        s.addEventListener("change", function () {
          if (s.value === "\u0002") { s.replaceWith(other()); return; }
          if (s.value !== "\u0001") { vals.push(s.value); commit(vals); redraw(); }
        });
        wrap.appendChild(s);
      } else wrap.appendChild(other());
    };
    var other = function () {
      var t = h("input", { type: "text", class: "bk-input bk-tag-add", placeholder: "+ Type and press Enter" });
      t.addEventListener("keydown", function (e) {
        if (e.key === "Enter") { e.preventDefault(); if (t.value !== "") { vals.push(t.value); commit(vals); redraw(); wrap.querySelector("input, select").focus(); } }
      });
      t.addEventListener("change", function () { if (t.value !== "") { vals.push(t.value); commit(vals); redraw(); } });
      return t;
    };
    redraw();
    return wrap;
  };
  // name = value rows
  Editor.prototype.pairsEditor = function (pairs, commit, keyLabel, valueLabel, keyColumns, valueIsCode) {
    var rows = pairs.map(function (p) { return p.slice(); });
    var wrap = h("div", { class: "bk-rows" });
    var redraw = function () {
      wrap.innerHTML = "";
      rows.forEach(function (p, k) {
        var key = keyColumns
          ? colSelect(keyColumns, p[0], function (v) { p[0] = v; commit(rows); })
          : textInput(p[0], function (v) { p[0] = v.trim(); commit(rows); }, keyLabel);
        key.classList.add("bk-pair-key");
        var val = valueIsCode ? codeInput(p[1], function (v) { p[1] = v; commit(rows); }, false, valueLabel) : textInput(p[1], function (v) { p[1] = v; commit(rows); }, valueLabel);
        wrap.appendChild(h("div", { class: "bk-pair" }, [key, h("span", { class: "bk-eq", text: "=" }), val, removeBtn(function () { rows.splice(k, 1); commit(rows); redraw(); })]));
      });
      wrap.appendChild(addBtn("Add", function () { rows.push(["", ""]); redraw(); var ins = $$("input, select", wrap); if (ins.length) ins[ins.length - 2].focus(); }));
    };
    redraw();
    return wrap;
  };
  Editor.prototype.aesEditor = function (n, pairs, commit) {
    var cols = this.columnsFor(n.id);
    var rows = pairs.map(function (p) { return p.slice(); });
    var aes = ["x", "y", "colour", "fill", "group", "shape", "size", "linetype", "label", "ymin", "ymax"];
    var wrap = h("div", { class: "bk-rows" });
    var redraw = function () {
      wrap.innerHTML = "";
      rows.forEach(function (p, k) {
        var a = h("select", { class: "bk-input bk-select bk-pair-key" });
        aes.concat(aes.indexOf(p[0]) < 0 && p[0] ? [p[0]] : []).forEach(function (x) { a.appendChild(h("option", { value: x, text: x, selected: x === p[0] })); });
        a.addEventListener("change", function () { p[0] = a.value; commit(rows); });
        var sym = R.unsym(p[1]);
        var val = sym !== null || !p[1] ? colSelect(cols, sym, function (v) { p[1] = v ? R.sym(v) : ""; commit(rows); }) : codeInput(p[1], function (v) { p[1] = v; commit(rows); });
        wrap.appendChild(h("div", { class: "bk-pair" }, [a, h("span", { class: "bk-eq", text: "=" }), val, removeBtn(function () { rows.splice(k, 1); commit(rows); redraw(); })]));
      });
      wrap.appendChild(addBtn("Add", function () {
        var used = rows.map(function (r) { return r[0]; });
        rows.push([aes.filter(function (x) { return used.indexOf(x) < 0; })[0] || "colour", ""]);
        redraw();
      }));
    };
    redraw();
    return wrap;
  };

  /* ---- Messages from R ------------------------------------------------------------------------- */

  Editor.prototype.onData = function (msg) {
    this.datasets = arr(msg.datasets);
    if (this.palTab === "blocks") this.fillPalette();
  };

  Editor.prototype.onGraph = function (msg) {
    var g = msg.graph || {};
    var fromCode = msg.source === "code";
    if (fromCode) {
      // the code was read: close the editor or the dialog it came from
      this.pendingCode = null;
      this.closePaste();
      this.stopEditCode();
    }
    var count = arr(g.nodes).filter(function (n) { return n.type !== "dataset"; }).length;
    if (msg.mode === "add" && this.g.nodes.length) {
      this.addGraph(g, msg.implicit);
      this.emit("graph", g);
      this.toast("Added " + count + (count === 1 ? " block" : " blocks") + " to the canvas: every block is editable. Undo (" + (isMac ? "\u2318" : "Ctrl+") + "Z) takes them back.");
      return;
    }
    this.setGraph(g, msg.implicit);
    this.emit("graph", g);
    if (this.tutorial) return;
    if (fromCode) this.toast("Your code is now " + count + (count === 1 ? " block" : " blocks") + ": every block is editable.");
    else this.toast("Loaded " + (g.template ? g.template + " \u00b7 " : "") + (g.title || "the template") + ": every block is editable.");
  };

  // Add a graph (pasted code) to the canvas: new ids, and names that do not
  // clash with the canvas's (renamed in the new blocks' code too); the
  // datasets already on the canvas are reused
  Editor.prototype.addGraph = function (g, implicit) {
    var self = this;
    this.remember();
    var cur = this.g;
    var ids = {};
    var num = parseInt(nextId(cur).slice(1), 10);
    var order = nextOrder(cur);
    var taken = {};
    cur.nodes.forEach(function (n) { if (n.name) taken[n.name] = true; });
    var dsNode = {};
    cur.nodes.forEach(function (n) { if (n.type === "dataset" && !dsNode[n.dataset]) dsNode[n.dataset] = n.id; });
    var incoming = arr(g.nodes);
    // 1. ids: datasets on the canvas already are reused, others are new
    incoming.forEach(function (n) {
      if (n.type === "dataset" && dsNode[n.dataset]) ids[n.id] = dsNode[n.dataset];
      else ids[n.id] = "n" + (num++);
    });
    // 2. names that clash: adsl -> adsl_2. In the new blocks' code, a name
    // means the new variable only after the code that defines it; before,
    // it still means the canvas's
    var rename = {};
    var defOrder = {};
    incoming.forEach(function (n) {
      if (!n.name || !taken[n.name]) return;
      var o = typeof n.order === "number" ? n.order : 0;
      if (defOrder[n.name] === undefined || o < defOrder[n.name]) defOrder[n.name] = o;
      if (rename[n.name]) return;
      var k = 2;
      while (taken[n.name + "_" + k]) k++;
      rename[n.name] = n.name + "_" + k;
      taken[rename[n.name]] = true;
    });
    var renameCode = function (code, at) {
      if (code === null || code === undefined || !Object.keys(rename).length) return code;
      // not inside strings or `backticks`, nor after $ or @
      return String(code).split(/("(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|`[^`]*`)/).map(function (part, i) {
        if (i % 2) return part;
        return part.replace(/(^|[^A-Za-z0-9._$@])([A-Za-z.][A-Za-z0-9._]*)(?![A-Za-z0-9._]|\s*=[^=])/g, function (m, pre, word) {
          return rename[word] && at > defOrder[word] ? pre + rename[word] : m;
        });
      }).join("");
    };
    var added = [];
    incoming.forEach(function (n) {
      if (n.type === "dataset" && dsNode[n.dataset]) return;
      var c = JSON.parse(JSON.stringify(n));
      c.id = ids[n.id];
      if (c.input) c.input = ids[c.input] || c.input;
      if (c.name && rename[c.name]) c.name = rename[c.name];
      if (c.type === "call") {
        c.args = arr(c.args).map(function (a) {
          var o = { name: a.name || "" };
          if (a.ref !== undefined && a.ref !== null) o.ref = ids[a.ref] || a.ref; else o.value = renameCode(a.value, n.order);
          return o;
        });
      }
      if (c.code !== undefined) c.code = renameCode(c.code, n.order);
      c.order = order + (typeof n.order === "number" ? n.order : 0);
      delete c.x;
      delete c.y;
      cur.nodes.push(c);
      added.push(c.id);
    });
    cur.packages = arr(cur.packages).concat(arr(g.packages).filter(function (p) { return arr(cur.packages).indexOf(p) < 0; }));
    if (!cur.result && g.result) cur.result = ids[g.result] || null;
    // what the new blocks use implicitly, for the layout
    if (implicit && !Array.isArray(implicit)) {
      Object.keys(implicit).forEach(function (id) {
        var nid = ids[id];
        if (!nid) return;
        self.status.nodes[nid] = self.status.nodes[nid] || {};
        self.status.nodes[nid].implicit = arr(implicit[id]).map(function (x) { return ids[x] || x; });
      });
    }
    this.render();
    this.arrange(false);
    this.changed();
    if (added.length) {
      this.select(added[added.length - 1]);
      this.reveal(added[0]);
    }
    this.fit(true);
  };

  // load a graph; `implicit`: its implicit dependencies (by id), for the layout
  Editor.prototype.setGraph = function (g, implicit) {
    if (this.g.nodes.length) this.remember();
    g.packages = arr(g.packages);
    g.nodes = arr(g.nodes).map(function (n) {
      if (n.type === "call") n.args = arr(n.args).map(function (a) { return { name: a.name || "", ref: a.ref, value: a.value }; }).map(function (a) {
        if (a.ref === undefined || a.ref === null) delete a.ref; else delete a.value;
        return a;
      });
      delete n.x;
      delete n.y;
      return n;
    });
    this.g = { version: 1, packages: g.packages, nodes: g.nodes, result: g.result || null, template: g.template || null, title: g.title || null };
    var nodes = {};
    if (implicit && !Array.isArray(implicit)) Object.keys(implicit).forEach(function (id) { nodes[id] = { implicit: arr(implicit[id]) }; });
    this.status = { nodes: nodes, rev: -1 };
    this.selected = null;
    this.input("selected", "");
    this.render();
    this.arrange(true);
    this.fit(false);
    this.baseJson = JSON.stringify(this.semantic());
    this.save();
    this.sendGraph();
    this.showPane("result");
  };

  Editor.prototype.onStatus = function (msg) {
    var self = this;
    var schemas = msg.schemas && !Array.isArray(msg.schemas) ? msg.schemas : {};
    Object.keys(schemas).forEach(function (k) { self.schemas[k] = schemas[k]; });
    var nodes = msg.nodes && !Array.isArray(msg.nodes) ? msg.nodes : {};
    this.status = { rev: msg.rev, nodes: nodes, result: msg.result || null, code: msg.code || "" };
    if (msg.rev >= this.rev) this.running = false;
    if (msg.error) this.toast("The canvas could not be run: " + msg.error);
    this.render();
    this.renderInspectorStatus();
    this.renderCode();
    this.renderResultHead();
    this.emit("status");
  };

  Editor.prototype.renderResultHead = function () {
    var el = this.resultHead;
    if (!el) return;
    var id = this.status.result;
    var n = id && this.node(id);
    if (!n) { el.innerHTML = ""; return; }
    var self = this;
    var info = this.info(n);
    var line = this.statusLine(n);
    el.innerHTML = "";
    el.appendChild(h("div", { class: "bk-result-title" }, [
      h("span", { class: "bk-insp-ico bk-cat-" + info.cat, html: icon(info.cat) }),
      h("span", { text: this.g.title || info.label }),
      this.g.template ? h("span", { class: "bk-tpl-chip", text: this.g.template }) : null
    ]));
    el.appendChild(h("div", { class: "bk-result-sub" }, [
      h("span", { text: "From the block " }),
      h("button", { type: "button", class: "bk-link", text: info.label + (n.name ? " (" + n.name + ")" : ""), onclick: function () { self.select(id); self.reveal(id); self.showPane("block"); } }),
      h("span", { class: "bk-result-status " + line.cls, html: " \u00b7 " + line.html })
    ]));
  };

  /* ---- Templates, new canvas, saving ---------------------------------------------------------------- */

  Editor.prototype.loadTemplate = function (id) {
    this.input("template", id, true);
  };
  Editor.prototype.newCanvas = function () {
    if (!this.g.nodes.length) return;
    this.remember();
    this.g = this.emptyGraph();
    this.select(null);
    this.changed();
    this.toast("Empty canvas. Undo (" + (isMac ? "\u2318" : "Ctrl+") + "Z) brings the blocks back.");
  };

  Editor.prototype.storeKey = function () { return "blockr:canvas:" + (this.ns || "main"); };
  Editor.prototype.save = function () {
    try {
      window.localStorage.setItem(this.storeKey(), JSON.stringify({ g: this.g, view: this.view }));
    } catch (e) { /* storage unavailable or full */ }
  };
  Editor.prototype.restore = function () {
    try {
      var raw = window.localStorage.getItem(this.storeKey());
      // before 0.1.3 the canvas was not inside the page (no "main-" in its id)
      if (raw === null && /main-$/.test(this.ns || "")) raw = window.localStorage.getItem("blockr:canvas:" + this.ns.replace(/main-$/, ""));
      var saved = JSON.parse(raw || "null");
      if (saved && saved.g && Array.isArray(saved.g.nodes)) {
        this.g = saved.g;
        if (saved.view && typeof saved.view.k === "number") this.view = saved.view;
        this.applyView();
      }
    } catch (e) { /* ignore */ }
  };

  // Shiny is connected: tell R the editor is ready and send the graph
  Editor.prototype.connected = function () {
    this.input("ready", Date.now(), true);
    this.sentJson = null;
    if (this.g.nodes.length) this.sendGraph();
    this.emit("connected");
  };

  /* ---- Empty state and toolbar ------------------------------------------------------------------------ */

  Editor.prototype.renderEmpty = function () {
    var self = this;
    var empty = !this.g.nodes.length;
    this.emptyEl.hidden = !empty || this.tutorial; // the tutorial's lessons say what to do
    this.root.classList.toggle("bk-is-empty", empty);
    if (!empty || this.emptyEl.childNodes.length) return;
    var cards = h("div", { class: "bk-cards" });
    arr(this.cfg.featured).forEach(function (id) {
      var t = self.templates.filter(function (x) { return x.id === id; })[0];
      if (!t) return;
      cards.appendChild(h("button", {
        type: "button", class: "bk-card", onclick: function () { self.loadTemplate(t.id); }
      }, [
        h("span", { class: "bk-card-top" }, [
          h("span", { class: "bk-card-ico bk-ttype-" + String(t.type).toLowerCase(), html: icon(t.type, 18) }),
          h("span", { class: "bk-card-id", text: t.id })
        ]),
        h("span", { class: "bk-card-title", text: t.title }),
        h("span", { class: "bk-card-sub", text: t.topic + " \u00b7 " + t.type })
      ]));
    });
    this.emptyEl.appendChild(h("div", { class: "bk-empty-inner" }, [
      h("div", { class: "bk-empty-kicker", text: "BlockR canvas" }),
      h("h2", { class: "bk-empty-title", text: "Build a table, a listing or a graph, block by block" }),
      h("p", { class: "bk-empty-lead", text: "Start from a TLG template: its steps appear as blocks you can change, remove or add to, and the result updates as you go. Or drag blocks from the left onto the canvas." }),
      cards,
      h("div", { class: "bk-empty-more" }, [
        h("button", { type: "button", class: "bk-btn", html: icon("template") + "<span>Browse all " + this.templates.length + " templates</span>", onclick: function () { self.showPalette("templates"); } }),
        h("button", { type: "button", class: "bk-btn", html: icon("data") + "<span>Start from a dataset</span>", onclick: function () { self.showPalette("blocks"); } }),
        h("button", { type: "button", class: "bk-btn", title: "Your R code becomes blocks (or press " + (isMac ? "\u2318" : "Ctrl+") + "V here)", html: icon("code") + "<span>Paste R code</span>", onclick: function () { self.openPaste(""); } })
      ])
    ]));
  };

  Editor.prototype.renderTop = function () {
    var g = this.g;
    this.titleEl.innerHTML = "";
    if (g.template) this.titleEl.appendChild(h("span", { class: "bk-tpl-chip", text: g.template }));
    this.titleEl.appendChild(h("span", { class: "bk-title-text", title: "Click to rename", text: g.title || (g.nodes.length ? "Untitled canvas" : "Empty canvas") }));
    if (g.template && this.baseJson && JSON.stringify(this.semantic()) !== this.baseJson) this.titleEl.appendChild(h("span", { class: "bk-edited", text: "edited" }));
    this.undoBtn.disabled = !this.undoStack.length;
    this.redoBtn.disabled = !this.redoStack.length;
    var st = this.status.nodes || {};
    var ids = Object.keys(st);
    var problems = ids.filter(function (k) { return st[k].state === "error" || st[k].state === "missing"; }).length;
    var total = ids.reduce(function (t, k) { return t + (st[k].ms || 0); }, 0);
    var s = this.statusEl;
    if (!g.nodes.length) { s.className = "bk-run"; s.innerHTML = ""; return; }
    this.root.classList.toggle("bk-busy", !!this.running);
    if (this.running) { s.className = "bk-run bk-running"; s.innerHTML = '<span class="bk-spin"></span>Running\u2026'; return; }
    if (problems) { s.className = "bk-run bk-run-error"; s.innerHTML = icon("warning", 14) + (problems === 1 ? "1 block needs attention" : problems + " blocks need attention"); return; }
    s.className = "bk-run bk-run-ok";
    s.innerHTML = icon("check", 14) + "Up to date" + (total ? ' <span class="bk-run-ms">' + (total < 1000 ? total + " ms" : (total / 1000).toFixed(1) + " s") + "</span>" : "");
  };

  /* ---- Boot ------------------------------------------------------------------------------------------- */

  var apps = {};
  function boot() {
    $$(".bk-app").forEach(function (el) {
      if (el._bk) return;
      el.classList.add("bk-booted");
      el._bk = new Editor(el);
      apps[el._bk.ns] = el._bk;
      if (window.Shiny && window.Shiny.shinyapp && window.Shiny.shinyapp.isConnected && window.Shiny.shinyapp.isConnected()) el._bk.connected();
    });
  }
  function route(fn) {
    return function (msg) {
      var app = apps[msg && msg.ns];
      if (!app) { boot(); app = apps[msg && msg.ns]; }
      if (app) app[fn](msg);
    };
  }
  var registered = false;
  function register() {
    if (registered || !window.Shiny || !window.Shiny.addCustomMessageHandler) return;
    registered = true;
    window.Shiny.addCustomMessageHandler("blockr-data", route("onData"));
    window.Shiny.addCustomMessageHandler("blockr-graph", route("onGraph"));
    window.Shiny.addCustomMessageHandler("blockr-status", route("onStatus"));
    window.Shiny.addCustomMessageHandler("blockr-sas", route("onSas"));
    window.Shiny.addCustomMessageHandler("blockr-code-error", route("onCodeError"));
  }
  register();

  /* ---- The page: the canvas and the Tutorial ------------------------------------------------------
     R/canvas.R puts two canvases on the page (.bk-view "main" and
     "tutorial") and the Tutorial button that switches between them. teal's
     "Add to report" / "Show R code" buttons, if this teal version still
     shows them for the canvas, are hidden and the Tutorial button takes
     their place. */

  var TEAL_BUTTON = /^\s*(\+\s*)?(add to report|show r code)\s*$/i;
  function tutorialSeen() {
    try { return window.localStorage.getItem("blockr:tutorial:seen") === "1"; } catch (e) { return true; }
  }
  function paintTutorialButtons(page) {
    var tut = page.getAttribute("data-view") === "tutorial";
    var seen = tutorialSeen();
    $$('[data-bk-page="' + page.id + '"]').forEach(function (b) {
      b.classList.toggle("bk-on", tut);
      b.classList.toggle("bk-new", !tut && !seen);
      b.setAttribute("data-bk-view", tut ? "main" : "tutorial");
      b.setAttribute("aria-pressed", tut ? "true" : "false");
      b.title = tut ? "Back to your canvas: the lessons keep their place" : "Eight short lessons, on a canvas of their own: your canvas stays as it is";
      var label = $(".bk-tutorial-label", b);
      var sub = $(".bk-tutorial-sub", b);
      var ico = $(".bk-tutorial-ico", b);
      if (label) label.textContent = tut ? "Back to my canvas" : "Tutorial";
      if (sub) sub.textContent = tut ? "" : "8 lessons";
      if (sub) sub.hidden = tut;
      if (ico) ico.innerHTML = icon(tut ? "back" : "grad", 18);
    });
  }
  function showView(page, view) {
    if (!page || !$('.bk-view[data-view="' + view + '"]', page)) return;
    // a canvas left in full screen would cover the other one
    $$(".bk-app.bk-full", page).forEach(function (el) { if (el._bk) el._bk.toggleFull(false); });
    page.setAttribute("data-view", view);
    $$(".bk-view", page).forEach(function (v) { v.hidden = v.getAttribute("data-view") !== view; });
    if (view === "tutorial") { try { window.localStorage.setItem("blockr:tutorial:seen", "1"); } catch (e) { /* ignore */ } }
    try { window.sessionStorage.setItem("blockr:view:" + page.id, view); } catch (e) { /* ignore */ }
    paintTutorialButtons(page);
    var el = $('.bk-view[data-view="' + view + '"] .bk-app', page);
    var a = el && el._bk;
    if (a) {
      a.avoidOverlays();
      a.fitHeight();
      a.render();
      if (!a.fitted && a.g.nodes.length) { a.fit(false); a.fitted = true; }
      a.emit("shown");
    }
    if (window.jQuery) window.jQuery(window).trigger("resize");
  }
  // hide teal's buttons next to the canvas; the Tutorial button goes there
  function dockTutorial(page) {
    var isTeal = function (b) { return !b.closest(".bk-page, .tn-header, .modal") && TEAL_BUTTON.test(b.textContent || ""); };
    // buttons outside every tab (a toolbar shared by the modules, if teal
    // draws one): hidden only while the canvas is shown
    var shown = isShown(page);
    $$("button, a").filter(function (b) { return !b.closest(".tab-pane") && isTeal(b); }).forEach(function (b) {
      if (shown && !b.hasAttribute("data-bk-hidden-here")) { b.setAttribute("data-bk-hidden-here", b.style.display || ""); b.style.display = "none"; }
      else if (!shown && b.hasAttribute("data-bk-hidden-here")) { b.style.display = b.getAttribute("data-bk-hidden-here"); b.removeAttribute("data-bk-hidden-here"); }
    });
    if (page.classList.contains("bk-bar-docked")) return;
    var scope = page.closest(".tab-pane") || (page.parentElement && page.parentElement.parentElement);
    if (!scope) return;
    var found = $$("button, a", scope).filter(isTeal);
    if (!found.length) return;
    var hide = function (el) {
      if (el.hasAttribute("data-bk-hidden")) return;
      el.setAttribute("data-bk-hidden", "");
      el.setAttribute("aria-hidden", "true");
      el.style.display = "none";
    };
    // a button alone in its wrapper: the wrapper goes too (not a bar that
    // holds both buttons: the Tutorial button goes there)
    var anchors = found.map(function (b) {
      hide(b);
      var a = b;
      while (a.parentElement && a.parentElement !== scope && !a.parentElement.contains(page) && a.parentElement.children.length === 1) {
        a = a.parentElement;
        hide(a);
      }
      return a;
    });
    var btn = $('[data-bk-page="' + page.id + '"]');
    if (btn && !btn.classList.contains("bk-docked") && anchors[0].parentElement) {
      anchors[0].parentElement.insertBefore(btn, anchors[0]);
      btn.classList.add("bk-docked");
      page.classList.add("bk-bar-docked");
    }
  }
  var pagesReady = {};
  function setupPages() {
    $$(".bk-page").forEach(function (page) {
      if (!pagesReady[page.id]) {
        pagesReady[page.id] = true;
        var saved = null;
        try { saved = window.sessionStorage.getItem("blockr:view:" + page.id); } catch (e) { saved = null; }
        if (saved === "tutorial") showView(page, "tutorial"); else paintTutorialButtons(page);
      }
      dockTutorial(page);
    });
  }
  function showTutorial(on) {
    var page = $(".bk-page");
    if (page) showView(page, on === false ? "main" : "tutorial");
  }

  // teal may draw its buttons after the canvas: look for them now and then
  // (at most twice a second), for the first minute
  var dockUntil = Date.now() + 60000;
  var dockQueued = false;
  function dockSoon() {
    if (dockQueued || Date.now() > dockUntil) return;
    dockQueued = true;
    window.setTimeout(function () { dockQueued = false; setupPages(); }, 500);
  }

  function start() {
    register();
    boot();
    setupPages();
    // the Tutorial button (wherever it is docked)
    doc.addEventListener("click", function (e) {
      var b = e.target.closest && e.target.closest("[data-bk-view]");
      if (!b) return;
      e.preventDefault();
      var page = doc.getElementById(b.getAttribute("data-bk-page")) || b.closest(".bk-page");
      showView(page, b.getAttribute("data-bk-view"));
    });
    if (window.MutationObserver) {
      var queued = false;
      new MutationObserver(function () {
        if (queued) return;
        queued = true;
        window.requestAnimationFrame(function () { queued = false; if ($(".bk-app:not(.bk-booted)")) boot(); dockSoon(); });
      }).observe(doc.body, { childList: true, subtree: true });
    }
    // "Open in canvas" buttons (TLG Catalog pages)
    doc.addEventListener("click", function (e) {
      var b = e.target.closest && e.target.closest("[data-bk-open]");
      if (!b) return;
      e.preventDefault();
      var id = b.getAttribute("data-bk-open");
      var app = Object.keys(apps).map(function (k) { return apps[k]; }).filter(function (a) { return !a.tutorial; })[0];
      if (!app) return;
      app.goTo();
      showView(app.root.closest(".bk-page"), "main");
      app.loadTemplate(id);
    });
    // the canvas tab became visible: draw the wires again (sizes were 0)
    // the page around the canvas changes (a sidebar opens or folds, ...): check
    // again what floats over the canvas, once its animation is over
    var recheck = debounce(function () {
      Object.keys(apps).forEach(function (k) { if (isShown(apps[k].root)) { apps[k].avoidOverlays(); apps[k].fitHeight(); } });
    }, 350);
    doc.addEventListener("click", function (e) { if (!(e.target.closest && e.target.closest(".bk-app"))) recheck(); }, true);
    doc.addEventListener("transitionend", function (e) {
      if (e.target.classList && (e.target.classList.contains("bslib-sidebar-layout") || e.target.classList.contains("sidebar"))) recheck();
    }, true);
    var visible = function () {
      setupPages();
      Object.keys(apps).forEach(function (k) {
        var a = apps[k];
        if (!isShown(a.root)) return;
        // once bslib has set up the sidebar (folding happens only once)
        window.setTimeout(function () { a.foldFilterPanel(); recheck(); }, 600);
        window.setTimeout(recheck, 1400);
        a.avoidOverlays();
        a.fitHeight();
        a.render();
        if (!a.fitted && a.g.nodes.length) { a.fit(false); a.fitted = true; }
      });
    };
    doc.addEventListener("shown.bs.tab", visible);
    window.addEventListener("resize", debounce(function () {
      Object.keys(apps).forEach(function (k) { if (isShown(apps[k].root)) { apps[k].avoidOverlays(); apps[k].fitHeight(); apps[k].drawWires(); } });
    }, 120));
    window.setTimeout(visible, 0);
  }
  Editor.prototype.goTo = function () {
    if (isShown(this.root)) return;
    var pane = this.root.closest(".tab-pane");
    var link = pane && pane.id ? doc.querySelector('[data-bs-target="#' + pane.id + '"], [href="#' + pane.id + '"]') : null;
    if (link) {
      try {
        if (window.bootstrap && window.bootstrap.Tab) { window.bootstrap.Tab.getOrCreateInstance(link).show(); return; }
      } catch (e) { /* fall back */ }
      link.click();
    } else if (window.blockr && window.blockr.navigate) {
      window.blockr.navigate("#/canvas");
    }
  };

  if (doc.readyState === "loading") doc.addEventListener("DOMContentLoaded", start); else start();
  if (window.jQuery) {
    window.jQuery(doc).on("shiny:connected", function () {
      register();
      boot();
      Object.keys(apps).forEach(function (k) { apps[k].connected(); });
    });
  }

  // for tests and power users
  window.BlockR = { apps: apps, R: R, parseCondition: parseCondition, writeCondition: writeCondition, matchArgs: matchArgs, showTutorial: showTutorial };
})();
