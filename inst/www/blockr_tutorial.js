/*
 * BlockR tutorial: lessons on a canvas of their own (the "Tutorial" page).
 *
 * Eight lessons, from the first click to wiring a table and writing R: each
 * lesson starts from its own canvas and lists steps; a step is ticked as
 * soon as the canvas shows it was done (the checks read the graph, the
 * status of the blocks and what the user opened), so the tutorial follows
 * the user instead of the other way round. "Show me" points at what to use.
 *
 * Lessons are plain data (LESSONS below): a start (an empty canvas, a few
 * blocks, R code or a TLG template) and steps, each with its text, a check
 * and optionally a hint and what to point at. The canvas editor
 * (blockr_canvas.js) attaches the tutorial to a canvas whose configuration
 * says `tutorial: true`.
 */
(function () {
  "use strict";

  var doc = document;

  function h(tag, attrs, children) {
    var node = doc.createElement(tag);
    Object.keys(attrs || {}).forEach(function (k) {
      var v = attrs[k];
      if (v === null || v === undefined || v === false) return;
      if (k === "class") node.className = v;
      else if (k === "html") node.innerHTML = v;
      else if (k === "text") node.textContent = v;
      else if (k.slice(0, 2) === "on") node.addEventListener(k.slice(2), v);
      else node.setAttribute(k, v === true ? "" : v);
    });
    (children || []).forEach(function (c) { if (c) node.appendChild(typeof c === "string" ? doc.createTextNode(c) : c); });
    return node;
  }
  function arr(x) { return x === null || x === undefined ? [] : Array.isArray(x) ? x : [x]; }
  var LEVELS = ["Beginner", "Intermediate", "Advanced"];

  /* ---- Checks: questions about the canvas --------------------------------------------- */

  function Q(ed, tut) { this.ed = ed; this.tut = tut; }
  Q.prototype.calls = function (fn) { return this.ed.g.nodes.filter(function (n) { return n.type === "call" && n.fn === fn; }); };
  Q.prototype.dataset = function (name) { return this.ed.g.nodes.filter(function (n) { return n.type === "dataset" && n.dataset === name; }); };
  Q.prototype.st = function (n) { return (n && this.ed.status.nodes[n.id]) || {}; };
  // is `n` (or a block above it, through inputs) the dataset `name`?
  Q.prototype.below = function (n, name) {
    var seen = {};
    while (n && !seen[n.id]) {
      seen[n.id] = true;
      if (n.type === "dataset" && n.dataset === name) return true;
      var up = this.ed.inputOf(n);
      n = up ? this.ed.node(up) : null;
    }
    return false;
  };
  Q.prototype.conds = function (n) {
    return arr(n.args).map(function (a) { return a.value ? window.BlockR.parseCondition(a.value) : null; }).filter(Boolean);
  };
  Q.prototype.hasCond = function (n, col, op, value) {
    return this.conds(n).some(function (c) {
      return c.col === col && c.op === op && (value === undefined || c.values.map(String).indexOf(String(value)) >= 0);
    });
  };
  // value (R code) of a function's argument, by its formal name
  Q.prototype.arg = function (n, formal) {
    var m = window.BlockR.matchArgs(n, this.ed.spec(n));
    var i = m.map[formal];
    return i === undefined ? null : n.args[i];
  };
  Q.prototype.strings = function (a) { return (a && a.value && window.BlockR.R.strings(a.value)) || []; };
  Q.prototype.dotsSymbols = function (n) {
    var m = window.BlockR.matchArgs(n, this.ed.spec(n));
    return m.dots.map(function (i) {
      var v = String(n.args[i].value || "").replace(/^desc\((.*)\)$/, "$1");
      return window.BlockR.R.unsym(v);
    }).filter(Boolean);
  };
  // did this happen since the step started? (pane, tip, select)
  Q.prototype.did = function (type, test) {
    return this.tut.events.some(function (e) { return e.type === type && (!test || test(e.arg)); });
  };
  Q.prototype.selected = function () { return this.ed.selected ? this.ed.node(this.ed.selected) : null; };
  Q.prototype.memo = function () { return this.tut.memo; };

  // where to point: a palette block, a block on the canvas, a part of the page
  function palette(attr, tab) { return { palette: tab || "blocks", sel: ".bk-pal-item[" + attr + "]" }; }
  // a block by its function (or dataset); `extra`: a part of it; `under`: in the stack of that dataset
  function block(fn, extra, under) { return { block: fn, extra: extra || "", under: under || null }; }

  /* ---- Lessons ----------------------------------------------------------------------------------- */

  var LESSONS = [
    {
      id: "meet", level: 0, title: "Meet the canvas",
      intro: "A canvas holds blocks. Each block does one thing: brings the rows of a dataset, filters them, counts them, builds a table. Let us start with one block.",
      start: { graph: [] },
      steps: [
        {
          text: "In the palette on the left (<b>Blocks</b> tab), click <b>ADSL</b> under <i>Data</i>: the subjects of the study appear on the canvas as a block.",
          check: function (q) { return q.dataset("ADSL").length > 0; },
          show: palette('data-dataset="ADSL"'),
          hint: "The palette is the list on the left. If you do not see it, click the first button of the toolbar."
        },
        {
          text: "Click the <b>ADSL</b> block. Its settings and its rows open on the right, in the <b>Block</b> panel. Its footer says how many rows and columns it has.",
          check: function (q) { var s = q.selected(); return !!s && s.type === "dataset" && s.dataset === "ADSL" && q.ed.pane === "block"; },
          show: block("ADSL")
        },
        {
          text: "Every block has a small <b>?</b> in its top right corner. Click the one of ADSL: it explains what the block does, here.",
          check: function (q) { return q.did("tip", function (a) { return /^block:/.test(a); }); },
          show: block("ADSL", ".bk-tip"),
          hint: "The parts of the page have a ? too: the palette, the canvas (top left), the panel on the right."
        },
        {
          text: "Open the <b>Result</b> tab, at the top of the right panel. The result of the canvas is always shown there.",
          check: function (q) { return q.did("pane", function (a) { return a === "result"; }); },
          show: { sel: '.bk-panel-tab[data-pane="result"]' }
        }
      ]
    },
    {
      id: "filter", level: 0, title: "Keep only some rows",
      intro: "Blocks stacked under each other form a pipeline: each block works on the rows of the block above it.",
      start: { graph: [{ dataset: "ADSL" }], title: "Women aged 40 and over" },
      steps: [
        {
          text: "Click the <b>ADSL</b> block to select it, then click <b>Filter rows</b> in the palette (under <i>Prepare</i>). The filter goes below ADSL, in the same stack.",
          check: function (q) { return q.calls("filter").some(function (n) { return q.below(n, "ADSL"); }); },
          show: palette('data-fn="filter"'),
          hint: "You can also drag Filter rows from the palette and drop it just below the ADSL block."
        },
        {
          text: "In the <b>Block</b> panel, click <b>Add a condition</b> and choose <b>SEX</b> \u00b7 <b>is</b> \u00b7 <b>F</b>. The footer of the filter says how many rows are left, and how many it removed (\u2212).",
          check: function (q) { return q.calls("filter").some(function (n) { return q.hasCond(n, "SEX", "==", "F"); }); },
          show: block("filter"),
          hint: "Select the Filter rows block first: its settings open on the right."
        },
        {
          text: "Add a second condition: <b>AGE</b> \u00b7 <b>\u2265</b> \u00b7 <b>40</b>. A row is kept only if every condition is true.",
          check: function (q) { return q.calls("filter").some(function (n) { return q.hasCond(n, "AGE", ">=", "40"); }); },
          show: block("filter"),
          hint: "Choose the \u2265 operator, then type 40 in the value field and press Enter."
        },
        {
          text: "Skip the filter for a moment: right-click it \u2192 <b>Bypass</b> (or the <b>Bypass</b> button in the Block panel). All 400 rows come back. Then turn it on again.",
          check: function (q) {
            var f = q.calls("filter");
            if (f.some(function (n) { return n.off; })) q.memo().off = true;
            return !!q.memo().off && f.length > 0 && !f.some(function (n) { return n.off; });
          },
          show: block("filter")
        }
      ]
    },
    {
      id: "listing", level: 0, title: "A listing of serious adverse events",
      intro: "A listing shows rows as they are, one per event. You will choose the rows, the columns and their order.",
      start: { graph: [{ dataset: "ADAE" }], title: "Serious adverse events" },
      steps: [
        {
          text: "Add <b>Filter rows</b> below ADAE with the condition <b>AESER</b> \u00b7 <b>is</b> \u00b7 <b>Y</b>: the serious adverse events.",
          check: function (q) { return q.calls("filter").some(function (n) { return q.below(n, "ADAE") && q.hasCond(n, "AESER", "==", "Y"); }); },
          show: palette('data-fn="filter"')
        },
        {
          text: "Below the filter, add <b>Keep columns</b> and choose <b>USUBJID</b>, <b>AEDECOD</b> and <b>AESEV</b> (with <i>+ Add a column</i>).",
          check: function (q) {
            return q.calls("select").some(function (n) {
              var s = q.dotsSymbols(n);
              return q.below(n, "ADAE") && ["USUBJID", "AEDECOD", "AESEV"].every(function (c) { return s.indexOf(c) >= 0; });
            });
          },
          show: palette('data-fn="select"'),
          hint: "Select the Filter rows block first: a block clicked in the palette goes below the selected block."
        },
        {
          text: "Add <b>Order rows</b> by <b>USUBJID</b>: the events of each patient together.",
          check: function (q) { return q.calls("arrange").some(function (n) { return q.below(n, "ADAE") && q.dotsSymbols(n).indexOf("USUBJID") >= 0; }); },
          show: palette('data-fn="arrange"')
        },
        {
          text: "Finish with <b>Listing</b> (under <i>Output</i>) at the bottom of the stack, then open the <b>Result</b> tab: your listing.",
          check: function (q) {
            return q.calls("as_listing").some(function (n) { return q.below(n, "ADAE"); }) && q.did("pane", function (a) { return a === "result"; });
          },
          show: palette('data-fn="as_listing"')
        }
      ]
    },
    {
      id: "table", level: 1, title: "Your first table",
      intro: "A table needs three things: the rows, a layout (its columns and what each row shows) and a Build table block that brings them together. BlockR can add them for you.",
      start: { code: 'adsl <- ADSL %>% filter(SAFFL == "Y")', title: "My first table" },
      steps: [
        {
          text: "Hover the stack and click the <b>+</b> under it, then choose <b>Summary table by arm</b>. BlockR adds the blocks of a table and wires them.",
          check: function (q) {
            return q.calls("analyze_vars").length > 0 && q.calls("build_table").some(function (n) {
              var df = q.arg(n, "df");
              return df && df.ref && q.below(q.ed.node(df.ref), "ADSL");
            });
          },
          show: block("filter", ".bk-add-next"),
          hint: "The + appears under the last block of a stack when the mouse is over it."
        },
        {
          text: "Select the <b>Summary statistics</b> block and add <b>BMRKR1</b> to its <b>Variables</b>.",
          check: function (q) { return q.calls("analyze_vars").some(function (n) { return q.strings(q.arg(n, "vars")).indexOf("BMRKR1") >= 0; }); },
          show: block("analyze_vars")
        },
        {
          text: "Select <b>Columns by</b> and change its column to <b>SEX</b>: one column per sex instead of one per arm.",
          check: function (q) { return q.calls("split_cols_by").some(function (n) { return q.strings(q.arg(n, "var"))[0] === "SEX"; }); },
          show: block("split_cols_by")
        },
        {
          text: "Open the <b>Result</b> tab: your table, computed on the data.",
          check: function (q) {
            var r = q.ed.status.result && q.ed.status.nodes[q.ed.status.result];
            return q.did("pane", function (a) { return a === "result"; }) && !!r && r.kind === "table";
          },
          show: { sel: '.bk-panel-tab[data-pane="result"]' }
        }
      ]
    },
    {
      id: "template", level: 1, title: "Change a template: AET02",
      intro: "Templates are tables, listings and graphs of the TLG Catalog, ready to be changed. This one counts adverse events by body system and preferred term.",
      start: { template: "AET02" },
      steps: [
        {
          text: "Click the <b>?</b> of the <b>Build table</b> block: it brings together the rows, the layout and the column counts.",
          check: function (q) {
            var bt = q.calls("build_table")[0];
            return !!bt && q.did("tip", function (a) { return a === "block:" + bt.id; });
          },
          show: block("build_table", ".bk-tip")
        },
        {
          text: "Select the <b>Filter rows</b> block of the ADAE stack and add <b>AESEV</b> \u00b7 <b>is</b> \u00b7 <b>SEVERE</b>: the table now counts severe events only.",
          check: function (q) { return q.calls("filter").some(function (n) { return q.below(n, "ADAE") && q.hasCond(n, "AESEV", "==", "SEVERE"); }); },
          show: block("filter", "", "ADAE")
        },
        {
          text: "Bypass <b>Drop empty rows</b> (right-click \u2192 Bypass): the rows where every count is zero come back.",
          check: function (q) { return q.calls("prune_table").some(function (n) { return n.off; }); },
          show: block("prune_table")
        },
        {
          text: "Click the title of the canvas, at the top left, and rename it, e.g. <i>Severe adverse events</i>.",
          check: function (q) { var t = q.ed.g.title; return !!t && t !== "Adverse Events"; },
          show: { sel: ".bk-title-text" }
        }
      ]
    },
    {
      id: "wires", level: 2, title: "Wire a table yourself",
      intro: "Here are the rows (ADAE) and a table layout, but nothing brings them together. You will do it with wires: they carry the result of a stack to a block that uses it.",
      start: {
        code: 'adae <- ADAE %>% filter(SAFFL == "Y", ANL01FL == "Y")\nlyt <- basic_table(show_colcounts = TRUE) %>%\n  split_cols_by("ACTARM") %>%\n  count_occurrences(vars = "AEDECOD")',
        title: "Adverse events by arm"
      },
      steps: [
        {
          text: "Drag <b>Build table</b> (under <i>Output</i>) from the palette onto the canvas, below the two stacks.",
          check: function (q) { return q.calls("build_table").length > 0; },
          show: palette('data-fn="build_table"')
        },
        {
          text: "Connect the layout: drag from the round port under the layout stack (below <i>Patients per value</i>) to the <b>Layout</b> socket of Build table.",
          check: function (q) {
            return q.calls("build_table").some(function (n) {
              var a = q.arg(n, "lyt");
              return a && a.ref && q.ed.node(a.ref) && q.ed.node(a.ref).fn === "count_occurrences";
            });
          },
          show: block("count_occurrences", ".bk-port-out"),
          hint: "Or click the Layout socket and choose the layout in the list."
        },
        {
          text: "Connect the rows: drag from the port under the ADAE stack to the <b>Rows</b> socket.",
          check: function (q) {
            return q.calls("build_table").some(function (n) { var a = q.arg(n, "df"); return a && a.ref && q.below(q.ed.node(a.ref), "ADAE"); });
          },
          show: block("filter", ".bk-port-out", "ADAE")
        },
        {
          text: "Percentages need the number of patients in each arm: add <b>ADSL</b>, put <b>Filter rows</b> with <b>SAFFL</b> \u00b7 <b>is</b> \u00b7 <b>Y</b> below it, and wire it to <b>Column counts (N) from</b>.",
          check: function (q) {
            return q.calls("build_table").some(function (n) {
              var a = q.arg(n, "alt_counts_df");
              if (!a || !a.ref) return false;
              var end = q.ed.node(a.ref);
              var ok = q.below(end, "ADSL");
              var cur = end;
              var filtered = false;
              while (cur && !filtered) {
                if (cur.fn === "filter" && q.hasCond(cur, "SAFFL", "==", "Y")) filtered = true;
                var up = q.ed.inputOf(cur);
                cur = up ? q.ed.node(up) : null;
              }
              return ok && filtered;
            });
          },
          show: palette('data-dataset="ADSL"'),
          hint: "Three moves: click ADSL in the palette; with ADSL selected, click Filter rows and set its condition; then drag from the port under that stack to the socket."
        }
      ]
    },
    {
      id: "plot", level: 2, title: "Make a graph",
      intro: "A graph is a stack too: a Plot block says what goes on each axis, and each block below it adds a layer.",
      start: { code: 'adsl <- ADSL %>% filter(SAFFL == "Y")', title: "Age of the patients" },
      steps: [
        {
          text: "Click the <b>+</b> under the stack and choose <b>Plot</b>: a histogram of age.",
          check: function (q) { return q.calls("ggplot").length > 0 && q.calls("geom_histogram").length > 0; },
          show: block("filter", ".bk-add-next")
        },
        {
          text: "Select the <b>Plot</b> block and, in <i>Axes and colours</i>, click <b>Add</b>, then choose <b>fill</b> = <b>SEX</b>: one colour per sex.",
          check: function (q) {
            return q.calls("ggplot").some(function (n) { return /\b(fill|colour|color)\s*=\s*SEX\b/.test(arr(n.args).map(function (a) { return a.value || ""; }).join(" ")); });
          },
          show: block("ggplot")
        },
        {
          text: "Add <b>Small multiples</b> (under <i>Plot layers</i>) below the histogram: one panel per arm (<code>~ARM</code>).",
          check: function (q) { return q.calls("facet_wrap").some(function (n) { return /ARM/.test(arr(n.args).map(function (a) { return a.value || ""; }).join(" ")); }); },
          show: palette('data-fn="facet_wrap"'),
          hint: "Select the histogram block first, then click Small multiples in the palette."
        },
        {
          text: "Add <b>Titles</b> and give the plot a title of your own.",
          check: function (q) {
            return q.calls("labs").some(function (n) { var t = q.arg(n, "title"); var v = t && window.BlockR.R.unstr(t.value); return !!v && v !== "Title"; });
          },
          show: palette('data-fn="labs"')
        }
      ]
    },
    {
      id: "derive", level: 2, title: "Derive your own column",
      intro: "When no block does what you need, a little R does: a new column computed from the others, which every block below can use.",
      start: { graph: [{ dataset: "ADAE" }], title: "Severe events by arm" },
      steps: [
        {
          text: "Add <b>New columns</b> below ADAE. Name the column <b>SEVERE</b> and give it the R code <code>AESEV == \"SEVERE\"</code>: TRUE for a severe event.",
          check: function (q) {
            return q.calls("mutate").some(function (n) {
              return q.below(n, "ADAE") && arr(n.args).some(function (a) { return a.name === "SEVERE" && /AESEV/.test(a.value || ""); });
            });
          },
          show: palette('data-fn="mutate"')
        },
        {
          text: "Below it, add <b>Filter rows</b> with the condition <b>SEVERE</b> \u00b7 <b>is true</b>.",
          check: function (q) {
            return q.calls("filter").some(function (n) { return q.hasCond(n, "SEVERE", "true") || q.hasCond(n, "SEVERE", "==", "TRUE"); });
          },
          show: palette('data-fn="filter"'),
          hint: "The new column SEVERE is in the list of columns of the filter once New columns has run."
        },
        {
          text: "Below, add <b>Count rows</b> by <b>ACTARM</b>: the number of severe events in each arm.",
          check: function (q) { return q.calls("count").some(function (n) { return q.dotsSymbols(n).indexOf("ACTARM") >= 0; }); },
          show: palette('data-fn="count"')
        },
        {
          text: "Open the <b>Code</b> tab: the R code of what you built, line by line. You just wrote R without typing it (almost).",
          check: function (q) { return q.did("pane", function (a) { return a === "code"; }); },
          show: { sel: '.bk-panel-tab[data-pane="code"]' }
        }
      ]
    }
  ];

  /* ---- The tutorial ------------------------------------------------------------------------------- */

  function Tutorial(ed) {
    var self = this;
    this.ed = ed;
    this.q = new Q(ed, this);
    this.key = "blockr:tutorial:" + (ed.ns || "main");
    this.state = this.load();
    this.events = [];
    this.memo = {};
    this.ready = false;
    this.build();
    var again = function () { self.schedule(); };
    ed.on("change", again);
    ed.on("status", again);
    ed.on("select", function (id) { self.events.push({ type: "select", arg: id }); again(); });
    ed.on("pane", function (p) { self.events.push({ type: "pane", arg: p }); again(); });
    ed.on("tip", function (w) { self.events.push({ type: "tip", arg: w }); again(); });
    ed.on("graph", function () { if (self.waiting) { self.waiting = false; self.started(); } });
    // a lesson that starts from R code or a template asks R: again once connected
    ed.on("connected", function () { if (self.waiting && self.request) self.request(); });
    // resume: the canvas was restored (from the browser) with the lesson in progress
    if (this.state.started && LESSONS[this.state.lesson]) {
      this.ready = true;
      this.render();
      this.schedule();
    } else {
      this.startLesson(this.state.lesson || 0);
    }
  }

  Tutorial.prototype.load = function () {
    try { return JSON.parse(window.localStorage.getItem(this.key) || "{}") || {}; } catch (e) { return {}; }
  };
  Tutorial.prototype.save = function () {
    try { window.localStorage.setItem(this.key, JSON.stringify(this.state)); } catch (e) { /* ignore */ }
  };
  Tutorial.prototype.lesson = function () { return LESSONS[this.state.lesson || 0]; };

  Tutorial.prototype.startLesson = function (i) {
    var L = LESSONS[i];
    if (!L) return;
    this.state.lesson = i;
    this.state.step = 0;
    this.state.started = true;
    this.state.finished = false;
    this.events = [];
    this.memo = {};
    this.ready = false;
    this.save();
    var ed = this.ed;
    ed.undoStack = [];
    ed.redoStack = [];
    var s = L.start || {};
    this.request = null;
    if (s.template) {
      this.waiting = true;
      this.request = function () { ed.loadTemplate(s.template); };
      this.request();
    } else if (s.code) {
      this.waiting = true;
      this.request = function () { ed.input("code", JSON.stringify({ code: s.code, title: s.title || L.title }), true); };
      this.request();
    } else {
      var nodes = arr(s.graph).map(function (x, k) { return { id: "n" + (k + 1), type: "dataset", dataset: x.dataset, order: k + 1 }; });
      ed.setGraph({ nodes: nodes, title: s.title || L.title }, null);
      ed.undoStack = [];
      this.started();
    }
    this.render();
  };
  // the lesson's canvas is on screen
  Tutorial.prototype.started = function () {
    this.ready = true;
    this.events = [];
    this.ed.undoStack = [];
    this.ed.renderTop();
    this.ed.showPalette("blocks");
    var self = this;
    window.setTimeout(function () { self.ed.fit(true); }, 60);
    this.render();
    this.schedule();
  };

  Tutorial.prototype.schedule = function () {
    var self = this;
    window.clearTimeout(this.timer);
    this.timer = window.setTimeout(function () { self.evaluate(); }, 60);
  };
  // tick every step that is done, in order
  Tutorial.prototype.evaluate = function () {
    if (!this.ready || this.state.finished) return;
    var L = this.lesson();
    var moved = false;
    while (this.state.step < L.steps.length) {
      var step = L.steps[this.state.step];
      var ok = false;
      try { ok = !!step.check(this.q); } catch (e) { ok = false; }
      if (!ok) break;
      this.state.step += 1;
      this.events = [];
      moved = true;
    }
    if (this.state.step >= L.steps.length) {
      this.state.finished = true;
      this.state.done = this.state.done || {};
      this.state.done[L.id] = true;
    }
    if (moved) {
      this.save();
      this.render(true);
    }
  };

  /* ---- The lesson card --------------------------------------------------------------------------- */

  Tutorial.prototype.build = function () {
    var self = this;
    this.card = h("div", { class: "bk-coach", role: "region", "aria-label": "Tutorial" });
    this.ed.stage.appendChild(this.card);
    this.card.addEventListener("click", function (e) {
      var b = e.target.closest("[data-act]");
      if (!b) return;
      var act = b.getAttribute("data-act");
      if (act === "collapse") self.collapse(!self.collapsed);
      else if (act === "next") self.startLesson((self.state.lesson || 0) + 1);
      else if (act === "prev") self.startLesson(Math.max(0, (self.state.lesson || 0) - 1));
      else if (act === "restart") self.startLesson(self.state.lesson || 0);
      else if (act === "lesson") self.startLesson(parseInt(b.getAttribute("data-i"), 10));
      else if (act === "show") self.showMe();
      else if (act === "hint") { var hint = self.card.querySelector(".bk-coach-hint"); if (hint) hint.hidden = !hint.hidden; }
      else if (act === "menu") self.lessonsMenu(b);
    });
  };
  Tutorial.prototype.collapse = function (on) {
    this.collapsed = on;
    this.render();
  };
  Tutorial.prototype.render = function (celebrate) {
    var self = this;
    var L = this.lesson();
    var i = this.state.lesson || 0;
    var done = this.state.done || {};
    var card = this.card;
    card.className = "bk-coach" + (this.collapsed ? " bk-coach-min" : "") + (this.state.finished ? " bk-coach-done" : "");
    this.ed.root.classList.toggle("bk-coach-folded", !!this.collapsed);
    card.innerHTML = "";
    this.ed.insets.left = this.collapsed ? 0 : 356; // fit() keeps the blocks clear of the card
    var stepNo = Math.min(this.state.step || 0, L.steps.length);
    if (this.collapsed) {
      card.appendChild(h("button", { type: "button", class: "bk-coach-pill", "data-act": "collapse", title: "Show the lesson" }, [
        h("span", { class: "bk-coach-pill-ico", html: "\ud83c\udf93" }),
        h("span", { text: "Lesson " + (i + 1) + "/" + LESSONS.length + " \u00b7 " + (this.state.finished ? "done" : "step " + (stepNo + 1) + "/" + L.steps.length) })
      ]));
      return;
    }
    card.appendChild(h("div", { class: "bk-coach-head" }, [
      h("span", { class: "bk-coach-kicker", text: "Lesson " + (i + 1) + " / " + LESSONS.length }),
      h("span", { class: "bk-level bk-level-" + L.level, text: LEVELS[L.level] }),
      h("button", { type: "button", class: "bk-coach-icon", "data-act": "menu", title: "All lessons", "aria-label": "All lessons", text: "\u2261" }),
      h("button", { type: "button", class: "bk-coach-icon", "data-act": "collapse", title: "Fold the lesson", "aria-label": "Fold the lesson", text: "\u2013" })
    ]));
    card.appendChild(h("h3", { class: "bk-coach-title", text: L.title }));
    card.appendChild(h("p", { class: "bk-coach-intro", text: L.intro }));
    var list = h("ol", { class: "bk-coach-steps" });
    L.steps.forEach(function (st, k) {
      var state = k < stepNo ? "done" : k === stepNo && !self.state.finished ? "now" : "todo";
      var li = h("li", { class: "bk-coach-step bk-" + state + (celebrate && k === stepNo - 1 ? " bk-just" : "") }, [
        h("span", { class: "bk-coach-num", html: state === "done" ? "\u2713" : String(k + 1) }),
        h("div", { class: "bk-coach-text" }, [h("div", { html: st.text })])
      ]);
      if (state === "now") {
        var acts = h("div", { class: "bk-coach-acts" }, [
          st.show ? h("button", { type: "button", class: "bk-btn bk-btn-sm", "data-act": "show", text: "Show me" }) : null,
          st.hint ? h("button", { type: "button", class: "bk-btn bk-btn-sm", "data-act": "hint", text: "Hint" }) : null
        ]);
        li.querySelector(".bk-coach-text").appendChild(acts);
        if (st.hint) li.querySelector(".bk-coach-text").appendChild(h("div", { class: "bk-coach-hint", hidden: true, text: st.hint }));
        if (!self.ready) li.querySelector(".bk-coach-text").appendChild(h("div", { class: "bk-coach-wait", text: "Preparing the canvas\u2026" }));
      }
      list.appendChild(li);
    });
    card.appendChild(list);
    if (this.state.finished) {
      var last = i === LESSONS.length - 1;
      card.appendChild(h("div", { class: "bk-coach-complete" }, [
        h("strong", { text: last ? "You finished the tutorial!" : "Lesson complete" }),
        h("p", { text: last ? "You can now build your own tables, listings and graphs on your canvas (\u201cBack to my canvas\u201d, top right), starting from any TLG template or from your own R code." : "Next: " + LESSONS[i + 1].title + " (" + LEVELS[LESSONS[i + 1].level] + ")." }),
        last ? null : h("button", { type: "button", class: "bk-btn bk-btn-primary bk-btn-sm", "data-act": "next", text: "Next lesson \u2192" })
      ]));
    }
    // progress: one mark per lesson, by level
    var prog = h("div", { class: "bk-coach-progress", "aria-label": "Lessons" });
    LESSONS.forEach(function (x, k) {
      prog.appendChild(h("button", {
        type: "button", class: "bk-coach-dot bk-level-" + x.level + (done[x.id] ? " bk-done" : "") + (k === i ? " bk-here" : ""),
        "data-act": "lesson", "data-i": String(k), title: (k + 1) + ". " + x.title + " (" + LEVELS[x.level] + ")" + (done[x.id] ? " \u2713" : ""),
        "aria-label": "Lesson " + (k + 1) + ": " + x.title
      }));
    });
    card.appendChild(h("div", { class: "bk-coach-foot" }, [
      prog,
      h("div", { class: "bk-coach-nav" }, [
        h("button", { type: "button", class: "bk-link", "data-act": "prev", text: "\u2190 Previous", disabled: i === 0 }),
        h("button", { type: "button", class: "bk-link", "data-act": "restart", text: "Restart lesson" }),
        h("button", { type: "button", class: "bk-link", "data-act": "next", text: "Skip \u2192", disabled: i === LESSONS.length - 1 })
      ])
    ]));
  };

  Tutorial.prototype.lessonsMenu = function (anchor) {
    var self = this;
    var done = this.state.done || {};
    var r = anchor.getBoundingClientRect();
    var items = [];
    LEVELS.forEach(function (lv, k) {
      items.push({ head: lv });
      LESSONS.forEach(function (x, i) {
        if (x.level !== k) return;
        items.push({ label: (i + 1) + ". " + x.title, sub: done[x.id] ? "Done" : x.steps.length + " steps", on: !!done[x.id], icon: done[x.id] ? "check" : "grad", run: function () { self.startLesson(i); } });
      });
    });
    this.ed.openMenu(items, r.left, r.bottom + 6);
  };

  // point at what the current step needs
  Tutorial.prototype.showMe = function () {
    var L = this.lesson();
    var st = L.steps[this.state.step];
    if (!st || !st.show) return;
    var ed = this.ed;
    var s = st.show;
    var el = null;
    if (s.palette) {
      ed.showPalette(s.palette);
      el = ed.palette.querySelector(s.sel);
      if (el) el.scrollIntoView({ block: "nearest" });
    } else if (s.block) {
      var q = this.q;
      var n = ed.g.nodes.filter(function (x) {
        var match = x.fn === s.block || (x.type === "dataset" && x.dataset === s.block);
        return match && (!s.under || q.below(x, s.under));
      })[0];
      if (n) {
        var end = s.extra === ".bk-add-next" || s.extra === ".bk-port-out" ? ed.stackEnd(n.id) : n.id;
        ed.reveal(end);
        var bel = ed.els[end];
        el = bel && s.extra ? bel.querySelector(s.extra) : bel;
        if (el && s.extra === ".bk-add-next") el.classList.add("bk-force");
      }
    } else if (s.sel) {
      el = ed.root.querySelector(s.sel);
    }
    if (!el) { ed.toast("Follow the step's text: it is not on the canvas yet."); return; }
    el.classList.remove("bk-spot");
    void el.offsetWidth;
    el.classList.add("bk-spot");
    window.setTimeout(function () { el.classList.remove("bk-spot"); el.classList.remove("bk-force"); }, 3200);
  };

  window.BlockRTutorial = {
    lessons: LESSONS,
    levels: LEVELS,
    attach: function (ed) { return new Tutorial(ed); }
  };
})();
