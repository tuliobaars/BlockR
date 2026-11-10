# BlockR

<!-- badges: start -->
<!-- badges: end -->

**BlockR** is a drag-and-drop canvas on top of
[teal](https://insightsengineering.github.io/teal/) for people who do not
write R. Pick an output of the
[TLG Catalog](https://insightsengineering.github.io/tlg-catalog/stable/tlg-index.html)
(say *AET02, Adverse Events*) and its R code appears as **blocks**: datasets,
filters, derived columns, the table's columns and rows, statistics, plot
layers. Every block can be changed, skipped, moved or removed, new ones can
be dragged in from a palette, and the result is recomputed as you go. Each
block shows what it produces (how many rows are left after a filter, a
table, a plot), and the R code of the whole canvas is always one click away,
also translated to **SAS**. It works the other way too: paste R code and it
becomes blocks.

BlockR is a fork of *teal_neue*: same data pipeline, same look, same TLG
Catalog and exploratory / safety modules, with the canvas as its home page.

```
   ADSL           ADAE              New table
   Filter rows    Filter rows       Columns by ACTARM
   1,934 rows     1,586 rows  -348  Total column
      |              |              Rows by AEBODSYS
      |              |              Patients per AEDECOD
      |    400       |    1,586           |
      +------+       +------+             |
             v              v             v
            Build table   (N from: adsl, rows: adae, layout: lyt)
            Drop empty rows
            Sort table rows  ★ Result  ->  the AET02 table, live
```

## What changed from teal_neue

- **The canvas is the home page.** It replaces the App Info page and the
  "study at a glance" dashboard.
- **The Efficacy section is gone**: its module group, its intent-to-treat
  filter, the `ADRS_EFF` dataset it used and its template. (The efficacy
  outputs of the TLG Catalog, such as KMG01, remain: they are templates.)
- **The TLG Catalog is one page** (one menu entry instead of 16): all 135
  outputs in a searchable list grouped by topic, each with
  **Open in Canvas**.
- **A Tutorial button** on the canvas (top right, where teal's greyed-out
  *Add to Report* / *Show R Code* buttons were: those are off for the
  canvas): a second canvas with eight lessons, from the first click to
  wiring a table and writing R.
- Menu: Canvas · TLG Catalog · Data Table · Variable Browser ·
  Exploratory Analysis · Safety Analysis.
- Concomitant medications (ADCM) have real medication names (naproxen,
  atorvastatin, amoxicillin, ...) with their WHO ATC classes, instead of
  `medname A_1/3`: see `cm_dictionary()`.
- Renamed: package `BlockR`, functions `blockr_*()`, options `blockr.*`
  (see [NEWS.md](NEWS.md)).

## The canvas

| | |
|---|---|
| **Start** | From a template (featured cards, or all 135 under *Templates*, or *Open in Canvas* on a TLG Catalog page), or from a dataset |
| **Stacks** | A pipeline is a stack of blocks, read top to bottom: *ADAE* → *Filter rows* → *Show missing values*. Each block works on the result of the block above it |
| **Wires** | When a block uses the result of another stack (the rows and the layout of *Build table*, the data of a plot), a wire goes from the round port under that stack to a socket of the block. Row counts travel on the wires. Dashed wires: a block's code mentions another block's result |
| **Edit** | Click a block: its settings open on the right, in plain language: conditions as *column / is / value* rows with the column's values to choose from, columns as tags, labels, statistics, plot axes. Anything else is R code |
| **Add** | Drag a block from the palette between or below blocks, or click it to add it below the selected block. The **+** under a stack suggests what to do next with its result, including recipes that add and wire several blocks at once (*Summary table by arm*, *Patients per value by arm*, *Listing*, *Plot*, *Build the table*) |
| **Change** | Drag a block to move it (the gap closes), shift-drag to take the blocks below it too, drag the first block to move the stack; right-click for *Bypass* (skip the block), *Show as the result*, *Duplicate*, *Delete* |
| **See** | *Result*: the output of the result block (★). *Block*: the settings and output of the selected block (data, table, layout, plot). *Code*: the code of the canvas, updated as you go, in **R or SAS** |
| **Code → blocks** | *Edit* in the Code pane: change the R code, *Draw the blocks* (Ctrl/Cmd+Enter) turns it back into blocks. Or paste R code: `···` → *Paste R code*, the *Paste R code* button of an empty canvas, or Ctrl/Cmd+V on the canvas. Pasted code replaces the canvas or is added to it (names that clash are renamed: `adsl` → `adsl_2`). Code BlockR cannot read is said so, with the line |
| **Keep** | The canvas is saved in the browser; undo / redo (Ctrl/Cmd+Z); rename it (click its title); export and import it as a file; copy or download its R or SAS code; *Show R code* gives the complete script, data loading included |
| **Learn** | Every part of the page (palette, canvas, panels, toolbar) and every block has a small **?** (*What is this?*): for a block, it explains what it does *here*: what it works on, what it gives, what uses it. Hide them in *Appearance* (the sliders in the header) or the canvas's `···` menu |
| **Room** | The canvas fills the window below the header; the palette and the right panel fold away (and the panel resizes); the last toolbar button shows the canvas full screen (Esc leaves) |

### SAS code

The *SAS* switch of the Code pane shows the canvas as SAS code
(`R/blocks_sas.R`), for SAS users and for double programming. It is a
translation, not a compiler, and it says so:

| Blocks | SAS |
|---|---|
| filter, mutate (`ifelse`, `case_when`, `cut`, `paste`, `substr`, `%in%`, ...), select, column labels, missing values | one DATA step |
| order, unique rows | PROC SORT (NODUPKEY) |
| count, summarise, joins (also `left_join(x, count(adsl, ARM))`) | PROC SQL |
| a table layout + Build table | PROC SQL / MEANS with the same columns (and the total column), row groups, patient counts and percentages (N from the column-count data), shown with PROC TRANSPOSE + PRINT |
| a listing | PROC REPORT |
| a plot and its layers | PROC SGPLOT (SGPANEL with small multiples) |
| a Kaplan-Meier plot | PROC LIFETEST |

The datasets come from a library `adam` (set its folder in the LIBNAME at the
top); each block's result is a WORK dataset named as in the R code. Whatever
has no SAS equivalent (tern's models, R code blocks, some layout
statistics) is a `/* TODO */` comment with its R code, so nothing is
silently dropped, and names that only differ by case (`n` and `N`, one name
in SAS) are pointed out. Check the SAS results against the R output.

### The Tutorial

The **Tutorial** button (top right of the canvas, in bright colours) switches
to a second canvas, with its own saved state, and a lesson card; *Back to my
canvas* returns to yours, untouched. The command bar (Ctrl/Cmd+K) has it too.
Eight lessons, increasing in difficulty:

| Level | Lessons |
|---|---|
| Beginner | 1. Meet the canvas · 2. Keep only some rows · 3. A listing of serious adverse events |
| Intermediate | 4. Your first table · 5. Change a template (AET02) |
| Advanced | 6. Wire a table yourself · 7. Make a graph · 8. Derive your own column (a little R) |

Each lesson starts from its own canvas (empty, a dataset, R code or a TLG
template) and lists steps; a step is ticked as soon as the canvas shows it
was done, whatever way the user did it. *Show me* points at what to use;
*Hint* helps when stuck. Progress is saved in the browser.

Every block that has a problem says so on the block itself, in words ("Place
this block below another block", "Connect a block to Rows", or R's error),
and the blocks after it wait; the rest of the canvas keeps working.

### How it works

```
 template code ──parse──► graph of blocks ──edit in the browser──► graph ──compile──► R code
                                                                        └─run block by block──► status of each block
```

- `blockr_graph_from_code()` (`R/blocks_graph.R`) turns R code into a graph:
  pipe chains (`%>%`) and ggplot2 layers (`+`) become stacks of blocks; a
  variable used as a whole argument (`build_table(lyt, df = adae)`) becomes a
  wire; variables used inside larger expressions become dashed dependencies;
  comments become notes on the blocks; other statements become *R code*
  blocks.
- `blockr_compile()` (`R/blocks_compile.R`) writes a graph as R code again:
  stacked blocks become pipe chains, in dependency order, formatted. **For
  every one of the 135 templates, code → blocks → code gives back the same R
  expressions**, and evaluating the graph block by block computes the same
  values (both checked by the tests).
- `blockr_run()` (`R/blocks_run.R`) evaluates the graph one block at a time
  on the canvas's (filtered) data, caching each block's value: changing a
  block only re-runs the blocks after it.
- `R/blocks_registry.R` describes the blocks the canvas knows (friendly
  name, explanation, one-line summary, editor of each setting) for the
  common dplyr, rtables / tern and ggplot2 functions. Any other function is
  still a block, with its settings edited as R code.
- The editor is `inst/www/blockr_canvas.js` (plain JavaScript, no
  dependencies) and `inst/www/blockr_canvas.css` (the app's tokens, so light
  and dark modes follow); the lessons are `inst/www/blockr_tutorial.js`.
- `blockr_sas()` (`R/blocks_sas.R`) writes a graph as SAS code (see above).

The canvas evaluates the R code of its blocks on the server, like any teal
"Show R code": deploy it for trusted users only.

## Installation

```r
# from the project folder (dependencies pinned with renv)
renv::restore()
devtools::install()

# the parquet data source also needs arrow
install.packages("arrow")
```

## Quick start

```r
library(BlockR)

run_app()                                                       # bundled synthetic data
run_app(source = "synthetic", synthetic = list(N = 200, seed = 7))  # fresh synthetic data
run_app(source = "parquet", parquet_dir = "path/to/adam")        # adsl.parquet, adae.parquet, ...
```

In RStudio you can also open `app.R` and press **Run App**.

After the app closes, the analysis datasets (`ADSL`, `ADAE`, ...) are available
in the Global Environment. Without the app:

```r
bk <- load_blockr_data(cache_global = TRUE)
head(ADSL)
```

All settings are R options; see `?blockr_options`.

## Keyboard

On the canvas:

| Keys | Does |
|---|---|
| `Delete` / `Backspace` | Delete the selected block |
| `Ctrl Z`, `Ctrl Shift Z` (`Cmd` on a Mac) | Undo, redo |
| `Ctrl D` | Duplicate the selected block |
| `Esc` | Close an explanation, deselect, leave full screen |
| `Ctrl` + scroll | Zoom |

Everywhere:

| Keys | Does |
|---|---|
| `Ctrl K` / `Cmd K`, `/` | Command bar: jump to any module, TLG output or action |
| `H` | The canvas (or click the logo) |
| `[`  `]` | Previous / next module |
| `T` `A` `D` | Light/dark, appearance panel, cycle spacing |
| `F` `\` | Focus mode, filter panel |
| `C` `P` `L` | Show R code, pins, copy a link to this view |
| `?` | All shortcuts |

## Filters

teal's filter panel still applies to every page (sex and age for all;
safety modules also start on the safety population). On the canvas the
panel is folded at first, because templates choose their population
themselves, with a block (e.g. *Filter rows: SAFFL is Y*); open it with `\`
or its toggle.

## TLG Catalog

The **TLG Catalog** page reproduces the outputs of the
[TLG Catalog](https://insightsengineering.github.io/tlg-catalog/stable/tlg-index.html):
87 tables, 29 listings and 19 graphs, grouped by topic (Adverse Events,
Concomitant Medications, Deaths, Demography, Disclosures, Disposition, ECG,
Efficacy, Exposure, Lab Results, Medical History, Pharmacokinetics, Anti-Drug
Antibodies, Risk Management Plan, Vital Signs, Other Graphs). Pick an output
in the list (search by ID, title or topic); it is computed on the app's
*filtered* data, **Show R code** gives the complete, runnable code, and
**Open in Canvas** turns it into blocks.

- The code follows the catalog (tern, rtables, rlistings, tern.mmrm) adapted
  to this app's datasets; each output links to its catalog page.
- Outputs that need datasets the app does not load (ADPC, ADPP, ADAB, ADMH,
  ADHY) use the *random.cdisc.data* examples, restricted to the filtered
  patients (`tlg_example_data()`); the page says so.
- RBMIT01 needs the *rbmi* package, which is not installed by default; its
  page explains this.

## Look and feel

```r
options(blockr.color_mode = "dark")  # default colour mode: "light" (default), "dark", "auto" (follows the OS)
options(blockr.motion = FALSE)       # no animations
options(blockr.app_cache = FALSE)    # rebuild the app on every run_app()
options(blockr.ggtheme = "bw")       # default ggplot2 theme of the exploratory plots ("minimal")
options(blockr.canvas_height = 0.95) # height of the canvas: at least this share of the window (0.85)
options(blockr.style = FALSE)        # plain teal look of the source templates
```

| What | Where |
|---|---|
| The canvas and the tutorial | `R/canvas.R` (module), `R/blocks_*.R` (graphs, compiler, runner, registry, SAS), `inst/www/blockr_canvas.*` (editor), `inst/www/blockr_tutorial.js` (lessons) |
| Fonts, colours, layout, dark mode | `inst/www/blockr.css` (Bootstrap CSS variables over teal's default theme) |
| Command bar, breadcrumb, deep links, shortcuts, plot and table tools, pins, viewer, colour mode | `inst/www/blockr.js` |
| Module index for the command bar | `R/app_ui.R` (`blockr_index()`) |
| TLG Catalog page | `R/tlg_catalog.R` (module, icons), `R/tlg_entries_*.R` (outputs), `inst/www/tlg_catalog.*` |
| Header, footer, favicon | `R/app_ui.R` |
| Plot defaults | `R/plot_theme.R` (`blockr_ggplot2_args()`, via teal's `teal.ggplot2_args` option) |

## Data pipeline

```
 source ──► ingest_adam() ──► validate_adam() ──► preprocess_adam() ──► cache ──► app
```

| Step | Function | Notes |
|---|---|---|
| Ingestion | `ingest_adam()` | `"bundled"`, `"synthetic"` or `"parquet"` |
| Contract | `validate_adam()`, `check_app_defaults()` | required datasets/columns; values the app pre-selects |
| Pre-processing | `preprocess_adam()` | derivations from the teal.gallery templates |
| Cache | `load_blockr_data()` | memory -> disk -> fresh build; `blockr_clear_cache()` |
| teal data | `build_blockr_data()` | reproducible code + join keys |

## Documentation

- `vignette("blockr")` - getting started: the canvas and a tour of the app
- `vignette("data-sources")` - data sources, the data contract, caching,
  switching to real data
- `vignette("development")` - package layout, the canvas's architecture,
  adding blocks and modules, tests, deployment

## Development

```r
devtools::load_all()
devtools::document()   # man/ and NAMESPACE from roxygen comments
devtools::test()       # test suite
devtools::check()
```

The canvas tests check, for every TLG template, that code → blocks → code
gives the same R expressions, that the block-by-block evaluation computes
the same values, and (with tern installed) that the canvas's result equals
the TLG Catalog's. Quick run of the data tests:
`Sys.setenv(BLOCKR_TEST_SCENARIOS = "bundled")`.

## License

MIT © BlockR authors. Bundled fonts: Inter and JetBrains Mono, SIL Open
Font License 1.1 (`inst/www/fonts/OFL.txt`). Built on the open-source
[teal](https://insightsengineering.github.io/teal/) framework, the
[TLG Catalog](https://insightsengineering.github.io/tlg-catalog/stable/) and
the teal.gallery templates (`inst/templates/`).
