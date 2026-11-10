# Previews

Renderings of the BlockR canvas (1400 x 875). Like the earlier previews,
they come from a test bench, not from the full teal app: the page has the
canvas's real markup, stylesheets (`blockr.css`, `blockr_canvas.css`) and
script (`blockr_canvas.js`), and its R side - the package's own
`blockr_graph_from_code()`, `blockr_compile()`, `blockr_run()`, registry and
status messages - runs in WebAssembly (webR) on the bundled ADaM data. tern,
rtables and ggplot2 cannot run there, so table layouts, tables and plots are
computed by stand-ins: the block statuses and data previews are real, the
tables and plots are not shown (in the app, the Result panel shows the real
tern / rtables / rlistings output and ggplot2 plots).

| File | Shows |
|---|---|
| `canvas-start.jpg` | The empty canvas: featured TLG templates, *Browse all 135 templates*, *Start from a dataset*; the palette of blocks |
| `canvas-aet01.jpg` | AET01 (Safety Summary) opened on the canvas: ADSL and ADAE stacks, the list of flags (`aesi_vars`) wired into *Patients with flags*, the layout stack and *Build table*; the *New columns* block selected, its 11 derived flags editable as rows |
| `canvas-template.jpg` | AET02 opened on the canvas: the ADSL and ADAE stacks, the table layout stack, *Build table* with its three wires (row counts on the wires), the ★ result; the *Filter rows* block selected, its conditions in plain language and its output (1,586 rows, 348 removed) |
| `canvas-next.jpg` | From scratch: the **+** under a dataset suggests what to do with its rows (recipes and steps) |
| `canvas-recipe.jpg` | After *Patients per value, by arm* and a *Filter rows* added under ADAE (AESER is Y): the filtered rows flow into the table |
| `canvas-tutorial.jpg` | The Tutorial page: lesson 2 (*Keep only some rows*, Beginner) in progress, steps ticked as they are done, the filter's condition set in plain words |
| `canvas-dark.jpg` | Dark mode (the lifted slate palette): AET01 with the *What is this?* explanation of its ADAE filter (what it does here, what it works on and gives, what uses it) |
