# Block registry: how the canvas presents the functions it knows.
#
# Every call in a graph is a block; a function listed here gets a friendly
# name, a plain-English explanation, a one-line summary and editors suited
# to its arguments (a column picker, a list of conditions, ...). Any other
# function still works as a block: its arguments are edited as R code.
#
# The registry travels to the browser as JSON (blockr_canvas_config()).
#
# Block fields
#   fn       function name as written in the code
#   label    name of the block
#   cat      category: data, prep, layout, analyze, output, plot, value
#   help     what the block does, in plain English
#   summary  one line shown on the block; {arg} is replaced by the
#            argument's value, {arg?text} shows `text` only if arg is set
#   input    TRUE: the block takes the block above it as input (a pipe step)
#   op       "+" for ggplot2 layers (added to a plot), else "%>%"
#   formals  the function's arguments in order (to recognise unnamed ones)
#   args     editors of the arguments (see bk_arg())
#   insert   arguments of a new block taken from the palette
#   palette  shown in the palette?
#   hint     name for the variable holding its result
#   pkg      package to attach for it (tern also attaches rtables); dplyr
#            is always attached by the app's data code
#
# Editors (bk_arg(editor = ...))
#   column     one column, as a string ("ACTARM")
#   columns    several columns, as a character vector (c("AGE", "SEX"))
#   symbols    several columns, unquoted, one argument each (select, arrange)
#   conditions conditions, one argument each (filter)
#   assign     new columns: name = expression (mutate, summarise)
#   labels     column = "label" pairs (var_relabel)
#   text       a string       number   a number       bool   TRUE / FALSE
#   choice     one of `choices` (R code)
#   strings    several strings, e.g. c("unique", "nonunique"), suggestions in `choices`
#   named      name = "text" pairs, e.g. c(unique = "Patients")
#   aes        ggplot2 aesthetics: aes(x = AGE, colour = ARM)
#   data       another block's result (a wire)
#   code       any R expression

#' One argument of a block
#' @noRd
bk_arg <- function(name, label = name, editor = "code", choices = NULL, help = NULL, dots = FALSE,
                   placeholder = NULL, main = FALSE, advanced = FALSE) {
  Filter(Negate(is.null), list(
    name = name, label = label, editor = editor, choices = if (!is.null(choices)) I(choices), help = help,
    dots = if (dots) TRUE, placeholder = placeholder, main = if (main) TRUE, advanced = if (advanced) TRUE
  ))
}

#' One block
#' @noRd
bk_block <- function(fn, label, cat, help, summary = NULL, input = FALSE, op = NULL, formals = NULL,
                     args = list(), insert = NULL, palette = FALSE, hint = NULL, icon = NULL, pkg = NULL) {
  Filter(Negate(is.null), list(
    fn = fn, label = label, cat = cat, help = help, summary = summary,
    input = input, op = op, formals = if (!is.null(formals)) I(formals), args = args,
    insert = insert, palette = palette, hint = hint, icon = icon, pkg = pkg
  ))
}

#' Values of new arguments (R code), e.g. bk_insert(var = '"ARM"')
#' @noRd
bk_insert <- function(...) {
  x <- list(...)
  lapply(seq_along(x), function(i) {
    v <- x[[i]]
    nm <- names(x)[i] %||% ""
    if (is.list(v)) c(list(name = nm), v) else list(name = nm, value = v)
  })
}

#' Packages the blocks need (functions not listed: base R, dplyr, or
#' written with their package, e.g. tidyr::pivot_wider)
#' @noRd
blockr_block_packages <- function() {
  tern <- c(
    "df_explicit_na", "var_relabel", "basic_table", "split_cols_by", "add_overall_col", "split_rows_by",
    "split_cols_by_multivar", "append_topleft", "summarize_num_patients", "count_occurrences",
    "count_patients_with_flags", "analyze_vars", "analyze_vars_in_cols", "summarize_occurrences_by_grade",
    "count_occurrences_by_grade", "summarize_row_groups", "estimate_proportion", "count_abnormal",
    "build_table", "prune_table", "sort_at_path", "g_km"
  )
  ggplot2 <- c(
    "ggplot", "geom_point", "geom_line", "geom_boxplot", "geom_col", "geom_bar", "geom_histogram",
    "geom_errorbar", "stat_summary", "facet_wrap", "labs", "theme_minimal", "theme", "scale_y_log10"
  )
  c(
    stats::setNames(rep("tern", length(tern)), tern),
    stats::setNames(rep("ggplot2", length(ggplot2)), ggplot2),
    c(as_listing = "rlistings")
  )
}

#' Packages a graph needs: its own (library() calls of its code), then those
#' of its blocks
#' @noRd
blockr_graph_packages <- function(graph, registry = NULL) {
  used <- unlist(lapply(graph$nodes, function(n) if (identical(n$type, "call")) registry[[n$fn]]$pkg))
  unique(c(as.character(graph$packages), used))
}

#' Categories of blocks, in palette order
#' @noRd
blockr_categories <- function() {
  list(
    list(id = "data", label = "Data", help = "Where the rows come from"),
    list(id = "prep", label = "Prepare", help = "Filter, derive, join and reshape rows"),
    list(id = "layout", label = "Table layout", help = "Columns and row groups of a table"),
    list(id = "analyze", label = "Statistics", help = "What each cell of a table shows"),
    list(id = "output", label = "Output", help = "Tables, listings and graphs"),
    list(id = "plot", label = "Plot layers", help = "Build a graph layer by layer"),
    list(id = "value", label = "Values", help = "Lists of values used by other blocks"),
    list(id = "code", label = "R code", help = "Anything else, written in R")
  )
}

#' Friendly names of the ADaM datasets
#' @noRd
blockr_dataset_labels <- function() {
  c(
    ADSL = "Subjects", ADAE = "Adverse events", ADAETTE = "AE time to event",
    ADCM = "Concomitant medications", ADEG = "ECG", ADEX = "Exposure",
    ADLB = "Lab results", ADLBPCA = "Lab results, one row per subject",
    ADQS = "Questionnaires", ADRS = "Tumour response", ADTTE = "Time to event",
    ADVS = "Vital signs", ADPC = "PK concentrations", ADPP = "PK parameters",
    ADAB = "Anti-drug antibodies", ADMH = "Medical history", ADHY = "Hy's law"
  )
}

#' The block registry
#' @return Named list of blocks (by function name).
#' @noRd
blockr_registry <- function() {
  cached <- get0("block_registry", envir = the, inherits = FALSE)
  if (!is.null(cached)) return(cached)
  blocks <- c(blockr_blocks_prep(), blockr_blocks_table(), blockr_blocks_output(), blockr_blocks_plot())
  reg <- stats::setNames(blocks, vapply(blocks, `[[`, character(1), "fn"))
  pkgs <- blockr_block_packages()
  for (fn in intersect(names(pkgs), names(reg))) reg[[fn]]$pkg <- pkgs[[fn]]
  assign("block_registry", reg, envir = the)
  reg
}

# ---- Data preparation (dplyr and friends) -------------------------------------

#' @noRd
blockr_blocks_prep <- function() {
  col <- function(name = "", label = "Column", ...) bk_arg(name, label, "column", ...)
  list(
    bk_block(
      "filter", "Filter rows", "prep", input = TRUE, palette = TRUE,
      help = "Keeps only the rows that meet every condition, e.g. the safety population (SAFFL is Y).",
      summary = "Keep rows where {...}",
      formals = c(".data", "...", ".by", ".preserve"),
      args = list(bk_arg("...", "Conditions", "conditions", dots = TRUE, main = TRUE))
    ),
    bk_block(
      "mutate", "New columns", "prep", input = TRUE, palette = TRUE,
      help = "Adds columns computed from the others (or replaces them), e.g. SER = AESER == \"Y\".",
      summary = "Add {...}",
      formals = c(".data", "...", ".by", ".keep", ".before", ".after"),
      args = list(bk_arg("...", "Columns", "assign", dots = TRUE, main = TRUE))
    ),
    bk_block(
      "select", "Keep columns", "prep", input = TRUE, palette = TRUE,
      help = "Keeps only the chosen columns, in that order.",
      summary = "Keep {...}",
      formals = c(".data", "..."),
      args = list(bk_arg("...", "Columns", "symbols", dots = TRUE, main = TRUE))
    ),
    bk_block(
      "arrange", "Order rows", "prep", input = TRUE, palette = TRUE,
      help = "Sorts the rows by the chosen columns (first column first).",
      summary = "Order by {...}",
      formals = c(".data", "...", ".by_group"),
      args = list(bk_arg("...", "Order by", "symbols", dots = TRUE, main = TRUE))
    ),
    bk_block(
      "distinct", "Unique rows", "prep", input = TRUE, palette = TRUE,
      help = "Removes duplicated rows. With columns: one row per combination of their values.",
      summary = "Unique rows{...? of }{?...}",
      formals = c(".data", "...", ".keep_all"),
      args = list(bk_arg("...", "Columns", "symbols", dots = TRUE, main = TRUE))
    ),
    bk_block(
      "left_join", "Add columns from", "prep", input = TRUE, palette = TRUE,
      help = "Adds the columns of another block's rows, matching rows on the key columns (e.g. the subject ID). Every row of the input is kept.",
      summary = "Add columns of {y} by {by}",
      formals = c("x", "y", "by", "copy", "suffix", "...", "keep", "na_matches", "multiple", "unmatched", "relationship"),
      args = list(
        bk_arg("y", "Columns from", "data", main = TRUE),
        bk_arg("by", "Match on", "columns")
      ),
      insert = bk_insert(y = list(ref = NULL), by = 'c("STUDYID", "USUBJID")')
    ),
    bk_block(
      "semi_join", "Keep matching rows", "prep", input = TRUE, palette = FALSE,
      help = "Keeps the rows that have a match in another block's rows.",
      summary = "Keep rows found in {y}",
      formals = c("x", "y", "by"),
      args = list(bk_arg("y", "Found in", "data", main = TRUE), bk_arg("by", "Match on", "columns"))
    ),
    bk_block(
      "bind_rows", "Stack rows", "prep", palette = TRUE,
      help = "Puts the rows of other blocks below these rows.",
      summary = "Stack with {...}",
      formals = c("...", ".id"),
      args = list(bk_arg("...", "Rows of", "data", dots = TRUE, main = TRUE)),
      insert = bk_insert(list(ref = NULL))
    ),
    bk_block(
      "group_by", "Group by", "prep", input = TRUE, palette = TRUE,
      help = "Groups the rows; the next blocks (Summarise, Count, ...) work group by group.",
      summary = "Group by {...}",
      formals = c(".data", "...", ".add", ".drop"),
      args = list(bk_arg("...", "Groups", "symbols", dots = TRUE, main = TRUE))
    ),
    bk_block(
      "summarise", "Summarise", "prep", input = TRUE, palette = TRUE,
      help = "One row per group, with the summaries you define, e.g. MEAN = mean(AVAL).",
      summary = "Summarise {...}",
      formals = c(".data", "...", ".by", ".groups"),
      args = list(bk_arg("...", "Summaries", "assign", dots = TRUE, main = TRUE),
                  bk_arg(".groups", "Then", "choice", choices = c('"drop"', '"keep"', '"drop_last"'), advanced = TRUE)),
      insert = bk_insert(n = "dplyr::n()")
    ),
    bk_block(
      "count", "Count rows", "prep", input = TRUE, palette = TRUE,
      help = "Counts the rows for each combination of the chosen columns.",
      summary = "Count rows{...? by }{?...}",
      formals = c("x", "...", "wt", "sort", "name", ".drop"),
      args = list(bk_arg("...", "By", "symbols", dots = TRUE, main = TRUE),
                  bk_arg("name", "Name of the count column", "text", advanced = TRUE))
    ),
    bk_block("ungroup", "Ungroup", "prep", input = TRUE, help = "Removes the grouping.", summary = "Remove the groups",
             formals = c("x", "...")),
    bk_block(
      "slice", "Pick rows", "prep", input = TRUE,
      help = "Keeps rows by position, e.g. 1 for the first row (of each group).",
      summary = "Keep rows {...}", formals = c(".data", "..."),
      args = list(bk_arg("...", "Rows", "code", dots = TRUE, main = TRUE))
    ),
    bk_block(
      "transmute", "Replace columns", "prep", input = TRUE,
      help = "Keeps only the columns you define from the others.",
      summary = "Make {...}", formals = c(".data", "..."),
      args = list(bk_arg("...", "Columns", "assign", dots = TRUE, main = TRUE))
    ),
    bk_block(
      "pull", "Take one column", "prep", input = TRUE,
      help = "Takes the values of one column.", summary = "Values of {var}", formals = c(".data", "var"),
      args = list(bk_arg("var", "Column", "code", main = TRUE))
    ),
    bk_block(
      "df_explicit_na", "Show missing values", "prep", input = TRUE, palette = TRUE,
      help = "Turns missing values into an explicit <Missing> level and text columns into factors, as tables expect.",
      summary = "Missing values shown as {na_level?!<Missing>}{?na_level}",
      formals = c("data", "omit_columns", "char_as_factor", "logical_as_factor", "na_level"),
      args = list(
        bk_arg("omit_columns", "Leave unchanged", "columns", advanced = TRUE),
        bk_arg("char_as_factor", "Text columns as factors", "bool", advanced = TRUE),
        bk_arg("logical_as_factor", "TRUE/FALSE columns as factors", "bool", advanced = TRUE),
        bk_arg("na_level", "Label of missing values", "text", advanced = TRUE)
      )
    ),
    bk_block(
      "var_relabel", "Column labels", "prep", input = TRUE, palette = TRUE,
      help = "Sets the labels shown for columns in tables and listings.",
      summary = "Label {...}",
      formals = c("x", "..."),
      args = list(bk_arg("...", "Labels", "labels", dots = TRUE, main = TRUE))
    ),
    bk_block(
      "tidyr::pivot_wider", "Rows to columns", "prep", input = TRUE,
      help = "Spreads one column's values into several columns (one per value of another column).",
      summary = "One column per {names_from}, values from {values_from}",
      formals = c("data", "id_cols", "id_expand", "names_from", "names_prefix", "names_sep", "names_glue", "names_sort", "names_vary", "names_expand", "names_repair", "values_from", "values_fill", "values_fn"),
      args = list(bk_arg("names_from", "Column names from", "code", main = TRUE), bk_arg("values_from", "Values from", "code"))
    ),
    bk_block(
      "BlockR::tlg_example_data", "Example dataset", "data", palette = TRUE,
      help = "Synthetic example data (random.cdisc.data) for datasets the app does not load, restricted to the subjects of the connected block.",
      summary = "{1} for the subjects of {adsl}",
      formals = c("dataset", "adsl"),
      args = list(
        bk_arg("dataset", "Dataset", "choice", choices = c('"ADPC"', '"ADPP"', '"ADAB"', '"ADMH"', '"ADHY"'), main = TRUE),
        bk_arg("adsl", "Subjects of", "data")
      ),
      insert = bk_insert(dataset = '"ADMH"'),
      hint = "example"
    ),
    bk_block(
      "grepl", "Text matches", "value",
      help = "TRUE for each value of a text that contains a pattern (a regular expression), e.g. the lab tests whose name contains \"baseline\".",
      summary = "{x} contains \u201c{pattern}\u201d",
      formals = c("pattern", "x", "ignore.case", "perl", "value", "fixed", "useBytes", "invert"),
      args = list(
        bk_arg("pattern", "Pattern", "text", main = TRUE),
        bk_arg("x", "Text", "code", main = TRUE),
        bk_arg("ignore.case", "Ignore upper / lower case", "bool"),
        bk_arg("fixed", "Exact text (not a pattern)", "bool", advanced = TRUE)
      )
    ),
    bk_block(
      "c", "List of values", "value", palette = TRUE,
      help = "A list of values (e.g. column names) that other blocks use.",
      summary = "{...}", formals = c("..."),
      args = list(bk_arg("...", "Values", "code", dots = TRUE, main = TRUE)),
      insert = bk_insert('"A"', '"B"'), hint = "values"
    ),
    bk_block(
      "list", "Named list", "value",
      help = "Named values that other blocks use, e.g. groups of grades.",
      summary = "{...}", formals = c("..."),
      args = list(bk_arg("...", "Items", "code", dots = TRUE, main = TRUE)),
      hint = "groups"
    )
  )
}

# ---- Tables (rtables, tern) ---------------------------------------------------------

#' @noRd
blockr_blocks_table <- function() {
  label_pos <- c('"hidden"', '"visible"', '"topleft"')
  show_labels <- c('"default"', '"visible"', '"hidden"')
  list(
    bk_block(
      "basic_table", "New table", "layout", palette = TRUE,
      help = "Starts a table. Stack blocks below it to choose its columns, rows and statistics; connect it to Build table.",
      summary = "A table{title? \u201c}{?title}{title?\u201d}{show_colcounts? with (N=xx) under each column}",
      formals = c("title", "subtitles", "main_footer", "prov_footer", "show_colcounts", "colcount_format"),
      args = list(
        bk_arg("title", "Title", "text"),
        bk_arg("show_colcounts", "Show (N=xx) under each column", "bool", main = TRUE),
        bk_arg("main_footer", "Footnote", "text"),
        bk_arg("subtitles", "Subtitle", "text", advanced = TRUE),
        bk_arg("colcount_format", "Format of the column counts", "text", advanced = TRUE)
      ),
      insert = bk_insert(show_colcounts = "TRUE"),
      hint = "lyt"
    ),
    bk_block(
      "split_cols_by", "Columns by", "layout", input = TRUE, palette = TRUE,
      help = "One column per value of a variable, usually the treatment arm.",
      summary = "One column per {var}",
      formals = c("lyt", "var", "labels_var", "split_label", "split_fun", "format", "nested", "child_labels", "extra_args", "ref_group"),
      args = list(
        bk_arg("var", "Column", "column", main = TRUE),
        bk_arg("ref_group", "Reference group", "text", advanced = TRUE),
        bk_arg("split_fun", "Which values", "code", advanced = TRUE),
        bk_arg("split_label", "Label", "text", advanced = TRUE)
      ),
      insert = bk_insert(var = '"ARM"')
    ),
    bk_block(
      "add_overall_col", "Total column", "layout", input = TRUE, palette = TRUE,
      help = "Adds a column for all patients together.",
      summary = "Add a \u201c{label}\u201d column",
      formals = c("lyt", "label"),
      args = list(bk_arg("label", "Label", "text", main = TRUE)),
      insert = bk_insert(label = '"All Patients"')
    ),
    bk_block(
      "split_rows_by", "Rows by", "layout", input = TRUE, palette = TRUE,
      help = "Groups the rows below by the values of a variable, e.g. body system. The blocks below it are repeated for each group.",
      summary = "Group rows by {var}",
      formals = c("lyt", "var", "labels_var", "split_label", "split_fun", "parent_name", "format", "na_str", "nested", "child_labels", "label_pos", "indent_mod"),
      args = list(
        bk_arg("var", "Column", "column", main = TRUE),
        bk_arg("split_label", "Label", "text"),
        bk_arg("label_pos", "Show the label", "choice", choices = label_pos),
        bk_arg("split_fun", "Which values", "choice", choices = c("drop_split_levels", "keep_split_levels()", "remove_split_levels()")),
        bk_arg("child_labels", "Group names", "choice", choices = c('"default"', '"visible"', '"hidden"'), advanced = TRUE),
        bk_arg("nested", "Inside the previous group", "bool", advanced = TRUE),
        bk_arg("indent_mod", "Indent", "number", advanced = TRUE),
        bk_arg("labels_var", "Labels from", "column", advanced = TRUE)
      ),
      insert = bk_insert(var = '""', split_fun = "drop_split_levels")
    ),
    bk_block(
      "split_cols_by_multivar", "Columns by variables", "layout", input = TRUE,
      help = "One column per variable.",
      summary = "One column per variable: {vars}",
      formals = c("lyt", "vars", "split_fun", "varlabels"),
      args = list(bk_arg("vars", "Variables", "columns", main = TRUE), bk_arg("varlabels", "Labels", "strings"))
    ),
    bk_block(
      "append_topleft", "Header note", "layout", input = TRUE, palette = TRUE,
      help = "Text in the top-left corner of the table, above the row labels.",
      summary = "Top-left: \u201c{1}\u201d",
      formals = c("lyt", "newlines"),
      args = list(bk_arg("newlines", "Text", "text", main = TRUE)),
      insert = bk_insert('"Label"')
    ),
    bk_block(
      "summarize_num_patients", "Patients and events", "analyze", input = TRUE, palette = TRUE,
      help = "Counts the patients with at least one record and the number of records (e.g. adverse events).",
      summary = "Count patients{.stats?: }{?.stats}",
      formals = c("lyt", "var", "required", "count_by", "unique_count_suffix", "na_str", ".stats", ".formats", ".labels", ".indent_mods"),
      args = list(
        bk_arg("var", "Patient ID", "column", main = TRUE),
        bk_arg(".stats", "Show", "strings", choices = c("unique", "nonunique", "unique_count")),
        bk_arg(".labels", "Row labels", "named")
      ),
      insert = bk_insert(var = '"USUBJID"', .stats = 'c("unique", "nonunique")')
    ),
    bk_block(
      "count_occurrences", "Patients per value", "analyze", input = TRUE, palette = TRUE,
      help = "For each value of a variable (e.g. preferred term), the number and percentage of patients with it.",
      summary = "Patients per {vars}",
      formals = c("lyt", "vars", "id", "drop", "var_labels", "show_labels", "riskdiff", "na_str", "nested", "...", "table_names", ".stats", ".formats", ".labels", ".indent_mods"),
      args = list(
        bk_arg("vars", "Variable", "column", main = TRUE),
        bk_arg("id", "Patient ID", "column", advanced = TRUE),
        bk_arg("var_labels", "Label", "text", advanced = TRUE),
        bk_arg("show_labels", "Show the label", "choice", choices = show_labels, advanced = TRUE),
        bk_arg(".indent_mods", "Indent", "number", advanced = TRUE)
      ),
      insert = bk_insert(vars = '""')
    ),
    bk_block(
      "count_patients_with_flags", "Patients with flags", "analyze", input = TRUE, palette = TRUE,
      help = "For each TRUE/FALSE flag column (e.g. Serious AE), the number and percentage of patients with at least one TRUE.",
      summary = "Patients with {flag_variables}",
      formals = c("lyt", "var", "flag_variables", "flag_labels", "var_labels", "show_labels", "riskdiff", "na_str", "nested", "...", "table_names", ".stats", ".formats", ".indent_mods"),
      args = list(
        bk_arg("var", "Patient ID", "column"),
        bk_arg("flag_variables", "Flags", "columns", main = TRUE),
        bk_arg("var_labels", "Heading", "text"),
        bk_arg("show_labels", "Show the heading", "choice", choices = show_labels),
        bk_arg("denom", "Percent of", "choice", choices = c('"N_col"', '"n"', '"N_s"'), advanced = TRUE),
        bk_arg("table_names", "Internal name", "text", advanced = TRUE)
      ),
      insert = bk_insert(var = '"USUBJID"', flag_variables = 'c("")')
    ),
    bk_block(
      "analyze_vars", "Summary statistics", "analyze", input = TRUE, palette = TRUE,
      help = "Describes variables: n, mean (SD), median and range for numbers; counts and percentages for categories.",
      summary = "Describe {vars}",
      formals = c("lyt", "vars", "var_labels", "na_str", "nested", "...", "na.rm", "show_labels", "table_names", "section_div", ".stats", ".formats", ".labels", ".indent_mods"),
      args = list(
        bk_arg("vars", "Variables", "columns", main = TRUE),
        bk_arg(".stats", "Statistics", "strings", choices = c("n", "mean_sd", "mean", "sd", "median", "range", "min", "max", "quantiles", "iqr", "mean_ci", "count", "count_fraction", "geom_mean")),
        bk_arg("var_labels", "Labels", "strings"),
        bk_arg("show_labels", "Show the labels", "choice", choices = show_labels, advanced = TRUE),
        bk_arg(".labels", "Statistic labels", "named", advanced = TRUE)
      ),
      insert = bk_insert(vars = 'c("")')
    ),
    bk_block(
      "analyze_vars_in_cols", "Statistics as columns", "analyze", input = TRUE,
      help = "Statistics of a variable side by side, one column each (e.g. n, mean, SD).",
      summary = "{.stats} of {vars}",
      formals = c("lyt", "vars", "...", ".stats", ".labels", "row_labels"),
      args = list(
        bk_arg("vars", "Variable", "columns", main = TRUE),
        bk_arg(".stats", "Statistics", "strings", choices = c("n", "mean", "sd", "se", "median", "min", "max", "range", "mean_sd", "geom_mean")),
        bk_arg(".labels", "Labels", "named")
      )
    ),
    bk_block(
      "summarize_occurrences_by_grade", "Patients by grade (group)", "analyze", input = TRUE,
      help = "For the current row group, the patients by their worst grade or intensity.",
      summary = "Patients by worst {var}",
      formals = c("lyt", "var", "id", "grade_groups", "remove_single", "only_grade_groups", "var_labels", "show_labels"),
      args = list(bk_arg("var", "Grade", "column", main = TRUE), bk_arg("grade_groups", "Grade groups", "code"))
    ),
    bk_block(
      "count_occurrences_by_grade", "Patients by grade", "analyze", input = TRUE,
      help = "The patients by their worst grade or intensity.",
      summary = "Patients by worst {var}",
      formals = c("lyt", "var", "id", "grade_groups", "remove_single", "only_grade_groups", "var_labels", "show_labels"),
      args = list(bk_arg("var", "Grade", "column", main = TRUE), bk_arg("grade_groups", "Grade groups", "code"))
    ),
    bk_block(
      "summarize_row_groups", "Group counts", "analyze", input = TRUE,
      help = "Shows counts on the row of each group.",
      summary = "Counts on group rows", formals = c("lyt", "var", "label_fstr"),
      args = list(bk_arg("label_fstr", "Label format", "text"))
    ),
    bk_block(
      "estimate_proportion", "Response rate", "analyze", input = TRUE,
      help = "The proportion of responders with its confidence interval.",
      summary = "Proportion of {vars}", formals = c("lyt", "vars"),
      args = list(bk_arg("vars", "Responder flag", "column", main = TRUE), bk_arg("method", "Interval", "text"))
    ),
    bk_block(
      "count_abnormal", "Abnormal values", "analyze", input = TRUE,
      help = "Patients with low or high values.",
      summary = "Abnormal {var}", formals = c("lyt", "var", "abnormal"),
      args = list(bk_arg("var", "Range indicator", "column", main = TRUE), bk_arg("abnormal", "Abnormal values", "code"))
    )
  )
}

# ---- Outputs ---------------------------------------------------------------------------

#' @noRd
blockr_blocks_output <- function() {
  list(
    bk_block(
      "build_table", "Build table", "output", palette = TRUE,
      help = "Computes the table: the layout applied to the rows. The column counts (N) can come from other rows, e.g. all subjects.",
      summary = "{lyt} applied to {df}{alt_counts_df?, N from }{?alt_counts_df}",
      formals = c("lyt", "df", "alt_counts_df", "col_counts"),
      args = list(
        bk_arg("lyt", "Layout", "data", main = TRUE),
        bk_arg("df", "Rows", "data", main = TRUE),
        bk_arg("alt_counts_df", "Column counts (N) from", "data")
      ),
      insert = bk_insert(lyt = list(ref = NULL), df = list(ref = NULL)),
      hint = "tbl"
    ),
    bk_block(
      "prune_table", "Drop empty rows", "output", input = TRUE, palette = TRUE,
      help = "Removes the rows where every count is zero.",
      summary = "Drop empty rows", formals = c("tt", "prune_func", "stop_depth", "depth"),
      args = list(bk_arg("prune_func", "Rule", "code", advanced = TRUE))
    ),
    bk_block(
      "sort_at_path", "Sort table rows", "output", input = TRUE, palette = TRUE,
      help = "Sorts groups of rows, e.g. body systems by the number of patients, most frequent first.",
      summary = "Sort {path} by {scorefun}",
      formals = c("tt", "path", "scorefun", "decreasing", "na.pos"),
      args = list(
        bk_arg("path", "Rows", "strings", main = TRUE),
        bk_arg("scorefun", "By", "choice", choices = c("cont_n_allcols", "score_occurrences", "cont_n_onecol(1)")),
        bk_arg("decreasing", "Largest first", "bool")
      ),
      insert = bk_insert(path = 'c("")', scorefun = "cont_n_allcols")
    ),
    bk_block(
      "as_listing", "Listing", "output", input = TRUE, palette = TRUE,
      help = "Shows the rows as a listing: key columns (e.g. arm, patient) are shown once per group.",
      summary = "Listing by {key_cols}",
      formals = c("df", "key_cols", "disp_cols", "non_disp_cols", "unique_rows"),
      args = list(
        bk_arg("key_cols", "Key columns", "columns", main = TRUE),
        bk_arg("disp_cols", "Other columns", "columns"),
        bk_arg("main_title", "Title", "text"),
        bk_arg("unique_rows", "Remove duplicates", "bool", advanced = TRUE)
      ),
      insert = bk_insert(key_cols = 'c("USUBJID")'),
      hint = "lst"
    ),
    bk_block(
      "g_km", "Kaplan-Meier plot", "output", palette = FALSE,
      help = "Survival curves by group, with the numbers at risk.",
      summary = "Kaplan-Meier curves of {df}", formals = c("df", "variables"),
      args = list(bk_arg("df", "Rows", "data", main = TRUE), bk_arg("variables", "Variables", "code")),
      hint = "plot"
    )
  )
}

# ---- Plots (ggplot2) -------------------------------------------------------------------

#' @noRd
blockr_blocks_plot <- function() {
  # a layer added to the plot above; its summary: its settings (NULL) or a template
  layer <- function(fn, label, help, summary = NULL, args = list(), palette = TRUE, insert = NULL,
                    formals = c("mapping", "data", "stat", "position", "...")) {
    bk_block(fn, label, "plot", input = TRUE, op = "+", help = help, summary = summary,
             args = args, palette = palette, insert = insert, formals = formals)
  }
  list(
    bk_block(
      "ggplot", "Plot", "plot", input = TRUE, palette = TRUE,
      help = "Starts a graph of the rows above it: choose what goes on each axis, then add layers (points, lines, boxes...) below.",
      summary = "Plot {mapping}",
      formals = c("data", "mapping"),
      args = list(bk_arg("mapping", "Axes and colours", "aes", main = TRUE)),
      insert = bk_insert("aes(x = AGE)"),
      hint = "plot"
    ),
    layer("geom_point", "Points", "A point per row.", args = list(bk_arg("size", "Size", "number"), bk_arg("alpha", "Opacity", "number"))),
    layer("geom_line", "Lines", "Lines joining the rows, in order of x.", args = list(bk_arg("linewidth", "Width", "number"), bk_arg("alpha", "Opacity", "number"))),
    layer("geom_boxplot", "Box plot", "Median, quartiles and outliers per group.", args = list(bk_arg("width", "Width", "number"))),
    layer("geom_col", "Bars", "A bar per row, as high as its y value.", args = list(bk_arg("width", "Width", "number"))),
    layer("geom_bar", "Bar counts", "A bar per value of x, as high as its number of rows."),
    layer("geom_histogram", "Histogram", "The distribution of x in bins.", args = list(bk_arg("bins", "Bins", "number")), insert = bk_insert(bins = "30")),
    layer("geom_errorbar", "Error bars", "Vertical intervals (ymin to ymax)."),
    layer("stat_summary", "Summary layer", "A summary (e.g. the mean) per value of x.",
          args = list(bk_arg("fun", "Summary", "code"), bk_arg("geom", "Drawn as", "text"))),
    layer("facet_wrap", "Small multiples", "One panel per value of a variable.", summary = "One panel per {1}",
          formals = c("facets", "nrow", "ncol", "scales"),
          args = list(bk_arg("facets", "Panels by", "code", main = TRUE), bk_arg("scales", "Axes", "choice", choices = c('"fixed"', '"free"', '"free_x"', '"free_y"'))),
          insert = bk_insert("~ARM")),
    layer("labs", "Titles", "The title and the names of the axes.", summary = "{title?\u201c}{?title}{title?\u201d}{title?!Axis and legend names}",
          formals = c("...", "title", "subtitle", "caption", "tag", "x", "y", "colour", "fill"),
          args = list(bk_arg("title", "Title", "text", main = TRUE), bk_arg("x", "x axis", "text"), bk_arg("y", "y axis", "text"),
                      bk_arg("colour", "Colour legend", "text"), bk_arg("fill", "Fill legend", "text")),
          insert = bk_insert(title = '"Title"')),
    layer("theme_minimal", "Minimal theme", "A clean look without background.", args = list(bk_arg("base_size", "Text size", "number")),
          formals = c("base_size", "base_family")),
    layer("theme", "Theme tweaks", "Fine-tunes the look (legend position, text, ...).", palette = FALSE),
    layer("scale_y_log10", "Log scale (y)", "Logarithmic y axis.")
  )
}

#' The registry for the browser: blocks, categories, dataset names
#' @noRd
blockr_registry_client <- function() {
  list(blocks = unname(blockr_registry()), categories = blockr_categories(), datasets = as.list(blockr_dataset_labels()))
}
