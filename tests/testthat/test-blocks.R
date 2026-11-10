# Block graphs: code -> blocks -> code, evaluation block by block, registry.

catalog <- tlg_catalog()

strip_library <- function(exprs) {
  Filter(function(s) !(is.call(s) && identical(s[[1]], as.name("library"))), as.list(exprs))
}
ids_of <- function(g) vapply(g$nodes, `[[`, character(1), "id")
node_of <- function(g, id) g$nodes[[match(id, ids_of(g))]]
first_node <- function(g, fn) Filter(function(n) identical(n$fn, fn), g$nodes)[[1]]

# A tiny pipe, so that the runner tests need no package
test_parent <- function(...) {
  env <- new.env(parent = globalenv())
  env[["%>%"]] <- function(lhs, rhs) {
    rhs <- substitute(rhs)
    if (is.name(rhs)) rhs <- as.call(list(rhs))
    rhs <- as.call(c(rhs[[1]], quote(.lhs), as.list(rhs)[-1]))
    eval(rhs, list(.lhs = lhs), parent.frame())
  }
  list2env(list(...), envir = env)
  env
}

# ---- Code -> blocks -> code ----------------------------------------------------

test_that("every TLG template becomes blocks and back into the same code", {
  for (e in catalog) {
    g <- blockr_graph_from_code(e$code)
    out <- blockr_compile(g)
    expect_length(out$errors, 0)
    expect_identical(
      strip_library(parse(text = out$code, keep.source = FALSE)),
      strip_library(parse(text = e$code, keep.source = FALSE)),
      info = e$id
    )
    expect_identical(g$packages, unique(vapply(
      Filter(function(s) is.call(s) && identical(s[[1]], as.name("library")), as.list(parse(text = e$code, keep.source = FALSE))),
      function(s) as.character(s[[2]]), character(1)
    )), info = e$id)
  }
})

test_that("block by block, every template computes the same values as its code", {
  # Expand both forms symbolically (each variable replaced by its expression)
  # and compare what each named variable ends up being.
  expand <- function(stmts) {
    subst <- function(e, map) {
      if (is.name(e)) return(if (!is.null(map[[as.character(e)]])) map[[as.character(e)]] else e)
      if (!is.call(e)) return(e)
      l <- as.list(e)
      for (i in seq_along(l)) if (!identical(l[[i]], quote(expr = ))) l[i] <- list(subst(l[[i]], map))
      as.call(l)
    }
    map <- list()
    trace <- list()
    for (s in strip_library(stmts)) {
      target <- blockr_assignment_target(s)
      if (!is.null(target)) {
        map[[target]] <- subst(s[[3]], map)
        if (!startsWith(target, ".bk_")) trace[[length(trace) + 1L]] <- list(target, map[[target]])
      } else {
        for (v in blockr_assigned_names(s)) map[[v]] <- NULL
        trace[[length(trace) + 1L]] <- list("<code>", subst(s, map))
      }
    }
    trace
  }
  for (e in catalog) {
    g <- blockr_graph_from_code(e$code)
    clean <- parse(text = blockr_compile(g)$code, keep.source = FALSE)
    debug <- unlist(lapply(blockr_compile(g, "debug")$steps, `[[`, "exprs"), recursive = FALSE)
    expect_identical(expand(debug), expand(clean), info = e$id)
  }
})

test_that("pipes become stacks, whole-argument variables become wires, comments become notes", {
  code <- '
library(tern)

# the safety population
adsl <- ADSL %>% filter(SAFFL == "Y")
lyt <- basic_table() %>% split_cols_by("ARM") %>% analyze_vars(vars = c("AGE", "SEX"))
result <- build_table(lyt, df = adsl)
'
  g <- blockr_graph_from_code(code)
  expect_identical(g$packages, "tern")
  types <- vapply(g$nodes, `[[`, character(1), "type")
  expect_identical(unname(types), c("dataset", "call", "call", "call", "call", "call"))
  adsl <- first_node(g, "filter")
  expect_identical(node_of(g, adsl$input)$dataset, "ADSL")
  expect_identical(adsl$name, "adsl")
  expect_identical(adsl$note, "the safety population")
  expect_identical(adsl$args, list(list(name = "", value = 'SAFFL == "Y"')))
  # the layout: three blocks, each taking the one above as input
  split <- first_node(g, "split_cols_by")
  expect_identical(node_of(g, split$input)$fn, "basic_table")
  expect_identical(split$op, "%>%")
  expect_identical(first_node(g, "analyze_vars")$name, "lyt")
  # build_table's arguments are wires to the two stacks
  bt <- first_node(g, "build_table")
  expect_identical(bt$args[[1]], list(name = "", ref = first_node(g, "analyze_vars")$id))
  expect_identical(bt$args[[2]], list(name = "df", ref = adsl$id))
  expect_identical(g$result, bt$id)
})

test_that("ggplot2 layers are added with +; other statements are code blocks", {
  g <- blockr_graph_from_code('
p <- ggplot(ADSL, aes(AGE)) + geom_histogram(bins = 10) + theme_minimal()
if (nrow(ADSL) > 0) n <- nrow(ADSL)
m <- n + 1
')
  hist <- first_node(g, "geom_histogram")
  expect_identical(hist$op, "+")
  expect_identical(node_of(g, hist$input)$fn, "ggplot")
  expect_identical(first_node(g, "theme_minimal")$name, "p")
  code <- Filter(function(n) identical(n$type, "code"), g$nodes)[[1]]
  expect_identical(blockr_node_defines(code), "n")
  # `n + 1` is not a chain: an expression block, which uses the code block
  m <- Filter(function(n) identical(n$name, "m"), g$nodes)[[1]]
  expect_identical(m$type, "expr")
  expect_identical(blockr_graph_deps(g)[[m$id]]$implicit, code$id)
})

test_that("variables resolve to their last assignment before the block", {
  g <- blockr_graph_from_code('
x <- c(1, 2)
y <- x[1] + 1
x <- c(x, 3)
z <- x[2] * 2
')
  d <- blockr_graph_deps(g)
  id <- function(name, k = 1) Filter(function(n) identical(n$name, name), g$nodes)[[k]]$id
  # inside an expression: implicit dependencies
  expect_identical(d[[id("y")]]$implicit, id("x", 1))
  expect_identical(d[[id("z")]]$implicit, id("x", 2))
  # a whole argument: a wire
  expect_identical(node_of(g, id("x", 2))$args[[1]], list(name = "", ref = id("x", 1)))
})

# ---- Compiling edited graphs ----------------------------------------------------

graph <- function(...) list(version = 1L, packages = character(0), nodes = list(...), result = NULL)
call_node <- function(id, fn, ..., input = NULL, name = NULL, order = NULL, off = NULL) {
  blockr_node_clean(list(
    id = id, type = "call", fn = fn, args = list(...), input = input, op = if (!is.null(input)) "%>%",
    name = name, order = order, off = off
  ))
}
arg <- function(value, name = "") list(name = name, value = value)
wire <- function(ref, name = "") list(name = name, ref = ref)

test_that("stacked blocks become one chain, named after its last named block; new chains get a name", {
  g <- graph(
    list(id = "d", type = "dataset", dataset = "ADAE", order = 1),
    call_node("f", "filter", arg('AESER == "Y"'), input = "d", order = 2),
    call_node("m", "mutate", arg("TRUE", "X"), input = "f", order = 3, name = "ae"),
    call_node("s", "head", arg("5"), input = "m", order = 4)
  )
  code <- blockr_compile(g)$code
  expect_identical(
    strip_library(parse(text = code, keep.source = FALSE)),
    as.list(expression(ae <- ADAE %>% filter(AESER == "Y") %>% mutate(X = TRUE), adae <- ae %>% head(5)))
  )
})

test_that("a bypassed block passes its input through", {
  g <- graph(
    list(id = "d", type = "dataset", dataset = "ADSL", order = 1),
    call_node("f", "filter", arg("AGE > 40"), input = "d", order = 2, off = TRUE),
    call_node("h", "head", arg("3"), input = "f", order = 3, name = "x")
  )
  expect_identical(strip_library(parse(text = blockr_compile(g)$code, keep.source = FALSE)), as.list(expression(x <- ADSL %>% head(3))))
  steps <- blockr_compile(g, "debug")$steps
  expect_identical(steps[[1]]$exprs[[1]], quote(.bk_f <- ADSL))
})

test_that("problems are reported on their block; the blocks after them wait", {
  reg <- blockr_registry()
  g <- graph(
    list(id = "d", type = "dataset", dataset = "ADSL", order = 1),
    call_node("f", "filter", arg("AGE > "), input = "d", order = 2),
    call_node("h", "head", arg("3"), input = "f", order = 3),
    call_node("lonely", "filter", arg("AGE > 1"), order = 4),
    call_node("b", "build_table", wire("nowhere"), order = 5),
    call_node("e", "mutate", arg(""), input = "d", order = 6),
    call_node("u", "build_table", list(name = "df", value = NULL), order = 7)
  )
  out <- blockr_compile(g, "debug", registry = reg)
  expect_match(out$errors$f, "not valid R code")
  expect_match(out$errors$lonely, "below another block")
  expect_match(out$errors$b, "no longer exists")
  expect_match(out$errors$e, "is empty")
  expect_match(out$errors$u, "not connected")
  expect_true("h" %in% out$failed)
  expect_false("h" %in% names(out$errors))
})

test_that("a loop is reported, not followed", {
  g <- graph(
    call_node("a", "head", input = "b", order = 1),
    call_node("b", "head", input = "a", order = 2)
  )
  out <- blockr_compile(g, "debug")
  expect_setequal(names(out$errors), c("a", "b"))
  expect_match(out$errors$a, "loop")
})

test_that("long code is wrapped readably and parses to the same expression", {
  e <- quote(result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>% prune_table() %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences, decreasing = TRUE))
  txt <- blockr_format(e, width = 60)
  expect_true(all(nchar(strsplit(txt, "\n")[[1]]) <= 70))
  expect_match(txt, "%>%\n  prune_table()", fixed = TRUE)
  expect_identical(parse(text = txt, keep.source = FALSE)[[1]], e)
  expect_identical(blockr_format(quote(x <- f(1))), "x <- f(1)")
  # names that are not syntactic keep their backticks
  e2 <- quote(g <- list(`- Any Grade -` = c("1", "2", "3", "4", "5"), `Grade 1-2` = c("1", "2"), `Grade 3-4` = c("3", "4")))
  expect_identical(parse(text = blockr_format(e2, width = 40), keep.source = FALSE)[[1]], e2)
})

# ---- Running -------------------------------------------------------------------------

datasets <- list(ADSL = data.frame(USUBJID = paste0("S", 1:10), AGE = c(25, 31, 38, 44, 47, 52, 58, 63, 67, 71)))

test_that("blocks run one by one: sizes, values and the result", {
  g <- blockr_graph_from_code("old <- ADSL %>% subset(AGE >= 50)\nn <- nrow(old)")
  r <- blockr_run(g, datasets, parent = test_parent(), registry = list())
  st <- r$status
  data_id <- ids_of(g)[1]
  old <- Filter(function(n) identical(n$name, "old"), g$nodes)[[1]]$id
  n <- Filter(function(n) identical(n$name, "n"), g$nodes)[[1]]$id
  expect_identical(st[[data_id]][c("state", "kind", "rows", "cols")], list(state = "ok", kind = "data", rows = 10L, cols = 2L))
  expect_identical(st[[old]]$rows, 5L)
  expect_identical(st[[n]]$kind, "value")
  expect_identical(blockr_value(r, g, n), 5L)
  expect_identical(r$result, n)
  # the columns of data blocks, for the column pickers
  expect_identical(vapply(st[[old]]$schema, `[[`, character(1), "n"), c("USUBJID", "AGE"))
})

test_that("a failing block shows its error; the blocks after it wait; the others still run", {
  g <- blockr_graph_from_code("a <- ADSL %>% subset(NOPE > 1)\nb <- nrow(a)\nc <- nrow(ADSL)")
  r <- blockr_run(g, datasets, parent = test_parent(), registry = list())
  state <- function(name) r$status[[Filter(function(n) identical(n$name, name), g$nodes)[[1]]$id]]
  expect_identical(state("a")$state, "error")
  expect_match(state("a")$error, "NOPE")
  expect_identical(state("b")$state, "blocked")
  expect_identical(state("c")$state, "ok")
})

test_that("values are cached per block: a change only re-runs the blocks after it", {
  calls <- 0
  counting <- function(x) {
    calls <<- calls + 1
    x
  }
  parent <- test_parent(counting = counting)
  cache <- new.env()
  code <- "a <- ADSL %>% counting()\nb <- a %>% head(3)"
  g <- blockr_graph_from_code(code)
  blockr_run(g, datasets, cache, parent = parent, registry = list())
  expect_identical(calls, 1)
  r <- blockr_run(g, datasets, cache, parent = parent, registry = list())
  expect_identical(calls, 1)
  g2 <- blockr_graph_from_code("a <- ADSL %>% counting()\nb <- a %>% head(4)")
  r2 <- blockr_run(g2, datasets, cache, parent = parent, registry = list())
  expect_identical(calls, 1)
  expect_identical(r2$status[[ids_of(g2)[3]]]$rows, 4L)
})

test_that("a code block that assigns nothing (e.g. set.seed) runs (regression)", {
  g <- blockr_graph_from_code("set.seed(1)\nx <- sample(nrow(ADSL), 1)")
  r <- blockr_run(g, datasets, parent = test_parent(), registry = list())
  states <- vapply(r$status, `[[`, character(1), "state")
  expect_true(all(states == "ok"))
  code <- Filter(function(n) identical(n$type, "code"), g$nodes)[[1]]
  expect_identical(r$status[[code$id]]$info, "no value")
  expect_null(blockr_value(r, g, code$id))
})

test_that("packages the code needs are checked first", {
  g <- blockr_graph_from_code("library(notapackage123)\nx <- nrow(ADSL)")
  r <- blockr_run(g, datasets, parent = test_parent(), registry = list())
  x <- Filter(function(n) identical(n$name, "x"), g$nodes)[[1]]$id
  expect_identical(r$status[[x]]$state, "missing")
  expect_match(r$status[[x]]$error, "notapackage123")
})

test_that("the result: the graph's choice, else `result`, else the last output nobody uses", {
  g <- blockr_graph_from_code("a <- nrow(ADSL)\nresult <- a + 1\nz <- 3")
  r <- blockr_run(g, datasets, parent = test_parent(), registry = list())
  expect_identical(r$result, Filter(function(n) identical(n$name, "result"), g$nodes)[[1]]$id)
  g$result <- ids_of(g)[1]
  expect_identical(blockr_run(g, datasets, parent = test_parent(), registry = list())$result, ids_of(g)[1])
})

test_that("values are described for the blocks' status line", {
  expect_identical(blockr_describe(data.frame(a = 1:3))[c("kind", "rows", "cols")], list(kind = "data", rows = 3L, cols = 1L))
  expect_identical(blockr_describe(c("a", "b"))$kind, "value")
  expect_match(blockr_describe(c("a", "b"))$info, '"a", "b"', fixed = TRUE)
  expect_match(blockr_describe(letters)$info, "26 values", fixed = TRUE)
  expect_identical(blockr_describe(list(a = 1))$info, "list: a")
  expect_identical(blockr_describe(structure(list(), class = "PreDataTableLayouts"))$kind, "layout")
  expect_identical(blockr_describe(structure(list(), class = c("gg", "ggplot")))$kind, "plot")
  schema <- blockr_schema(data.frame(
    f = factor(c("b", "a")), n = c(1.5, 3), l = c(TRUE, NA), s = c("x", "y"), i = 1:2,
    stringsAsFactors = FALSE
  ))
  expect_identical(vapply(schema, `[[`, character(1), "t"), c("fct", "num", "lgl", "chr", "int"))
  expect_identical(as.character(schema[[1]]$v), c("a", "b"))
  expect_identical(schema[[2]]$r, c(1.5, 3))
  expect_identical(as.character(schema[[3]]$v), "TRUE")
})

# ---- Registry ---------------------------------------------------------------------------

test_that("the registry: one entry per function, valid categories and editors", {
  reg <- blockr_registry()
  fns <- vapply(reg, `[[`, character(1), "fn")
  expect_false(anyDuplicated(fns) > 0)
  cats <- vapply(blockr_categories(), `[[`, character(1), "id")
  editors <- c("column", "columns", "symbols", "conditions", "assign", "labels", "text", "number", "bool",
               "choice", "strings", "named", "aes", "data", "code")
  for (b in reg) {
    expect_true(b$cat %in% cats, info = b$fn)
    expect_true(nzchar(b$label) && nzchar(b$help), info = b$fn)
    for (a in b$args) expect_true(a$editor %in% editors, info = paste(b$fn, a$name))
    # summaries only use arguments the block knows
    keys <- gsub("^\\{\\??|[?}].*$", "", regmatches(b$summary %||% "", gregexpr("\\{[^}]+\\}", b$summary %||% ""))[[1]])
    known <- c(as.character(b$formals), vapply(b$args, `[[`, character(1), "name"), "...", as.character(seq_along(b$formals)))
    expect_true(all(keys %in% known), info = b$fn)
  }
})

test_that("every palette block can be added and compiled", {
  reg <- blockr_registry()
  for (b in Filter(function(b) isTRUE(b$palette), reg)) {
    args <- lapply(b$insert, function(a) if ("ref" %in% names(a)) list(name = a$name, ref = "d") else a)
    n <- list(id = "n", type = "call", fn = b$fn, args = args, input = if (isTRUE(b$input)) "d", op = b$op %||% "%>%", order = 2)
    g <- graph(list(id = "d", type = "dataset", dataset = "ADSL", order = 1), blockr_node_clean(n))
    out <- blockr_compile(g, registry = reg)
    # a block may need settings (e.g. a column) before it runs, but its code is valid R
    if (length(out$errors)) expect_match(out$errors$n, "empty", info = b$fn) else expect_no_error(parse(text = out$code))
  }
})

test_that("a canvas built from scratch attaches the packages its blocks need", {
  reg <- blockr_registry()
  g <- graph(
    call_node("t", "basic_table", order = 1, name = "lyt"),
    list(id = "d", type = "dataset", dataset = "ADSL", order = 2),
    call_node("p", "ggplot", arg("aes(x = AGE)"), input = "d", order = 3)
  )
  expect_identical(blockr_graph_packages(g, reg), c("tern", "ggplot2"))
  expect_match(blockr_compile(g, registry = reg)$code, "^library\\(tern\\)\nlibrary\\(ggplot2\\)")
  # the templates already attach what they use
  for (e in catalog) {
    g <- blockr_graph_from_code(e$code)
    expect_identical(blockr_graph_packages(g, reg), g$packages, info = e$id)
  }
})

test_that("every template's blocks compile without problems against the registry", {
  reg <- blockr_registry()
  for (e in catalog) {
    out <- blockr_compile(blockr_graph_from_code(e$code), "debug", registry = reg)
    expect_length(out$errors, 0)
  }
})

# ---- The templates on the app's data -----------------------------------------------------

test_that("on the canvas, every template gives the same result as in the TLG Catalog", {
  skip_on_cran()
  skip_if(identical(Sys.getenv("BLOCKR_SKIP_TLG"), "true"), "BLOCKR_SKIP_TLG=true")
  skip_if_not_installed("tern")
  local_blockr_options()
  data <- suppressMessages(build_blockr_data(cache_global = FALSE))
  datasets <- stats::setNames(lapply(names(data), function(n) data[[n]]), names(data))
  failures <- character(0)
  for (e in catalog) {
    reference <- tlg_run(e, data)
    if (!isTRUE(reference$ok)) next # missing package (e.g. rbmi): covered by the TLG Catalog tests
    g <- blockr_graph_from_code(e$code)
    r <- suppressWarnings(blockr_run(g, datasets))
    bad <- Filter(function(s) !identical(s$state, "ok"), r$status)
    if (length(bad)) {
      failures <- c(failures, sprintf("%s: %s", e$id, bad[[1]]$error))
      next
    }
    value <- blockr_value(r, g, r$result)
    if (!identical(class(value), class(reference$value))) {
      failures <- c(failures, sprintf("%s: a %s instead of a %s", e$id, class(value)[1], class(reference$value)[1]))
    } else if (reference$kind == "table" && !identical(rtables::toString(value), rtables::toString(reference$value))) {
      failures <- c(failures, sprintf("%s: a different table", e$id))
    } else if (reference$kind %in% c("listing", "data") && !identical(nrow(value), nrow(reference$value))) {
      failures <- c(failures, sprintf("%s: a different listing", e$id))
    }
  }
  expect(length(failures) == 0, paste(failures, collapse = "\n"))
})
