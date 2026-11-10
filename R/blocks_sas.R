# Block graphs as SAS code.
#
# A translation of the canvas for SAS users, not a compiler: the blocks BlockR
# knows become their closest SAS equivalent, everything else a TODO comment
# with the R code, so nothing is silently dropped.
#   row-wise steps (filter, mutate, select, column labels, missing values)
#                                   one DATA step
#   order / unique rows             PROC SORT (NODUPKEY)
#   count, summarise, joins         PROC SQL
#   stack rows                      DATA step with several SET datasets
#   table layout + Build table      PROC SQL / MEANS: the same columns (with
#                                   the total column), row groups, counts and
#                                   percentages, shown with PROC TRANSPOSE +
#                                   PROC PRINT
#   listing                         PROC REPORT
#   plot and its layers             PROC SGPLOT (SGPANEL with small multiples)
#   Kaplan-Meier plot               PROC LIFETEST
# Datasets are read from the library `adam` (LIBNAME at the top); every
# variable of the R code is a WORK dataset of the same name.

#' SAS code of a block graph
#' @param graph A graph (see R/blocks_graph.R).
#' @param registry Block registry (which blocks are steps).
#' @param status Status of the blocks after blockr_run() (column types, used
#'   to tell numbers from categories), may be empty.
#' @param libname SAS library of the ADaM datasets.
#' @return The SAS code (one string).
#' @noRd
blockr_sas <- function(graph, registry = blockr_registry(), status = list(), libname = "adam") {
  comp <- blockr_compile(graph, registry = registry)
  ids <- vapply(graph$nodes, `[[`, character(1), "id")
  nodes <- stats::setNames(graph$nodes, ids)
  chain_of <- list()
  for (ch in comp$chains) for (m in ch$members) chain_of[[m]] <- ch
  ctx <- new.env(parent = emptyenv())
  ctx$layouts <- list()
  ctx$libname <- libname
  ctx$nodes <- nodes
  ctx$status <- status
  ctx$chain_of <- chain_of
  ctx$tmp <- 0L
  ctx$vars <- vapply(comp$chains, function(ch) ch$var, character(1))

  out <- c(
    "/* BlockR canvas, translated to SAS.",
    "   The ADaM datasets are read from the library ADAM; each step writes a WORK",
    "   dataset named as in the R code. Steps without a SAS equivalent are marked",
    "   TODO with their R code. Check the results against the R output. */",
    sprintf("libname %s \"path/to/adam\";  /* TODO: the folder of the ADaM datasets */", libname),
    "options validvarname=any;",
    ""
  )
  for (ch in comp$chains) {
    first <- nodes[[ch$members[1]]]
    if (identical(first$type, "dataset")) next
    part <- tryCatch(sas_chain(ch, ctx), error = function(e) {
      sas_todo(sprintf("this step could not be translated (%s)", conditionMessage(e)), sas_r_code(ch, nodes))
    })
    if (length(part)) out <- c(out, part, "")
  }
  # SAS names ignore case: n and N would be one and the same column
  vars <- tryCatch(all.vars(parse(text = comp$code, keep.source = FALSE)), error = function(e) character(0))
  # adsl (WORK) and ADSL (the ADAM library) are told apart by their library
  vars <- vars[!toupper(vars) %in% names(blockr_dataset_labels())]
  clash <- split(unique(vars), tolower(unique(vars)))
  clash <- Filter(function(v) length(v) > 1, clash)
  if (length(clash)) {
    out <- c(out, sprintf("/* NOTE: SAS names ignore case, so %s are the same name in SAS: rename one of them. */",
                          paste(vapply(clash, function(v) paste(v, collapse = " and "), character(1)), collapse = "; ")))
  }
  if (length(comp$errors)) {
    out <- c(out, sprintf("/* Not translated: %d block(s) with a problem on the canvas. */", length(comp$errors)))
  }
  paste(out, collapse = "\n")
}

# ---- Names and helpers --------------------------------------------------------------

#' SAS name of the data a node holds
#' @noRd
sas_data_of <- function(id, ctx) {
  n <- ctx$nodes[[id]]
  if (is.null(n)) return("work._missing")
  if (identical(n$type, "dataset")) return(paste0(ctx$libname, ".", tolower(n$dataset)))
  ch <- ctx$chain_of[[id]]
  paste0("work.", sas_name(if (!is.null(ch)) ch$var else id))
}

#' A valid SAS name (n-literal when needed)
#' @noRd
sas_name <- function(x) {
  x <- as.character(x)
  if (grepl("^[A-Za-z_][A-Za-z0-9_]{0,31}$", x)) x else paste0("\"", gsub("\"", "\"\"", x), "\"n")
}

#' @noRd
sas_str <- function(x) paste0("\"", gsub("\"", "\"\"", x), "\"")

#' @noRd
sas_todo <- function(what, r_code = NULL) {
  c(
    paste0("/* TODO: ", what),
    if (length(r_code)) paste0("   R: ", gsub("\\*/", "* /", strsplit(paste(r_code, collapse = "\n"), "\n")[[1]])),
    "*/"
  )
}

#' R code of a chain (for TODO comments)
#' @noRd
sas_r_code <- function(ch, nodes) {
  g <- list(version = 1L, packages = character(0), nodes = unname(nodes))
  code <- tryCatch(blockr_compile(g)$code, error = function(e) "")
  block <- strsplit(code, "\n\n")[[1]]
  hit <- block[grepl(paste0("^(#[^\n]*\n)*", gsub("([.])", "\\\\\\1", ch$var), " <- "), block)]
  if (length(hit)) hit[[length(hit)]] else ch$var
}

#' R code of one call block, e.g. filter(SAFFL == "Y") (wires as the block id)
#' @noRd
sas_call_code <- function(n) {
  args <- vapply(n$args, function(x) {
    paste0(if (nzchar(x$name %||% "")) paste0(x$name, " = "), x$value %||% x$ref %||% "")
  }, character(1))
  paste0(n$fn, "(", paste(args, collapse = ", "), ")")
}

#' Arguments of a call node matched to the function's formals (like R)
#' @noRd
sas_args <- function(n) {
  spec <- blockr_registry()[[n$fn]]
  formals <- as.character(spec$formals)
  if (!is.null(n$input) && !identical(spec$op, "+") && length(formals) && formals[1] != "...") formals <- formals[-1]
  named <- list()
  dots <- list()
  pos <- setdiff(formals, "...")
  used <- character(0)
  for (a in n$args) {
    val <- if (!is.null(a$ref)) list(ref = a$ref) else list(code = a$value)
    nm <- a$name %||% ""
    if (nzchar(nm) && nm != "..." && (nm %in% formals || !("..." %in% formals))) {
      named[[nm]] <- val
      used <- c(used, nm)
    } else if (!nzchar(nm)) {
      free <- setdiff(pos, used)
      stop_at <- match("...", formals)
      free <- free[is.na(stop_at) | match(free, formals) < stop_at]
      if (length(free)) {
        named[[free[1]]] <- val
        used <- c(used, free[1])
      } else {
        dots[[length(dots) + 1L]] <- c(val, list(name = ""))
      }
    } else {
      dots[[length(dots) + 1L]] <- c(val, list(name = nm))
    }
  }
  list(named = named, dots = dots)
}

#' R code of an argument as an expression (NULL if absent)
#' @noRd
sas_arg_expr <- function(a) if (!is.null(a$code)) tryCatch(str2lang(a$code), error = function(e) NULL)

#' Strings of an argument: "A" or c("A", "B"), also through a wire to a
#' c("A", "B") block (NULL if not strings)
#' @noRd
sas_arg_strings <- function(a, ctx = NULL) {
  if (!is.null(a$ref) && !is.null(ctx)) {
    n <- ctx$nodes[[a$ref]]
    if (identical(n$type, "call") && identical(n$fn, "c")) {
      vals <- lapply(n$args, function(x) sas_arg_expr(list(code = x$value)))
      if (all(vapply(vals, is.character, logical(1)))) return(unlist(vals))
    }
    return(NULL)
  }
  e <- sas_arg_expr(a)
  if (is.character(e)) return(e)
  if (is.call(e) && identical(e[[1]], as.name("c")) && all(vapply(as.list(e)[-1], is.character, logical(1)))) {
    return(unlist(as.list(e)[-1]))
  }
  NULL
}

#' Column names of the data a node holds, by type (from the run's status)
#' @noRd
sas_columns <- function(id, ctx) {
  st <- ctx$status[[id]]
  sch <- st$schema
  if (is.null(sch) || is.character(sch)) return(NULL)
  stats::setNames(vapply(sch, `[[`, character(1), "t"), vapply(sch, `[[`, character(1), "n"))
}

#' @noRd
sas_tmp <- function(ctx, base) {
  ctx$tmp <- ctx$tmp + 1L
  paste0("work._", base, ctx$tmp)
}

# ---- Expressions ----------------------------------------------------------------------

#' An R expression as a SAS expression (NULL when there is no translation)
#' @param agg Allow summary functions (PROC SQL: mean, sum, count, ...).
#' @noRd
sas_expr <- function(e, agg = FALSE) {
  rec <- function(x) sas_expr(x, agg)
  if (is.name(e)) {
    s <- as.character(e)
    if (s %in% c("T", "TRUE")) return("1")
    if (s %in% c("F", "FALSE")) return("0")
    return(sas_name(s))
  }
  if (is.character(e) && length(e) == 1) return(if (is.na(e)) "\"\"" else sas_str(e))
  if (is.logical(e) && length(e) == 1) return(if (is.na(e)) "." else if (e) "1" else "0")
  if (is.numeric(e) && length(e) == 1) return(if (is.na(e)) "." else format(e, scientific = FALSE, trim = TRUE))
  if (!is.call(e)) return(NULL)
  f <- if (is.name(e[[1]])) as.character(e[[1]]) else sub("^.*::", "", paste(deparse(e[[1]]), collapse = ""))
  args <- as.list(e)[-1]
  nm <- names(args) %||% rep("", length(args))
  bin <- c("==" = "=", "!=" = "ne", ">" = ">", ">=" = ">=", "<" = "<", "<=" = "<=",
           "&" = "and", "&&" = "and", "|" = "or", "||" = "or", "+" = "+", "-" = "-",
           "*" = "*", "/" = "/", "^" = "**")
  all_ok <- function(xs) if (any(vapply(xs, is.null, logical(1)))) NULL else xs
  if (f == "(") {
    x <- rec(args[[1]])
    return(if (is.null(x)) NULL else paste0("(", x, ")"))
  }
  if (f %in% names(bin) && length(args) == 2) {
    a <- rec(args[[1]])
    b <- rec(args[[2]])
    if (is.null(a) || is.null(b)) return(NULL)
    return(paste(a, bin[[f]], b))
  }
  if (f == "-" && length(args) == 1) {
    x <- rec(args[[1]])
    return(if (is.null(x)) NULL else paste0("-", x))
  }
  if (f == "!" && length(args) == 1) {
    inner <- args[[1]]
    if (is.call(inner) && identical(inner[[1]], as.name("%in%"))) {
      x <- sas_in(inner, agg)
      return(if (is.null(x)) NULL else sub(" in \\(", " not in (", x))
    }
    if (is.call(inner) && identical(inner[[1]], as.name("is.na"))) {
      x <- rec(inner[[2]])
      return(if (is.null(x)) NULL else paste0("not missing(", x, ")"))
    }
    x <- rec(inner)
    return(if (is.null(x)) NULL else paste0("not (", x, ")"))
  }
  if (f == "%in%") return(sas_in(e, agg))
  if (f == "%%") {
    xs <- all_ok(lapply(args, rec))
    return(if (is.null(xs)) NULL else sprintf("mod(%s, %s)", xs[[1]], xs[[2]]))
  }
  if (f == "is.na") {
    x <- rec(args[[1]])
    return(if (is.null(x)) NULL else paste0("missing(", x, ")"))
  }
  if (f %in% c("ifelse", "if_else") && length(args) >= 3) {
    xs <- all_ok(lapply(args[1:3], rec))
    if (is.null(xs)) return(NULL)
    char <- is.character(args[[2]]) || is.character(args[[3]])
    return(sprintf("%s(%s, %s, %s)", if (char) "ifc" else "ifn", xs[[1]], xs[[2]], xs[[3]]))
  }
  if (f %in% c("paste", "paste0")) {
    sep <- if (f == "paste0") "" else if ("sep" %in% nm) args[[match("sep", nm)]] else " "
    parts <- args[nm != "sep" & nm != "collapse"]
    xs <- all_ok(lapply(parts, rec))
    if (is.null(xs) || !is.character(sep)) return(NULL)
    return(if (nzchar(sep)) sprintf("catx(%s, %s)", sas_str(sep), paste(xs, collapse = ", ")) else sprintf("cats(%s)", paste(xs, collapse = ", ")))
  }
  if (f == "substr" && length(args) == 3) {
    xs <- all_ok(lapply(args, rec))
    if (is.null(xs)) return(NULL)
    len <- if (is.numeric(args[[2]]) && is.numeric(args[[3]])) as.character(args[[3]] - args[[2]] + 1) else sprintf("(%s) - (%s) + 1", xs[[3]], xs[[2]])
    return(sprintf("substr(%s, %s, %s)", xs[[1]], xs[[2]], len))
  }
  if (f == "grepl" && length(args) >= 2 && is.character(args[[1]])) {
    x <- rec(args[[2]])
    if (is.null(x)) return(NULL)
    flags <- if ("ignore.case" %in% nm && isTRUE(args[[match("ignore.case", nm)]])) "i" else ""
    return(sprintf("prxmatch(%s, %s) > 0", sas_str(paste0("/", gsub("/", "\\\\/", args[[1]]), "/", flags)), x))
  }
  if (f %in% c("round", "signif") && length(args) >= 1) {
    x <- rec(args[[1]])
    digits <- if (length(args) > 1 && is.numeric(args[[2]])) args[[2]] else 0
    return(if (is.null(x)) NULL else sprintf("round(%s, %s)", x, format(10^-digits, scientific = FALSE)))
  }
  # functions with an equivalent of the same shape
  same <- c(abs = "abs", sqrt = "sqrt", exp = "exp", log = "log", log10 = "log10", log2 = "log2",
            floor = "floor", ceiling = "ceil", toupper = "upcase", tolower = "lowcase", trimws = "strip",
            nchar = "length", pmin = "min", pmax = "max", as.integer = "int", coalesce = "coalesce")
  if (f %in% names(same)) {
    xs <- all_ok(lapply(args[nm == ""], rec))
    return(if (is.null(xs)) NULL else sprintf("%s(%s)", same[[f]], paste(xs, collapse = ", ")))
  }
  # no factors in SAS: the values themselves
  if (f %in% c("factor", "as.factor", "droplevels", "as.vector", "I", "reorder") && length(args)) return(rec(args[[1]]))
  if (f %in% c("as.character")) {
    x <- rec(args[[1]])
    return(if (is.null(x)) NULL else sprintf("cats(%s)", x))
  }
  if (f %in% c("as.numeric", "as.double")) {
    x <- rec(args[[1]])
    return(if (is.null(x)) NULL else sprintf("input(cats(%s), best32.)", x))
  }
  if (f == "as.Date" && length(args) == 1) {
    x <- rec(args[[1]])
    return(if (is.null(x)) NULL else sprintf("datepart(%s)", x))
  }
  if (f == "day2month") {
    x <- rec(args[[1]])
    return(if (is.null(x)) NULL else sprintf("(%s / 30.4375)", x))
  }
  if (agg) {
    sql <- c(mean = "mean", sum = "sum", min = "min", max = "max", sd = "std", median = "median", var = "var")
    if (f %in% names(sql) && length(args) >= 1) {
      x <- rec(args[[1]])
      return(if (is.null(x)) NULL else sprintf("%s(%s)", sql[[f]], x))
    }
    if (f == "n" && !length(args)) return("count(*)")
    if (f == "n_distinct" && length(args) == 1) {
      x <- rec(args[[1]])
      return(if (is.null(x)) NULL else sprintf("count(distinct %s)", x))
    }
  }
  NULL
}

#' x %in% c(...) -> x in (...)
#' @noRd
sas_in <- function(e, agg = FALSE) {
  x <- sas_expr(e[[2]], agg)
  rhs <- e[[3]]
  vals <- if (is.call(rhs) && identical(rhs[[1]], as.name("c"))) as.list(rhs)[-1] else list(rhs)
  if (is.null(x) || !all(vapply(vals, function(v) is.character(v) || is.numeric(v), logical(1)))) return(NULL)
  paste0(x, " in (", paste(vapply(vals, function(v) sas_expr(v), character(1)), collapse = " "), ")")
}

#' select() arguments as a keep= (or drop=, attribute "drop") list; NULL when
#' there is no translation
#' @noRd
sas_select <- function(exprs) {
  strings_of <- function(e) {
    if (is.character(e)) return(e)
    if (is.call(e) && identical(e[[1]], as.name("c")) && all(vapply(as.list(e)[-1], is.character, logical(1)))) return(unlist(as.list(e)[-1]))
    NULL
  }
  keep <- character(0)
  drop <- character(0)
  for (e in exprs) {
    if (is.name(e)) {
      keep <- c(keep, as.character(e))
    } else if (is.call(e) && identical(e[[1]], as.name("-")) && length(e) == 2 && is.name(e[[2]])) {
      drop <- c(drop, as.character(e[[2]]))
    } else if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("any_of", "all_of") && length(e) == 2 && !is.null(strings_of(e[[2]]))) {
      keep <- c(keep, strings_of(e[[2]]))
    } else {
      return(NULL)
    }
  }
  if (length(keep) && length(drop)) return(NULL)
  if (length(drop)) return(structure(vapply(drop, sas_name, character(1), USE.NAMES = FALSE), drop = TRUE))
  vapply(keep, sas_name, character(1), USE.NAMES = FALSE)
}

#' cut(x, breaks, labels, right) as SELECT / WHEN statements
#' @noRd
sas_cut <- function(target, e) {
  args <- as.list(e)[-1]
  nm <- names(args) %||% rep("", length(args))
  get <- function(name, pos) if (name %in% nm) args[[match(name, nm)]] else if (length(args) >= pos && nm[pos] == "") args[[pos]]
  x <- sas_expr(get("x", 1))
  breaks <- tryCatch(eval(get("breaks", 2), baseenv()), error = function(err) NULL)
  labels <- tryCatch(eval(get("labels", 3), baseenv()), error = function(err) NULL)
  right <- !isFALSE(get("right", 99))
  if (is.null(x) || !is.numeric(breaks) || length(breaks) < 2) return(NULL)
  k <- length(breaks) - 1
  if (is.null(labels)) labels <- sprintf(if (right) "(%s,%s]" else "[%s,%s)", breaks[-length(breaks)], breaks[-1])
  if (length(labels) != k) return(NULL)
  num <- function(v) format(v, scientific = FALSE, trim = TRUE)
  out <- "  select;"
  for (i in seq_len(k)) {
    lo <- breaks[i]
    hi <- breaks[i + 1]
    cond <- c(
      if (is.finite(lo)) sprintf("%s %s %s", num(lo), if (right) "<" else "<=", x),
      if (is.finite(hi)) sprintf("%s %s %s", x, if (right) "<=" else "<", num(hi))
    )
    cond <- if (length(cond)) paste(cond, collapse = " and ") else sprintf("not missing(%s)", x)
    out <- c(out, sprintf("    when (not missing(%s) and %s) %s = %s;", x, cond, target, sas_str(labels[i])))
  }
  c(out, sprintf("    otherwise %s = \"\";", target), "  end;")
}

#' new column = R expression, as DATA step statements (case_when becomes SELECT)
#' @noRd
sas_assign <- function(name, e) {
  target <- sas_name(name)
  if (is.call(e) && identical(e[[1]], as.name("cut"))) return(sas_cut(target, e))
  if (is.call(e) && identical(e[[1]], as.name("case_when"))) {
    out <- "  select;"
    for (cl in as.list(e)[-1]) {
      if (!(is.call(cl) && identical(cl[[1]], as.name("~")))) return(NULL)
      val <- sas_expr(cl[[3]])
      if (is.null(val)) return(NULL)
      if (isTRUE(cl[[2]]) || identical(cl[[2]], as.name("TRUE"))) {
        out <- c(out, sprintf("    otherwise %s = %s;", target, val))
      } else {
        cond <- sas_expr(cl[[2]])
        if (is.null(cond)) return(NULL)
        out <- c(out, sprintf("    when (%s) %s = %s;", cond, target, val))
      }
    }
    if (!any(grepl("otherwise", out))) out <- c(out, "    otherwise;")
    return(c(out, "  end;"))
  }
  x <- sas_expr(e)
  if (is.null(x)) return(NULL)
  # a condition: 1 when true, 0 when false (brackets make it readable)
  test <- c("==", "!=", ">", ">=", "<", "<=", "&", "|", "&&", "||", "%in%", "!", "is.na")
  if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% test) x <- paste0("(", x, ")")
  sprintf("  %s = %s;", target, x)
}

# ---- Chains -----------------------------------------------------------------------------

#' SAS code of one chain (stacked blocks assigned to one variable)
#' @noRd
sas_chain <- function(ch, ctx) {
  nodes <- ctx$nodes
  members <- Filter(function(m) !isTRUE(nodes[[m]]$off) || is.null(nodes[[m]]$input), ch$members)
  first <- nodes[[ch$members[1]]]
  target <- paste0("work.", sas_name(ch$var))

  if (identical(first$type, "code") || identical(first$type, "expr")) {
    return(sas_todo("R code, no SAS translation", if (identical(first$type, "code")) first$code else paste(ch$var, "<-", first$code)))
  }
  head_fn <- sub("^.*::", "", first$fn %||% "")

  # a table layout: nothing to run yet, it is used by Build table
  if (head_fn == "basic_table") {
    ctx$layouts[[ch$var]] <- sas_layout(members, ctx)
    return(c(sprintf("/* %s: a table layout (%s); see the table that uses it. */", ch$var, ctx$layouts[[ch$var]]$what)))
  }

  # a plot: the ggplot block and its layers (possibly after data steps)
  gg <- which(vapply(members, function(m) identical(nodes[[m]]$fn, "ggplot"), logical(1)))
  out <- character(0)
  src <- if (!is.null(first$input)) sas_data_of(first$input, ctx) else NULL
  if (length(gg)) {
    k <- gg[1]
    pre <- members[seq_len(k - 1)]
    if (length(pre) && is.null(first$input)) {
      head <- sas_head(first, target, ctx)
      out <- c(out, head$code)
      src <- head$data
      pre <- pre[-1]
    }
    if (length(pre)) {
      res <- sas_steps(pre, src, target, ctx)
      out <- c(out, res$code)
      src <- res$data
    }
    return(c(out, sas_plot(members[k:length(members)], src, ctx)))
  }
  steps <- members
  if (is.null(first$input)) {
    head <- sas_head(first, target, ctx)
    out <- c(out, head$code)
    src <- head$data
    steps <- members[-1]
    if (isTRUE(head$final)) {
      rest <- vapply(steps, function(m) nodes[[m]]$fn %||% "", character(1))
      if (length(rest)) out <- c(out, sas_table_tail(rest))
      return(out)
    }
  }
  if (!length(steps)) {
    if (!is.null(src) && !identical(src, target)) out <- c(out, sprintf("data %s;", target), sprintf("  set %s;", src), "run;")
    return(out)
  }
  res <- sas_steps(steps, src, target, ctx)
  c(out, res$code)
}

#' The first block of a chain without an input (a call such as build_table())
#' @return list(code, data, final): `final` TRUE when what follows applies to a
#'   table (not data)
#' @noRd
sas_head <- function(n, target, ctx) {
  fn <- sub("^.*::", "", n$fn %||% "")
  a <- sas_args(n)
  ref_or <- function(x) if (!is.null(x$ref)) sas_data_of(x$ref, ctx)
  switch(fn,
    build_table = list(code = sas_table(n, a, target, ctx), data = target, final = TRUE),
    as_listing = {
      df <- a$named$df
      list(code = sas_listing(ref_or(df) %||% "work._missing", a, ctx, df$ref), data = target, final = TRUE)
    },
    ggplot = list(code = character(0), data = NULL),
    c = {
      # a list of values wired into other blocks (their SAS code already has
      # the values); kept as a macro variable for reference
      vals <- lapply(n$args, function(x) sas_arg_expr(list(code = x$value)))
      ok <- length(vals) && all(vapply(vals, function(v) is.character(v) || is.numeric(v), logical(1)))
      name <- sub("^work\\.", "", target)
      list(code = if (ok) sprintf("%%let %s = %s;  /* values used by the blocks below */", name, paste(unlist(vals), collapse = " "))
                  else sas_todo("a list of values without SAS translation", NULL),
           data = target, final = TRUE)
    },
    g_km = {
      df <- a$named$df
      list(code = sas_km(ref_or(df) %||% "work._missing", a$named$variables), data = target, final = TRUE)
    },
    tlg_example_data = {
      ds <- sas_arg_strings(a$named$dataset) %||% "ADXX"
      subj <- ref_or(a$named$adsl)
      list(code = c(
        sprintf("/* %s: not loaded by the app (the R code uses the random.cdisc.data example); your study's %s: */", ds, ds),
        if (!is.null(subj)) c(
          "proc sql;",
          sprintf("  create table %s as select * from %s.%s", target, ctx$libname, tolower(ds)),
          sprintf("  where USUBJID in (select USUBJID from %s);", subj),
          "quit;"
        ) else c(sprintf("data %s;", target), sprintf("  set %s.%s;", ctx$libname, tolower(ds)), "run;")
      ), data = target)
    },
    bind_rows = {
      sets <- vapply(a$dots, function(d) ref_or(d) %||% NA_character_, character(1))
      if (anyNA(sets)) {
        list(code = sas_todo("stacking rows of data that are not blocks", NULL), data = target)
      } else {
        list(code = c(sprintf("data %s;", target), sprintf("  set %s;", paste(sets, collapse = " ")), "run;"), data = target)
      }
    },
    list(code = sas_todo(sprintf("%s() has no SAS translation", n$fn), sas_call_code(n)), data = target, final = FALSE)
  )
}

#' Data steps of a chain
#' @return list(code, data)
#' @noRd
sas_steps <- function(steps, src, target, ctx) {
  nodes <- ctx$nodes
  out <- character(0)
  stmts <- character(0)
  keep <- NULL
  groups <- character(0)
  cur <- src
  # write the pending DATA step statements; a plain copy only at the end
  # (the next procedure can read the data where it is)
  flush <- function(final = FALSE) {
    if (!length(stmts) && is.null(keep) && (identical(cur, target) || !final)) return()
    out <<- c(out,
      sprintf("data %s%s;", target, if (!is.null(keep)) sprintf("(%s=%s)", if (isTRUE(attr(keep, "drop"))) "drop" else "keep", paste(keep, collapse = " ")) else ""),
      sprintf("  set %s;", cur %||% "work._missing"),
      stmts,
      "run;"
    )
    stmts <<- character(0)
    keep <<- NULL
    cur <<- target
  }
  prev <- if (length(steps)) nodes[[steps[1]]]$input
  for (id in steps) {
    data_id <- prev  # the block before: its data (and columns) go into this one
    prev <- id
    n <- nodes[[id]]
    if (isTRUE(n$off)) next
    fn <- sub("^.*::", "", n$fn %||% "")
    a <- sas_args(n)
    dots_expr <- lapply(a$dots, sas_arg_expr)
    dots_syms <- vapply(dots_expr, function(e) if (is.name(e)) as.character(e) else NA_character_, character(1))
    todo <- function(why) {
      flush()
      out <<- c(out, sas_todo(why, sas_call_code(n)))
    }
    if (fn == "filter") {
      conds <- lapply(dots_expr, sas_expr)
      if (any(vapply(conds, is.null, logical(1)))) { todo("a condition without SAS translation"); next }
      conds <- unlist(conds)
      # several conditions: all must hold (one with "or" keeps its brackets)
      if (length(conds) > 1) conds <- ifelse(grepl(" or ", conds, fixed = TRUE), paste0("(", conds, ")"), conds)
      if (length(conds)) stmts <- c(stmts, sprintf("  if %s;", paste(conds, collapse = " and ")))
    } else if (fn %in% c("mutate", "transmute")) {
      lines <- character(0)
      ok <- TRUE
      kept <- character(0)
      for (i in seq_along(a$dots)) {
        nm <- a$dots[[i]]$name %||% ""
        if (!nzchar(nm)) {
          # transmute(USUBJID, ...): a column kept as it is
          if (is.name(dots_expr[[i]])) { kept <- c(kept, as.character(dots_expr[[i]])); next }
          ok <- FALSE
          next
        }
        kept <- c(kept, nm)
        l <- sas_assign(nm, dots_expr[[i]])
        if (is.null(l)) ok <- FALSE else lines <- c(lines, l)
      }
      if (!ok) { todo("a new column without SAS translation"); next }
      if (!is.null(keep)) flush()  # the new columns are not in the keep list yet
      stmts <- c(stmts, lines)
      if (fn == "transmute") keep <- vapply(kept, sas_name, character(1), USE.NAMES = FALSE)
    } else if (fn == "select") {
      sel <- sas_select(dots_expr)
      if (is.null(sel)) { todo("a column selection without SAS translation"); next }
      if (!is.null(keep)) flush()
      keep <- sel
    } else if (fn == "var_relabel") {
      lab <- vapply(a$dots, function(d) {
        s <- sas_arg_expr(d)
        if (is.character(s)) sprintf("  label %s = %s;", sas_name(d$name), sas_str(s)) else NA_character_
      }, character(1))
      stmts <- c(stmts, stats::na.omit(lab))
    } else if (fn == "df_explicit_na") {
      stmts <- c(stmts, "  /* missing text values shown as <Missing> */",
                 "  array _chars _character_;", "  do over _chars; if missing(_chars) then _chars = \"<Missing>\"; end;")
    } else if (fn == "arrange") {
      by <- vapply(dots_expr, function(e) {
        if (is.name(e)) sas_name(as.character(e))
        else if (is.call(e) && identical(e[[1]], as.name("desc")) && is.name(e[[2]])) paste("descending", sas_name(as.character(e[[2]])))
        else NA_character_
      }, character(1))
      if (anyNA(by)) { todo("an order without SAS translation"); next }
      flush()
      out <- c(out, sprintf("proc sort data=%s out=%s;", cur, target), sprintf("  by %s;", paste(by, collapse = " ")), "run;")
      cur <- target
    } else if (fn == "distinct") {
      flush()
      if (!length(dots_syms)) {
        out <- c(out, sprintf("proc sort data=%s out=%s noduprecs;", cur, target), "  by _all_;", "run;")
      } else if (anyNA(dots_syms)) { todo("unique rows of expressions"); next } else {
        cols <- vapply(dots_syms, sas_name, character(1))
        out <- c(out, sprintf("proc sort data=%s(keep=%s) out=%s nodupkey;", cur, paste(cols, collapse = " "), target),
                 sprintf("  by %s;", paste(cols, collapse = " ")), "run;")
      }
      cur <- target
    } else if (fn == "group_by") {
      if (anyNA(dots_syms)) { todo("groups of expressions"); next }
      groups <- vapply(dots_syms, sas_name, character(1))
    } else if (fn == "ungroup") {
      groups <- character(0)
    } else if (fn == "count") {
      if (anyNA(dots_syms)) { todo("counts by expressions"); next }
      by <- c(groups, vapply(dots_syms, sas_name, character(1)))
      nm <- sas_arg_strings(a$named$name) %||% "n"
      flush()
      out <- c(out, "proc sql;",
        sprintf("  create table %s as", target),
        sprintf("  select %scount(*) as %s", if (length(by)) paste0(paste(by, collapse = ", "), ", ") else "", sas_name(nm)),
        sprintf("  from %s%s;", cur, if (length(by)) paste0(" group by ", paste(by, collapse = ", ")) else ""),
        "quit;")
      cur <- target
    } else if (fn == "summarise") {
      items <- vapply(seq_along(a$dots), function(i) {
        x <- sas_expr(dots_expr[[i]], agg = TRUE)
        if (is.null(x)) NA_character_ else paste(x, "as", sas_name(a$dots[[i]]$name))
      }, character(1))
      if (anyNA(items)) { todo("a summary without SAS translation"); next }
      flush()
      out <- c(out, "proc sql;",
        sprintf("  create table %s as", target),
        sprintf("  select %s", paste(c(groups, items), collapse = ", ")),
        sprintf("  from %s%s;", cur, if (length(groups)) paste0(" group by ", paste(groups, collapse = ", ")) else ""),
        "quit;")
      cur <- target
      groups <- character(0)
    } else if (fn %in% c("left_join", "semi_join")) {
      y <- a$named$y
      by <- sas_arg_strings(a$named$by, ctx)
      inline <- if (is.null(y$ref)) sas_inline_data(sas_arg_expr(y), ctx)
      if ((is.null(y$ref) && is.null(inline)) || is.null(by)) { todo("a join of data that is not a block, or without key columns"); next }
      flush()
      if (!is.null(inline)) out <- c(out, inline$code)
      other <- if (!is.null(inline)) inline$data else sas_data_of(y$ref, ctx)
      on <- paste(sprintf("a.%s = b.%s", vapply(by, sas_name, ""), vapply(by, sas_name, "")), collapse = " and ")
      out <- c(out, "proc sql;", sprintf("  create table %s as", target),
        if (fn == "left_join") c(
          "  select a.*, b.*",
          sprintf("  from %s as a left join %s as b on %s;", cur, other, on)
        ) else c(
          "  select a.*",
          sprintf("  from %s as a where exists (select 1 from %s as b where %s);", cur, other, on)
        ),
        "quit;")
      cur <- target
    } else if (fn == "bind_rows") {
      sets <- vapply(a$dots, function(d) if (!is.null(d$ref)) sas_data_of(d$ref, ctx) else NA_character_, character(1))
      if (anyNA(sets)) { todo("stacking rows of data that are not blocks"); next }
      flush()
      out <- c(out, sprintf("data %s;", target), sprintf("  set %s;", paste(c(cur, sets), collapse = " ")), "run;")
      cur <- target
    } else if (fn == "head") {
      k <- sas_arg_expr(a$named$n %||% (if (length(a$dots)) a$dots[[1]]))
      flush()
      out <- c(out, sprintf("data %s;", target), sprintf("  set %s(obs=%s);", cur, if (is.numeric(k)) k else 6), "run;")
      cur <- target
    } else if (fn == "as_listing") {
      flush()
      out <- c(out, sas_listing(cur, a, ctx, data_id))
      cur <- target  # a listing, not data: nothing to copy
    } else if (fn == "build_table") {
      flush()
      out <- c(out, sas_todo("a table built from a piped layout", NULL))
      cur <- target
    } else {
      todo(sprintf("%s() has no SAS translation", n$fn))
    }
  }
  flush(final = TRUE)
  list(code = out, data = cur)
}

#' Data written inside an argument, e.g. left_join(x, count(adsl, ARM)):
#' a dataset name, or count() of one
#' @return list(code, data) or NULL
#' @noRd
sas_inline_data <- function(e, ctx) {
  data_of_name <- function(x) {
    if (!is.name(x)) return(NULL)
    v <- as.character(x)
    if (v %in% ctx$vars) return(paste0("work.", sas_name(v)))
    if (toupper(v) %in% names(blockr_dataset_labels())) return(paste0(ctx$libname, ".", tolower(v)))
    NULL
  }
  direct <- data_of_name(e)
  if (!is.null(direct)) return(list(code = character(0), data = direct))
  if (!(is.call(e) && identical(e[[1]], as.name("count")))) return(NULL)
  args <- as.list(e)[-1]
  nm <- names(args) %||% rep("", length(args))
  src <- data_of_name(args[[1]])
  by <- args[-1][nm[-1] == ""]
  if (is.null(src) || !all(vapply(by, is.name, logical(1)))) return(NULL)
  by <- vapply(by, function(b) sas_name(as.character(b)), character(1))
  cnt <- if ("name" %in% nm && is.character(args[[match("name", nm)]])) args[[match("name", nm)]] else "n"
  tmp <- sas_tmp(ctx, "count")
  list(data = tmp, code = c(
    "proc sql;",
    sprintf("  create table %s as", tmp),
    sprintf("  select %s", paste(c(by, sprintf("count(*) as %s", sas_name(cnt))), collapse = ", ")),
    sprintf("  from %s%s;", src, if (length(by)) paste0(" group by ", paste(by, collapse = ", ")) else ""),
    "quit;"
  ))
}

#' Steps after Build table: comments on what they do to the table
#' @noRd
sas_table_tail <- function(fns) {
  notes <- c(
    prune_table = "rows where every count is zero are left out",
    sort_at_path = "row groups are sorted by their counts, largest first"
  )
  known <- intersect(fns, names(notes))
  if (!length(known)) return(character(0))
  sprintf("/* In the R table, %s. */", paste(notes[known], collapse = "; "))
}

# ---- Tables ---------------------------------------------------------------------------------

#' What a layout stack does: columns, row groups and the analyses
#' @noRd
sas_layout <- function(members, ctx) {
  spec <- list(col = NULL, overall = NULL, rows = character(0), analyses = list(), todo = character(0))
  for (m in members) {
    n <- ctx$nodes[[m]]
    if (isTRUE(n$off)) next
    fn <- sub("^.*::", "", n$fn %||% "")
    a <- sas_args(n)
    strs <- function(name) sas_arg_strings(a$named[[name]], ctx)
    first_str <- function(name) { s <- strs(name); if (length(s)) s[[1]] }
    add <- function(type, ...) spec$analyses[[length(spec$analyses) + 1L]] <<- c(list(type = type, rows = spec$rows), list(...))
    switch(fn,
      basic_table = NULL,
      split_cols_by = spec$col <- first_str("var"),
      add_overall_col = spec$overall <- first_str("label") %||% "All Patients",
      split_rows_by = spec$rows <- c(spec$rows, first_str("var")),
      split_cols_by_multivar = spec$colvars <- strs("vars"),
      summarize_colvars = add("colvars", vars = spec$colvars),
      append_topleft = NULL,
      summarize_row_groups = NULL,
      summarize_num_patients = add("patients", id = first_str("var") %||% "USUBJID"),
      count_occurrences = add("occurrences", vars = strs("vars"), id = first_str("id") %||% "USUBJID"),
      analyze_vars = add("describe", vars = strs("vars")),
      count_patients_with_flags = add("flags", id = first_str("var") %||% "USUBJID", flags = strs("flag_variables")),
      count_occurrences_by_grade = add("grade", var = first_str("var"), id = first_str("id") %||% "USUBJID"),
      summarize_occurrences_by_grade = add("grade", var = first_str("var"), id = first_str("id") %||% "USUBJID"),
      spec$todo <- c(spec$todo, n$fn)
    )
  }
  what <- paste(c(
    if (!is.null(spec$col)) paste("columns by", spec$col),
    if (!is.null(spec$overall)) sprintf("a \"%s\" column", spec$overall),
    if (length(spec$colvars)) paste("statistics of", paste(spec$colvars, collapse = " and "), "side by side"),
    if (length(spec$rows)) paste("rows by", paste(spec$rows, collapse = " > "))
  ), collapse = ", ")
  spec$what <- if (nzchar(what)) what else "one column"
  spec
}

#' build_table(lyt, df, alt_counts_df) as PROC SQL / MEANS steps
#' @noRd
sas_table <- function(n, a, target, ctx) {
  lyt <- a$named$lyt
  df <- a$named$df
  if (is.null(lyt$ref) || is.null(df$ref)) return(sas_todo("a table whose layout or rows are not blocks", NULL))
  lch <- ctx$chain_of[[lyt$ref]]
  spec <- ctx$layouts[[lch$var]]
  if (is.null(spec)) return(sas_todo("a table whose layout is not a table layout stack", NULL))
  rows_data <- sas_data_of(df$ref, ctx)
  n_data <- if (!is.null(a$named$alt_counts_df$ref)) sas_data_of(a$named$alt_counts_df$ref, ctx) else rows_data
  types <- sas_columns(df$ref, ctx)
  col <- spec$col
  base <- sub("^work\\.", "", target)
  rows_t <- paste0("work._", base, "_rows")
  n_t <- paste0("work._", base, "_n")

  twice <- function(from, to) c(
    sprintf("data %s;", to),
    if (!is.null(col) && !is.null(spec$overall)) sprintf("  length %s $ 200;", sas_name(col)),
    sprintf("  set %s;", from),
    "  output;",
    if (!is.null(col) && !is.null(spec$overall)) c(sprintf("  %s = %s;", sas_name(col), sas_str(spec$overall)), "  output;"),
    "run;"
  )
  colv <- if (!is.null(col)) sas_name(col)
  out <- c(
    sprintf("/* ---- Table %s: %s; the rows from %s, N from %s ---- */", base, spec$what, rows_data, n_data),
    if (!is.null(spec$overall)) sprintf("/* every row once per column, and once more for \"%s\" */", spec$overall),
    twice(rows_data, rows_t),
    twice(n_data, paste0(n_t, "_src")),
    "proc sql;",
    sprintf("  create table %s as select %scount(distinct USUBJID) as N", n_t, if (!is.null(colv)) paste0(colv, ", ") else ""),
    sprintf("  from %s_src%s;", n_t, if (!is.null(colv)) paste0(" group by ", colv) else ""),
    "quit;"
  )
  if (length(spec$todo)) out <- c(out, sas_todo(paste("layout blocks without SAS translation:", paste(spec$todo, collapse = ", ")), NULL))
  for (k in seq_along(spec$analyses)) {
    an <- spec$analyses[[k]]
    res <- sprintf("work._%s_%d", base, k)
    grp <- vapply(an$rows, sas_name, character(1))
    # the grouping columns ("a.AEBODSYS, a.ACTARM"); `...` adds more items to
    # the list, empty pieces are dropped so there is never a dangling comma
    by_cols <- function(extra = character(0), ...) {
      v <- c(grp, extra, colv)
      items <- c(if (length(v)) paste0("a.", v), ...)
      paste(items[nzchar(items)], collapse = ", ")
    }
    join_n <- sprintf("from %s as a left join %s as b on %s", rows_t, n_t, if (!is.null(colv)) sprintf("a.%s = b.%s", colv, colv) else "1 = 1")
    pct <- function(what) sprintf("calculated %s / b.N * 100 as pct_%s format=5.1", what, what)
    body <- switch(an$type,
      patients = c(
        sprintf("/* patients with at least one record, and the number of records%s */", if (length(grp)) paste(" by", paste(an$rows, collapse = " > ")) else ""),
        "proc sql;",
        sprintf("  create table %s as", res),
        sprintf("  select %s", by_cols(character(0), sprintf("count(distinct a.%s) as patients", sas_name(an$id)), pct("patients"), "count(*) as records")),
        sprintf("  %s", join_n),
        sprintf("  group by %s;", by_cols(character(0), "b.N")),
        "quit;"
      ),
      occurrences = unlist(lapply(an$vars, function(v) c(
        sprintf("/* patients per %s%s */", v, if (length(grp)) paste(" within", paste(an$rows, collapse = " > ")) else ""),
        "proc sql;",
        sprintf("  create table %s as", res),
        sprintf("  select %s", by_cols(sas_name(v), sprintf("count(distinct a.%s) as patients", sas_name(an$id)), pct("patients"))),
        sprintf("  %s", join_n),
        sprintf("  group by %s;", by_cols(sas_name(v), "b.N")),
        "quit;"
      ))),
      flags = c(
        "/* patients with at least one record where the flag is true */",
        "proc sql;",
        sprintf("  create table %s as", res),
        sprintf("  select %s", by_cols(character(0), vapply(an$flags, function(f) sprintf("count(distinct case when a.%s = 1 then a.%s end) as %s", sas_name(f), sas_name(an$id), sas_name(f)), character(1)))),
        sprintf("  %s", join_n),
        sprintf("  group by %s;", by_cols(character(0), "b.N")),
        "quit;"
      ),
      grade = c(
        sprintf("/* patients by their worst %s */", an$var),
        "proc sql;",
        sprintf("  create table %s_worst as", res),
        sprintf("  select %s", by_cols(character(0), paste0("a.", sas_name(an$id)), sprintf("max(a.%s) as worst", sas_name(an$var)))),
        sprintf("  from %s as a", rows_t),
        sprintf("  group by %s;", by_cols(character(0), paste0("a.", sas_name(an$id)))),
        sprintf("  create table %s as", res),
        sprintf("  select %s", by_cols(character(0), sprintf("a.worst as %s", sas_name(an$var)), "count(*) as patients", pct("patients"))),
        sprintf("  from %s_worst as a left join %s as b on %s", res, n_t, if (!is.null(colv)) sprintf("a.%s = b.%s", colv, colv) else "1 = 1"),
        sprintf("  group by %s;", by_cols(character(0), "a.worst", "b.N")),
        "quit;"
      ),
      colvars = c(
        sprintf("/* %s: n, mean, SD, median, range%s */", paste(an$vars, collapse = ", "), if (length(grp)) paste(" by", paste(an$rows, collapse = " > ")) else ""),
        sprintf("proc means data=%s n mean std median min max maxdec=2 nway;", rows_t),
        if (length(c(grp, colv))) sprintf("  class %s;", paste(c(grp, colv), collapse = " ")),
        sprintf("  var %s;", paste(vapply(an$vars, sas_name, ""), collapse = " ")),
        sprintf("  output out=%s(drop=_type_ _freq_) n= mean= std= median= min= max= / autoname;", res),
        "run;"
      ),
      describe = {
        vars <- an$vars
        num <- vars[!is.null(types) & vars %in% names(types)[types %in% c("num", "int")]]
        cat <- setdiff(vars, num)
        c(
          if (length(num)) c(
            sprintf("/* %s: n, mean, SD, median, range */", paste(num, collapse = ", ")),
            sprintf("proc means data=%s n mean std median min max maxdec=1 nway;", rows_t),
            if (length(c(grp, colv))) sprintf("  class %s;", paste(c(grp, colv), collapse = " ")),
            sprintf("  var %s;", paste(vapply(num, sas_name, ""), collapse = " ")),
            sprintf("  output out=%s_num(drop=_type_ _freq_) n= mean= std= median= min= max= / autoname;", res),
            "run;"
          ),
          unlist(lapply(cat, function(v) c(
            sprintf("/* %s: counts and percentages */", v),
            "proc sql;",
            sprintf("  create table %s_%s as", res, v),
            sprintf("  select %s", by_cols(sas_name(v), "count(*) as n", "calculated n / b.N * 100 as pct format=5.1")),
            sprintf("  %s", join_n),
            sprintf("  group by %s;", by_cols(sas_name(v), "b.N")),
            "quit;"
          )))
        )
      }
    )
    shown <- switch(an$type,
      describe = NULL,
      colvars = sprintf("proc print data=%s noobs label; run;", res),
      flags = sprintf("proc print data=%s noobs label; run;", res),
      sas_show(res, c(an$rows, if (an$type == "occurrences") an$vars, if (an$type == "grade") an$var), col)
    )
    out <- c(out, body, shown)
  }
  out
}

#' Show a table result with the columns side by side (one per value of `col`)
#' @noRd
sas_show <- function(res, rows, col) {
  if (is.null(col)) return(c(sprintf("proc print data=%s noobs label; run;", res)))
  by <- paste(vapply(rows, sas_name, character(1)), collapse = " ")
  c(
    sprintf("proc sort data=%s; by %s; run;", res, if (nzchar(by)) by else sas_name(col)),
    sprintf("proc transpose data=%s out=%s_wide(drop=_name_);", res, res),
    if (nzchar(by)) sprintf("  by %s;", by),
    sprintf("  id %s;", sas_name(col)),
    "  var patients;",
    "run;",
    sprintf("proc print data=%s_wide noobs label; run;", res)
  )
}

# ---- Listings, plots, survival ------------------------------------------------------------

#' as_listing(df, key_cols, disp_cols) as PROC REPORT
#' @noRd
sas_listing <- function(data, a, ctx, data_id = NULL) {
  keys <- sas_arg_strings(a$named$key_cols, ctx) %||% "USUBJID"
  disp <- sas_arg_strings(a$named$disp_cols, ctx)
  # disp_cols = names(out): the columns of the data, when the app ran it
  if (is.null(disp) && !is.null(data_id)) disp <- names(sas_columns(data_id, ctx))
  cols <- unique(c(keys, disp))
  title <- sas_arg_strings(a$named$main_title)
  c(
    "/* the listing: key columns shown once per group */",
    if (length(title)) sprintf("title %s;", sas_str(title[1])),
    sprintf("proc report data=%s nowd headline split='|';", data),
    if (length(disp)) sprintf("  column %s;", paste(vapply(cols, sas_name, ""), collapse = " ")),
    sprintf("  define %s / order;", vapply(keys, sas_name, "")),
    "run;",
    if (length(title)) "title;"
  )
}

#' g_km(df, variables = list(tte, is_event, arm)) as PROC LIFETEST
#' @noRd
sas_km <- function(data, variables) {
  e <- sas_arg_expr(variables)
  v <- if (is.call(e) && identical(e[[1]], as.name("list"))) as.list(e)[-1] else list()
  tte <- v$tte %||% "AVAL"
  event <- v$is_event %||% "is_event"
  arm <- v$arm
  c(
    "/* Kaplan-Meier estimates and plot, with the numbers at risk */",
    sprintf("proc lifetest data=%s plots=survival(atrisk);", data),
    sprintf("  time %s * %s(0);", sas_name(tte), sas_name(event)),
    if (!is.null(arm)) sprintf("  strata %s;", sas_name(arm)),
    "run;"
  )
}

#' A ggplot2 chain (ggplot() + layers) as PROC SGPLOT / SGPANEL
#' @noRd
sas_plot <- function(members, data, ctx) {
  nodes <- ctx$nodes
  gg <- nodes[[members[1]]]
  a <- sas_args(gg)
  if (is.null(data)) {
    d <- a$named$data
    data <- if (!is.null(d$ref)) sas_data_of(d$ref, ctx) else "work._missing"
  }
  aes_of <- function(code) {
    e <- tryCatch(str2lang(code), error = function(err) NULL)
    if (!(is.call(e) && identical(e[[1]], as.name("aes")))) return(list())
    args <- as.list(e)[-1]
    nm <- names(args) %||% rep("", length(args))
    nm[nm == ""] <- c("x", "y")[seq_len(sum(nm == ""))]
    nm[nm == "color"] <- "colour"
    stats::setNames(args, nm)
  }
  mapping <- aes_of(a$named$mapping$code %||% (if (length(a$dots)) a$dots[[1]]$code) %||% "")
  types <- sas_columns(gg$input %||% a$named$data$ref %||% "", ctx)
  var <- function(e) if (is.null(e)) NULL else if (is.name(e)) sas_name(as.character(e)) else if (is.call(e) && length(e) > 1 && is.name(e[[2]])) sas_name(as.character(e[[2]])) else NULL
  is_num <- function(e) { v <- var(e); !is.null(v) && !is.null(types) && isTRUE(types[[gsub("\"|n$", "", v)]] %in% c("num", "int")) }
  x <- var(mapping$x)
  y <- var(mapping$y)
  group <- var(mapping$fill %||% mapping$colour %||% mapping$group)
  grp <- if (!is.null(group)) paste0(" group=", group) else ""
  stmts <- character(0)
  opts <- character(0)
  panel <- NULL
  todo <- character(0)
  for (m in members[-1]) {
    n <- nodes[[m]]
    if (isTRUE(n$off)) next
    fn <- sub("^.*::", "", n$fn %||% "")
    la <- sas_args(n)
    lmap <- aes_of(la$named$mapping$code %||% (if (length(la$dots) && !nzchar(la$dots[[1]]$name)) la$dots[[1]]$code) %||% "")
    switch(fn,
      geom_histogram = stmts <- c(stmts, sprintf("  histogram %s /%s transparency=0.3;", x %||% y, grp)),
      geom_bar = stmts <- c(stmts, sprintf("  vbar %s /%s;", x, grp)),
      geom_col = {
        # bars along y: a category on y (reorder() makes one even without the run's column types)
        horizontal <- (!is.null(mapping$y) && !is_num(mapping$y) && is_num(mapping$x)) ||
          (is.call(mapping$y) && identical(mapping$y[[1]], as.name("reorder")))
        order <- if (is.call(mapping$y) && identical(mapping$y[[1]], as.name("reorder"))) " categoryorder=respdesc" else ""
        stmts <- c(stmts, if (horizontal) sprintf("  hbar %s / response=%s%s groupdisplay=cluster%s;", y, x, grp, order)
                   else sprintf("  vbar %s / response=%s%s groupdisplay=cluster;", x, y, grp))
      },
      geom_point = stmts <- c(stmts, sprintf("  scatter x=%s y=%s /%s;", x, y, grp)),
      geom_line = stmts <- c(stmts, sprintf("  series x=%s y=%s /%s;", x, y, grp)),
      geom_boxplot = stmts <- c(stmts, sprintf("  vbox %s / category=%s%s;", y, x, grp)),
      geom_errorbar = {
        lo <- var(lmap$ymin %||% lmap$xmin)
        hi <- var(lmap$ymax %||% lmap$xmax)
        stmts <- c(stmts, if (!is.null(lmap$xmin)) sprintf("  highlow y=%s low=%s high=%s;", y, lo, hi) else sprintf("  highlow x=%s low=%s high=%s;", x, lo, hi))
      },
      geom_vline = stmts <- c(stmts, sprintf("  refline %s / axis=x lineattrs=(pattern=dash);", la$named$xintercept$code %||% "0")),
      geom_hline = stmts <- c(stmts, sprintf("  refline %s / axis=y lineattrs=(pattern=dash);", la$named$yintercept$code %||% "0")),
      facet_wrap = {
        f <- sas_arg_expr(la$named$facets %||% (if (length(la$dots)) la$dots[[1]]))
        if (is.call(f) && identical(f[[1]], as.name("~"))) panel <- var(f[[length(f)]])
      },
      labs = {
        s <- function(k) { v <- sas_arg_strings(la$named[[k]]); if (length(v)) v[[1]] }
        if (!is.null(s("title"))) opts <- c(opts, sprintf("title %s;", sas_str(s("title"))))
        if (!is.null(s("x"))) stmts <- c(stmts, sprintf("  %s label=%s;", if (!is.null(panel)) "colaxis" else "xaxis", sas_str(s("x"))))
        if (!is.null(s("y"))) stmts <- c(stmts, sprintf("  %s label=%s;", if (!is.null(panel)) "rowaxis" else "yaxis", sas_str(s("y"))))
      },
      scale_y_log10 = stmts <- c(stmts, "  yaxis type=log logbase=10;"),
      theme_minimal = NULL, theme = NULL, theme_bw = NULL, theme_classic = NULL,
      todo <- c(todo, n$fn)
    )
  }
  proc <- if (!is.null(panel)) c(sprintf("proc sgpanel data=%s;", data), sprintf("  panelby %s / novarname;", panel)) else sprintf("proc sgplot data=%s;", data)
  stmts <- sub("^  xaxis ", if (!is.null(panel)) "  colaxis " else "  xaxis ", stmts)
  c(
    "/* the plot */",
    opts,
    proc,
    stmts,
    if (length(todo)) sprintf("  /* TODO: layers without SAS translation: %s */", paste(todo, collapse = ", ")),
    "run;",
    if (length(opts)) "title;"
  )
}
