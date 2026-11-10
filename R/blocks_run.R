# Running block graphs: every block is evaluated on its own, so the canvas
# can show what each block produces (rows, columns, a table, a plot) and
# exactly which block fails.
#
# Values are cached per block: the key of a block is its code plus the keys
# of the blocks it depends on, so changing one block only re-runs it and the
# blocks after it. The cache belongs to one data version (the caller empties
# it when the filtered data change).

#' Evaluate a block graph
#'
#' @param graph A graph (see R/blocks_graph.R).
#' @param datasets Named list of the datasets (data.frames).
#' @param cache Environment caching block values between runs.
#' @param parent Parent environment of the evaluation (attached packages).
#' @param registry Block registry, to check that blocks that need an input
#'   have one.
#' @return list(status, result, env, vars): `status`, per block (by id):
#'   state ("ok", "error", "blocked", "missing"), kind ("data", "listing",
#'   "table", "layout", "plot", "value"), rows, cols, schema (data: the
#'   columns, see blockr_schema()), info, error and ms (time to compute);
#'   `result`: id of the block shown as the result; `env`: the environment
#'   holding every block's value (see blockr_value()).
#' @noRd
blockr_run <- function(graph, datasets, cache = new.env(parent = emptyenv()), parent = globalenv(),
                       registry = blockr_registry()) {
  comp <- blockr_compile(graph, "debug", registry = registry)
  nodes <- stats::setNames(graph$nodes, vapply(graph$nodes, `[[`, character(1), "id"))
  deps <- blockr_graph_deps(graph)
  env <- new.env(parent = parent)
  list2env(datasets, envir = env)
  status <- list()
  keys <- list()
  failed <- character(0)

  # packages the code attaches (its library() calls, and those its blocks need)
  packages <- blockr_graph_packages(graph, registry)
  missing_pkgs <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  pkg_error <- if (length(missing_pkgs)) {
    sprintf("Needs the package%s %s, which %s not installed.",
            if (length(missing_pkgs) > 1) "s" else "", paste(sQuote(missing_pkgs, FALSE), collapse = ", "),
            if (length(missing_pkgs) > 1) "are" else "is")
  }
  if (is.null(pkg_error)) {
    for (p in packages) suppressPackageStartupMessages(library(p, character.only = TRUE))
  }

  for (id in comp$order) {
    n <- nodes[[id]]
    if (identical(n$type, "dataset")) {
      if (is.null(datasets[[n$dataset]])) {
        status[[id]] <- list(state = "error", error = sprintf("The dataset %s is not available in this app.", n$dataset))
        failed <- c(failed, id)
      } else {
        keys[[id]] <- paste0("dataset:", n$dataset)
        status[[id]] <- blockr_status_of(datasets[[n$dataset]], cache, keys[[id]])
      }
    }
  }

  up_of <- function(id) {
    d <- deps[[id]]
    intersect(unique(c(d$input, d$refs, d$implicit)), names(nodes))
  }
  steps <- stats::setNames(comp$steps, vapply(comp$steps, `[[`, character(1), "id"))
  for (id in comp$order) {
    n <- nodes[[id]]
    if (identical(n$type, "dataset")) next
    up <- up_of(id)
    if (!is.null(comp$errors[[id]])) {
      status[[id]] <- list(state = "error", error = comp$errors[[id]])
      failed <- c(failed, id)
      next
    }
    if (any(up %in% failed) || id %in% comp$failed) {
      status[[id]] <- list(state = "blocked", error = "Waiting for a block above, which has a problem.")
      failed <- c(failed, id)
      next
    }
    if (!is.null(pkg_error)) {
      status[[id]] <- list(state = "missing", error = pkg_error)
      failed <- c(failed, id)
      next
    }
    step <- steps[[id]]
    key <- blockr_hash(list(step$text, unlist(keys[up])))
    keys[[id]] <- key
    hit <- cache[[key]]
    if (!is.null(hit)) {
      for (v in names(hit$values)) assign(v, hit$values[[v]], envir = env)
      status[[id]] <- hit$status
      next
    }
    t0 <- Sys.time()
    warnings <- character(0)
    res <- tryCatch(
      withCallingHandlers(
        {
          for (e in step$exprs) eval(e, env)
          TRUE
        },
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        },
        message = function(m) invokeRestart("muffleMessage")
      ),
      error = function(e) e
    )
    ms <- round(1000 * as.numeric(difftime(Sys.time(), t0, units = "secs")))
    if (inherits(res, "error")) {
      status[[id]] <- list(state = "error", error = blockr_error_message(res), ms = ms)
      failed <- c(failed, id)
      next
    }
    values <- mget(step$assigns, envir = env, ifnotfound = list(NULL))
    # a code block may assign nothing (e.g. set.seed(1)): it has no value
    st <- blockr_status_of(if (length(values)) values[[1]], cache, key)
    st$ms <- ms
    if (length(warnings)) st$warnings <- utils::head(unique(warnings), 3)
    status[[id]] <- st
    assign(key, list(values = values, status = st), envir = cache)
  }

  # dashed wires on the canvas: variables a block's code mentions
  for (id in names(status)) {
    implicit <- deps[[id]]$implicit
    if (length(implicit)) status[[id]]$implicit <- I(implicit)
  }

  # keep only what this graph uses
  used <- unlist(keys, use.names = FALSE)
  stale <- setdiff(ls(cache, all.names = TRUE), c(used, paste0("schema:", used)))
  if (length(stale)) rm(list = stale, envir = cache)

  list(status = status, result = blockr_pick_result(graph, status, comp$order, deps), env = env, order = comp$order)
}

#' Value of a block after blockr_run()
#' @noRd
blockr_value <- function(run, graph, id) {
  n <- Filter(function(x) identical(x$id, id), graph$nodes)
  if (!length(n)) return(NULL)
  n <- n[[1]]
  var <- switch(n$type,
    dataset = n$dataset,
    code = blockr_node_defines(n)[1],
    paste0(".bk_", id)
  )
  if (is.na(var) || is.null(var)) return(NULL)
  get0(var, envir = run$env, inherits = FALSE)
}

#' Block shown as the result: the graph's choice, else the block assigned to
#' `result`, else the last table, listing or plot that nothing uses
#' @noRd
blockr_pick_result <- function(graph, status, order, deps) {
  if (!is.null(graph$result) && graph$result %in% order) return(graph$result)
  named <- Filter(function(n) identical(n$name, "result"), graph$nodes)
  if (length(named)) return(named[[length(named)]]$id)
  used <- unique(unlist(lapply(deps, function(d) c(d$input, d$refs, d$implicit))))
  sinks <- rev(setdiff(order, used))
  outputs <- Filter(function(id) isTRUE(status[[id]]$kind %in% c("table", "listing", "plot")), sinks)
  if (length(outputs)) return(outputs[[1]])
  if (length(sinks)) sinks[[1]]
}

#' Status of a value: what it is and its size
#' @noRd
blockr_status_of <- function(x, cache, key) {
  st <- c(list(state = "ok"), blockr_describe(x))
  if (is.data.frame(x)) {
    schema_key <- paste0("schema:", key)
    schema <- cache[[schema_key]]
    if (is.null(schema)) {
      schema <- blockr_schema(x)
      assign(schema_key, schema, envir = cache)
    }
    st$schema <- schema
  }
  st
}

#' What a value is, for the block's status line
#' @noRd
blockr_describe <- function(x) {
  if (inherits(x, "listing_df")) return(list(kind = "listing", rows = nrow(x), cols = ncol(x)))
  if (is.data.frame(x)) return(list(kind = "data", rows = nrow(x), cols = ncol(x)))
  if (inherits(x, "VTableTree")) {
    d <- tryCatch(c(nrow(x), ncol(x)), error = function(e) NULL)
    if (length(d) != 2) d <- tryCatch(dim(x), error = function(e) NULL)
    return(list(kind = "table", rows = if (length(d) == 2) d[[1]], cols = if (length(d) == 2) d[[2]]))
  }
  if (inherits(x, "PreDataTableLayouts")) return(list(kind = "layout"))
  if (inherits(x, c("ggplot", "gg", "grob", "gtable", "gTree", "trellis", "recordedplot"))) return(list(kind = "plot"))
  if (is.function(x)) return(list(kind = "value", info = "a function"))
  if (is.null(x)) return(list(kind = "value", info = "no value"))
  if (is.atomic(x)) {
    shown <- utils::head(x, 6)
    text <- if (is.character(x) || is.factor(x)) paste0("\"", as.character(shown), "\"") else format(shown)
    return(list(
      kind = "value", rows = length(x),
      info = paste0(paste(text, collapse = ", "), if (length(x) > 6) paste0(", \u2026 (", length(x), " values)"))
    ))
  }
  if (is.list(x)) {
    nm <- names(x)
    info <- if (length(nm)) paste(utils::head(nm, 6), collapse = ", ") else paste(length(x), "items")
    return(list(kind = "value", rows = length(x), info = paste0("list: ", info, if (length(x) > 6) ", \u2026")))
  }
  list(kind = "value", info = paste(class(x), collapse = " / "))
}

#' Columns of a data.frame, for the canvas's column pickers
#'
#' Per column: n (name), l (label), t (type: num, int, chr, fct, lgl, date,
#' dttm, other), v (values, for columns with at most 200 different values,
#' not numbers) and r (range of numbers).
#' @noRd
blockr_schema <- function(df, max_levels = 200L) {
  lapply(names(df), function(n) {
    x <- df[[n]]
    type <- if (is.factor(x)) {
      "fct"
    } else if (is.logical(x)) {
      "lgl"
    } else if (inherits(x, "Date")) {
      "date"
    } else if (inherits(x, "POSIXt")) {
      "dttm"
    } else if (is.integer(x)) {
      "int"
    } else if (is.numeric(x)) {
      "num"
    } else if (is.character(x)) {
      "chr"
    } else {
      "other"
    }
    label <- attr(x, "label", exact = TRUE)
    out <- list(n = n, l = if (length(label) && !is.na(label[1])) as.character(label[1]), t = type)
    if (type %in% c("fct", "chr", "lgl")) {
      values <- if (type == "fct") levels(x) else sort(unique(as.character(x[!is.na(x)])))
      if (length(values) <= max_levels) out$v <- I(as.character(values))
    } else if (type %in% c("num", "int") && any(!is.na(x))) {
      out$r <- range(x, na.rm = TRUE)
    }
    out
  })
}

#' Error message without the call, shortened
#' @noRd
blockr_error_message <- function(e) {
  msg <- conditionMessage(e)
  msg <- gsub("\033\\[[0-9;]*m", "", msg) # colours of cli messages
  if (nchar(msg) > 600) msg <- paste0(substr(msg, 1, 600), "\u2026")
  msg
}

#' Hash of an R object
#' @noRd
blockr_hash <- function(x) rlang::hash(x)
