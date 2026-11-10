# Block graphs to R code (see R/blocks_graph.R for the graph).
#
# blockr_compile() writes a graph as R code, in two forms:
# * "clean": the code users read and copy ("Show R code"). Blocks stacked
#   on top of each other become one pipe chain again, assigned to the
#   variable of its last block; statements come in dependency order (ties in
#   the order of the original code), with the template's comments.
# * "debug": one statement per block, each assigning its own variable
#   (.bk_n1, .bk_n2, ...), so that R can evaluate the graph block by block,
#   cache every block's value and show it on the canvas.
# Both give the same values.

#' Compile a block graph to R code
#' @param graph A graph (see R/blocks_graph.R).
#' @param mode "clean" or "debug".
#' @param width Line width of the clean code.
#' @param registry Block registry (see blockr_registry()): blocks that take
#'   an input must have one.
#' @return list(code, order, errors, steps, chains). `code`: the clean code
#'   (one string). `chains` (clean): the stacked blocks written as one
#'   statement, each with the variable it is assigned to. `order`: node ids in evaluation order. `errors`: named list of
#'   messages of the blocks that cannot be compiled (by id). `steps`
#'   ("debug"): per block, in order, list(id, exprs, assigns, text).
#' @noRd
blockr_compile <- function(graph, mode = c("clean", "debug"), width = 80L, registry = NULL) {
  mode <- match.arg(mode)
  nodes <- graph$nodes
  ids <- vapply(nodes, `[[`, character(1), "id")
  names(nodes) <- ids
  deps <- blockr_graph_deps(graph)
  errors <- list()

  # ---- problems found before running: broken wires, invalid R code
  for (n in nodes) {
    msg <- blockr_node_problem(n, ids, nodes, registry)
    if (!is.null(msg)) errors[[n$id]] <- msg
  }

  order <- blockr_topo_order(nodes, deps)
  for (id in attr(order, "cycle")) errors[[id]] <- "This block is part of a loop: a block cannot depend on itself."

  # a block whose input or references failed cannot be built either
  failed <- names(errors)
  for (id in order) {
    d <- deps[[id]]
    up <- c(d$input, d$refs, d$implicit)
    if (!id %in% failed && any(up %in% failed)) failed <- c(failed, id)
  }

  debug_var <- function(id) {
    n <- nodes[[id]]
    if (identical(n$type, "dataset")) n$dataset else paste0(".bk_", id)
  }

  if (mode == "debug") {
    steps <- list()
    for (id in order) {
      n <- nodes[[id]]
      if (id %in% failed || identical(n$type, "dataset")) next
      exprs <- if (identical(n$type, "code")) {
        as.list(parse(text = n$code, keep.source = FALSE))
      } else {
        value <- blockr_node_expr(n, debug_var)
        c(
          list(call("<-", as.name(debug_var(id)), value)),
          if (length(n$name)) list(call("<-", as.name(n$name), as.name(debug_var(id))))
        )
      }
      assigns <- if (identical(n$type, "code")) blockr_node_defines(n) else c(debug_var(id), n$name)
      steps[[length(steps) + 1L]] <- list(
        id = id, exprs = exprs, assigns = unique(assigns),
        text = paste(vapply(exprs, blockr_text, character(1)), collapse = "\n")
      )
    }
    return(list(code = NULL, order = order, errors = errors, failed = failed, steps = steps))
  }

  # ---- clean code: chains of stacked blocks
  consumers <- blockr_consumers(deps)
  continues <- function(id) {
    n <- nodes[[id]]
    x <- n$input
    if (is.null(x) || !x %in% ids || identical(nodes[[x]]$type, "dataset")) return(FALSE)
    if (identical(nodes[[x]]$type, "code") || identical(n$type, "code")) return(FALSE)
    # a named block keeps its variable: the chain ends there
    if (length(nodes[[x]]$name)) return(FALSE)
    cons <- consumers[[x]]
    length(cons$id) == 1 && identical(cons$id, id) && identical(cons$via, "pipe") &&
      !identical(graph$result, x)
  }
  chain_of <- list()
  chains <- list()
  for (id in order) {
    if (id %in% failed) next
    if (continues(id) && !is.null(chain_of[[nodes[[id]]$input]])) {
      k <- chain_of[[nodes[[id]]$input]]
      chains[[k]] <- c(chains[[k]], id)
    } else {
      k <- length(chains) + 1L
      chains[[k]] <- id
    }
    chain_of[[id]] <- k
  }

  # variable of each chain: the name of its last named block, else a new one
  taken <- unique(c(unlist(lapply(nodes, blockr_node_defines)), unlist(lapply(nodes, function(n) n$dataset))))
  chain_var <- character(length(chains))
  for (k in seq_along(chains)) {
    members <- chains[[k]]
    first <- nodes[[members[1]]]
    if (identical(first$type, "dataset")) {
      chain_var[k] <- first$dataset
      next
    }
    if (identical(first$type, "code")) next
    named <- Filter(length, lapply(rev(members), function(m) nodes[[m]]$name))
    if (length(named)) {
      chain_var[k] <- named[[1]]
    } else {
      base <- blockr_auto_name(members[1], nodes)
      v <- base
      i <- 1L
      while (v %in% taken) {
        i <- i + 1L
        v <- paste0(base, "_", i)
      }
      taken <- c(taken, v)
      chain_var[k] <- v
    }
  }
  clean_var <- function(id) chain_var[[chain_of[[id]]]]

  statements <- character(0)
  for (k in seq_along(chains)) {
    members <- chains[[k]]
    first <- nodes[[members[1]]]
    if (identical(first$type, "dataset")) next
    notes <- unlist(lapply(members, function(m) nodes[[m]]$note))
    comment <- if (length(notes)) paste0("# ", blockr_wrap(notes, width - 2L), collapse = "\n")
    if (identical(first$type, "code")) {
      body <- vapply(as.list(parse(text = first$code, keep.source = FALSE)), blockr_format, character(1), width = width)
    } else {
      value <- NULL
      for (m in members) {
        n <- nodes[[m]]
        has_input <- !is.null(n$input)
        if (is.null(value) && has_input) value <- as.name(clean_var(n$input))
        if (isTRUE(n$off) && has_input) next
        value <- blockr_node_expr(n, clean_var, input = value)
      }
      body <- blockr_format(call("<-", as.name(chain_var[k]), value), width = width)
    }
    statements <- c(statements, paste(c(comment, body), collapse = "\n"))
  }

  packages <- blockr_graph_packages(graph, registry)
  code <- paste(c(
    if (length(packages)) paste(c(paste0("library(", packages, ")"), ""), collapse = "\n"),
    paste(statements, collapse = "\n\n")
  ), collapse = "\n")
  list(
    code = code, order = order, errors = errors, failed = failed,
    vars = stats::setNames(lapply(order, function(id) if (!is.null(chain_of[[id]])) clean_var(id)), order),
    # the chains, in the order of the code (for other translations, e.g. SAS)
    chains = lapply(seq_along(chains), function(k) list(var = chain_var[[k]], members = chains[[k]]))
  )
}

#' A node's value as an R expression
#' @param var Function giving the variable of a node (by id).
#' @param input Expression of the piped input (clean code: the chain so
#'   far). Default: the input node's variable.
#' @noRd
blockr_node_expr <- function(n, var, input = NULL) {
  if (identical(n$type, "dataset")) return(as.name(n$dataset))
  if (is.null(input) && !is.null(n$input)) input <- as.name(var(n$input))
  if (isTRUE(n$off) && !is.null(input)) return(input)
  own <- switch(n$type,
    call = {
      args <- lapply(n$args, function(a) if (!is.null(a$ref)) as.name(var(a$ref)) else str2lang(a$value))
      names(args) <- vapply(n$args, function(a) a$name %||% "", character(1))
      if (all(names(args) == "")) names(args) <- NULL
      as.call(c(list(str2lang(n$fn)), args))
    },
    expr = str2lang(n$code)
  )
  if (is.null(input)) own else call(n$op %||% "%>%", input, own)
}

#' What prevents a block from being compiled (NULL if nothing)
#' @noRd
blockr_node_problem <- function(n, ids, nodes, registry = NULL) {
  if (!is.null(n$input) && !n$input %in% ids) return("Its input block is missing: connect it below another block.")
  spec <- if (identical(n$type, "call") && !is.null(n$fn)) registry[[n$fn]]
  if (is.null(n$input) && isTRUE(spec$input) && !identical(spec$op, "+")) {
    # the data can also be given as the first argument, wired to a block:
    # ggplot(adsl, ...), as_listing(out, ...)
    first <- spec$formals[1]
    given <- length(n$args) && (
      (!nzchar(n$args[[1]]$name %||% "") && !is.null(n$args[[1]]$ref)) ||
        any(vapply(n$args, function(a) identical(a$name, first), logical(1)))
    )
    if (!given) return("Place this block below another block: it works on the result of the block above.")
  }
  if (is.null(n$input) && identical(spec$op, "+")) {
    return("Place this layer below a Plot block: it is added to the plot above.")
  }
  if (!is.null(n$input) && identical(nodes[[n$input]]$type, "code")) {
    return("A code block has no output: connect this block to another block.")
  }
  check <- function(text, what) {
    if (is.null(text) || !nzchar(trimws(text))) return(sprintf("%s is empty.", what))
    e <- tryCatch(parse(text = text, keep.source = FALSE), error = function(e) e)
    if (inherits(e, "error")) {
      return(sprintf("%s is not valid R code: %s", what, sub("^<text>:", "", conditionMessage(e))))
    }
    if (length(e) != 1L) return(sprintf("%s must be a single R expression.", what))
    NULL
  }
  switch(n$type,
    dataset = if (is.null(n$dataset) || !nzchar(n$dataset)) "Choose a dataset.",
    call = {
      if (is.null(n$fn) || !blockr_is_simple_call(tryCatch(str2lang(paste0(n$fn, "()")), error = function(e) NULL))) {
        return(sprintf("%s is not a function name.", sQuote(n$fn %||% "", FALSE)))
      }
      for (a in n$args) {
        what <- if (nzchar(a$name %||% "")) sprintf("Setting %s", sQuote(a$name, FALSE)) else "A setting"
        if (!is.null(a$ref)) {
          if (!a$ref %in% ids) return(sprintf("%s is connected to a block that no longer exists.", what))
          if (identical(nodes[[a$ref]]$type, "code")) return(sprintf("%s is connected to a code block, which has no output.", what))
          next
        }
        if (is.null(a$value)) return(sprintf("%s is not connected: connect a block to it.", what))
        msg <- check(a$value, what)
        if (!is.null(msg)) return(msg)
      }
      NULL
    },
    expr = check(n$code, "The expression"),
    code = {
      if (is.null(n$code) || !nzchar(trimws(n$code))) return("The code is empty.")
      e <- tryCatch(parse(text = n$code, keep.source = FALSE), error = function(e) e)
      if (inherits(e, "error")) sprintf("The code is not valid R code: %s", sub("^<text>:", "", conditionMessage(e)))
    },
    sprintf("Unknown block type %s.", sQuote(n$type %||% "", FALSE))
  )
}

#' Evaluation order: dependencies first, otherwise the order of the code
#'
#' Kahn's algorithm, always taking the ready block that comes first in the
#' code: a graph made from code is evaluated in the code's own order.
#' Blocks in a loop are left out (attribute "cycle").
#' @noRd
blockr_topo_order <- function(nodes, deps) {
  ids <- names(deps)
  ord <- vapply(nodes[ids], function(n) as.numeric(n$order %||% NA), numeric(1))
  ord[is.na(ord)] <- max(c(0, ord), na.rm = TRUE) + seq_len(sum(is.na(ord)))
  need <- lapply(deps, function(d) intersect(unique(c(d$input, d$refs, d$implicit)), ids))
  done <- character(0)
  out <- character(0)
  left <- ids
  while (length(left)) {
    ready <- left[vapply(left, function(id) all(need[[id]] %in% done), logical(1))]
    if (!length(ready)) break
    pick <- ready[which.min(ord[ready])]
    out <- c(out, pick)
    done <- c(done, pick)
    left <- setdiff(left, pick)
  }
  structure(out, cycle = left)
}

#' Consumers of every node: who uses it, and how ("pipe", "ref", "implicit")
#' @noRd
blockr_consumers <- function(deps) {
  out <- stats::setNames(rep(list(list(id = character(0), via = character(0))), length(deps)), names(deps))
  push <- function(from, to, via) {
    if (is.null(from) || !from %in% names(out)) return()
    out[[from]]$id <<- c(out[[from]]$id, to)
    out[[from]]$via <<- c(out[[from]]$via, via)
  }
  for (id in names(deps)) {
    d <- deps[[id]]
    push(d$input, id, "pipe")
    for (r in d$refs) push(r, id, "ref")
    for (r in d$implicit) push(r, id, "implicit")
  }
  out
}

#' Name for the variable of a chain without a named block: after its
#' dataset (adae), else its first function (basic_table -> lyt)
#' @noRd
blockr_auto_name <- function(id, nodes) {
  seen <- character(0)
  n <- nodes[[id]]
  while (!is.null(n$input) && !n$input %in% seen && !is.null(nodes[[n$input]])) {
    seen <- c(seen, n$input)
    n <- nodes[[n$input]]
  }
  if (identical(n$type, "dataset")) return(tolower(n$dataset))
  fn <- sub("^.*::", "", n$fn %||% "")
  switch(fn,
    basic_table = "lyt",
    build_table = "tbl",
    as_listing = "lst",
    ggplot = "plot",
    c = "values",
    if (nzchar(fn)) gsub("[^A-Za-z0-9_.]", "_", fn) else "x"
  )
}

#' Wrap text at `width` characters
#' @noRd
blockr_wrap <- function(text, width = 78L) {
  unlist(lapply(text, function(t) strwrap(t, width = width)))
}

# ---- Formatting ---------------------------------------------------------------

#' Format an expression as readable R code
#'
#' One line when it fits; otherwise pipe chains get one step per line and
#' function calls one argument per line, recursively.
#' @param e Expression.
#' @param indent Indentation of the lines after the first.
#' @param col Column where the expression starts.
#' @noRd
blockr_format <- function(e, width = 80L, indent = 0L, col = indent) {
  one <- blockr_text(e)
  if (!grepl("\n", one, fixed = TRUE) && col + nchar(one, type = "width") <= width) return(one)
  pad <- function(n) strrep(" ", n)

  # assignment: format the value
  if (is.call(e) && length(e) == 3 && identical(e[[1]], as.name("<-")) && is.name(e[[2]])) {
    lhs <- paste0(blockr_text(e[[2]]), " <- ")
    return(paste0(lhs, blockr_format(e[[3]], width, indent, col + nchar(lhs))))
  }

  # pipe chain / ggplot layers: one step per line
  chain <- blockr_flatten_chain(e)
  if (length(chain$steps)) {
    out <- blockr_format(chain$head, width, indent, col)
    for (s in chain$steps) {
      out <- paste0(out, " ", s$op, "\n", pad(indent + 2L), blockr_format(s$expr, width, indent + 2L))
    }
    return(out)
  }

  # function call: one argument per line
  if (blockr_is_simple_call(e) && length(e) > 1) {
    args <- as.list(e)[-1]
    nms <- names(args)
    if (is.null(nms)) nms <- rep("", length(args))
    items <- vapply(seq_along(args), function(i) {
      prefix <- if (nzchar(nms[i])) paste0(blockr_arg_name(nms[i]), " = ") else ""
      a <- args[[i]]
      value <- if (identical(a, quote(expr = ))) "" else blockr_format(a, width, indent + 2L, indent + 2L + nchar(prefix))
      paste0(prefix, value)
    }, character(1))
    return(paste0(blockr_text(e[[1]]), "(\n", paste0(pad(indent + 2L), items, collapse = ",\n"), "\n", pad(indent), ")"))
  }

  # anything else (if, function, ...): R's own layout, indented
  lines <- strsplit(paste(deparse(e, width.cutoff = max(40L, width - indent)), collapse = "\n"), "\n", fixed = TRUE)[[1]]
  paste(c(lines[1], if (length(lines) > 1) paste0(pad(indent), lines[-1])), collapse = "\n")
}

#' Argument name as written in code (backticks when needed)
#' @noRd
blockr_arg_name <- function(name) {
  if (identical(make.names(name), name)) name else paste0("`", gsub("`", "\\\\`", name), "`")
}
