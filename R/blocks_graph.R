# Block graphs: R code as a graph of blocks, and back.
#
# The canvas (R/canvas.R, inst/www/blockr_canvas.js) edits a *graph*: a plain
# list that travels as JSON between the browser and R. R code becomes a graph
# with blockr_graph_from_code(); a graph becomes R code again with
# blockr_compile() (R/blocks_compile.R). For the code of every TLG Catalog
# template, code -> graph -> code gives back the same R expressions (see
# tests/testthat/test-blocks.R), so a template opened on the canvas is
# exactly the template.
#
# Graph
#   version   1
#   packages  packages attached by the code (its library() calls)
#   nodes     list of nodes (below)
#   result    id of the node whose value is the final output (may be NULL)
#   template, title   the TLG template it came from (may be NULL)
#
# Node
#   id        "n1", "n2", ...
#   type      "dataset"  an app dataset (ADSL, ADAE, ...), by name
#             "call"     a function call, e.g. filter(SAFFL == "Y"): `fn`
#                        and `args`, each argument R code (`value`) or a
#                        reference to another node (`ref`)
#             "expr"     any other R expression (`code`)
#             "code"     a whole R statement that is not an assignment of a
#                        value (e.g. `if (...) x <- y`); it may assign
#                        variables (see blockr_node_defines())
#   input     id of the node piped into this one (`%>%`), or added to it
#             (`+`, ggplot2 layers); the pipe operator is `op`
#   name      variable the node's value is assigned to (may be NULL)
#   note      comment that preceded the statement in the code (may be NULL)
#   order     position in the code; orders independent statements
#   off       TRUE: the block is bypassed (its input passes through)
#
# Pipe chains (`x <- ADAE %>% filter(...) %>% mutate(...)`) become one node
# per step, each taking the previous one as `input`: the canvas shows them as
# a stack of blocks. References to variables that are whole arguments
# (`build_table(lyt, df = adae)`) become `ref`s: wires on the canvas.
# References inside larger expressions (`flag_variables = aesi_vars[1:3]`)
# are *implicit* dependencies (blockr_graph_deps()).

#' Operators that chain blocks
#' @noRd
blockr_chain_ops <- function() c("%>%", "+")

#' Graph of blocks from R code
#'
#' @param code R code (character; lines are joined).
#' @param datasets Names that refer to datasets (symbols not assigned by the
#'   code). Default: the ADaM datasets BlockR knows (`ADSL`, `ADAE`, ...; see
#'   blockr_dataset_labels()). Only these names: a column such as `ADY`
#'   (analysis day) is not a dataset.
#' @return A graph (list, see the top of this file).
#' @noRd
blockr_graph_from_code <- function(code, datasets = NULL) {
  code <- paste(code, collapse = "\n")
  exprs <- parse(text = code, keep.source = TRUE)
  notes <- blockr_statement_notes(exprs)
  if (is.null(datasets)) datasets <- names(blockr_dataset_labels())
  is_dataset <- function(name) name %in% datasets

  nodes <- list()
  defs <- list() # variable name -> id of the node that last assigned it
  data_nodes <- list() # dataset name -> node id
  packages <- character(0)
  counter <- 0L
  new_id <- function() {
    counter <<- counter + 1L
    paste0("n", counter)
  }
  add <- function(node) {
    node$order <- length(nodes) + 1
    nodes[[node$id]] <<- node
    node$id
  }
  dataset_node <- function(name) {
    if (is.null(data_nodes[[name]])) {
      data_nodes[[name]] <<- add(list(id = new_id(), type = "dataset", dataset = name))
    }
    data_nodes[[name]]
  }
  # Datasets used anywhere in an expression get their node (so they can be
  # found as dependencies), in order of appearance
  touch_datasets <- function(e) {
    for (s in unique(all.names(e))) {
      if (is.null(defs[[s]]) && is_dataset(s)) dataset_node(s)
    }
  }
  # Node holding a variable, if it can be wired (code statements cannot:
  # they may assign several variables; they are implicit dependencies)
  wired <- function(s) {
    id <- defs[[s]]
    if (!is.null(id) && !identical(nodes[[id]]$type, "code")) id
  }
  # A whole-argument symbol that names a variable or dataset becomes a ref
  arg_of <- function(name, value) {
    if (is.name(value) && !identical(value, quote(expr = ))) {
      s <- as.character(value)
      if (!is.null(wired(s))) return(list(name = name, ref = wired(s)))
      if (!is.null(defs[[s]])) return(list(name = name, value = s))
      if (is_dataset(s)) return(list(name = name, ref = dataset_node(s)))
    }
    touch_datasets(value)
    list(name = name, value = blockr_text(value))
  }
  call_node <- function(e, input = NULL, op = NULL) {
    if (is.name(e)) e <- as.call(list(e)) # `x %>% f` is `x %>% f()`
    args <- as.list(e)[-1]
    nms <- names(args)
    if (is.null(nms)) nms <- rep("", length(args))
    list(
      id = new_id(), type = "call", fn = blockr_text(e[[1]]),
      args = unname(Map(arg_of, nms, args)),
      input = input, op = op
    )
  }
  expr_node <- function(e) {
    touch_datasets(e)
    list(id = new_id(), type = "expr", code = blockr_text(e))
  }
  code_node <- function(e) {
    touch_datasets(e)
    list(id = new_id(), type = "code", code = blockr_text(e))
  }
  # The node a chain starts from: a variable or dataset (no node of its
  # own), a function call or another expression
  head_of <- function(e) {
    if (is.name(e)) {
      s <- as.character(e)
      if (!is.null(wired(s))) return(list(input = wired(s)))
      if (is.null(defs[[s]]) && is_dataset(s)) return(list(input = dataset_node(s)))
    }
    node <- if (blockr_is_simple_call(e)) call_node(e) else expr_node(e)
    list(input = add(node))
  }

  for (i in seq_along(exprs)) {
    e <- exprs[[i]]
    note <- notes[[i]]

    # library(pkg): the graph's packages
    if (is.call(e) && identical(e[[1]], as.name("library")) && length(e) == 2 && is.name(e[[2]])) {
      packages <- union(packages, as.character(e[[2]]))
      next
    }

    target <- blockr_assignment_target(e)
    if (is.null(target)) {
      id <- add(c(code_node(e), list(note = note)))
      for (v in blockr_assigned_names(e)) defs[[v]] <- id
      next
    }

    rhs <- e[[3]]
    chain <- blockr_flatten_chain(rhs)
    if (length(chain$steps) && all(vapply(chain$steps, function(s) blockr_is_simple_call(s$expr) || is.name(s$expr), logical(1)))) {
      before <- names(nodes)
      last <- head_of(chain$head)$input
      for (s in chain$steps) last <- add(call_node(s$expr, input = last, op = s$op))
      # the note goes to the first new block of the statement (not a dataset)
      fresh <- Filter(function(id) !identical(nodes[[id]]$type, "dataset"), setdiff(names(nodes), before))
      if (!is.null(note) && length(fresh)) nodes[[fresh[[1]]]]$note <- note
    } else if (length(chain$steps) == 0 && blockr_is_simple_call(rhs)) {
      last <- add(c(call_node(rhs), list(note = note)))
    } else {
      last <- add(c(expr_node(rhs), list(note = note)))
    }
    nodes[[last]]$name <- target
    defs[[target]] <- last
  }

  named <- Filter(function(n) identical(n$name, "result"), nodes)
  list(
    version = 1L,
    packages = packages,
    nodes = unname(lapply(nodes, blockr_node_clean)),
    result = if (length(named)) named[[length(named)]]$id
  )
}

#' Remove empty fields of a node
#' @noRd
blockr_node_clean <- function(node) Filter(Negate(is.null), node)

#' R code of an expression, as text (one line when it fits)
#' @noRd
blockr_text <- function(e) {
  paste(deparse(e, width.cutoff = 500L, backtick = TRUE), collapse = "\n")
}

#' Is `e` a call to a named function, e.g. `f(x)` or `pkg::f(x)` (not an
#' operator, `if`, `function`, `[`, ...)?
#' @noRd
blockr_is_simple_call <- function(e) {
  if (!is.call(e)) return(FALSE)
  f <- e[[1]]
  if (is.name(f)) {
    s <- as.character(f)
    return(nzchar(s) && identical(make.names(s), s))
  }
  is.call(f) && (identical(f[[1]], as.name("::")) || identical(f[[1]], as.name(":::")))
}

#' Variable assigned by a statement `name <- value` (or `name = value`), else NULL
#' @noRd
blockr_assignment_target <- function(e) {
  if (is.call(e) && length(e) == 3 && (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) && is.name(e[[2]])) {
    as.character(e[[2]])
  }
}

#' Variables assigned anywhere inside a statement
#' @noRd
blockr_assigned_names <- function(e) {
  out <- character(0)
  walk <- function(x) {
    if (!is.call(x)) return()
    f <- x[[1]]
    if ((identical(f, as.name("<-")) || identical(f, as.name("=")) || identical(f, as.name("<<-"))) && length(x) == 3) {
      lhs <- x[[2]]
      # x <- ..., x[i] <- ..., x$a <- ..., names(x) <- ...
      while (is.call(lhs) && length(lhs) >= 2) lhs <- lhs[[2]]
      if (is.name(lhs)) out <<- c(out, as.character(lhs))
      walk(x[[3]])
      return()
    }
    if (identical(f, as.name("function"))) return() # its own scope
    for (a in as.list(x)[-1]) if (!missing(a)) walk(a)
  }
  walk(e)
  unique(out)
}

#' Split a chain into its head and steps
#'
#' `a %>% f() %>% g()` -> head `a`, steps f(), g(). A `+` chain is a ggplot2
#' plot and its layers: `ggplot(d, aes(x)) + geom_point()`.
#' @return list(head, steps = list(list(op, expr)))
#' @noRd
blockr_flatten_chain <- function(e) {
  if (is.call(e) && length(e) == 3 && identical(e[[1]], as.name("%>%"))) {
    inner <- blockr_flatten_chain(e[[2]])
    inner$steps <- c(inner$steps, list(list(op = "%>%", expr = e[[3]])))
    return(inner)
  }
  if (is.call(e) && length(e) == 3 && identical(e[[1]], as.name("+")) && blockr_is_gg_chain(e)) {
    inner <- blockr_flatten_chain(e[[2]])
    inner$steps <- c(inner$steps, list(list(op = "+", expr = e[[3]])))
    return(inner)
  }
  list(head = e, steps = list())
}

#' Is `e` a ggplot2 plot built with `+` (leftmost operand: a ggplot() call,
#' possibly at the end of a pipe)?
#' @noRd
blockr_is_gg_chain <- function(e) {
  while (is.call(e) && length(e) == 3 && identical(e[[1]], as.name("+"))) e <- e[[2]]
  if (is.call(e) && length(e) == 3 && identical(e[[1]], as.name("%>%"))) e <- e[[3]]
  is.call(e) && blockr_text(e[[1]]) %in% c("ggplot", "ggplot2::ggplot")
}

#' Comments written before each statement (or inside it), one string per
#' statement (NULL when there are none)
#' @noRd
blockr_statement_notes <- function(exprs) {
  out <- vector("list", length(exprs))
  pd <- tryCatch(utils::getParseData(exprs), error = function(e) NULL)
  refs <- attr(exprs, "srcref")
  if (is.null(pd) || is.null(refs) || !nrow(pd)) return(out)
  comments <- pd[pd$token == "COMMENT", c("line1", "text")]
  if (!nrow(comments)) return(out)
  previous_end <- 0L
  for (i in seq_along(exprs)) {
    end <- refs[[i]][3]
    hit <- comments$line1 > previous_end & comments$line1 <= end
    if (any(hit)) {
      text <- trimws(sub("^#+'?\\s?", "", comments$text[hit]))
      text <- text[nzchar(text) & !grepl("^-{2,}", text)]
      if (length(text)) out[[i]] <- paste(text, collapse = " ")
    }
    previous_end <- end
  }
  out
}

# ---- Dependencies -------------------------------------------------------------

#' Variables a node assigns: its `name`, or (statements) every variable
#' assigned inside it
#' @noRd
blockr_node_defines <- function(node) {
  if (identical(node$type, "code")) {
    e <- tryCatch(parse(text = node$code, keep.source = FALSE), error = function(e) NULL)
    if (is.null(e)) return(character(0))
    return(unique(unlist(lapply(e, blockr_assigned_names))))
  }
  if (identical(node$type, "dataset")) return(node$dataset)
  as.character(node$name)
}

#' Symbols a node's own R code mentions (not its refs or input)
#' @noRd
blockr_node_symbols <- function(node) {
  texts <- switch(node$type,
    call = c(node$fn, unlist(lapply(node$args, function(a) a$value))),
    expr = node$code,
    code = node$code,
    character(0)
  )
  out <- character(0)
  for (t in texts) {
    e <- tryCatch(parse(text = t, keep.source = FALSE), error = function(e) NULL)
    if (!is.null(e)) out <- c(out, unlist(lapply(e, all.names)))
  }
  unique(out)
}

#' Dependencies of every node
#'
#' For each node: `input` (piped in), `refs` (arguments wired to another
#' node) and `implicit` (variables its code mentions, resolved to the node
#' that assigns them: the last one before it, in `order`).
#' @return Named list (by node id) of list(input, refs, implicit).
#' @noRd
blockr_graph_deps <- function(graph) {
  nodes <- graph$nodes
  ids <- vapply(nodes, `[[`, character(1), "id")
  order <- vapply(nodes, function(n) as.numeric(n$order %||% NA), numeric(1))
  order[is.na(order)] <- seq_along(order)[is.na(order)]
  defines <- lapply(nodes, blockr_node_defines)
  out <- lapply(seq_along(nodes), function(i) {
    n <- nodes[[i]]
    refs <- unlist(lapply(n$args, function(a) a$ref))
    implicit <- character(0)
    for (s in blockr_node_symbols(n)) {
      who <- which(vapply(defines, function(d) s %in% d, logical(1)) & ids != n$id)
      if (!length(who)) next
      before <- who[order[who] < order[i]]
      pick <- if (length(before)) before[which.max(order[before])] else who[which.min(order[who])]
      implicit <- c(implicit, ids[pick])
    }
    list(
      input = n$input,
      refs = intersect(unique(refs), ids),
      implicit = setdiff(unique(implicit), c(n$input, refs))
    )
  })
  stats::setNames(out, ids)
}

#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
