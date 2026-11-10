# The canvas: BlockR's home page.
#
# A drag-and-drop editor of block graphs (R/blocks_graph.R) on the app's
# filtered data. Pick a TLG template (or start from an empty canvas): its R
# code becomes blocks, stacked into pipelines and wired together; every block
# can be edited, bypassed, moved, deleted or added from the palette, and the
# result (a table, a listing or a graph) is recomputed as you go. Every block
# shows what it produces (rows, a table, a plot) and "Show R code" gives the
# code of the whole canvas, runnable on its own (and its SAS translation,
# R/blocks_sas.R). R code pasted or typed in becomes blocks.
#
# The page holds two canvases: the user's ("main") and the Tutorial's
# ("tutorial", with lessons: inst/www/blockr_tutorial.js), switched by the
# Tutorial button at the top; each has its own saved state. teal's "Add to
# report" and "Show R code" buttons are turned off for this module (the
# canvas has its own code view); the Tutorial button takes their place.
#
# Browser side: inst/www/blockr_canvas.js (editor) and blockr_canvas.css.
# Messages between the two (per canvas):
#   browser -> R   input$ready     the editor is ready (sends the datasets)
#                  input$graph     the graph (JSON) after every change
#                  input$template  load a template (id)
#                  input$code      R code to draw as blocks (JSON: code, title,
#                                  mode "replace" or "add"; pasted code, the
#                                  Code pane's editor, the tutorial's lessons)
#                  input$selected  the selected block (its preview)
#                  input$lang      "r" or "sas": the language of the Code pane
#                  input$show_code open the code
#   R -> browser   blockr-data     datasets: names, labels, rows, columns
#                  blockr-graph    a graph to load (a template, or code; with
#                                  its mode)
#                  blockr-code-error  the code could not be read (message)
#                  blockr-status   per block: state, kind, size, error; the
#                                  code and the result block
#                  blockr-sas      the SAS code (while the Code pane shows SAS)

#' The canvas module
#' @param label Tab label.
#' @param datanames Datasets the canvas can use.
#' @param catalog TLG Catalog entries (the templates).
#' @param tutorial Add the Tutorial: a button at the top that switches to a
#'   second canvas with lessons (inst/www/blockr_tutorial.js), with its own
#'   saved state.
#' @noRd
tm_blockr_canvas <- function(label = "Canvas", datanames = "all", catalog = tlg_catalog(), tutorial = TRUE) {
  m <- module(
    label = label,
    server = srv_blockr_canvas,
    ui = ui_blockr_canvas,
    ui_args = list(catalog = catalog, tutorial = tutorial),
    server_args = list(catalog = catalog, tutorial = tutorial),
    datanames = datanames
  )
  blockr_without_teal_buttons(m)
}

#' Turn off teal's "Add to report" and "Show R code" buttons of a module
#'
#' With teal's disable_report() and disable_src() when this teal version has
#' them; otherwise the canvas hides the buttons in the browser
#' (inst/www/blockr_canvas.js, dockTutorial()).
#' @noRd
blockr_without_teal_buttons <- function(m) {
  for (f in c("disable_report", "disable_src")) {
    fun <- get0(f, envir = asNamespace("teal"), mode = "function", inherits = FALSE)
    if (!is.null(fun)) m <- tryCatch(fun(m), error = function(e) m)
  }
  m
}

#' Templates offered first on an empty canvas
#' @noRd
blockr_featured <- function() {
  c("AET01", "AET02", "DMT01", "AET10", "BRG01", "AEL02", "LBT01", "KMG01")
}

#' Static assets of the canvas
#' @noRd
blockr_canvas_dependency <- function() {
  htmltools::htmlDependency(
    name = "blockr-canvas",
    version = blockr_version(),
    src = c(file = blockr_www()),
    stylesheet = "blockr_canvas.css",
    script = c("blockr_canvas.js", "blockr_tutorial.js"),
    all_files = FALSE
  )
}

#' Configuration of the editor: block registry and template index (JSON)
#' @noRd
blockr_canvas_config <- function(catalog = tlg_catalog(), tutorial = FALSE) {
  templates <- unname(lapply(catalog, function(e) {
    list(id = e$id, title = e$title, type = e$type, topic = e$topic)
  }))
  json <- jsonlite::toJSON(
    list(
      registry = blockr_registry_client(),
      templates = templates,
      topics = tlg_topics(),
      featured = intersect(blockr_featured(), names(catalog)),
      tutorial = isTRUE(tutorial),
      height = blockr_canvas_height()
    ),
    auto_unbox = TRUE, null = "null", na = "null"
  )
  gsub("</", "<\\/", json, fixed = TRUE)
}

#' The canvas page: the Tutorial button, the canvas, and the Tutorial's
#' canvas (hidden until the button is pressed)
#' @noRd
ui_blockr_canvas <- function(id, catalog = tlg_catalog(), tutorial = TRUE) {
  ns <- shiny::NS(id)
  tutorial <- isTRUE(tutorial)
  htmltools::tagList(
    blockr_canvas_dependency(),
    htmltools::div(
      class = "bk-page", id = ns("page"), `data-view` = "main",
      if (tutorial) {
        htmltools::div(
          class = "bk-page-bar",
          htmltools::tags$button(
            type = "button", class = "bk-tutorial-btn", `data-bk-view` = "tutorial", `data-bk-page` = ns("page"),
            title = "Eight short lessons, on a canvas of their own: your canvas stays as it is",
            htmltools::span(class = "bk-tutorial-ico", `aria-hidden` = "true", htmltools::HTML(blockr_tutorial_icon())),
            htmltools::span(class = "bk-tutorial-label", "Tutorial"),
            htmltools::span(class = "bk-tutorial-sub", "8 lessons")
          )
        )
      },
      htmltools::div(class = "bk-view", `data-view` = "main", ui_blockr_canvas_app(ns("main"), catalog)),
      if (tutorial) {
        htmltools::div(
          class = "bk-view", `data-view` = "tutorial", hidden = NA,
          ui_blockr_canvas_app(ns("tutorial"), catalog, tutorial = TRUE)
        )
      }
    )
  )
}

#' Graduation cap (the Tutorial button)
#' @noRd
blockr_tutorial_icon <- function() {
  paste0(
    '<svg viewBox="0 0 16 16" width="18" height="18" fill="none" stroke="currentColor" stroke-width="1.5" ',
    'stroke-linecap="round" stroke-linejoin="round"><path d="M1.5 6 8 3l6.5 3L8 9z"/>',
    '<path d="M4 7.3v3.2c0 .9 1.8 2 4 2s4-1.1 4-2V7.3M14.5 6v4"/></svg>'
  )
}

#' One canvas (editor, palette, result panel)
#' @param tutorial With the Tutorial's lessons.
#' @noRd
ui_blockr_canvas_app <- function(id, catalog = tlg_catalog(), tutorial = FALSE) {
  ns <- shiny::NS(id)
  pane <- function(name, ...) htmltools::div(class = "bk-pane", `data-pane` = name, hidden = if (name != "result") NA, ...)
  htmltools::tagList(
    htmltools::div(
      class = paste(c("bk-app", if (isTRUE(tutorial)) "bk-tutorial"), collapse = " "), id = ns("app"), `data-ns` = ns(""),
      htmltools::tags$script(type = "application/json", class = "bk-config", htmltools::HTML(blockr_canvas_config(catalog, tutorial))),
      htmltools::div(class = "bk-top"),
      htmltools::div(
        class = "bk-body",
        htmltools::tags$aside(class = "bk-palette", `aria-label` = "Blocks and templates"),
        htmltools::div(
          class = "bk-stage", tabindex = "0", `aria-label` = "Canvas",
          htmltools::div(class = "bk-boot", "Loading the canvas\u2026")
        ),
        htmltools::tags$section(
          class = "bk-panel", `aria-label` = "Result and block settings",
          htmltools::div(class = "bk-panel-head"),
          pane("result", shiny::uiOutput(ns("result"), class = "bk-output")),
          pane("block", htmltools::div(class = "bk-inspector"), shiny::uiOutput(ns("preview"), class = "bk-output bk-preview")),
          pane("code", htmltools::tags$pre(class = "bk-code", htmltools::tags$code()))
        )
      )
    )
  )
}

#' @noRd
srv_blockr_canvas <- function(id, data, catalog = tlg_catalog(), tutorial = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    srv_blockr_canvas_app("main", data, catalog)
    if (isTRUE(tutorial)) srv_blockr_canvas_app("tutorial", data, catalog)
    # the app's data, unchanged: what teal expects a module to return (its
    # report and code buttons are off for the canvas, see
    # blockr_without_teal_buttons())
    data
  })
}

#' Server of one canvas
#' @noRd
srv_blockr_canvas_app <- function(id, data, catalog = tlg_catalog()) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    graph <- shiny::reactiveVal(NULL)
    cache <- new.env(parent = emptyenv())
    cached_for <- NULL
    sent_schemas <- new.env(parent = emptyenv())

    send <- function(type, message) session$sendCustomMessage(type, c(list(ns = ns("")), message))

    datasets <- shiny::reactive({
      d <- data()
      nms <- setdiff(names(d), "")
      nms <- nms[vapply(nms, function(n) is.data.frame(d[[n]]), logical(1))]
      stats::setNames(lapply(nms, function(n) d[[n]]), nms)
    })

    # the editor is ready: tell it which datasets there are
    shiny::observeEvent(input$ready, {
      ds <- datasets()
      labels <- blockr_dataset_labels()
      send("blockr-data", list(datasets = unname(lapply(names(ds), function(n) {
        label <- if (n %in% names(labels)) unname(labels[[n]]) else n
        list(name = n, label = label, rows = nrow(ds[[n]]), cols = ncol(ds[[n]]))
      }))))
    })

    shiny::observeEvent(input$template, {
      entry <- catalog[[input$template]]
      shiny::req(entry)
      g <- blockr_template_graph(entry)
      send("blockr-graph", list(graph = g, implicit = blockr_implicit_deps(g)))
    })

    # R code to draw as blocks: pasted, typed in the Code pane, or a lesson's
    shiny::observeEvent(input$code, {
      x <- tryCatch(jsonlite::fromJSON(input$code, simplifyVector = FALSE), error = function(e) NULL)
      shiny::req(is.list(x), is.character(x$code))
      mode <- if (identical(x$mode, "add")) "add" else "replace"
      g <- blockr_code_graph(x$code, names(datasets()))
      if (!is.null(g$error)) {
        send("blockr-code-error", list(message = g$error, mode = mode))
        return()
      }
      g$title <- if (is.character(x$title) && nzchar(x$title)) x$title
      send("blockr-graph", list(graph = g, implicit = blockr_implicit_deps(g), mode = mode, source = "code"))
    })

    shiny::observeEvent(input$graph, {
      g <- tryCatch(blockr_graph_from_json(input$graph), error = function(e) NULL)
      if (!is.null(g)) graph(g)
    })

    run <- shiny::reactive({
      g <- graph()
      shiny::req(g)
      d <- datasets()
      if (!identical(cached_for, d)) {
        rm(list = ls(cache, all.names = TRUE), envir = cache)
        cached_for <<- d
      }
      # a problem here must not end the session: it is shown on the canvas
      r <- tryCatch(blockr_run(g, d, cache), error = function(e) {
        list(status = list(), result = NULL, env = new.env(), order = character(0), error = conditionMessage(e))
      })
      clean <- tryCatch(blockr_compile(g, registry = blockr_registry()), error = function(e) NULL)
      r$code <- if (is.null(clean)) "# The code could not be written." else clean$code
      # the variable holding each block's result in the code
      for (id in intersect(names(clean$vars), names(r$status))) {
        if (!is.null(clean$vars[[id]])) r$status[[id]]$var <- clean$vars[[id]]
      }
      r
    })

    shiny::observe({
      r <- run()
      g <- shiny::isolate(graph())
      payload <- tryCatch(
        blockr_status_payload(r, g, sent_schemas),
        error = function(e) list(rev = g$rev %||% 0, nodes = list(), error = conditionMessage(e))
      )
      send("blockr-status", payload)
    })

    # the SAS code, only while the Code pane shows SAS
    shiny::observe({
      shiny::req(identical(input$lang, "sas"))
      r <- run()
      g <- shiny::isolate(graph())
      sas <- tryCatch(
        blockr_sas(g, status = r$status),
        error = function(e) paste0("/* The SAS code could not be written: ", gsub("*/", "* /", conditionMessage(e), fixed = TRUE), " */")
      )
      send("blockr-sas", list(rev = g$rev %||% 0, code = sas))
    })

    # ---- result and the selected block
    value_of <- function(r, id) if (!is.null(id)) blockr_value(r, shiny::isolate(graph()), id)
    selected <- shiny::reactive({
      id <- input$selected
      if (is.null(id) || !nzchar(id)) NULL else id
    })

    output$result <- shiny::renderUI({
      r <- run()
      blockr_output_ui(r, r$result, ns, "result", graph())
    })
    output$preview <- shiny::renderUI({
      r <- run()
      blockr_output_ui(r, selected(), ns, "preview", graph())
    })
    output$result_plot <- shiny::renderPlot({
      r <- run()
      v <- value_of(r, r$result)
      shiny::req(identical(blockr_describe(v)$kind, "plot"))
      tlg_draw(v)
    }, res = 96)
    output$preview_plot <- shiny::renderPlot({
      r <- run()
      v <- value_of(r, selected())
      shiny::req(identical(blockr_describe(v)$kind, "plot"))
      tlg_draw(v)
    }, res = 96)
    output$result_dt <- DT::renderDT({
      r <- run()
      v <- value_of(r, r$result)
      shiny::req(is.data.frame(v))
      tlg_datatable(v)
    })
    output$preview_dt <- DT::renderDT({
      r <- run()
      v <- value_of(r, selected())
      shiny::req(is.data.frame(v))
      tlg_datatable(v)
    })

    shiny::observeEvent(input$show_code, {
      r <- run()
      g <- graph()
      title <- g$title %||% "Canvas"
      code <- paste(c(
        teal.code::get_code(data()), "",
        paste0("# ---- BlockR canvas", if (!is.null(g$template)) paste0(": ", g$template), " \u00b7 ", title, " ----"),
        r$code
      ), collapse = "\n")
      code_id <- ns("code_text")
      shiny::showModal(shiny::modalDialog(
        title = paste0(title, " \u00b7 R code"),
        size = "l", easyClose = TRUE,
        htmltools::tags$pre(
          class = "tn-tlg-code", .noWS = c("after-begin", "before-end"),
          htmltools::tags$code(id = code_id, code, .noWS = c("outside", "inside"))
        ),
        footer = htmltools::tagList(
          htmltools::tags$button(
            type = "button", class = "btn btn-default",
            onclick = sprintf(
              "navigator.clipboard && navigator.clipboard.writeText(document.getElementById('%s').innerText); this.innerText = 'Copied';",
              code_id
            ),
            "Copy"
          ),
          htmltools::tags$button(type = "button", class = "btn btn-primary", `data-dismiss` = "modal", `data-bs-dismiss` = "modal", "Done")
        )
      ))
    })
  })
}

#' Graph of R code typed or pasted by the user
#' @param code R code.
#' @param datasets Names of the app's datasets (besides the known ADaM ones).
#' @return The graph, or list(error = message) when the code cannot be read
#'   (or has nothing to draw).
#' @noRd
blockr_code_graph <- function(code, datasets = character(0)) {
  known <- union(datasets, names(blockr_dataset_labels()))
  g <- tryCatch(blockr_graph_from_code(code, datasets = known), error = function(e) e)
  if (inherits(g, "error")) {
    msg <- conditionMessage(g)
    # parse errors: "<text>:2:7: unexpected symbol" -> "Line 2: unexpected symbol"
    msg <- sub("^<text>:(\\d+):(\\d+): ", "Line \\1: ", msg)
    return(list(error = paste("This is not R code BlockR can read.", msg)))
  }
  if (!length(g$nodes)) return(list(error = "There is no R code to draw here."))
  g
}

#' Graph of a TLG Catalog template (cached: templates do not change)
#' @noRd
blockr_template_graph <- function(entry) {
  key <- paste0("template:", entry$id)
  g <- get0(key, envir = the, inherits = FALSE)
  if (is.null(g)) {
    g <- blockr_graph_from_code(entry$code)
    g$template <- entry$id
    g$title <- entry$title
    g$type <- entry$type
    assign(key, g, envir = the)
  }
  g
}

#' Implicit dependencies of every block (by id; only blocks that have some),
#' so that the first layout of a template already knows them
#' @noRd
blockr_implicit_deps <- function(graph) {
  deps <- blockr_graph_deps(graph)
  out <- lapply(deps, function(d) if (length(d$implicit)) I(d$implicit))
  Filter(Negate(is.null), out)
}

#' Graph sent by the browser (JSON), with every field in its R type
#' @noRd
blockr_graph_from_json <- function(json) {
  x <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  blockr_graph_normalize(x)
}

#' @noRd
blockr_graph_normalize <- function(x) {
  one <- function(v) {
    if (is.null(v) || !length(v)) return(NULL)
    v <- as.character(v[[1]])
    if (is.na(v)) NULL else v
  }
  opt <- function(v) {
    v <- one(v)
    if (is.null(v) || !nzchar(v)) NULL else v
  }
  nodes <- lapply(x$nodes, function(n) {
    type <- one(n$type) %||% "code"
    args <- lapply(n$args, function(a) {
      ref <- opt(a$ref)
      if (!is.null(ref)) return(list(name = one(a$name) %||% "", ref = ref))
      list(name = one(a$name) %||% "", value = one(a$value))
    })
    blockr_node_clean(list(
      id = one(n$id), type = type, dataset = opt(n$dataset), fn = opt(n$fn),
      args = if (identical(type, "call")) args,
      code = if (type %in% c("expr", "code")) one(n$code) %||% "",
      input = opt(n$input), op = opt(n$op), name = opt(n$name), note = opt(n$note),
      order = as.numeric(n$order %||% NA), off = if (isTRUE(as.logical(n$off))) TRUE
    ))
  })
  nodes <- Filter(function(n) !is.null(n$id), nodes)
  list(
    version = 1L,
    packages = as.character(unlist(x$packages)),
    nodes = nodes,
    result = opt(x$result),
    template = opt(x$template),
    title = opt(x$title),
    rev = as.numeric(x$rev %||% 0)
  )
}

#' Status message for the browser: per block, without the values; column
#' lists are sent once per session (by hash)
#' @noRd
blockr_status_payload <- function(run, graph, sent) {
  schemas <- list()
  nodes <- lapply(run$status, function(s) {
    if (!is.null(s$schema)) {
      h <- blockr_hash(s$schema)
      if (!exists(h, envir = sent, inherits = FALSE)) {
        schemas[[h]] <<- s$schema
        assign(h, TRUE, envir = sent)
      }
      s$schema <- h
    }
    s
  })
  list(rev = graph$rev %||% 0, nodes = nodes, schemas = schemas, result = run$result, code = run$code, error = run$error)
}

#' Output of a block: table, listing, plot, layout or value
#' @param where "result" or "preview" (names of the outputs).
#' @noRd
blockr_output_ui <- function(run, id, ns, where, graph) {
  if (is.null(id)) {
    return(htmltools::div(class = "bk-empty", if (where == "result") "Nothing to show yet: add blocks, or start from a template." else "Select a block to see what it produces."))
  }
  st <- run$status[[id]]
  if (is.null(st)) return(htmltools::div(class = "bk-empty", "This block has not run yet."))
  if (!identical(st$state, "ok")) {
    return(htmltools::div(
      class = paste("bk-message", paste0("bk-message-", st$state)),
      htmltools::strong(switch(st$state, blocked = "Waiting", missing = "Package needed", "This block has a problem")),
      htmltools::p(st$error)
    ))
  }
  v <- blockr_value(run, graph, id)
  switch(st$kind,
    table = htmltools::div(class = "bk-scroll tn-tlg-rtable", rtables::as_html(v)),
    listing = DT::DTOutput(ns(paste0(where, "_dt"))),
    data = DT::DTOutput(ns(paste0(where, "_dt"))),
    plot = shiny::plotOutput(ns(paste0(where, "_plot")), height = if (where == "result") "560px" else "380px"),
    layout = htmltools::tagList(
      htmltools::p(class = "bk-hint", "A table layout: connect it to a Build table block to see the table."),
      htmltools::tags$pre(class = "bk-pre", paste(utils::capture.output(print(v)), collapse = "\n"))
    ),
    htmltools::tags$pre(class = "bk-pre", paste(utils::head(utils::capture.output(utils::str(v, max.level = 2, list.len = 20)), 60), collapse = "\n"))
  )
}
