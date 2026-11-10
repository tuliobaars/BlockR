# The canvas module: page, configuration, messages, server.

catalog <- tlg_catalog()

test_that("the canvas page carries its configuration and assets", {
  rendered <- htmltools::renderTags(ui_blockr_canvas("canvas", catalog, tutorial = FALSE))
  html <- rendered$html
  expect_match(html, 'class="bk-app"', fixed = TRUE)
  expect_match(html, 'data-ns="canvas-main-"', fixed = TRUE)
  for (x in c("bk-top", "bk-palette", "bk-stage", "bk-panel", 'data-pane="result"', 'data-pane="block"', 'data-pane="code"')) {
    expect_match(html, x, fixed = TRUE, info = x)
  }
  # the Shiny outputs of the result and of the selected block
  expect_match(html, 'id="canvas-main-result"', fixed = TRUE)
  expect_match(html, 'id="canvas-main-preview"', fixed = TRUE)
  # without the tutorial: one canvas, no Tutorial button
  expect_no_match(html, "bk-tutorial", fixed = TRUE)
  dep <- Filter(function(d) identical(d$name, "blockr-canvas"), rendered$dependencies)
  expect_length(dep, 1)
  expect_identical(unname(unlist(dep[[1]]$script)), c("blockr_canvas.js", "blockr_tutorial.js"))
  # the configuration is JSON, safe inside <script>
  json <- sub('.*<script type="application/json" class="bk-config">(.*?)</script>.*', "\\1", html)
  cfg <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_length(cfg$templates, length(catalog))
  expect_true(all(unlist(cfg$featured) %in% names(catalog)))
  expect_identical(unlist(cfg$topics), tlg_topics())
  fns <- vapply(cfg$registry$blocks, `[[`, character(1), "fn")
  expect_true(all(c("filter", "mutate", "basic_table", "build_table", "ggplot") %in% fns))
  # vectors of one value stay arrays (the browser expects lists)
  c_block <- cfg$registry$blocks[[match("c", fns)]]
  expect_type(c_block$formals, "list")
  expect_false(cfg$tutorial)
  # the canvas's height, as a share of the window (option blockr.canvas_height)
  expect_identical(cfg$height, 0.85)
  withr::with_options(list(blockr.canvas_height = 0.95), {
    expect_identical(jsonlite::fromJSON(blockr_canvas_config(catalog))$height, 0.95)
  })
})

test_that("the canvas height option is validated", {
  expect_identical(blockr_canvas_height(0.6), 0.6)
  withr::with_options(list(blockr.canvas_height = NULL), expect_identical(blockr_canvas_height(), 0.85))
  for (bad in list(0.2, 1.5, "tall", NA_real_, c(0.5, 0.6))) expect_error(blockr_canvas_height(bad), "blockr.canvas_height")
})

test_that("the canvas moves aside from controls floating over it, and fills its height", {
  js <- paste(readLines(file.path(blockr_www(), "blockr_canvas.js"), warn = FALSE), collapse = "\n")
  expect_match(js, "avoidOverlays", fixed = TRUE)
  expect_match(js, "elementsFromPoint", fixed = TRUE)
  expect_match(js, "this.cfg.height", fixed = TRUE)
})

test_that("the Tutorial is a button on the canvas page, with a canvas of its own", {
  html <- htmltools::renderTags(ui_blockr_canvas("canvas", catalog))$html
  # two canvases: the user's, and the tutorial's (hidden until the button)
  expect_match(html, 'class="bk-page"', fixed = TRUE)
  expect_match(html, 'data-ns="canvas-main-"', fixed = TRUE)
  expect_match(html, 'data-ns="canvas-tutorial-"', fixed = TRUE)
  expect_match(html, 'class="bk-app bk-tutorial"', fixed = TRUE)
  expect_match(html, '<div class="bk-view" data-view="tutorial" hidden>', fixed = TRUE)
  # the button switches to the tutorial's view
  expect_match(html, 'class="bk-tutorial-btn" data-bk-view="tutorial" data-bk-page="canvas-page"', fixed = TRUE)
  expect_match(html, ">Tutorial<", fixed = TRUE)
  configs <- regmatches(html, gregexpr('<script type="application/json" class="bk-config">(.*?)</script>', html))[[1]]
  expect_length(configs, 2)
  tut <- vapply(configs, function(x) {
    jsonlite::fromJSON(sub('^<script[^>]*>(.*)</script>$', "\\1", x), simplifyVector = FALSE)$tutorial
  }, logical(1))
  expect_identical(unname(tut), c(FALSE, TRUE))
  # the module has the tutorial by default, and no separate Tutorial page
  m <- tm_blockr_canvas("Canvas", datanames = "ADSL", catalog = catalog)
  expect_identical(m$label, "Canvas")
  # the page script switches the views and hides teal's report / code buttons
  js <- paste(readLines(file.path(blockr_www(), "blockr_canvas.js"), warn = FALSE), collapse = "\n")
  for (x in c("function showView", "function dockTutorial", "add to report|show r code", "showTutorial")) {
    expect_match(js, x, fixed = TRUE, info = x)
  }
})

test_that("teal's report and code buttons are turned off when teal can", {
  m <- list(label = "x")
  out <- blockr_without_teal_buttons(m)
  has <- vapply(c("disable_report", "disable_src"), function(f) {
    is.function(get0(f, envir = asNamespace("teal"), mode = "function", inherits = FALSE))
  }, logical(1))
  # without those teal functions the module is unchanged (the browser hides
  # the buttons); errors from them never stop the app
  if (!any(has)) expect_identical(out, m)
  expect_s3_class(tm_blockr_canvas("Canvas", datanames = "ADSL", catalog = catalog), "teal_module")
})

test_that("the lessons start from valid R code and existing templates", {
  js <- paste(readLines(file.path(blockr_www(), "blockr_tutorial.js"), warn = FALSE), collapse = "\n")
  codes <- regmatches(js, gregexpr("code: '([^'\\\\]|\\\\.)*'", js))[[1]]
  expect_gte(length(codes), 3)
  for (x in codes) {
    code <- gsub("\\\\n", "\n", gsub('\\\\"', '"', sub("^code: '(.*)'$", "\\1", x)))
    g <- blockr_graph_from_code(code)
    expect_length(blockr_compile(g, registry = blockr_registry())$errors, 0)
  }
  templates <- gsub('template: "|"', "", regmatches(js, gregexpr('template: "[A-Z0-9_]+"', js))[[1]])
  expect_true(length(templates) > 0 && all(templates %in% names(catalog)))
  # eight lessons, three levels, each lesson a few steps
  expect_identical(lengths(regmatches(js, gregexpr('id: "[a-z]+", level: [0-2]', js))), 8L)
})

test_that("the canvas script and styles are self-contained", {
  js <- paste(readLines(file.path(blockr_www(), "blockr_canvas.js"), warn = FALSE), collapse = "\n")
  css <- paste(readLines(file.path(blockr_www(), "blockr_canvas.css"), warn = FALSE), collapse = "\n")
  expect_no_match(js, "https?://")
  expect_no_match(js, "\\beval\\(")
  expect_no_match(css, "url\\(")
  for (x in c("blockr-data", "blockr-graph", "blockr-status", "setInputValue", "shown.bs.tab", "data-bk-open")) {
    expect_match(js, x, fixed = TRUE, info = x)
  }
  tut <- paste(readLines(file.path(blockr_www(), "blockr_tutorial.js"), warn = FALSE), collapse = "\n")
  expect_no_match(tut, "https?://")
  expect_no_match(tut, "\\beval\\(")
  # the "What is this?" buttons can be hidden
  expect_match(css, 'html[data-tn-tips="off"] .bk-tip', fixed = TRUE)
})

test_that("a graph from the browser gets its R types back", {
  json <- '{"version":1,"packages":["tern"],"result":null,"template":"AET02","title":"T","rev":7,
    "nodes":[
      {"id":"n1","type":"dataset","dataset":"ADAE","order":1,"x":10,"y":20},
      {"id":"n2","type":"call","fn":"filter","args":[{"name":"","value":"AESER == \\"Y\\""}],"input":"n1","op":"%>%","order":2,"name":"","off":true},
      {"id":"n3","type":"call","fn":"build_table","args":[{"name":"df","ref":"n2"},{"name":"lyt","ref":null}],"order":3},
      {"type":"call","fn":"orphan"}
    ]}'
  g <- blockr_graph_from_json(json)
  expect_identical(g$packages, "tern")
  expect_null(g$result)
  expect_identical(g$template, "AET02")
  expect_identical(g$rev, 7)
  expect_length(g$nodes, 3) # a node without an id is dropped
  expect_identical(g$nodes[[1]], list(id = "n1", type = "dataset", dataset = "ADAE", order = 1))
  n2 <- g$nodes[[2]]
  expect_null(n2$name) # "" is no name
  expect_true(n2$off)
  expect_identical(n2$args, list(list(name = "", value = 'AESER == "Y"')))
  expect_identical(g$nodes[[3]]$args, list(list(name = "df", ref = "n2"), list(name = "lyt", value = NULL)))
  # and it compiles
  expect_match(blockr_compile(g)$code, "ADAE", fixed = TRUE)
})

test_that("a template's graph is cached and knows where it comes from", {
  g <- blockr_template_graph(catalog$AET02)
  expect_identical(g$template, "AET02")
  expect_identical(g$title, catalog$AET02$title)
  expect_identical(blockr_template_graph(catalog$AET02), g)
  # implicit dependencies travel with it, for the first layout
  dmt <- blockr_template_graph(catalog$DMT01)
  imp <- blockr_implicit_deps(dmt)
  expect_gt(length(imp), 0)
  expect_true(all(unlist(imp) %in% vapply(dmt$nodes, `[[`, character(1), "id")))
})

test_that("the status message sends each list of columns once", {
  run <- list(
    status = list(
      a = list(state = "ok", kind = "data", schema = list(list(n = "X", t = "num"))),
      b = list(state = "ok", kind = "data", schema = list(list(n = "X", t = "num"))),
      c = list(state = "error", error = "boom")
    ),
    result = "c", code = "x <- 1"
  )
  sent <- new.env()
  p1 <- blockr_status_payload(run, list(rev = 3), sent)
  expect_identical(p1$rev, 3)
  expect_length(p1$schemas, 1)
  expect_identical(p1$nodes$a$schema, p1$nodes$b$schema)
  expect_identical(p1$nodes$c$error, "boom")
  p2 <- blockr_status_payload(run, list(rev = 4), sent)
  expect_length(p2$schemas, 0)
  expect_identical(p2$code, "x <- 1")
})

test_that("outputs: the problem of a block, a layout, a value", {
  ns <- shiny::NS("c")
  run <- list(status = list(a = list(state = "error", error = "oh no"), b = list(state = "blocked", error = "wait")))
  html <- as.character(blockr_output_ui(run, "a", ns, "result", list(nodes = list())))
  expect_match(html, "oh no", fixed = TRUE)
  expect_match(html, "bk-message-error", fixed = TRUE)
  expect_match(as.character(blockr_output_ui(run, "b", ns, "preview", list(nodes = list()))), "Waiting", fixed = TRUE)
  expect_match(as.character(blockr_output_ui(run, NULL, ns, "result", list())), "Nothing to show yet", fixed = TRUE)
  expect_match(as.character(blockr_output_ui(run, NULL, ns, "preview", list())), "Select a block", fixed = TRUE)
})

test_that("the canvas server runs a graph on the filtered data and shows the result", {
  skip_if_not_installed("tern")
  local_blockr_options()
  data <- suppressMessages(build_blockr_data(cache_global = FALSE))
  g <- blockr_template_graph(catalog$DMT01)
  json <- jsonlite::toJSON(c(g, list(rev = 1)), auto_unbox = TRUE, null = "null")
  shiny::testServer(srv_blockr_canvas_app, args = list(data = shiny::reactive(data), catalog = catalog), {
    session$setInputs(ready = 1)
    session$setInputs(graph = as.character(json))
    r <- run()
    states <- vapply(r$status, `[[`, character(1), "state")
    expect_true(all(states == "ok"))
    expect_identical(r$status[[r$result]]$kind, "table")
    expect_match(r$code, "build_table(lyt, df = adsl)", fixed = TRUE)
    expect_match(output$result$html, "<table", fixed = TRUE)
    # the selected block's preview
    adsl <- Filter(function(n) identical(n$name, "adsl"), g$nodes)[[1]]$id
    session$setInputs(selected = adsl)
    expect_match(output$preview$html, "preview_dt", fixed = TRUE)
    # a template can be loaded (the graph goes to the browser), and R code (lessons)
    expect_no_error(session$setInputs(template = "AET01"))
    expect_no_error(session$setInputs(code = '{"code": "adsl <- ADSL %>% filter(SAFFL == \\"Y\\")", "title": "T"}'))
    # pasted code added to the canvas, code that cannot be read, the SAS view
    expect_no_error(session$setInputs(code = '{"code": "x <- ADAE %>% count(AEBODSYS)", "mode": "add"}'))
    expect_no_error(session$setInputs(code = '{"code": "x <- (", "mode": "replace"}'))
    expect_no_error(session$setInputs(lang = "sas"))
  })
  # the page runs both canvases
  shiny::testServer(srv_blockr_canvas, args = list(data = shiny::reactive(data), catalog = catalog), {
    expect_no_error(session$setInputs(`main-ready` = 1, `tutorial-ready` = 1))
  })
})

test_that("pasted code becomes a graph, or a message saying what is wrong", {
  g <- blockr_code_graph("x <- ADAE %>% filter(AESER == \"Y\") %>% count(AEBODSYS)")
  expect_null(g$error)
  expect_identical(vapply(g$nodes, `[[`, character(1), "type"), c("dataset", "call", "call"))
  # the app's own datasets count as datasets too
  g2 <- blockr_code_graph("y <- MYDATA %>% head()", datasets = "MYDATA")
  expect_identical(g2$nodes[[1]]$dataset, "MYDATA")
  bad <- blockr_code_graph("x <- ADAE %>% filter(")
  expect_match(bad$error, "not R code BlockR can read", fixed = TRUE)
  expect_match(bad$error, "Line 2|Line 1")
  expect_match(blockr_code_graph("   ")$error, "no R code", fixed = TRUE)
})
