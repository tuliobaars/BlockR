# The app is built once for this file (without launching it).
app_parts <- local({
  parts <- NULL
  function() {
    if (is.null(parts)) {
      local_blockr_options(env = environment())
      warnings <- character(0)
      withCallingHandlers(
        {
          data <- build_blockr_data(cache_global = FALSE)
          modules <- blockr_modules(data)
          app <- blockr_app(cache_global = FALSE)
        },
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        },
        message = function(m) invokeRestart("muffleMessage")
      )
      parts <<- list(data = data, modules = modules, filters = blockr_filters(modules), app = app, warnings = warnings)
    }
    parts
  }
})

leaf_modules <- function(x) {
  if (inherits(x, "teal_modules")) do.call(c, lapply(x$children, leaf_modules)) else list(x)
}
group_nodes <- function(x) {
  if (!inherits(x, "teal_modules")) return(list())
  c(list(x), do.call(c, lapply(x$children, group_nodes)))
}
child_group <- function(modules, label) Filter(function(x) identical(x$label, label), modules$children)[[1]]

test_that("the teal app builds without warnings", {
  p <- app_parts()
  expect_true(is.function(p$app$server))
  expect_false(is.null(p$app$ui))
  expect_identical(p$warnings, character(0))
})

test_that("run_app() returns a Shiny app object", {
  local_blockr_options()
  expect_s3_class(suppressMessages(run_app(cache_global = FALSE)), "shiny.appobj")
})

render_ui <- function(app) {
  ui <- if (is.function(app$ui)) app$ui(list()) else app$ui
  htmltools::renderTags(ui)
}

test_that("the UI renders (regression: an unlabeled module group broke ui_teal)", {
  p <- app_parts()
  rendered <- render_ui(p$app)
  html <- rendered$html
  expect_match(html, "blockr_dark_mode", fixed = TRUE)
  expect_match(html, "tn-header", fixed = TRUE)
  expect_match(html, "Clinical analysis, block by block", fixed = TRUE)
  expect_match(html, "bk-app", fixed = TRUE) # the canvas
  expect_match(html, "subjects", fixed = TRUE) # data badge
  expect_no_match(html, "Roche|Genentech")
})

test_that("the UI ships the blockr assets and makes no third-party requests", {
  p <- app_parts()
  rendered <- render_ui(p$app)
  deps <- vapply(rendered$dependencies, function(d) d$name, character(1))
  expect_true("blockr" %in% deps)
  page <- paste(rendered$head, rendered$html)
  # the favicon is inlined; fonts are served by the app itself
  expect_no_match(page, "raw.githubusercontent.com|fonts.googleapis.com|fonts.gstatic.com")
  expect_match(page, "data:image/png;base64,", fixed = TRUE)
})

test_that("option blockr.style = FALSE gives the plain teal header", {
  local_blockr_options()
  withr::local_options(blockr.style = FALSE)
  app <- suppressMessages(blockr_app(cache_global = FALSE))
  rendered <- render_ui(app)
  expect_match(rendered$html, "BlockR: Clinical analysis, block by block", fixed = TRUE)
  expect_match(rendered$html, "blockr_dark_mode", fixed = TRUE)
  expect_false("blockr" %in% vapply(rendered$dependencies, function(d) d$name, character(1)))
})

test_that("every module group has a label and all module labels are unique", {
  p <- app_parts()
  groups <- group_nodes(p$modules)[-1] # the root may be unlabeled
  for (g in groups) expect_true(length(g$label) == 1 && nzchar(g$label))
  expect_false(anyDuplicated(leaf_labels(p$modules)) > 0)
  expect_identical(
    vapply(p$modules$children, function(x) x$label, character(1)),
    c("Canvas", "TLG Catalog", "Data Table", "Variable Browser", "Exploratory Analysis", "Safety Analysis")
  )
  # the canvas is the home page (the Tutorial is a button on it); the TLG
  # Catalog is one page (not a group)
  for (i in 1:2) expect_false(inherits(p$modules$children[[i]], "teal_modules"))
  expect_false("Tutorial" %in% leaf_labels(p$modules))
})

test_that("every module only refers to datasets that exist", {
  p <- app_parts()
  used <- unique(unlist(lapply(leaf_modules(p$modules), function(m) m$datanames)))
  expect_identical(setdiff(used, c("all", names(p$data))), character(0))
})

test_that("module-specific filters: mapping refers to real modules and filter ids", {
  p <- app_parts()
  mapping <- attr(p$filters, "mapping")
  ids <- vapply(p$filters, function(s) shiny::isolate(s$id), character(1))
  labels <- leaf_labels(p$modules)
  expect_true(all(unlist(mapping) %in% ids))
  expect_true(all(names(mapping) %in% c(labels, "global_filters")))
  expect_setequal(mapping$global_filters, c("ADSL SEX", "ADSL AGE"))
  safety <- leaf_labels(child_group(p$modules, "Safety Analysis"))
  for (l in safety) expect_true("ADSL SAFFL" %in% mapping[[l]], info = l)
  # everything else (the canvas, the TLG Catalog, ...) starts from all subjects
  for (l in setdiff(labels, safety)) expect_false("ADSL SAFFL" %in% mapping[[l]], info = l)
  expect_false("ADSL ITTFL" %in% ids) # no Efficacy population any more
})

test_that("the File viewer points at files that exist", {
  expect_true(dir.exists(blockr_file_viewer_paths(list(source = "bundled"))[[1]]))
  expect_identical(blockr_file_viewer_paths(list(source = "parquet", parquet_dir = "x"))[[1]], "x")
})

test_that("exploratory plot modules get the app's default ggplot2 theme where supported", {
  p <- app_parts()
  skip_if_not("ggtheme" %in% names(formals(teal.modules.general::tm_g_scatterplot)), "tm_g_scatterplot has no ggtheme")
  scatter <- Filter(function(m) identical(m$label, "Scatterplot"), leaf_modules(p$modules))[[1]]
  args <- c(scatter$ui_args, scatter$server_args)
  skip_if(is.null(args[["ggtheme"]]), "module arguments not stored in ui_args/server_args")
  expect_identical(args[["ggtheme"]], "minimal")
})
