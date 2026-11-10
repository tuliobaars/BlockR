#' Run the BlockR app
#'
#' `blockr_app()` builds the teal app: data ([build_blockr_data()]), the
#' canvas (the home page: drag-and-drop blocks, starting from any TLG
#' Catalog template), the TLG Catalog, the exploratory and safety module
#' groups, module-specific filters (safety modules start on the safety
#' population), the header with the dark mode toggle and the BlockR look
#' (see the "Look and feel" section of [blockr_options]).
#' `run_app()` wraps it in a Shiny app object, which starts the app when
#' printed at the console (or when returned from an `app.R` file).
#'
#' @inheritParams load_blockr_data
#' @param cache_global Copy every dataset into the Global Environment for easy
#'   console access? `run_app()` defaults to `TRUE`.
#' @return `blockr_app()`: a teal app (list with `ui` and `server`).
#'   `run_app()`: a `shiny.appobj`.
#' @seealso [blockr_options] for all settings.
#' @examples
#' \dontrun{
#' run_app()
#' run_app(source = "synthetic", synthetic = list(N = 200, seed = 7))
#' run_app(source = "parquet", parquet_dir = "path/to/adam")
#' }
#' @export
run_app <- function(source = getOption("blockr.data_source", "bundled"),
                    synthetic = getOption("blockr.synthetic", list()),
                    parquet_dir = getOption("blockr.parquet_dir", "cache"),
                    cache_global = getOption("blockr.cache_global", TRUE)) {
  app <- blockr_app(source, synthetic, parquet_dir, cache_global)
  shiny::shinyApp(blockr_fast_ui(app$ui), app$server)
}

#' @rdname run_app
#' @export
blockr_app <- function(source = getOption("blockr.data_source", "bundled"),
                          synthetic = getOption("blockr.synthetic", list()),
                          parquet_dir = getOption("blockr.parquet_dir", "cache"),
                          cache_global = getOption("blockr.cache_global", FALSE)) {
  t0 <- Sys.time()
  steps <- character(0)
  tick <- function(what) {
    t1 <- Sys.time()
    steps[[length(steps) + 1L]] <<- sprintf("%s %.2fs", what, as.numeric(difftime(t1, t0, units = "secs")))
    t0 <<- t1
  }
  start <- t0
  style <- blockr_style_enabled()
  blockr_color_mode() # validate the options before building anything
  blockr_canvas_height()
  blockr_set_app_options()

  # The same app, built earlier in this R session with the same settings and
  # data, is reused: a relaunch is instant.
  key <- blockr_app_key(source, synthetic, parquet_dir, style)
  cached <- get0("app_cache", envir = the, inherits = FALSE)
  if (!is.null(key) && is.list(cached) && identical(cached$key, key)) {
    if (isTRUE(cache_global)) load_blockr_data(source, synthetic, parquet_dir, cache_global = TRUE)
    message(sprintf(
      "[BlockR] App reused from this R session in %.2fs",
      as.numeric(difftime(Sys.time(), start, units = "secs"))
    ))
    return(cached$app)
  }

  data <- build_blockr_data(source, synthetic, parquet_dir, cache_global)
  tick("data")
  modules <- blockr_modules(data)
  tick("modules")
  data_label <- blockr_data_label(data[[".blockr"]]$source, nrow(data[["ADSL"]]))
  index <- if (style) blockr_index(modules)
  tick("index")

  app <- teal::init(
    data = data,
    filter = blockr_filters(modules),
    modules = modules
  ) |>
    teal::modify_title(title = "BlockR \u00b7 Clinical analysis, block by block", favicon = blockr_favicon()) |>
    teal::modify_header(blockr_header(data_label, style = style, index = index)) |>
    teal::modify_footer(blockr_footer(style = style))
  tick("teal")

  if (!is.null(key)) assign("app_cache", list(key = key, app = app), envir = the)
  message(sprintf(
    "[BlockR] App built in %.1fs (%s)",
    as.numeric(difftime(Sys.time(), start, units = "secs")), paste(steps, collapse = ", ")
  ))
  app
}

#' Key of the app cache: everything the app depends on
#'
#' `NULL` (no caching) when option `blockr.app_cache` is `FALSE` or a
#' data refresh is requested.
#' @noRd
blockr_app_key <- function(source, synthetic, parquet_dir, style) {
  if (!isTRUE(getOption("blockr.app_cache", TRUE)) || isTRUE(getOption("blockr.refresh_cache", FALSE))) {
    return(NULL)
  }
  config <- tryCatch(blockr_source_config(source, synthetic, parquet_dir), error = function(e) NULL)
  if (is.null(config)) return(NULL)
  rlang::hash(list(
    version = blockr_version(),
    data = blockr_data_signature(config),
    style = style,
    color_mode = blockr_color_mode(),
    motion = blockr_motion(),
    ggtheme = blockr_ggtheme(),
    canvas_height = blockr_canvas_height(),
    ggplot2_args = getOption("teal.ggplot2_args")
  ))
}

#' Serve the app's page from memory
#'
#' teal builds the page (every module's interface) on each browser request.
#' The page does not change between requests, so the first one is kept and
#' served again: reloads and new sessions get the page at once. Requests with
#' a query string (e.g. a bookmark to restore) are always built afresh.
#' @param ui The app's UI: a function of the request, or a tag list.
#' @noRd
blockr_fast_ui <- function(ui) {
  if (!is.function(ui) || !isTRUE(getOption("blockr.app_cache", TRUE))) return(ui)
  page <- NULL
  function(request) {
    query <- if (is.null(request)) NULL else request$QUERY_STRING
    plain <- is.null(query) || !nzchar(as.character(query))
    if (plain && !is.null(page)) return(page)
    out <- if (length(formals(ui))) ui(request) else ui()
    if (plain) page <<- out
    out
  }
}
