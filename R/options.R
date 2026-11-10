#' BlockR options
#'
#' All settings are regular R options. Set them before building the app (or
#' pass the equivalent arguments to [run_app()] / [load_blockr_data()]).
#'
#' @section Data source:
#' \describe{
#'   \item{`blockr.data_source`}{`"bundled"` (default), `"synthetic"` or
#'     `"parquet"`. See [ingest_adam()].}
#'   \item{`blockr.synthetic`}{Settings for the `"synthetic"` source:
#'     `list(N = 400, seed = 2025, args = list())`. `N` is the number of
#'     subjects; change `seed` for a new draw (`NA` draws a random seed once per
#'     session); `args` holds extra arguments per dataset for the
#'     `random.cdisc.data::rad*()` generators, e.g.
#'     `list(ADAE = list(max_n_aes = 5))`.}
#'   \item{`blockr.parquet_dir`}{Folder read by the `"parquet"` source
#'     (default `"cache"`).}
#' }
#'
#' @section Caching:
#' \describe{
#'   \item{`blockr.use_cache`}{Use the session memory and disk caches
#'     (default `TRUE`).}
#'   \item{`blockr.refresh_cache`}{Force re-ingestion and re-processing
#'     (default `FALSE`).}
#'   \item{`blockr.cache_global`}{Copy every dataset (`ADSL`, `ADAE`, ...) into
#'     the Global Environment for easy console access (default `FALSE`;
#'     [run_app()] turns it on).}
#'   \item{`blockr.app_cache`}{Keep the built app in memory for the R
#'     session and reuse it when [run_app()] is called again with the same
#'     settings and data (instant relaunch), and serve the app's page from
#'     memory after the first request (default `TRUE`).}
#'   \item{`blockr.cache_dir`}{Folder of the disk cache (default
#'     `tools::R_user_dir("BlockR", "cache")`).}
#' }
#'
#' @section Look and feel:
#' \describe{
#'   \item{`blockr.style`}{Use the BlockR look and its interactive
#'     layer: the header with the breadcrumb and command bar (Ctrl/Cmd+K),
#'     keyboard shortcuts, deep links, plot and table tools, pins and the
#'     viewer, refined light and dark palettes, self-hosted fonts and cleaner
#'     plot defaults (default `TRUE`). `FALSE` gives the plain teal look of
#'     the source templates.}
#'   \item{`blockr.motion`}{Animations (default `TRUE`): short
#'     transitions, the sliding tab indicator, the colour-mode reveal. `FALSE`
#'     turns them off; they are also off for users who ask their system for
#'     reduced motion.}
#'   \item{`blockr.color_mode`}{Colour mode for first-time visitors:
#'     `"light"` (default), `"dark"` or `"auto"` (follows the operating
#'     system). A choice made with the header toggle is remembered by the
#'     browser and takes precedence.}
#'   \item{`blockr.canvas_height`}{Height of the canvas (and of the
#'     tutorial's canvas), as a share of the browser window: at least this
#'     much (default `0.85`, i.e. 85%), more when the window has room below
#'     the header. Between `0.4` and `1`. The canvas also has a full-screen
#'     button.}
#'   \item{`blockr.ggtheme`}{Default ggplot2 theme of the exploratory plot
#'     modules (default `"minimal"`; any of `"gray"`, `"bw"`, `"linedraw"`,
#'     `"light"`, `"dark"`, `"minimal"`, `"classic"`). Users can still change it
#'     in each module. `NULL` keeps each module's own default.}
#' }
#'
#' The app also sets two options of its building blocks, only if you have not
#' set them yourself: `teal.ggplot2_args` (plot captions and typography, see
#' [blockr_ggplot2_args()]) and `shiny.useragg` (`FALSE`, as in the
#' teal.gallery templates).
#'
#' @examples
#' \dontrun{
#' options(blockr.data_source = "synthetic", blockr.synthetic = list(N = 200, seed = 7))
#' options(blockr.color_mode = "dark")
#' run_app()
#' }
#' @name blockr_options
NULL

blockr_cache_dir_default <- function() {
  getOption("blockr.cache_dir", tools::R_user_dir("BlockR", which = "cache"))
}

#' @noRd
blockr_style_enabled <- function(style = getOption("blockr.style", TRUE)) {
  if (!(isTRUE(style) || isFALSE(style))) {
    stop("[BlockR] Option blockr.style must be TRUE or FALSE.", call. = FALSE)
  }
  style
}

#' @noRd
blockr_color_mode <- function(mode = getOption("blockr.color_mode", "light")) {
  modes <- c("auto", "light", "dark")
  if (!(is.character(mode) && length(mode) == 1 && mode %in% modes)) {
    stop("[BlockR] Option blockr.color_mode must be one of: ", toString(dQuote(modes, FALSE)), call. = FALSE)
  }
  mode
}

#' @noRd
blockr_motion <- function(motion = getOption("blockr.motion", TRUE)) {
  if (!(isTRUE(motion) || isFALSE(motion))) {
    stop("[BlockR] Option blockr.motion must be TRUE or FALSE.", call. = FALSE)
  }
  motion
}

#' @noRd
blockr_canvas_height <- function(height = getOption("blockr.canvas_height", 0.85)) {
  if (!(is.numeric(height) && length(height) == 1 && !is.na(height) && height >= 0.4 && height <= 1)) {
    stop("[BlockR] Option blockr.canvas_height must be a number between 0.4 and 1 (a share of the window).", call. = FALSE)
  }
  height
}

#' @noRd
blockr_ggtheme <- function(ggtheme = getOption("blockr.ggtheme", "minimal")) {
  themes <- c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void")
  if (!is.null(ggtheme) && !(is.character(ggtheme) && length(ggtheme) == 1 && ggtheme %in% themes)) {
    stop("[BlockR] Option blockr.ggtheme must be NULL or one of: ", toString(dQuote(themes, FALSE)), call. = FALSE)
  }
  ggtheme
}

