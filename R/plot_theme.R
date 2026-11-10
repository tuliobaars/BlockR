# Plot defaults: cleaner typography for every ggplot2-based module, and a
# modern default theme for the exploratory plot modules.
#
# Both go through teal's own, documented mechanisms (the `teal.ggplot2_args`
# option and the modules' `ggtheme` arguments), so they appear in each
# module's "Show R code" and the reproduced plots match the app exactly.
# Fonts are deliberately *not* set in the plots: a font family in the code
# would break "Show R code" and PDF downloads on machines without that font.

#' Plot defaults for the teal modules
#'
#' `blockr_ggplot2_args()` returns the value the app gives to option
#' `teal.ggplot2_args` (unless you set that option yourself): the
#' teal.gallery caption plus a cleaner, editorial look that works on top of
#' any ggplot2 theme the user picks in a module: large left-aligned bold
#' titles, a graded ink scale (titles, axis titles, labels, captions),
#' facet labels as bold left-aligned headings instead of grey strips, no
#' tick marks or minor grid lines, plain legend keys and more breathing room.
#' Data colours (the NEST palette from 'nestcolor') are left unchanged.
#'
#' `blockr_plot_theme_args()` returns just the theme part, as quoted
#' `ggplot2` calls so that "Show R code" prints readable code.
#'
#' @param style Use the BlockR refinements? `FALSE` returns the plain
#'   teal.gallery default (caption only). Defaults to option `blockr.style`.
#' @return `blockr_ggplot2_args()`: a `teal.widgets::ggplot2_args()`
#'   object. `blockr_plot_theme_args()`: a named list of arguments for
#'   [ggplot2::theme()].
#' @seealso [blockr_options]
#' @examples
#' theme_args <- blockr_plot_theme_args()
#' names(theme_args)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(mtcars, aes(wt, mpg)) +
#'     geom_point() +
#'     labs(title = "Fuel economy", caption = "NEST PROJECT") +
#'     theme_bw() +
#'     do.call(theme, lapply(theme_args, eval))
#' }
#' @export
blockr_ggplot2_args <- function(style = blockr_style_enabled()) {
  labs <- list(caption = "NEST PROJECT")
  if (!isTRUE(style)) {
    return(teal.widgets::ggplot2_args(labs = labs))
  }
  teal.widgets::ggplot2_args(labs = labs, theme = blockr_plot_theme_args())
}

#' @rdname blockr_ggplot2_args
#' @export
blockr_plot_theme_args <- function() {
  ink <- "#0F1720"   # titles, facet labels
  ink2 <- "#3A4651"  # axis and legend titles, legend labels
  ink3 <- "#66737F"  # axis labels, subtitles
  ink4 <- "#95A1AB"  # captions
  list(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = bquote(ggplot2::element_text(face = "bold", size = ggplot2::rel(1.25), colour = .(ink), margin = ggplot2::margin(b = 4))),
    plot.subtitle = bquote(ggplot2::element_text(colour = .(ink3), margin = ggplot2::margin(b = 10))),
    plot.caption = bquote(ggplot2::element_text(colour = .(ink4), size = ggplot2::rel(0.72), hjust = 1, margin = ggplot2::margin(t = 10))),
    plot.margin = quote(ggplot2::margin(12, 16, 10, 12)),
    axis.title = bquote(ggplot2::element_text(colour = .(ink2), size = ggplot2::rel(0.92))),
    axis.text = bquote(ggplot2::element_text(colour = .(ink3))),
    axis.ticks = quote(ggplot2::element_blank()),
    panel.grid.minor = quote(ggplot2::element_blank()),
    panel.spacing = quote(ggplot2::unit(14, "pt")),
    strip.background = quote(ggplot2::element_blank()),
    strip.text = bquote(ggplot2::element_text(face = "bold", colour = .(ink), hjust = 0, size = ggplot2::rel(0.95), margin = ggplot2::margin(4, 0, 6, 0))),
    legend.title = bquote(ggplot2::element_text(face = "bold", colour = .(ink2), size = ggplot2::rel(0.9))),
    legend.text = bquote(ggplot2::element_text(colour = .(ink2))),
    legend.key = quote(ggplot2::element_blank())
  )
}

#' Set the plot-related options the app relies on
#'
#' Only options the user has not set are changed. `teal.ggplot2_args` is also
#' updated when it still holds the value set by an earlier app build in this
#' session (e.g. after changing option `blockr.style`).
#' @noRd
blockr_set_app_options <- function(memory_env = the) {
  if (is.null(getOption("shiny.useragg"))) options(shiny.useragg = FALSE)

  current <- getOption("teal.ggplot2_args")
  if (is.null(current) || identical(current, memory_env$ggplot2_args)) {
    memory_env$ggplot2_args <- blockr_ggplot2_args()
    options(teal.ggplot2_args = memory_env$ggplot2_args)
  }
  invisible()
}

#' Call a teal.modules.general plot module with the app's default ggplot2 theme
#'
#' Adds the module's theme argument(s) (`ggtheme`, or `distribution_theme` and
#' `association_theme` for the association module) set to `ggtheme`, but only
#' the ones the installed module version accepts and whose allowed values
#' include `ggtheme`. If the module warns that a theme argument is deprecated,
#' it is built again without it. A change in a module's arguments can therefore
#' never break the app (or flood it with warnings); the module's own default is
#' used instead. Arguments given explicitly in `...` always win.
#' @param module A module function, e.g. `tm_g_scatterplot`.
#' @param ... Arguments for `module`.
#' @param ggtheme Theme name (option `blockr.ggtheme`); `NULL` adds nothing.
#' @noRd
blockr_plot_module <- function(module, ..., ggtheme = blockr_ggtheme()) {
  args <- list(...)
  theme_args <- blockr_theme_args(module, ggtheme, exclude = names(args))
  if (!length(theme_args)) {
    return(do.call(module, args))
  }
  deprecated <- FALSE
  result <- withCallingHandlers(
    do.call(module, c(args, theme_args)),
    warning = function(w) {
      about_theme <- any(vapply(names(theme_args), grepl, logical(1), x = conditionMessage(w), fixed = TRUE))
      if (about_theme && (inherits(w, "lifecycle_warning_deprecated") || grepl("deprecat", conditionMessage(w), ignore.case = TRUE))) {
        deprecated <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )
  if (deprecated) do.call(module, args) else result
}

#' @noRd
blockr_theme_args <- function(module, ggtheme = blockr_ggtheme(), exclude = character(0)) {
  if (is.null(ggtheme)) {
    return(list())
  }
  fmls <- formals(module)
  candidates <- setdiff(intersect(c("ggtheme", "distribution_theme", "association_theme"), names(fmls)), exclude)
  supported <- vapply(candidates, function(arg) {
    choices <- tryCatch(eval(fmls[[arg]], envir = environment(module)), error = function(e) NULL)
    is.character(choices) && ggtheme %in% choices
  }, logical(1))
  stats::setNames(rep(list(ggtheme), sum(supported)), candidates[supported])
}
