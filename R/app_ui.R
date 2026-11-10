# Look and feel: header (with dark mode toggle), footer, favicon and the static
# assets in inst/www (fonts, CSS, colour-mode script).
#
# The assets are a thin layer over teal's default Bootstrap 5 theme: they
# redefine Bootstrap's CSS variables instead of compiling a custom theme, so
# start-up needs no Sass compilation and the app stays recognisably teal.
# Everything is served by the app itself: no fonts or icons are fetched from
# third-party servers.

#' Folder with the static assets
#' @noRd
blockr_www <- function() {
  path <- system.file("www", package = "BlockR")
  if (!nzchar(path) || !file.exists(file.path(path, "blockr.css"))) {
    stop("[BlockR] Static assets not found (inst/www). Reinstall the package.", call. = FALSE)
  }
  path
}

#' @noRd
blockr_version <- function() as.character(utils::packageVersion("BlockR"))

#' HTML dependency: fonts, stylesheet and script (colour mode, motion)
#'
#' The inline `head` script runs before the first paint and applies dark mode
#' early (saved choice, app default, or OS preference), so dark-mode users do
#' not see a white flash, and applies the appearance chosen in the app (text
#' size, spacing, accent, ...) and the motion setting; blockr.js (loaded
#' with `defer`, so it never delays the first paint) does the rest.
#' @noRd
blockr_dependency <- function(color_mode = blockr_color_mode(), motion = blockr_motion()) {
  htmltools::htmlDependency(
    name = "blockr",
    version = blockr_version(),
    src = c(file = blockr_www()),
    stylesheet = "blockr.css",
    script = list(src = "blockr.js", defer = NA),
    head = blockr_head_script(color_mode, motion),
    all_files = TRUE
  )
}

#' @noRd
blockr_head_script <- function(color_mode = blockr_color_mode(), motion = blockr_motion()) {
  paste0(
    "<script>",
    "window.blockr={colorMode:", jsonlite::toJSON(color_mode, auto_unbox = TRUE),
    ",motion:", if (isTRUE(motion)) "true" else "false", "};",
    if (!isTRUE(motion)) "document.documentElement.classList.add('tn-still');",
    # appearance chosen in the app (text size, spacing, accent, ...)
    "try{var r=document.documentElement,",
    "a=JSON.parse(window.localStorage.getItem('blockr:appearance')||'{}')||{},",
    "k={text:'data-tn-text',density:'data-tn-density',accent:'data-tn-accent',",
    "corners:'data-tn-corners',contrast:'data-tn-contrast',canvas:'data-tn-canvas',tips:'data-tn-tips'};",
    "for(var x in k){if(a[x]){r.setAttribute(k[x],a[x]);}}",
    "if(a.motion==='off'){r.classList.add('tn-still');}if(a.motion==='on'){r.classList.remove('tn-still');}",
    "}catch(e){}",
    "(function(){var m=null;",
    "try{m=window.localStorage.getItem('blockr:color-mode');}catch(e){}",
    "if(m!=='dark'&&m!=='light'&&m!=='auto'){m=window.blockr.colorMode;}",
    "if(m==='dark'||(m==='auto'&&window.matchMedia&&",
    "window.matchMedia('(prefers-color-scheme: dark)').matches)){",
    "document.documentElement.setAttribute('data-bs-theme','dark');}",
    "})();",
    "</script>"
  )
}

#' Favicon as a data URI (no request to an external server)
#' @noRd
blockr_favicon <- function() {
  path <- file.path(blockr_www(), "favicon.png")
  paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(path, "raw", file.size(path))))
}

#' Inline SVG logo
#' @noRd
blockr_logo <- function() {
  svg <- paste(readLines(file.path(blockr_www(), "logo.svg"), warn = FALSE), collapse = "\n")
  svg <- sub("<svg ", "<svg class=\"tn-logo\" aria-hidden=\"true\" focusable=\"false\" ", svg, fixed = TRUE)
  htmltools::HTML(svg)
}

#' Short description of the data, shown as a badge in the header
#' @param source Resolved source settings (see blockr_source_config()).
#' @param n_subjects Number of subjects (rows of ADSL).
#' @noRd
blockr_data_label <- function(source, n_subjects = NULL) {
  what <- switch(source$source,
    bundled = "random.cdisc.data (bundled)",
    synthetic = sprintf("Synthetic data, seed %s", source$synthetic$seed),
    parquet = sprintf("Parquet: %s", basename(source$parquet_dir))
  )
  if (!is.null(n_subjects)) what <- sprintf("%s \u00b7 %s subjects", what, format(n_subjects, big.mark = ","))
  what
}

#' App header
#'
#' One row: brand (a link to the home page); the breadcrumb of the current module (filled in by
#' blockr.js); the command button (opens the command bar: jump to any
#' module or TLG output, run actions), the data status (source, number of
#' subjects, time of the last update), the appearance button and the
#' colour-mode toggle. The module index for the command bar travels with it
#' as JSON.
#'
#' The colour mode starts in the user's last choice, else the app default
#' (option `blockr.color_mode`), else the operating-system preference.
#' Plots are rendered server-side as light images and stay white "paper" in
#' dark mode, so the app matches its R code.
#' @param data_label Text of the data status (NULL: none), e.g.
#'   "random.cdisc.data (bundled) \u00b7 400 subjects"; the part after the
#'   last " \u00b7 " is shown as a figure.
#' @param style Use the BlockR look (option `blockr.style`)? `FALSE`
#'   gives the plain teal header.
#' @param index Module index for the command bar (see blockr_index());
#'   NULL: the command bar indexes the tabs it finds on the page.
#' @noRd
blockr_header <- function(data_label = NULL, style = blockr_style_enabled(), index = NULL) {
  toggle <- bslib::input_dark_mode(id = "blockr_dark_mode", mode = NULL)

  if (!isTRUE(style)) {
    return(blockr_header_plain(toggle))
  }

  header <- htmltools::tags$header(
    class = "tn-header",
    htmltools::tags$a(
      class = "tn-brand", href = "#/", `data-tn-action` = "home",
      title = "Canvas (H)", `aria-label` = "BlockR, go to the canvas",
      blockr_logo(),
      htmltools::div(
        class = "tn-titles",
        htmltools::span(class = "tn-title", htmltools::HTML("Block<span class=\"tn-title-accent\">R</span>")),
        htmltools::span(class = "tn-subtitle", "Clinical analysis, block by block")
      )
    ),
    htmltools::tags$nav(class = "tn-crumbs", `aria-label` = "Current module"),
    htmltools::div(
      class = "tn-actions",
      htmltools::tags$button(
        type = "button", class = "tn-cmd", `data-tn-action` = "palette",
        `aria-label` = "Jump to a module, an output or an action", `aria-keyshortcuts` = "Control+K Meta+K",
        blockr_icon("search"),
        htmltools::span(class = "tn-cmd-label", "Jump to module, output, action\u2026"),
        htmltools::tags$kbd(class = "tn-cmd-key", "Ctrl K")
      ),
      if (!is.null(data_label)) blockr_status(data_label),
      htmltools::tags$button(
        type = "button", class = "tn-iconbtn", `data-tn-action` = "appearance",
        `aria-haspopup` = "dialog", `aria-expanded` = "false",
        title = "Appearance (A)", `aria-label` = "Appearance: text size, spacing, accent and more",
        blockr_icon("density")
      ),
      htmltools::span(class = "tn-toggle", title = "Light / dark (T)", toggle)
    ),
    if (!is.null(index)) {
      htmltools::tags$script(
        type = "application/json", id = "tn-index",
        htmltools::HTML(gsub("</", "<\\/", jsonlite::toJSON(index, auto_unbox = TRUE, na = "null", null = "null"), fixed = TRUE))
      )
    }
  )
  htmltools::attachDependencies(header, blockr_dependency(), append = TRUE)
}

#' Data status of the header: a live dot (amber while the server computes),
#' the source, the number of subjects as a figure and the duration of the
#' last update (filled in by blockr.js)
#' @noRd
blockr_status <- function(data_label) {
  parts <- strsplit(data_label, " \u00b7 ", fixed = TRUE)[[1]]
  figure <- if (length(parts) > 1) parts[length(parts)] else NULL
  source <- paste(if (is.null(figure)) parts else parts[-length(parts)], collapse = " \u00b7 ")
  htmltools::span(
    class = "tn-status", title = paste("Data:", data_label), role = "status",
    htmltools::span(class = "tn-status-dot", `aria-hidden` = "true"),
    htmltools::span(class = "tn-status-source", source),
    if (!is.null(figure)) {
      htmltools::span(
        class = "tn-status-figure",
        htmltools::span(class = "tn-status-num", sub(" .*$", "", figure)),
        " ",
        htmltools::span(class = "tn-status-unit", sub("^\\S+\\s*", "", figure))
      )
    },
    htmltools::span(class = "tn-status-time")
  )
}

#' Module index for the command bar
#'
#' One entry per module (its path of group labels and its kind, for the
#' icon) and one per TLG Catalog output (ID, title, type, topic). The command
#' bar finds the matching tabs on the page; the index gives it the order,
#' the kinds and the outputs.
#' @param modules The app's teal modules (see blockr_modules()).
#' @param catalog TLG Catalog entries (see tlg_catalog()).
#' @noRd
blockr_index <- function(modules, catalog = tlg_catalog()) {
  walk <- function(x, path) {
    if (inherits(x, "teal_modules")) {
      here <- if (length(path) || !identical(x$label, "root")) c(path, x$label) else path
      unlist(lapply(x$children, walk, path = here), recursive = FALSE)
    } else {
      list(list(path = c(path, x$label), kind = blockr_module_kind(x$label, path)))
    }
  }
  # the top-level modules() call is the root; its label is not shown
  entries <- if (inherits(modules, "teal_modules")) {
    unlist(lapply(modules$children, walk, path = character(0)), recursive = FALSE)
  } else {
    walk(modules, character(0))
  }
  list(
    modules = lapply(entries, function(e) list(path = as.list(e$path), kind = e$kind)),
    tlg = unname(lapply(catalog, function(e) list(id = e$id, title = e$title, type = e$type, topic = e$topic)))
  )
}

#' Kind of a module, for its icon in the command bar
#' @noRd
blockr_module_kind <- function(label, path = character(0)) {
  l <- tolower(label)
  if (l == "canvas") return("canvas")
  if (l == "tutorial") return("tutorial")
  if (l == "tlg catalog" || "TLG Catalog" %in% path) return("catalog")
  if (grepl("data table|variable browser|file viewer", l)) return("data")
  if (grepl("plot|forest|kaplan|scatter|bivariate|association|\\bregression\\b|distribution|outlier|pca|missing", l)) {
    return("graph")
  }
  "table"
}

#' Plain header (option blockr.style = FALSE): title and toggle, with the
#' minimal dark-mode fixes of the original app
#' @noRd
blockr_header_plain <- function(toggle) {
  dark_mode_css <- htmltools::tags$style(htmltools::HTML("
    [data-bs-theme='dark'] .shiny-plot-output img {
      background-color: #fff;
      border-radius: 0.5rem;
      padding: 0.25rem;
    }
    [data-bs-theme='dark'] table { color: var(--bs-body-color); }
  "))
  htmltools::tags$span(
    style = "display: flex; align-items: center; justify-content: space-between; margin: 10px 0 10px 0;",
    dark_mode_css,
    htmltools::tags$span("BlockR: Clinical analysis, block by block", style = "font-size: 30px;"),
    htmltools::tags$span(
      style = "display: flex; align-items: center; gap: 8px;",
      htmltools::tags$span("Dark mode", class = "text-body-secondary", style = "font-size: 14px;"),
      toggle
    )
  )
}

#' App footer
#' @noRd
blockr_footer <- function(style = blockr_style_enabled()) {
  gallery_source <- "https://github.com/insightsengineering/teal.gallery/tree/main"
  link <- function(href, text) htmltools::tags$a(href = href, target = "_blank", rel = "noopener", text)
  sep <- function() htmltools::span(class = "tn-sep", `aria-hidden` = "true", "\u00b7")

  credits <- htmltools::span(
    "Built on the open-source ", link("https://insightsengineering.github.io/teal/", "teal"),
    " framework. Templates from the ",
    link("https://insightsengineering.github.io/tlg-catalog/stable/", "TLG Catalog"),
    " and teal.gallery: ",
    link(file.path(gallery_source, "exploratory"), "Exploratory"), sep(),
    link(file.path(gallery_source, "safety"), "Safety")
  )
  if (!isTRUE(style)) {
    return(htmltools::tags$p(credits))
  }
  htmltools::div(
    class = "tn-footer",
    credits,
    htmltools::span(
      class = "tn-footer-keys",
      htmltools::span(htmltools::tags$kbd("Ctrl K"), " jump"),
      htmltools::span(htmltools::tags$kbd("?"), " shortcuts")
    ),
    htmltools::span(class = "tn-version", paste("BlockR", blockr_version()))
  )
}
