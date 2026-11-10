# Look and feel: static assets, options, header/footer, plot defaults.

www <- function(...) file.path(blockr_www(), ...)

# ---- Static assets --------------------------------------------------------------

css_text <- function() paste(readLines(www("blockr.css"), warn = FALSE), collapse = "\n")
mode_block <- function(css, mode) {
  pattern <- if (identical(mode, "dark")) 'html\\[data-bs-theme="dark"\\] \\{[^}]+\\}' else 'html:not\\(\\[data-bs-theme="dark"\\]\\) \\{[^}]+\\}'
  regmatches(css, regexpr(pattern, css))
}

test_that("all static assets are installed", {
  for (f in c("blockr.css", "blockr.js", "tlg_catalog.css", "tlg_catalog.js", "blockr_canvas.css", "blockr_canvas.js",
              "blockr_tutorial.js",
              "logo.svg", "favicon.png",
              "fonts/InterVariable.woff2", "fonts/JetBrainsMonoNL-Regular.woff2", "fonts/JetBrainsMonoNL-Bold.woff2",
              "fonts/OFL.txt")) {
    expect_true(file.exists(www(f)), info = f)
  }
})

test_that("fonts are real WOFF2 files and nothing loads from other servers", {
  css <- css_text()
  urls <- regmatches(css, gregexpr('url\\("[^"]+"\\)', css))[[1]]
  urls <- gsub('^url\\("|"\\)$', "", urls)
  urls <- urls[!startsWith(urls, "data:")] # inline icons
  expect_setequal(urls, c("fonts/InterVariable.woff2", "fonts/JetBrainsMonoNL-Regular.woff2", "fonts/JetBrainsMonoNL-Bold.woff2"))
  for (u in urls) expect_identical(readBin(www(u), "raw", 4), charToRaw("wOF2"), info = u)
  expect_no_match(css, "url\\(\\s*[\"']?(https?:)?//")
  expect_no_match(css, "@import", fixed = TRUE)
  # the system font comes first, so Apple devices download nothing
  expect_match(css, "--tn-font-sans: -apple-system, BlinkMacSystemFont, \"Inter\"", fixed = TRUE)
  ofl <- readLines(www("fonts", "OFL.txt"), warn = FALSE)
  expect_true(any(grepl("SIL OPEN FONT LICENSE Version 1.1", ofl, fixed = TRUE)))
  expect_false(any(grepl("Roboto", ofl, fixed = TRUE)))
})

test_that("light and dark palettes define the instrument tokens and Bootstrap's colours", {
  css <- css_text()
  for (mode in c("light", "dark")) {
    block <- mode_block(css, mode)
    expect_length(block, 1)
    for (token in c("--tn-canvas", "--tn-dot", "--tn-panel", "--tn-panel-2", "--tn-line", "--tn-ink", "--tn-ink-3",
                    "--tn-accent", "--tn-accent-rgb", "--tn-accent-soft", "--tn-signal", "--tn-code-bg",
                    "--bs-body-bg", "--bs-body-color", "--bs-primary", "--bs-link-color", "--bs-border-color")) {
      expect_match(block, paste0(token, ":"), fixed = TRUE, info = paste(mode, token))
    }
  }
  expect_match(mode_block(css, "dark"), "color-scheme: dark;", fixed = TRUE)
  # appearance settings and section tints redefine the tokens
  for (x in c('html[data-tn-density="compact"] {', 'html[data-tn-density="roomy"] {', 'html[data-tn-text="xl"]',
              'html[data-tn-corners="crisp"]', 'html[data-tn-contrast="high"]', 'html[data-tn-canvas="plain"]',
              '[data-tn-section="safety"]', '[data-tn-section="explore"]',
              '[data-tn-section="catalog"]', 'html[data-tn-accent="ocean"]')) {
    expect_match(css, x, fixed = TRUE, info = x)
  }
  # the Efficacy section and the home-page dashboard are gone
  expect_no_match(css, 'data-tn-section="efficacy"', fixed = TRUE)
  expect_no_match(css, "\\.tn-ov(-|\\s|\\{|,)") # (not .tn-overlay)
})

test_that("the interactive layer and the TLG pages are styled; drawing stays cheap", {
  css <- css_text()
  for (x in c(".tn-header {", ".tn-crumbs", ".tn-cmd", ".tn-status-time", ".nav.tn-rail::after",
              ".tn-pal-item", ".tn-keys", ".tn-tools", ".tn-viewer", ".tn-swipe-handle", ".tn-diff", ".tn-tray",
              ".tn-where", ".tn-appear", ".tn-seg", ".tn-toast", "html.tn-focus", ".tn-progress", "html .tn-tlg-item.active",
              "html.tn-still", "@media (prefers-reduced-motion: reduce)", "::view-transition-new(root)")) {
    expect_match(css, x, fixed = TRUE, info = x)
  }
  # no blur, no ambient animation, no background images other than inline SVG
  expect_no_match(css, "backdrop-filter")
  expect_no_match(css, "infinite alternate")
})

test_that("the script is self-contained and wires every feature", {
  js <- paste(readLines(www("blockr.js"), warn = FALSE), collapse = "\n")
  expect_no_match(js, "https?://")       # no remote code or assets
  expect_no_match(js, "\\beval\\(")    # no eval
  for (x in c("startViewTransition", "shiny:busy", "shiny:idle", "history.pushState", "popstate", "tn-index",
              "bootstrap.Tab", "ClipboardItem", "init.dt", "prefers-reduced-motion")) {
    expect_match(js, x, fixed = TRUE, info = x)
  }
  # every action of the command bar has a key, and the keys are handled
  keys <- regmatches(js, gregexpr('keys: \\["[^"]+"\\]', js))[[1]]
  expect_gte(length(keys), 10)
  # home is the canvas; the dashboard is gone
  expect_match(js, 'e.kind === "canvas"', fixed = TRUE)
  # the canvas's "What is this?" tips are an appearance setting, settable from the canvas
  expect_match(js, 'id: "tips"', fixed = TRUE)
  expect_match(js, "window.blockr.setting", fixed = TRUE)
  expect_no_match(js, "renderOverview", fixed = TRUE)
})

test_that("favicon is an inlined PNG (no request to an external server)", {
  uri <- blockr_favicon()
  expect_match(uri, "^data:image/png;base64,")
  png <- jsonlite::base64_dec(sub("^data:image/png;base64,", "", uri))
  expect_identical(png[2:4], charToRaw("PNG"))
})

# ---- Options ------------------------------------------------------------------------

test_that("look-and-feel options are validated", {
  expect_true(blockr_style_enabled(TRUE))
  expect_false(blockr_style_enabled(FALSE))
  expect_error(blockr_style_enabled("yes"), "TRUE or FALSE")

  for (m in c("auto", "light", "dark")) expect_identical(blockr_color_mode(m), m)
  withr::with_options(list(blockr.color_mode = NULL), expect_identical(blockr_color_mode(), "light"))
  withr::with_options(list(blockr.ggtheme = NULL), expect_identical(blockr_ggtheme(), "minimal"))
  expect_error(blockr_color_mode("sepia"), "blockr.color_mode")

  expect_true(blockr_motion(TRUE))
  expect_false(blockr_motion(FALSE))
  withr::with_options(list(blockr.motion = NULL), expect_true(blockr_motion()))
  expect_error(blockr_motion("slow"), "blockr.motion")

  expect_identical(blockr_ggtheme("minimal"), "minimal")
  expect_null(blockr_ggtheme(NULL))
  expect_error(blockr_ggtheme("excel"), "blockr.ggtheme")
})

# ---- Header, footer, colour mode -------------------------------------------------------

test_that("the head script carries the colour mode and applies dark mode early", {
  for (m in c("auto", "light", "dark")) {
    js <- blockr_head_script(m)
    expect_match(js, sprintf('colorMode:"%s"', m), fixed = TRUE)
    expect_match(js, "^<script>.*</script>$")
    expect_identical(lengths(regmatches(js, gregexpr("</script>", js, fixed = TRUE))), 1L)
  }
  # "light" is never pre-set in <head> (bslib's toggle mishandles it; see blockr.js)
  expect_no_match(blockr_head_script("light"), "setAttribute('data-bs-theme','light')", fixed = TRUE)
  # animations: on by default, turned off before the first paint if not wanted
  expect_match(blockr_head_script("light", TRUE), "motion:true", fixed = TRUE)
  expect_no_match(blockr_head_script("light", TRUE), "document.documentElement.classList.add('tn-still')", fixed = TRUE)
  expect_match(blockr_head_script("light", FALSE), "motion:false", fixed = TRUE)
  expect_match(blockr_head_script("light", FALSE), "classList.add('tn-still')", fixed = TRUE)
  # the saved appearance is applied before the first paint too
  js <- blockr_head_script("light")
  for (x in c("blockr:appearance", "data-tn-text", "data-tn-density", "data-tn-accent", "data-tn-corners",
              "data-tn-contrast", "data-tn-canvas", "data-tn-tips")) {
    expect_match(js, x, fixed = TRUE, info = x)
  }
  # a saved "auto" colour mode follows the operating system
  expect_match(js, "m!=='auto'", fixed = TRUE)
})

test_that("header: brand, breadcrumb, command button, status, switches and the asset dependency", {
  header <- blockr_header("random.cdisc.data (bundled) \u00b7 400 subjects", style = TRUE)
  rendered <- htmltools::renderTags(header)
  expect_match(rendered$html, '<header class="tn-header">', fixed = TRUE)
  # the brand is a link home
  expect_match(rendered$html, '<a class="tn-brand" href="#/" data-tn-action="home"', fixed = TRUE)
  expect_match(rendered$html, "<svg", fixed = TRUE)
  expect_match(rendered$html, "Block<span class=\"tn-title-accent\">R</span>", fixed = TRUE)
  expect_match(rendered$html, "Clinical analysis, block by block", fixed = TRUE)
  expect_match(rendered$html, '<nav class="tn-crumbs" aria-label="Current module">', fixed = TRUE)
  expect_match(rendered$html, 'data-tn-action="palette"', fixed = TRUE)
  expect_match(rendered$html, 'data-tn-action="appearance"', fixed = TRUE)
  # the status shows the source and the number of subjects as a figure
  expect_match(rendered$html, '<span class="tn-status-source">random.cdisc.data (bundled)</span>', fixed = TRUE)
  expect_match(rendered$html, '<span class="tn-status-num">400</span>', fixed = TRUE)
  expect_match(rendered$html, '<span class="tn-status-unit">subjects</span>', fixed = TRUE)
  expect_match(rendered$html, "blockr_dark_mode", fixed = TRUE)
  dep <- Filter(function(d) identical(d$name, "blockr"), rendered$dependencies)
  expect_length(dep, 1)
  expect_identical(dep[[1]]$stylesheet, "blockr.css")
  expect_identical(dep[[1]]$script$src, "blockr.js")
  expect_true("defer" %in% names(dep[[1]]$script)) # never delays the first paint
  expect_match(dep[[1]]$head, "blockr", fixed = TRUE)

  # a label without a figure is shown as is; no status without a label
  html <- htmltools::renderTags(blockr_header("random.cdisc.data (bundled)", style = TRUE))$html
  expect_match(html, '<span class="tn-status-source">random.cdisc.data (bundled)</span>', fixed = TRUE)
  expect_no_match(html, "tn-status-figure", fixed = TRUE)
  expect_no_match(htmltools::renderTags(blockr_header(NULL, style = TRUE))$html, "tn-status", fixed = TRUE)

  # the index travels as JSON, safe inside <script>
  idx <- list(modules = list(list(path = list("A </script> B"), kind = "table")), tlg = list())
  html <- htmltools::renderTags(blockr_header("x", style = TRUE, index = idx))$html
  expect_match(html, '<script type="application/json" id="tn-index">', fixed = TRUE)
  expect_no_match(html, "A </script>", fixed = TRUE)
  json <- sub('.*<script type="application/json" id="tn-index">(.*?)</script>.*', "\\1", html)
  expect_identical(jsonlite::fromJSON(json, simplifyVector = FALSE)$modules[[1]]$path[[1]], "A </script> B")
})

test_that("the module index lists every module with its path and kind, and every TLG output", {
  fake_module <- function(label) structure(list(label = label), class = "teal_module")
  fake_modules <- function(label, ...) structure(list(label = label, children = list(...)), class = "teal_modules")
  mods <- fake_modules(
    "root",
    fake_module("Canvas"),
    fake_module("TLG Catalog"),
    fake_module("Data Table"),
    fake_modules("Safety Analysis", fake_modules("Adverse Events", fake_module("AE Summary")), fake_module("Line Plot"))
  )
  catalog <- list(AET01 = list(id = "AET01", title = "Safety Summary", type = "Table", topic = "Adverse Events"))
  idx <- blockr_index(mods, catalog = catalog)
  paths <- vapply(idx$modules, function(m) paste(unlist(m$path), collapse = " / "), character(1))
  expect_identical(paths, c("Canvas", "TLG Catalog", "Data Table",
                            "Safety Analysis / Adverse Events / AE Summary", "Safety Analysis / Line Plot"))
  kinds <- vapply(idx$modules, `[[`, character(1), "kind")
  expect_identical(kinds, c("canvas", "catalog", "data", "table", "graph"))
  expect_null(idx$overview)
  expect_identical(idx$tlg[[1]], list(id = "AET01", title = "Safety Summary", type = "Table", topic = "Adverse Events"))
  # single paths stay arrays in JSON
  expect_match(as.character(jsonlite::toJSON(idx, auto_unbox = TRUE)), '"path":["Canvas"]', fixed = TRUE)
})

test_that("module kinds follow the label", {
  expect_identical(blockr_module_kind("Kaplan Meier Plot"), "graph")
  expect_identical(blockr_module_kind("Regression"), "graph")
  expect_identical(blockr_module_kind("Cox Reg"), "table")
  expect_identical(blockr_module_kind("Variable Browser"), "data")
  expect_identical(blockr_module_kind("Deaths", "TLG Catalog"), "catalog")
  expect_identical(blockr_module_kind("TLG Catalog"), "catalog")
  expect_identical(blockr_module_kind("Canvas"), "canvas")
  expect_identical(blockr_module_kind("Tutorial"), "tutorial")
})

test_that("the plain header (style = FALSE) has no blockr assets", {
  rendered <- htmltools::renderTags(blockr_header("x", style = FALSE))
  expect_match(rendered$html, "BlockR: Clinical analysis, block by block", fixed = TRUE)
  expect_match(rendered$html, "blockr_dark_mode", fixed = TRUE)
  expect_false("blockr" %in% vapply(rendered$dependencies, function(d) d$name, character(1)))
})

test_that("the footer credits teal, the TLG Catalog and the templates", {
  for (style in c(TRUE, FALSE)) {
    html <- as.character(blockr_footer(style = style))
    for (x in c("teal", "TLG Catalog", "Exploratory", "Safety")) expect_match(html, x, fixed = TRUE)
    expect_no_match(html, "Efficacy", fixed = TRUE)
  }
  expect_match(as.character(blockr_footer(style = TRUE)), paste("BlockR", blockr_version()), fixed = TRUE)
  expect_match(as.character(blockr_footer(style = TRUE)), "tn-footer-keys", fixed = TRUE)
})

test_that("the data status describes every source", {
  expect_identical(
    blockr_data_label(list(source = "bundled"), 400),
    "random.cdisc.data (bundled) \u00b7 400 subjects"
  )
  expect_match(blockr_data_label(list(source = "synthetic", synthetic = list(N = 20, seed = 7)), 20), "seed 7")
  expect_match(blockr_data_label(list(source = "parquet", parquet_dir = "/data/adam"), 1234), "Parquet: adam \u00b7 1,234 subjects")
  expect_identical(blockr_data_label(list(source = "bundled")), "random.cdisc.data (bundled)")
})

# ---- Plot defaults -----------------------------------------------------------------------

test_that("plot theme arguments are valid ggplot2 theme settings and render", {
  skip_if_not_installed("ggplot2")
  withr::local_pdf(NULL) # draw off-screen, no Rplots.pdf
  args <- blockr_plot_theme_args()
  expect_true(all(names(args) %in% methods::formalArgs(ggplot2::theme)))
  # quoted calls, so "Show R code" prints readable code
  expect_true(all(vapply(args, function(x) is.character(x) || is.call(x), logical(1))))

  # built the way teal.widgets builds it: ggplot2::theme(<args>) as a call
  theme_call <- as.call(c(list(quote(ggplot2::theme)), args))
  expect_no_error(eval(parse(text = paste(deparse(theme_call), collapse = "\n"))))
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~cyl) +
    ggplot2::labs(title = "t", subtitle = "s", caption = "NEST PROJECT") +
    ggplot2::theme_light() +
    eval(theme_call)
  expect_no_error(ggplot2::ggplotGrob(plot))
})

test_that("teal.ggplot2_args: caption always, refinements only with the blockr style", {
  skip_if_not_installed("teal.widgets")
  modern <- blockr_ggplot2_args(style = TRUE)
  plain <- blockr_ggplot2_args(style = FALSE)
  expect_s3_class(modern, "ggplot2_args")
  expect_identical(modern$labs$caption, "NEST PROJECT")
  expect_identical(plain$labs$caption, "NEST PROJECT")
  expect_identical(modern$theme, blockr_plot_theme_args())
  expect_length(plain$theme, 0)
})

test_that("app options: set when unset or ours, never override the user's", {
  skip_if_not_installed("teal.widgets")
  mem <- new.env()
  withr::local_options(teal.ggplot2_args = NULL, shiny.useragg = NULL, blockr.style = TRUE)
  blockr_set_app_options(memory_env = mem)
  expect_identical(getOption("teal.ggplot2_args"), blockr_ggplot2_args(style = TRUE))
  expect_false(getOption("shiny.useragg"))

  # an earlier value set by the app is refreshed (e.g. after changing the style)
  withr::local_options(blockr.style = FALSE)
  blockr_set_app_options(memory_env = mem)
  expect_identical(getOption("teal.ggplot2_args"), blockr_ggplot2_args(style = FALSE))

  # the user's own value is left alone
  mine <- teal.widgets::ggplot2_args(labs = list(caption = "MY STUDY"))
  withr::local_options(teal.ggplot2_args = mine, shiny.useragg = TRUE)
  blockr_set_app_options(memory_env = mem)
  expect_identical(getOption("teal.ggplot2_args"), mine)
  expect_true(getOption("shiny.useragg"))
})

test_that("module theme arguments are only added where the module supports them", {
  with_ggtheme <- function(label, ggtheme = c("gray", "bw", "light", "minimal")) list(label = label, ggtheme = ggtheme)
  association <- function(label, distribution_theme = c("gray", "light"), association_theme = c("gray", "light")) NULL
  other_choices <- function(label, ggtheme = c("gray", "bw")) NULL
  no_theme <- function(label, x) NULL

  expect_identical(blockr_theme_args(with_ggtheme, "light"), list(ggtheme = "light"))
  expect_identical(blockr_theme_args(association, "light"), list(distribution_theme = "light", association_theme = "light"))
  expect_identical(blockr_theme_args(other_choices, "light"), setNames(list(), character(0)))
  expect_identical(blockr_theme_args(no_theme, "light"), setNames(list(), character(0)))
  expect_identical(blockr_theme_args(with_ggtheme, NULL), list())
  expect_identical(blockr_theme_args(with_ggtheme, "light", exclude = "ggtheme"), setNames(list(), character(0)))

  # arguments are passed through; an explicit ggtheme wins
  expect_identical(blockr_plot_module(with_ggtheme, "A", ggtheme = "light"), list(label = "A", ggtheme = "light"))
  expect_identical(
    blockr_plot_module(with_ggtheme, "A", ggtheme = "minimal"),
    list(label = "A", ggtheme = "minimal")
  )
  expect_identical(blockr_plot_module(no_theme, "B", x = 1, ggtheme = "light"), NULL)
})

test_that("a deprecated theme argument falls back to the module default, silently", {
  deprecating <- function(label, ggtheme = c("gray", "light")) {
    if (!missing(ggtheme)) warning("`ggtheme` is deprecated; use decorators instead.", call. = FALSE)
    list(label = label, ggtheme = if (missing(ggtheme)) "module default" else ggtheme)
  }
  expect_no_warning(res <- blockr_plot_module(deprecating, "A", ggtheme = "light"))
  expect_identical(res, list(label = "A", ggtheme = "module default"))

  # other warnings are not swallowed
  noisy <- function(label, ggtheme = c("gray", "light")) {
    warning("something else")
    label
  }
  expect_warning(blockr_plot_module(noisy, "B", ggtheme = "light"), "something else")
})

test_that("icons: 16 px line icons drawn in the text colour", {
  for (name in c("Table", "Listing", "Graph", "code", "external", "search", "warning", "data", "density", "blocks")) {
    icon <- as.character(blockr_icon(name))
    expect_match(icon, 'viewBox="0 0 16 16"', fixed = TRUE, info = name)
    expect_match(icon, 'stroke="currentColor"', fixed = TRUE, info = name)
    expect_match(icon, 'aria-hidden="true"', fixed = TRUE, info = name)
  }
})
