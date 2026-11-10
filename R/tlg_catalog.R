# TLG Catalog: every output of the TLG Catalog
# (https://insightsengineering.github.io/tlg-catalog/stable/tlg-index.html),
# on one page: a searchable list of all outputs, grouped by topic (Adverse
# Events, Concomitant Medications, ...). Each output can be opened on the
# canvas (R/canvas.R), where its code becomes editable blocks.
#
# Each output is a registry entry (see tlg()) whose R code is a plain string,
# evaluated with teal's eval_code() on the module's *filtered* data. The same
# string is shown under "Show R code", so every output is reproducible. The
# code follows the catalog (same packages: tern, rtables, rlistings,
# tern.mmrm), adapted to this app's datasets. A failing output shows its error
# and code; it never breaks the page.
#
# Entries live in R/tlg_entries_*.R, one function per group of topics.

# ---- Registry ---------------------------------------------------------------

#' Topics (pages) of the TLG Catalog, in menu order
#' @noRd
tlg_topics <- function() {
  c(
    "Adverse Events", "Concomitant Medications", "Deaths", "Demography",
    "Disclosures", "Disposition", "ECG", "Efficacy", "Exposure", "Lab Results",
    "Medical History", "Pharmacokinetics", "Anti-Drug Antibodies",
    "Risk Management Plan", "Vital Signs", "Other Graphs"
  )
}

#' Define one catalog output
#' @param id Catalog identifier, e.g. "AET01".
#' @param title Catalog title.
#' @param type "Table", "Listing" or "Graph".
#' @param topic Page (one of tlg_topics()).
#' @param slug Catalog folder of the output (for the link to the catalog page).
#' @param data App datasets the code uses (filtered by the app).
#' @param extra Extra example datasets from 'random.cdisc.data' (see
#'   tlg_example_data()), e.g. "ADPC".
#' @param packages Packages the code needs.
#' @param code R code (string) that creates `result`.
#' @param note Optional note shown with the output (e.g. how it was adapted).
#' @param height Plot height in pixels (graphs).
#' @noRd
tlg <- function(id, title, type = c("Table", "Listing", "Graph"), topic, slug,
                data = "ADSL", extra = character(0), packages = NULL, code,
                note = NULL, height = 560L) {
  type <- match.arg(type)
  if (is.null(packages)) {
    packages <- switch(type,
      Table = "tern",
      Listing = c("tern", "rlistings"),
      Graph = c("tern", "ggplot2")
    )
  }
  list(
    id = id, title = title, type = type, topic = topic, slug = slug,
    data = data, extra = extra, packages = packages,
    code = trimws(code), note = note, height = as.integer(height)
  )
}

#' All catalog entries, named by id
#'
#' The registry is fixed for a package version, so it is built once per R
#' session and then served from memory.
#' @noRd
tlg_catalog <- function() {
  cached <- get0("tlg_catalog", envir = the, inherits = FALSE)
  if (!is.null(cached)) return(cached)
  catalog <- tlg_catalog_build()
  assign("tlg_catalog", catalog, envir = the)
  catalog
}

#' @noRd
tlg_catalog_build <- function() {
  entries <- c(
    tlg_entries_adverse_events(),
    tlg_entries_conmed(),
    tlg_entries_deaths_demography(),
    tlg_entries_disclosures_disposition(),
    tlg_entries_ecg(),
    tlg_entries_efficacy(),
    tlg_entries_exposure_rmp(),
    tlg_entries_lab(),
    tlg_entries_medical_history(),
    tlg_entries_pk(),
    tlg_entries_ada(),
    tlg_entries_vital_signs(),
    tlg_entries_other_graphs()
  )
  stats::setNames(entries, vapply(entries, `[[`, character(1), "id"))
}

#' Entries of one topic, tables first, then listings, then graphs
#' @noRd
tlg_topic_entries <- function(topic, catalog = tlg_catalog()) {
  entries <- Filter(function(e) identical(e$topic, topic), catalog)
  type_order <- match(vapply(entries, `[[`, character(1), "type"), c("Table", "Listing", "Graph"))
  entries[order(type_order, seq_along(entries))]
}

#' Link to the output's page in the TLG Catalog
#' @noRd
tlg_catalog_url <- function(entry) {
  base <- "https://insightsengineering.github.io/tlg-catalog/stable"
  folder <- switch(entry$type, Table = "tables", Listing = "listings", Graph = "graphs")
  sprintf("%s/%s/%s/%s.html", base, folder, entry$slug, tolower(entry$id))
}

#' Example data for outputs that need datasets the app does not load
#'
#' Some catalog outputs need datasets that are not part of the app's data
#' (e.g. `ADPC`, `ADPP`, `ADAB`, `ADMH`, `ADHY`). `tlg_example_data()` returns
#' the synthetic version bundled with 'random.cdisc.data' (`cadpc`, ...),
#' restricted to the subjects of `adsl` so that the app's filters apply. If
#' none of the subjects match (e.g. with your own study data), the full
#' example dataset is returned.
#'
#' @param dataset Dataset name, e.g. `"ADPC"`.
#' @param adsl Subject-level data whose subjects to keep (optional).
#' @return A data.frame.
#' @examples
#' \donttest{
#' adpc <- tlg_example_data("ADPC")
#' }
#' @export
tlg_example_data <- function(dataset, adsl = NULL) {
  name <- paste0("c", tolower(dataset))
  df <- tryCatch(
    getExportedValue("random.cdisc.data", name),
    error = function(e) stop("[BlockR] 'random.cdisc.data' has no dataset ", sQuote(name, FALSE), call. = FALSE)
  )
  if (!is.null(adsl) && "USUBJID" %in% names(df) && "USUBJID" %in% names(adsl)) {
    keep <- as.character(df$USUBJID) %in% as.character(adsl$USUBJID)
    if (any(keep)) df <- dplyr::filter(df, keep)
  }
  df
}

# ---- Running and rendering ----------------------------------------------------

#' Run one catalog entry on a teal_data object
#' @return list(ok, kind, value, error, seconds)
#' @noRd
tlg_run <- function(entry, data) {
  missing_pkgs <- entry$packages[!vapply(entry$packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs)) {
    return(list(
      ok = FALSE, kind = "missing",
      error = sprintf("This output needs the package%s %s, which %s not installed.",
                      if (length(missing_pkgs) > 1) "s" else "",
                      paste(sQuote(missing_pkgs, FALSE), collapse = ", "),
                      if (length(missing_pkgs) > 1) "are" else "is")
    ))
  }
  t0 <- Sys.time()
  q <- tryCatch(
    suppressWarnings(suppressMessages(teal.code::eval_code(data, entry$code))),
    error = function(e) e
  )
  seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (inherits(q, c("qenv.error", "error"))) {
    return(list(ok = FALSE, kind = "error", error = conditionMessage(q), seconds = seconds))
  }
  value <- tryCatch(q[["result"]], error = function(e) NULL)
  if (is.null(value)) {
    return(list(ok = FALSE, kind = "error", error = "The code did not create an object called `result`.", seconds = seconds))
  }
  list(ok = TRUE, kind = tlg_kind(value), value = value, seconds = seconds)
}

#' Kind of output object: table, listing, plot, data or text
#' @noRd
tlg_kind <- function(x) {
  if (inherits(x, "VTableTree")) return("table")
  if (inherits(x, "listing_df")) return("listing")
  if (inherits(x, c("ggplot", "gg", "grob", "gtable", "gTree", "trellis", "recordedplot"))) return("plot")
  if (is.data.frame(x)) return("data")
  "text"
}

#' Draw a plot object (ggplot, grid grob/gtable, lattice)
#' @noRd
tlg_draw <- function(x) {
  if (inherits(x, c("ggplot", "gg", "trellis", "recordedplot"))) {
    print(x)
  } else {
    grid::grid.newpage()
    grid::grid.draw(x)
  }
  invisible()
}

#' Column labels of a data.frame (label attribute, else the name)
#' @noRd
tlg_labels <- function(df) {
  vapply(names(df), function(n) {
    l <- attr(df[[n]], "label", exact = TRUE)
    if (is.null(l) || !nzchar(l)) n else as.character(l)[1]
  }, character(1), USE.NAMES = FALSE)
}

#' Listing or data.frame as an interactive table
#' @noRd
tlg_datatable <- function(x) {
  df <- as.data.frame(x)
  cols <- names(df)
  if (inherits(x, "listing_df")) {
    shown <- tryCatch(
      c(rlistings::get_keycols(x), rlistings::listing_dispcols(x)),
      error = function(e) cols
    )
    cols <- unique(intersect(shown, cols))
    if (!length(cols)) cols <- names(df)
  }
  df <- df[cols]
  DT::datatable(
    df,
    colnames = tlg_labels(df),
    rownames = FALSE,
    class = "compact hover",
    options = list(pageLength = 25, scrollX = TRUE, autoWidth = FALSE, dom = "ftip")
  )
}

# ---- Module -------------------------------------------------------------------

#' The TLG Catalog page: every output, grouped by topic
#' @param datanames Datasets available in the app.
#' @noRd
module_tlg_catalog <- function(datanames, catalog = tlg_catalog()) {
  entries <- tlg_catalog_entries(catalog)
  used <- unique(unlist(lapply(entries, `[[`, "data")))
  module(
    label = "TLG Catalog",
    server = srv_tlg_catalog,
    ui = ui_tlg_catalog,
    ui_args = list(entries = entries),
    server_args = list(entries = entries),
    datanames = intersect(union("ADSL", used), datanames)
  )
}

#' Every entry, in page order: by topic, then tables, listings, graphs
#' @noRd
tlg_catalog_entries <- function(catalog = tlg_catalog()) {
  entries <- unlist(lapply(tlg_topics(), tlg_topic_entries, catalog = catalog), recursive = FALSE)
  stats::setNames(entries, vapply(entries, `[[`, character(1), "id"))
}

#' Static assets of the TLG Catalog pages (always loaded, with or without
#' the BlockR style)
#' @noRd
tlg_catalog_dependency <- function() {
  htmltools::htmlDependency(
    name = "blockr-tlg",
    version = blockr_version(),
    src = c(file = blockr_www()),
    stylesheet = "tlg_catalog.css",
    script = "tlg_catalog.js",
    all_files = FALSE
  )
}

#' @noRd
tlg_count_label <- function(entries) {
  types <- vapply(entries, `[[`, character(1), "type")
  parts <- vapply(c("Table", "Listing", "Graph"), function(t) {
    n <- sum(types == t)
    if (!n) return(NA_character_)
    sprintf("%d %s%s", n, tolower(t), if (n > 1) "s" else "")
  }, character(1))
  paste(stats::na.omit(parts), collapse = " \u00b7 ")
}

#' Icons for the TLG Catalog pages
#'
#' Thin line icons in the style of SF Symbols (16 px grid, stroked with the
#' current text colour).
#' @param name Icon name: "Table", "Listing", "Graph", "code", "external",
#'   "search", "warning", "data", "density" or "blocks".
#' @noRd
blockr_icon <- function(name) {
  paths <- switch(name,
    Table = '<rect x="2" y="3" width="12" height="10" rx="2"/><path d="M2 6.5h12M6.5 6.5V13"/>',
    Listing = '<path d="M6 4.5h8M6 8h8M6 11.5h8"/><circle cx="2.8" cy="4.5" r=".6"/><circle cx="2.8" cy="8" r=".6"/><circle cx="2.8" cy="11.5" r=".6"/>',
    Graph = '<path d="M2.5 13.5h11"/><path d="M4.5 11V8M8 11V4.5M11.5 11V6.5"/>',
    code = '<path d="M5.5 4.5 2 8l3.5 3.5M10.5 4.5 14 8l-3.5 3.5"/>',
    external = '<path d="M6 3.5H3.5v9h9V10"/><path d="M9 2.5h4.5V7M13.5 2.5 7.5 8.5"/>',
    search = '<circle cx="7" cy="7" r="4.5"/><path d="m10.5 10.5 3 3"/>',
    warning = '<path d="M8 2.5 14 13H2L8 2.5Z"/><path d="M8 6.5v3"/><circle cx="8" cy="11.2" r=".5"/>',
    data = '<ellipse cx="8" cy="4" rx="5" ry="2"/><path d="M3 4v8c0 1.1 2.2 2 5 2s5-.9 5-2V4M3 8c0 1.1 2.2 2 5 2s5-.9 5-2"/>',
    density = '<path d="M2.5 4h11M2.5 8h11M2.5 12h11"/><path d="M5 2.5v3M11 6.5v3M7 10.5v3"/>',
    blocks = '<rect x="2.5" y="2.5" width="11" height="3.5" rx="1"/><rect x="2.5" y="7" width="11" height="3" rx="1"/><rect x="2.5" y="11" width="7" height="2.5" rx="1"/>',
    ""
  )
  htmltools::HTML(paste0(
    '<svg class="tn-icon" viewBox="0 0 16 16" width="16" height="16" fill="none" stroke="currentColor" ',
    'stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" focusable="false">',
    paths, "</svg>"
  ))
}

#' @noRd
ui_tlg_catalog <- function(id, entries) {
  ns <- shiny::NS(id)
  topics <- unique(vapply(entries, `[[`, character(1), "topic"))
  first_id <- entries[[1]]$id
  groups <- lapply(topics, function(topic) {
    items <- Filter(function(e) identical(e$topic, topic), entries)
    htmltools::div(
      class = "tn-tlg-group", `data-topic` = topic,
      htmltools::div(
        class = "tn-tlg-group-label",
        htmltools::span(topic),
        htmltools::span(class = "tn-tlg-group-count", length(items))
      ),
      lapply(items, function(e) {
        first <- identical(e$id, first_id)
        htmltools::tags$button(
          type = "button",
          class = paste("tn-tlg-item", if (first) "active"),
          role = "option",
          `aria-selected` = if (first) "true" else "false",
          `data-value` = e$id,
          `data-type` = e$type,
          title = paste(e$id, e$title, sep = ": "),
          htmltools::span(class = "tn-tlg-item-icon", blockr_icon(e$type)),
          htmltools::span(
            class = "tn-tlg-item-text",
            htmltools::span(class = "tn-tlg-item-id", e$id),
            htmltools::span(class = "tn-tlg-item-title", e$title)
          )
        )
      })
    )
  })

  htmltools::tagList(
    tlg_catalog_dependency(),
    htmltools::div(
      class = "tn-tlg",
      htmltools::div(
        class = "tn-tlg-window",
        htmltools::tags$aside(
          class = "tn-tlg-sidebar",
          htmltools::div(
            class = "tn-tlg-sidebar-head",
            htmltools::h2(class = "tn-tlg-heading", "TLG Catalog"),
            htmltools::p(class = "tn-tlg-sub", paste0(tlg_count_label(entries), " \u00b7 ", length(topics), " topics"))
          ),
          htmltools::div(
            class = "tn-tlg-search-wrap",
            blockr_icon("search"),
            htmltools::tags$input(
              type = "search", class = "tn-tlg-search",
              placeholder = "Search by ID, title or topic", `aria-label` = "Search the TLG Catalog",
              autocomplete = "off", spellcheck = "false"
            )
          ),
          htmltools::div(
            class = "tn-tlg-list", role = "listbox",
            `aria-label` = "TLG Catalog outputs",
            `data-input-id` = ns("tlg"),
            groups
          ),
          htmltools::div(
            class = "tn-tlg-sidebar-foot",
            "From the ",
            htmltools::tags$a(
              href = "https://insightsengineering.github.io/tlg-catalog/stable/tlg-index.html",
              target = "_blank", rel = "noopener", "TLG Catalog"
            )
          )
        ),
        htmltools::tags$section(
          class = "tn-tlg-main",
          shiny::uiOutput(ns("header")),
          htmltools::div(class = "tn-tlg-output", shiny::uiOutput(ns("output")))
        )
      )
    )
  )
}

#' @noRd
srv_tlg_catalog <- function(id, data, entries) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected <- shiny::reactive({
      chosen <- input$tlg
      if (is.null(chosen) || !chosen %in% names(entries)) names(entries)[1] else chosen
    })
    entry <- shiny::reactive(entries[[selected()]])

    # Results are cached per output and thrown away when the (filtered) data
    # change, so switching back and forth between outputs is instant.
    cache <- new.env(parent = emptyenv())
    cached_for <- NULL
    result <- shiny::reactive({
      d <- data()
      if (!identical(cached_for, d)) {
        rm(list = ls(cache, all.names = TRUE), envir = cache)
        cached_for <<- d
      }
      e <- entry()
      if (is.null(cache[[e$id]])) cache[[e$id]] <- tlg_run(e, d)
      cache[[e$id]]
    })

    output$header <- shiny::renderUI({
      e <- entry()
      htmltools::tags$header(
        class = "tn-tlg-toolbar",
        htmltools::div(
          class = "tn-tlg-title-block",
          htmltools::div(
            class = "tn-tlg-meta",
            htmltools::span(class = "tn-tlg-chip tn-tlg-chip-id", e$id),
            htmltools::span(class = "tn-tlg-chip tn-tlg-chip-topic", e$topic),
            htmltools::span(class = paste0("tn-tlg-chip tn-tlg-chip-", tolower(e$type)), blockr_icon(e$type), e$type)
          ),
          htmltools::h3(class = "tn-tlg-title", e$title),
          if (!is.null(e$note)) htmltools::p(class = "tn-tlg-note", e$note)
        ),
        htmltools::div(
          class = "tn-tlg-actions",
          htmltools::tags$button(
            type = "button", class = "btn btn-primary tn-tlg-action tn-tlg-open", `data-bk-open` = e$id,
            title = "Open this output on the canvas: its steps become blocks you can change",
            blockr_icon("blocks"), "Open in Canvas"
          ),
          shiny::actionButton(ns("show_code"), label = htmltools::tagList(blockr_icon("code"), "Show R code"), class = "tn-tlg-action"),
          htmltools::tags$a(
            class = "btn btn-default tn-tlg-action", href = tlg_catalog_url(e),
            target = "_blank", rel = "noopener", title = "Open this output in the TLG Catalog",
            blockr_icon("external"), "Catalog"
          )
        )
      )
    })

    output$output <- shiny::renderUI({
      res <- result()
      e <- entry()
      if (!isTRUE(res$ok)) {
        missing <- identical(res$kind, "missing")
        return(htmltools::div(
          class = paste("tn-tlg-message", if (missing) "tn-tlg-message-info" else "tn-tlg-message-error"),
          htmltools::div(class = "tn-tlg-message-icon", blockr_icon("warning")),
          htmltools::h4(if (missing) "Package needed" else "This output could not be produced with the current data"),
          htmltools::p(res$error),
          htmltools::p(class = "tn-tlg-message-hint", "Use \u201cShow R code\u201d to inspect the code, or open the catalog page for the reference version.")
        ))
      }
      switch(res$kind,
        table = htmltools::div(class = "tn-tlg-scroll tn-tlg-rtable", rtables::as_html(res$value)),
        listing = DT::DTOutput(ns("listing")),
        data = DT::DTOutput(ns("listing")),
        plot = shiny::plotOutput(ns("plot"), height = paste0(e$height, "px")),
        htmltools::tags$pre(paste(utils::capture.output(print(res$value)), collapse = "\n"))
      )
    })

    output$listing <- DT::renderDT({
      res <- result()
      shiny::req(isTRUE(res$ok), res$kind %in% c("listing", "data"))
      tlg_datatable(res$value)
    })

    output$plot <- shiny::renderPlot({
      res <- result()
      shiny::req(isTRUE(res$ok), identical(res$kind, "plot"))
      tlg_draw(res$value)
    }, res = 96)

    shiny::observeEvent(input$show_code, {
      e <- entry()
      code <- paste(c(teal.code::get_code(data()), "", paste0("# ---- ", e$id, ": ", e$title, " ----"), e$code), collapse = "\n")
      code_id <- ns("code_text")
      shiny::showModal(shiny::modalDialog(
        title = paste0(e$id, " \u00b7 R code"),
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
          htmltools::tags$button(
            type = "button", class = "btn btn-primary",
            `data-dismiss` = "modal", `data-bs-dismiss` = "modal", "Done"
          )
        )
      ))
    })
  })
}
