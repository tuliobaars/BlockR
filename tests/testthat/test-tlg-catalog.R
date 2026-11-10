# TLG Catalog: registry, pages, runner and every output on the bundled data.

catalog <- tlg_catalog()
app_datasets <- c("ADSL", "ADRS", "ADLB", "ADLBPCA", "ADTTE", "ADQS",
                  "ADAE", "ADAETTE", "ADEX", "ADEG", "ADVS", "ADCM")

test_that("registry: unique ids, valid fields, every topic has outputs", {
  ids <- vapply(catalog, `[[`, character(1), "id", USE.NAMES = FALSE)
  expect_false(anyDuplicated(ids) > 0)
  expect_identical(names(catalog), ids)
  expect_gt(length(catalog), 130)
  for (e in catalog) {
    expect_true(e$type %in% c("Table", "Listing", "Graph"), info = e$id)
    expect_true(e$topic %in% tlg_topics(), info = e$id)
    expect_true(nzchar(e$title) && nzchar(e$slug), info = e$id)
    expect_type(e$packages, "character")
  }
  topics <- vapply(catalog, `[[`, character(1), "topic")
  expect_setequal(unique(topics), tlg_topics())
})

test_that("every output's code parses and creates `result`", {
  for (e in catalog) {
    expect_no_error(parse(text = e$code), message = e$id)
    expect_match(e$code, "(^|\n)\\s*result <-", info = e$id)
  }
})

test_that("outputs only use app datasets or declared example datasets", {
  for (e in catalog) {
    expect_true(all(e$data %in% app_datasets), info = e$id)
    used_examples <- regmatches(e$code, gregexpr('tlg_example_data\\("[A-Z]+"', e$code))[[1]]
    used_examples <- gsub('tlg_example_data\\("|"', "", used_examples)
    expect_setequal(used_examples, e$extra)
  }
})

test_that("catalog links point to the stable TLG Catalog", {
  expect_identical(
    tlg_catalog_url(catalog$AET01),
    "https://insightsengineering.github.io/tlg-catalog/stable/tables/adverse-events/aet01.html"
  )
  expect_match(tlg_catalog_url(catalog$KMG01), "/graphs/efficacy/kmg01.html$")
  expect_match(tlg_catalog_url(catalog$AEL01), "/listings/adverse-events/ael01.html$")
})

test_that("the page lists every output once, grouped by topic, tables first in each topic", {
  entries <- tlg_catalog_entries(catalog)
  expect_setequal(names(entries), names(catalog))
  expect_identical(unique(vapply(entries, `[[`, character(1), "topic")), tlg_topics())
  for (topic in tlg_topics()) {
    types <- vapply(Filter(function(e) identical(e$topic, topic), entries), `[[`, character(1), "type")
    expect_false(is.unsorted(match(types, c("Table", "Listing", "Graph"))), info = topic)
  }
  rendered <- htmltools::renderTags(ui_tlg_catalog("page", entries))
  html <- rendered$html
  n_items <- lengths(regmatches(html, gregexpr("<button", html, fixed = TRUE)))
  expect_identical(n_items, length(entries))
  n_groups <- lengths(regmatches(html, gregexpr('class="tn-tlg-group"', html, fixed = TRUE)))
  expect_identical(n_groups, length(tlg_topics()))
  for (topic in tlg_topics()) expect_match(html, sprintf('data-topic="%s"', htmltools::htmlEscape(topic, TRUE)), fixed = TRUE, info = topic)
  expect_match(html, 'data-input-id="page-tlg"', fixed = TRUE)
  expect_match(html, "TLG Catalog", fixed = TRUE)
  # the first output is selected
  expect_match(html, sprintf('class="tn-tlg-item active"[^>]*data-value="%s"', entries[[1]]$id))
  expect_true("blockr-tlg" %in% vapply(rendered$dependencies, function(d) d$name, character(1)))
})

test_that("the TLG Catalog is one module (a single menu entry), using app datasets only", {
  m <- module_tlg_catalog(datanames = app_datasets, catalog = catalog)
  expect_s3_class(m, "teal_module")
  expect_identical(m$label, "TLG Catalog")
  expect_true(all(m$datanames %in% app_datasets))
  expect_true("ADSL" %in% m$datanames)
})

test_that("output kinds are recognised", {
  expect_identical(tlg_kind(data.frame(a = 1)), "data")
  expect_identical(tlg_kind("x"), "text")
  skip_if_not_installed("ggplot2")
  expect_identical(tlg_kind(ggplot2::ggplot()), "plot")
})

test_that("tlg_example_data() keeps the patients of adsl", {
  skip_if_not_installed("random.cdisc.data")
  adsl <- head(random.cdisc.data::cadsl, 25)
  admh <- tlg_example_data("ADMH", adsl)
  expect_gt(nrow(admh), 0)
  expect_true(all(admh$USUBJID %in% adsl$USUBJID))
  # no overlap (e.g. other study data): the full example is returned
  other <- data.frame(USUBJID = "NOT-A-SUBJECT")
  expect_identical(nrow(tlg_example_data("ADMH", other)), nrow(random.cdisc.data::cadmh))
  expect_error(tlg_example_data("ADXX"), "no dataset")
})

test_that("a catalog page serves the selected output and its code", {
  local_blockr_options()
  data <- suppressMessages(build_blockr_data(cache_global = FALSE))
  entries <- tlg_catalog_entries(catalog)
  shiny::testServer(srv_tlg_catalog, args = list(data = shiny::reactive(data), entries = entries), {
    expect_identical(selected(), entries[[1]]$id)
    session$setInputs(tlg = "DMT01")
    expect_identical(selected(), "DMT01")
    expect_true(result()$ok)
    expect_identical(result()$kind, "table")
    expect_match(output$output$html, "<table", fixed = TRUE)
    expect_match(output$header$html, "Show R code", fixed = TRUE)
    # every output can be opened on the canvas
    expect_match(output$header$html, 'data-bk-open="DMT01"', fixed = TRUE)
    expect_match(output$header$html, "Open in Canvas", fixed = TRUE)
  })
})

test_that("every catalog output renders on the bundled data", {
  skip_on_cran()
  skip_if(identical(Sys.getenv("BLOCKR_SKIP_TLG"), "true"), "BLOCKR_SKIP_TLG=true")
  local_blockr_options()
  data <- suppressMessages(build_blockr_data(cache_global = FALSE))
  expected <- list(Table = c("table", "data"), Listing = "listing", Graph = "plot")

  failures <- character(0)
  skipped <- character(0)
  for (e in catalog) {
    res <- tlg_run(e, data)
    if (identical(res$kind, "missing")) {
      skipped <- c(skipped, e$id)
    } else if (!isTRUE(res$ok)) {
      failures <- c(failures, sprintf("%s: %s", e$id, res$error))
    } else if (!res$kind %in% expected[[e$type]]) {
      failures <- c(failures, sprintf("%s: produced a %s, expected a %s", e$id, res$kind, e$type))
    }
  }
  if (length(skipped)) message("Outputs skipped (package not installed): ", toString(skipped))
  expect(
    length(failures) == 0,
    sprintf("%d of %d output(s) failed:\n%s", length(failures), length(catalog), paste(failures, collapse = "\n"))
  )
})
