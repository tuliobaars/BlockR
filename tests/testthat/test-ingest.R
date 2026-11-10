# ---- Every data source produces contract-compliant raw data -----------------

for (sc in blockr_scenarios()) {
  test_that(sprintf("[%s] scenario builds without error", sc), {
    if (sc == "parquet") skip_if_not_installed("arrow")
    err <- scenario_error(sc)
    expect_null(err, info = if (!is.null(err)) conditionMessage(err))
  })

  test_that(sprintf("[%s] raw data has every contract dataset and column", sc), {
    raw <- scenario(sc)$raw
    expect_true(all(names(adam_contract()) %in% names(raw)))
    expect_no_error(validate_adam(raw))
    for (ds in names(adam_contract())) {
      expect_s3_class(raw[[ds]], "data.frame")
      expect_gt(nrow(raw[[ds]]), 0)
    }
  })

  test_that(sprintf("[%s] ingestion raised no contract or default-value warnings", sc), {
    expect_identical(scenario(sc)$warnings[grepl("Subjects without|default selections", scenario(sc)$warnings)], character(0))
  })

  test_that(sprintf("[%s] app default selections exist in the data", sc), {
    missing <- suppressWarnings(check_app_defaults(scenario(sc)$raw))
    expect_identical(as.character(missing), character(0))
  })
}

# ---- Source selection and settings --------------------------------------------

test_that("unknown data source is rejected", {
  expect_error(blockr_source_config("excel"), "should be one of")
})

test_that("synthetic settings are completed with defaults and validated", {
  s <- synthetic_settings(list(N = 50))
  expect_identical(s$N, 50L)
  expect_identical(s$seed, 2025L)
  expect_identical(s$args, list())
  expect_error(synthetic_settings(list(N = 0)), "positive whole number")
  expect_error(synthetic_settings(list(N = 2.5)), "positive whole number")
  expect_error(synthetic_settings(list(seed = "a")), "single number")
  expect_error(synthetic_settings(list(args = list(ADXX = list()))), "Unknown: ADXX")
})

test_that("an NA seed draws a random seed once and stores it for the session", {
  withr::local_options(blockr.synthetic = list(N = 10, seed = NA))
  expect_message(s1 <- synthetic_settings(getOption("blockr.synthetic")), "Random synthetic-data seed")
  expect_false(is.na(s1$seed))
  expect_identical(getOption("blockr.synthetic")$seed, s1$seed)
  s2 <- synthetic_settings(getOption("blockr.synthetic"))
  expect_identical(s2$seed, s1$seed)
})

test_that("synthetic data honours N and is reproducible for a given seed", {
  a <- scenario("synthetic_small")$raw
  expect_identical(nrow(a$ADSL), 40L)
  b <- suppressMessages(suppressWarnings(ingest_synthetic(synthetic_settings(list(N = 40, seed = 1)))))
  expect_identical(a$ADSL, b$ADSL)
  expect_identical(a$ADAE, b$ADAE)
})

test_that("different seeds give different synthetic data", {
  a <- scenario("synthetic_small")$raw$ADSL
  b <- suppressMessages(suppressWarnings(ingest_synthetic(synthetic_settings(list(N = 40, seed = 2)))))$ADSL
  expect_false(identical(a$AGE, b$AGE))
})

test_that("per-dataset arguments reach the generator", {
  s <- synthetic_settings(list(N = 20, seed = 3, args = list(ADAE = list(max_n_aes = 1L))))
  raw <- suppressMessages(suppressWarnings(ingest_synthetic(s)))
  expect_lte(max(table(raw$ADAE$USUBJID)), 1L)
})

# ---- Parquet folder ------------------------------------------------------------

test_that("dataset names are derived from parquet file names", {
  expect_identical(
    adam_name_from_file(c("adsl.parquet", "ADAE.parquet", "adlb_adam.parquet", "x/y/adtte.PARQUET", "adam.parquet")),
    c("ADSL", "ADAE", "ADLB", "ADTTE", "")
  )
})

test_that("only ADaM parquet files are picked up from the folder", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("adsl.parquet", "adae_adam.parquet", "notes.txt", "summary.parquet", "adam.parquet", "blockr_adam.rds")))
  files <- adam_parquet_files(dir)
  expect_setequal(names(files), c("ADSL", "ADAE"))
})

test_that("parquet folder problems give clear errors", {
  expect_error(adam_parquet_files(file.path(tempdir(), "does-not-exist")), "folder not found")
  empty <- withr::local_tempdir()
  expect_error(adam_parquet_files(empty), "No ADaM parquet files")
  dup <- withr::local_tempdir()
  file.create(file.path(dup, c("adsl.parquet", "adsl_adam.parquet")))
  expect_error(adam_parquet_files(dup), "More than one parquet file for: ADSL")
})

test_that("parquet round trip preserves the data", {
  skip_if_not_installed("arrow")
  raw <- ingest_bundled()
  dir <- withr::local_tempdir()
  export_adam_parquet(raw, dir)
  expect_error(export_adam_parquet(raw, dir), "already exist")
  back <- ingest_parquet(dir)
  expect_setequal(names(back), names(raw))
  for (ds in names(raw)) {
    expect_identical(dim(back[[ds]]), dim(raw[[ds]]), info = ds)
    expect_identical(names(back[[ds]]), names(raw[[ds]]), info = ds)
    expect_equal(as.data.frame(back[[ds]]), as.data.frame(raw[[ds]]), ignore_attr = TRUE, info = ds)
  }
})

test_that("a parquet folder missing a required dataset fails the contract", {
  skip_if_not_installed("arrow")
  raw <- ingest_bundled()
  dir <- withr::local_tempdir()
  export_adam_parquet(raw[setdiff(names(raw), "ADVS")], dir)
  expect_error(suppressMessages(ingest_adam("parquet", parquet_dir = dir)), "missing datasets: ADVS")
})

test_that("extra parquet datasets are reported and ignored", {
  skip_if_not_installed("arrow")
  raw <- ingest_bundled()
  dir <- withr::local_tempdir()
  export_adam_parquet(c(raw, list(ADXTRA = raw$ADSL[1:5, c("STUDYID", "USUBJID")])), dir)
  expect_message(out <- ingest_adam("parquet", parquet_dir = dir), "not used by the app: ADXTRA")
  expect_true("ADXTRA" %in% names(out))
})

test_that("a missing arrow package gives install instructions", {
  expect_error(require_arrow(available = FALSE), "renv::install")
  expect_no_error(require_arrow(available = TRUE))
})
