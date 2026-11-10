test_that("minimal contract-compliant data passes validation", {
  expect_no_error(validate_adam(minimal_raw()))
  expect_invisible(validate_adam(minimal_raw()))
})

test_that("a missing dataset is reported by name", {
  raw <- minimal_raw()
  raw$ADTTE <- NULL
  expect_error(validate_adam(raw), "missing datasets: ADTTE")
})

test_that("missing columns are reported per dataset, all at once", {
  raw <- minimal_raw()
  raw$ADAE$AESER <- NULL
  raw$ADLB$AVALU <- NULL
  err <- expect_error(validate_adam(raw))
  expect_match(conditionMessage(err), "ADAE is missing columns: AESER")
  expect_match(conditionMessage(err), "ADLB is missing columns: AVALU")
})

test_that("empty datasets and non-data.frames are rejected", {
  raw <- minimal_raw()
  raw$ADCM <- raw$ADCM[0, ]
  raw$ADEG <- list(a = 1)
  err <- expect_error(validate_adam(raw))
  expect_match(conditionMessage(err), "ADCM has no rows")
  expect_match(conditionMessage(err), "ADEG is not a data.frame")
})

test_that("ADSL must have one row per subject", {
  raw <- minimal_raw()
  raw$ADSL <- rbind(raw$ADSL, raw$ADSL[1, ])
  expect_error(validate_adam(raw), "duplicated USUBJID")
})

test_that("subjects missing from ADSL give a warning, not an error", {
  raw <- minimal_raw()
  raw$ADAE$USUBJID[1] <- "NOT-IN-ADSL"
  expect_warning(validate_adam(raw), "ADAE \\(1\\)")
})

test_that("extra datasets and extra columns are allowed", {
  raw <- minimal_raw()
  raw$ADMH <- data.frame(STUDYID = "STUDY1", USUBJID = "S01")
  raw$ADSL$EXTRA <- 1
  expect_no_error(validate_adam(raw))
})

test_that("contract columns include the join keys for every dataset", {
  for (ds in names(adam_contract())) {
    expect_true(all(c("STUDYID", "USUBJID") %in% adam_contract()[[ds]]), info = ds)
  }
})

test_that("missing default-selection values are listed in one warning", {
  raw <- minimal_raw()
  expect_warning(missing <- check_app_defaults(raw), "default selections")
  expect_true(any(grepl("^ADTTE\\$PARAMCD: OS$", missing)))
  expect_true(any(grepl("^ADSL\\$ARMCD: ARM A, ARM B, ARM C$", missing)))
})

test_that("no warning when all default-selection values are present", {
  raw <- minimal_raw(n = 3)
  for (d in adam_app_defaults()) {
    vals <- d[[3]]
    raw[[d[[1]]]][[d[[2]]]] <- rep(vals, length.out = nrow(raw[[d[[1]]]]))
    if (length(vals) > nrow(raw[[d[[1]]]])) {
      extra <- raw[[d[[1]]]][rep(1, length(vals)), ]
      extra[[d[[2]]]] <- vals
      raw[[d[[1]]]] <- rbind(raw[[d[[1]]]], extra)
    }
  }
  expect_no_warning(missing <- check_app_defaults(raw))
  expect_length(missing, 0)
})
