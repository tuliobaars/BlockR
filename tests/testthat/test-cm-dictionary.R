# Concomitant medications: placeholder codes -> real names.

test_that("every medication has a distinct name, a reported name and a class", {
  d <- cm_dictionary()
  expect_length(d$medication, 9)
  expect_false(anyDuplicated(d$medication) > 0)
  expect_setequal(sub("^medname ", "", names(d$medication)), names(d$reported))
  expect_setequal(names(d$class), c("medcl A", "medcl B", "medcl C"))
  for (k in 1:4) {
    expect_length(d$atc[[k]], 8)
    expect_identical(names(d$atc_code[[k]]), names(d$atc[[k]]))
    # WHO ATC codes: 1, 3, 4 and 5 characters
    expect_true(all(nchar(d$atc_code[[k]]) == c(1, 3, 4, 5)[k]))
  }
  # the chemical subgroups (level 4) of the paths are distinct
  expect_false(anyDuplicated(d$atc_code$ATC4CD) > 0)
  expect_false(any(grepl("medname|medcl|ATCCLAS|_[0-9]/", unlist(d))))
})

test_that("decoding keeps rows, labels and factor-ness; unknown codes are kept with a warning", {
  cm <- data.frame(
    CMDECOD = factor(c("medname B_1/4", "medname C_2/2", "PARACETAMOL")),
    CMTRT = c("B_1/4", "C_2/2", "TYLENOL"),
    CMCLAS = c("medcl B", "medcl C", "medcl A"),
    ATC2 = c("ATCCLAS2 B p2", "ATCCLAS2 C p3", "ATCCLAS2 A"),
    ATC2CD = c("ATCCLAS2 B p2", "ATCCLAS2 C p3", "ATCCLAS2 A"),
    stringsAsFactors = FALSE
  )
  attr(cm$CMDECOD, "label") <- "Standardized Medication Name"
  out <- decode_cm_terms(cm)
  expect_identical(nrow(out), 3L)
  expect_s3_class(out$CMDECOD, "factor")
  expect_identical(attr(out$CMDECOD, "label"), "Standardized Medication Name")
  expect_identical(as.character(out$CMDECOD), c("ATORVASTATIN", "AMPICILLIN", "PARACETAMOL"))
  expect_identical(out$CMTRT, c("LIPITOR", "AMPICILLIN 500MG", "TYLENOL"))
  expect_identical(out$CMCLAS, c("LIPID MODIFYING AGENTS", "ANTIBACTERIALS FOR SYSTEMIC USE", "ANTIINFLAMMATORY AGENTS (NSAIDS)"))
  expect_identical(out$ATC2, c("LIPID MODIFYING AGENTS", "OPHTHALMOLOGICALS", "ANTIINFLAMMATORY AND ANTIRHEUMATIC PRODUCTS"))
  expect_identical(out$ATC2CD, c("C10", "S01", "M01"))
  expect_warning(decode_cm_terms(data.frame(CMDECOD = "medname D_1/1")), "medname D_1/1")
})

for (sc in blockr_scenarios()) {
  test_that(sprintf("[%s] ADCM has real medication names, and as many medications as before", sc), {
    raw <- scenario(sc)$raw$ADCM
    cm <- scenario(sc)$processed$datasets$ADCM
    expect_identical(nrow(cm), nrow(raw))
    for (col in intersect(c("CMDECOD", "CMTRT", "CMCLAS", "ATC1", "ATC2", "ATC3", "ATC4"), names(cm))) {
      expect_false(any(grepl("medname|medcl|ATCCLAS|^[A-Z]_[0-9]+/[0-9]+$", as.character(cm[[col]]))), info = col)
    }
    expect_identical(length(unique(cm$CMDECOD)), length(unique(raw$CMDECOD)))
  })
}
