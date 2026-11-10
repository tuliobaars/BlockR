code_depth <- function(code) lengths(strsplit(code, ".", fixed = TRUE))

test_that("dictionary names are unique within each hierarchy level (no merged AE groups)", {
  dict <- ae_dictionary()
  expect_false(anyDuplicated(names(dict)) > 0)
  for (depth in unique(code_depth(names(dict)))) {
    level <- dict[code_depth(names(dict)) == depth]
    expect_false(anyDuplicated(unname(level)) > 0, info = paste("depth", depth))
  }
})

test_that("every coded term has its parent levels in the dictionary", {
  codes <- names(ae_dictionary())
  for (code in codes[code_depth(codes) > 1]) {
    parent <- sub("\\.[0-9]+$", "", code)
    expect_true(parent %in% codes, info = code)
  }
})

test_that("whole-value codes are decoded at every level", {
  expect_identical(
    decode_ae_values(c("cl A", "cl B.2", "hlgt C.2.1", "hlt D.1.1.4", "dcd D.1.1.4.2", "llt A.1.1.1.1", "trm A.1.1.1.2")),
    c("Gastrointestinal disorders", "Vascular disorders", "Allergic conditions", "Headaches NEC",
      "Headache", "Diarrhoea", "Nausea")
  )
})

test_that("unknown codes, plain names and NA pass through unchanged", {
  expect_identical(
    decode_ae_values(c("dcd Z.9.9.9.9", "Headache", NA, "")),
    c("dcd Z.9.9.9.9", "Headache", NA, "")
  )
})

test_that("codes embedded in basket names are decoded without partial matches", {
  expect_identical(
    decode_ae_text(c("C.1.1.1.3/B.2.2.3.1 AESI", "Y.9.9.9.9 AESI", "A.1.1.1.12", NA)),
    c("Pneumonia/Hypotension AESI", "Y.9.9.9.9 AESI", "A.1.1.1.12", NA)
  )
})

test_that("recoding keeps factor levels order, NA and column labels", {
  f <- factor(c("dcd A.1.1.1.2", "dcd A.1.1.1.1", NA), levels = c("dcd A.1.1.1.2", "dcd A.1.1.1.1"))
  attr(f, "label") <- "Dictionary-Derived Term"
  out <- recode_column(f, decode_ae_values)
  expect_s3_class(out, "factor")
  expect_identical(levels(out), c("Nausea", "Diarrhoea"))
  expect_identical(as.character(out), c("Nausea", "Diarrhoea", NA))
  expect_identical(attr(out, "label"), "Dictionary-Derived Term")

  ch <- c("cl A", "cl B", "cl A")
  attr(ch, "label") <- "SOC"
  out <- recode_column(ch, decode_ae_values)
  expect_identical(as.vector(out), c("Gastrointestinal disorders", "Cardiovascular disorders", "Gastrointestinal disorders"))
  expect_identical(attr(out, "label"), "SOC")

  expect_identical(recode_column(1:3, decode_ae_values), 1:3)
})

test_that("decode_ae_terms recodes all AE columns and warns about unknown codes", {
  adae <- data.frame(
    AETERM = c("trm A.1.1.1.1", "trm Q.1.1.1.1"),
    AEDECOD = factor(c("dcd A.1.1.1.1", "dcd Q.1.1.1.1")),
    AEBODSYS = c("cl A.1", "cl Q.1"),
    SMQ01NAM = c("C.1.1.1.3/B.2.2.3.1 AESI", NA),
    OTHER = c("dcd A.1.1.1.1", "x"),
    stringsAsFactors = FALSE
  )
  expect_warning(out <- decode_ae_terms(adae), "dcd Q.1.1.1.1")
  expect_identical(out$AETERM, c("Diarrhoea", "trm Q.1.1.1.1"))
  expect_identical(as.character(out$AEDECOD), c("Diarrhoea", "dcd Q.1.1.1.1"))
  expect_identical(out$SMQ01NAM, c("Pneumonia/Hypotension AESI", NA))
  expect_identical(out$OTHER, adae$OTHER) # only AE term columns are touched
  expect_identical(dim(out), dim(adae))
})

test_that("decode_ae_terms is silent when every code is known", {
  adae <- data.frame(AEDECOD = c("dcd B.1.1.1.1", "dcd D.2.1.5.3"), AESOC = c("cl B", "cl D"))
  expect_no_warning(out <- decode_ae_terms(adae))
  expect_identical(out$AEDECOD, c("Myocardial infarction", "Fatigue"))
})

test_that("decoding is idempotent (already-decoded data is left alone)", {
  adae <- data.frame(AEDECOD = c("dcd A.1.1.1.1", "dcd C.1.1.1.3"))
  once <- decode_ae_terms(adae)
  expect_identical(decode_ae_terms(once), once)
})
