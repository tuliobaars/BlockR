# The canvas as SAS code (R/blocks_sas.R): a translation, not a compiler.

catalog <- tlg_catalog()
sas_of <- function(code) blockr_sas(blockr_graph_from_code(code))
sas_lines <- function(x) strsplit(x, "\n", fixed = TRUE)[[1]]

test_that("R expressions become SAS expressions", {
  e <- function(x) sas_expr(str2lang(x))
  expect_identical(e('AESER == "Y" & AGE >= 65'), 'AESER = "Y" and AGE >= 65')
  expect_identical(e('SEX != "F" | is.na(AGE)'), 'SEX ne "F" or missing(AGE)')
  expect_identical(e("!is.na(AENDY)"), "not missing(AENDY)")
  expect_identical(e('ARM %in% c("A: Drug X", "B: Placebo")'), 'ARM in ("A: Drug X" "B: Placebo")')
  expect_identical(e('!(ARM %in% c("A", "B"))'), 'not ((ARM in ("A" "B")))')
  expect_identical(e('!AEDECOD %in% c("X")'), 'AEDECOD not in ("X")')
  expect_identical(e('ifelse(AESER == "Y", "Yes", "No")'), 'ifc(AESER = "Y", "Yes", "No")')
  expect_identical(e("ifelse(AGE > 65, 1, 0)"), "ifn(AGE > 65, 1, 0)")
  expect_identical(e('paste(SITEID, SUBJID, sep = "/")'), 'catx("/", SITEID, SUBJID)')
  expect_identical(e("paste0(A, B)"), "cats(A, B)")
  expect_identical(e("substr(x, 1, 3)"), "substr(x, 1, 3)")
  expect_identical(e("round(AVAL, 1)"), "round(AVAL, 0.1)")
  expect_identical(e("as.Date(TRTSDTM)"), "datepart(TRTSDTM)")
  expect_identical(e("`Age group`"), '"Age group"n')
  expect_identical(sas_expr(str2lang("mean(AVAL)"), agg = TRUE), "mean(AVAL)")
  expect_identical(sas_expr(str2lang("n()"), agg = TRUE), "count(*)")
  expect_identical(sas_expr(str2lang("n_distinct(USUBJID)"), agg = TRUE), "count(distinct USUBJID)")
  # no translation: NULL (the step becomes a TODO)
  expect_null(e("mean(AVAL)"))
  expect_null(e("some_function(x)"))
})

test_that("new columns: conditions, case_when and cut", {
  expect_identical(sas_assign("SER", str2lang('AESER == "Y"')), '  SER = (AESER = "Y");')
  cw <- sas_assign("GRP", str2lang('case_when(AGE < 18 ~ "child", AGE < 65 ~ "adult", TRUE ~ "senior")'))
  expect_identical(cw, c(
    "  select;",
    '    when (AGE < 18) GRP = "child";',
    '    when (AGE < 65) GRP = "adult";',
    '    otherwise GRP = "senior";',
    "  end;"
  ))
  ct <- sas_assign("AGEGR", str2lang('cut(AGE, breaks = c(0, 18, 65, Inf), right = FALSE, labels = c("<18", "18-64", ">=65"))'))
  expect_identical(ct[2], '    when (not missing(AGE) and 0 <= AGE and AGE < 18) AGEGR = "<18";')
  expect_identical(ct[4], '    when (not missing(AGE) and 65 <= AGE) AGEGR = ">=65";')
})

test_that("data steps: filter, mutate, select, order, count, joins", {
  s <- sas_of('
adsl <- ADSL %>% filter(SAFFL == "Y", AGE >= 65 | SEX == "F") %>% mutate(OLD = AGE >= 65) %>% select(USUBJID, ARM, OLD)
adae <- ADAE %>% filter(AESER == "Y") %>% select(-AETERM) %>% arrange(USUBJID, desc(ASTDY))
n_arm <- adsl %>% count(ARM, name = "N")
anl <- adae %>% left_join(adsl, by = "USUBJID") %>% left_join(count(adsl, ARM, name = "N_ARM"), by = "ARM")
')
  l <- sas_lines(s)
  expect_true('libname adam "path/to/adam";  /* TODO: the folder of the ADaM datasets */' %in% l)
  expect_true("  set adam.adsl;" %in% l)
  expect_true('  if SAFFL = "Y" and (AGE >= 65 or SEX = "F");' %in% l)
  expect_true('  OLD = (AGE >= 65);' %in% l)
  expect_true("data work.adsl(keep=USUBJID ARM OLD);" %in% l)
  expect_true("data work.adae(drop=AETERM);" %in% l)
  expect_true("  by USUBJID descending ASTDY;" %in% l)
  expect_true("  select ARM, count(*) as N" %in% l)
  expect_true("  from work.adae as a left join work.adsl as b on a.USUBJID = b.USUBJID;" %in% l)
  # count() written inside the join: a table of its own
  expect_true(any(grepl("^  create table work._count[0-9]+ as$", l)))
  expect_true("  select ARM, count(*) as N_ARM" %in% l)
})

test_that("tables: columns, the total column, row groups and their counts", {
  s <- blockr_sas(blockr_template_graph(catalog$AET02))
  l <- sas_lines(s)
  expect_true('  ACTARM = "All Patients";' %in% l)
  expect_true("  from work._result_n_src group by ACTARM;" %in% l)
  expect_true("  group by a.AEBODSYS, a.AEDECOD, a.ACTARM, b.N;" %in% l)
  expect_true("  select a.ACTARM, count(distinct a.USUBJID) as patients, calculated patients / b.N * 100 as pct_patients format=5.1, count(*) as records" %in% l)
  expect_true(any(grepl("^proc transpose", l)))
  # never an empty column list
  expect_false(any(grepl("a\\.,|select ,|by ,|, ,", l)))
  # DMT01: numbers with PROC MEANS, categories with counts
  g <- blockr_template_graph(catalog$DMT01)
  bt <- Filter(function(n) identical(n$fn, "build_table"), g$nodes)[[1]]
  df_id <- Filter(function(a) identical(a$name, "df"), bt$args)[[1]]$ref
  status <- stats::setNames(list(list(schema = list(list(n = "AGE", t = "num"), list(n = "SEX", t = "fct")))), df_id)
  d <- sas_lines(blockr_sas(g, status = status))
  expect_true(any(grepl("^proc means data=work._result_rows", d)))
  expect_true("  var AGE;" %in% d)
  expect_true("/* SEX: counts and percentages */" %in% d)
  # without the run's column types: counts for every variable
  expect_false(any(grepl("^proc means", sas_lines(blockr_sas(g)))))
  expect_false(any(grepl("a\\.,|select ,|by ,", d)))
  # LBT01: statistics of two variables side by side
  b <- sas_lines(blockr_sas(blockr_template_graph(catalog$LBT01)))
  expect_true("  class PARAM AVISIT ACTARM;" %in% b)
  expect_true("  var AVAL CHG;" %in% b)
})

test_that("listings, plots and survival curves", {
  l <- sas_lines(blockr_sas(blockr_template_graph(catalog$AEL02)))
  expect_true(any(grepl("^proc report data=work.out", l)))
  expect_true("  define ACTARM / order;" %in% l)
  k <- sas_lines(blockr_sas(blockr_template_graph(catalog$KMG01)))
  expect_true("proc lifetest data=work.anl plots=survival(atrisk);" %in% k)
  expect_true("  strata ARM;" %in% k)
  b <- sas_lines(blockr_sas(blockr_template_graph(catalog$BRG01)))
  expect_true("proc sgplot data=work.anl;" %in% b)
  expect_true(any(grepl("^  hbar AEBODSYS / response=pct group=ACTARM", b)))
  # n and N are one name in SAS: said so
  expect_true(any(grepl("SAS names ignore case, so n and N", b, fixed = TRUE)))
})

test_that("what has no SAS equivalent is a TODO with its R code, never dropped", {
  s <- sas_of('x <- ADSL %>% filter(SAFFL == "Y") %>% my_step(AGE) %>% mutate(B = strange(AGE))')
  expect_match(s, "/* TODO: my_step() has no SAS translation", fixed = TRUE)
  expect_match(s, "R: my_step(AGE)", fixed = TRUE)
  expect_match(s, "/* TODO: a new column without SAS translation", fixed = TRUE)
  code <- sas_of("set.seed(1)\nx <- ADSL %>% head(5)")
  expect_match(code, "R: set.seed(1)", fixed = TRUE)
  expect_match(code, "set adam.adsl(obs=5);", fixed = TRUE)
})

test_that("every template translates without failing", {
  for (id in names(catalog)) {
    s <- blockr_sas(blockr_template_graph(catalog[[id]]))
    expect_type(s, "character")
    expect_no_match(s, "could not be translated", fixed = TRUE, info = id)
  }
})
