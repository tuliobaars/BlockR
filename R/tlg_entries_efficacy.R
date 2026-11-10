# TLG Catalog entries: Efficacy (tables, listing, graphs) and Other graphs.
# Efficacy code uses the intent-to-treat population and the planned arm (ARM),
# with placebo ("B: Placebo") as the reference arm. Time-to-event values are
# converted from days to months, as in the catalog.

#' @noRd
tlg_entries_efficacy <- function() {
  topic <- "Efficacy"
  slug <- "efficacy"

  tte_summary <- function(paramcd, label) {
    sprintf(r"--(
library(tern)

adsl <- ADSL %%>%% filter(ITTFL == "Y") %%>%% df_explicit_na()
adtte <- ADTTE %%>%%
  filter(ITTFL == "Y", PARAMCD == "%s") %%>%%
  mutate(
    AVAL = day2month(AVAL),
    is_event = CNSR == 0,
    is_not_event = CNSR == 1
  ) %%>%%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %%>%%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %%>%%
  analyze_vars(
    "is_event", .stats = "count_fraction",
    .labels = c(count_fraction = "Patients with event (%%)"),
    show_labels = "hidden", table_names = "events"
  ) %%>%%
  analyze_vars(
    "is_not_event", .stats = "count_fraction",
    .labels = c(count_fraction = "Patients without event (%%)"),
    show_labels = "hidden", table_names = "non_events"
  ) %%>%%
  surv_time(vars = "AVAL", var_labels = "%s (Months)", is_event = "is_event", table_names = "time_to_event") %%>%%
  coxph_pairwise(
    vars = "AVAL", is_event = "is_event", var_labels = "Unstratified Analysis",
    control = control_coxph(pval_method = "log-rank"), table_names = "coxph_unstratified"
  ) %%>%%
  coxph_pairwise(
    vars = "AVAL", is_event = "is_event", var_labels = "Stratified Analysis",
    strata = c("STRATA1", "STRATA2"),
    control = control_coxph(pval_method = "log-rank"), table_names = "coxph_stratified"
  ) %%>%%
  surv_timepoint(
    vars = "AVAL", var_labels = "Months", time_point = 12, is_event = "is_event",
    method = "both", control = control_surv_timepoint()
  )

result <- build_table(lyt, df = adtte, alt_counts_df = adsl)
)--", paramcd, label)
  }

  ancova_data <- r"--(
adsl <- ADSL %>% filter(ITTFL == "Y") %>% df_explicit_na()
adqs <- ADQS %>%
  filter(ITTFL == "Y") %>%
  mutate(ARMCD = factor(as.character(ARMCD), levels = c("ARM B", "ARM A", "ARM C"))) %>%
  df_explicit_na()
)--"

  mmrm_fit <- r"--(
adsl <- ADSL %>%
  filter(ITTFL == "Y") %>%
  mutate(ARMCD = factor(as.character(ARMCD), levels = c("ARM B", "ARM A", "ARM C"))) %>%
  df_explicit_na()
adqs <- ADQS %>%
  filter(ITTFL == "Y", PARAMCD == "FKSI-FWB") %>%
  mutate(
    ARMCD = factor(as.character(ARMCD), levels = c("ARM B", "ARM A", "ARM C")),
    AVISIT = droplevels(factor(AVISIT)),
    USUBJID = factor(USUBJID)
  ) %>%
  df_explicit_na()

mmrm_results <- fit_mmrm(
  vars = list(
    response = "AVAL",
    covariates = c("STRATA1", "BMRKR2"),
    id = "USUBJID",
    arm = "ARMCD",
    visit = "AVISIT"
  ),
  data = adqs,
  cor_struct = "unstructured",
  weights_emmeans = "equal"
)
)--"

  rsp_data <- r"--(
adsl <- ADSL %>% filter(ITTFL == "Y") %>% df_explicit_na()
adrs <- ADRS %>%
  filter(ITTFL == "Y", PARAMCD == "BESRSPI") %>%
  mutate(
    is_rsp = AVALC %in% c("CR", "PR"),
    AVALC = factor(as.character(AVALC), levels = c("CR", "PR", "SD", "PD", "NE"))
  ) %>%
  df_explicit_na()
)--"

  list(
    tlg("AOVT01", "ANCOVA for Multiple End Points", "Table", topic, slug, data = c("ADSL", "ADQS"), code = paste0(
      "library(tern)\n", ancova_data, r"--(
adqs_f <- adqs %>% filter(AVISIT == "WEEK 1 DAY 8", PARAMCD %in% c("FKSI-FWB", "FKSI-TSE", "FATIGI"))

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARMCD", ref_group = "ARM B") %>%
  split_rows_by("PARAMCD", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %>%
  summarize_ancova(
    vars = "CHG",
    variables = list(arm = "ARMCD", covariates = NULL),
    conf_level = 0.95,
    var_labels = "Unadjusted comparison",
    .labels = c(lsmean = "Mean", lsmean_diff = "Difference in Means"),
    table_names = "unadjusted"
  ) %>%
  summarize_ancova(
    vars = "CHG",
    variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
    conf_level = 0.95,
    var_labels = "Adjusted comparison (covariates BASE and STRATA1)",
    table_names = "adjusted"
  )

result <- build_table(lyt, df = adqs_f, alt_counts_df = adsl)
)--")),

    tlg("AOVT02", "ANCOVA with Single End Point and Customized Table", "Table", topic, slug, data = c("ADSL", "ADQS"), code = paste0(
      "library(tern)\n", ancova_data, r"--(
adqs_f <- adqs %>% filter(AVISIT == "WEEK 1 DAY 8", PARAMCD == "FKSI-FWB")

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARMCD", ref_group = "ARM B") %>%
  summarize_ancova(
    vars = "CHG",
    variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
    conf_level = 0.95,
    var_labels = "FKSI-FWB: change from baseline at week 1",
    .stats = c("n", "lsmean", "lsmean_diff", "lsmean_diff_ci", "pval"),
    .labels = c(
      n = "Number of patients",
      lsmean = "Adjusted mean",
      lsmean_diff = "Difference in adjusted means (vs. ARM B)",
      lsmean_diff_ci = "95% CI",
      pval = "p-value"
    )
  )

result <- build_table(lyt, df = adqs_f, alt_counts_df = adsl)
)--")),

    tlg("AOVT03", "ANCOVA with Consideration of Interaction", "Table", topic, slug, data = c("ADSL", "ADQS"), code = paste0(
      "library(tern)\n", ancova_data, r"--(
adqs_f <- adqs %>% filter(AVISIT == "WEEK 1 DAY 8", PARAMCD == "FKSI-FWB")

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARMCD", ref_group = "ARM B") %>%
  summarize_ancova(
    vars = "CHG",
    variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1")),
    conf_level = 0.95,
    var_labels = "Without interaction",
    table_names = "main"
  ) %>%
  summarize_ancova(
    vars = "CHG",
    variables = list(arm = "ARMCD", covariates = c("BASE", "STRATA1", "ARMCD*BASE")),
    conf_level = 0.95,
    var_labels = "With treatment-by-baseline interaction",
    table_names = "interaction"
  )

result <- build_table(lyt, df = adqs_f, alt_counts_df = adsl)
)--")),

    tlg("CFBT01", "Change from Baseline by Visit Table", "Table", topic, slug, data = c("ADSL", "ADVS"), code = r"--(
library(tern)

adsl <- ADSL %>% filter(ITTFL == "Y") %>% df_explicit_na()
advs <- ADVS %>%
  filter(ITTFL == "Y", PARAMCD %in% c("DIABP", "SYSBP"), AVISIT != "SCREENING") %>%
  mutate(CHG = ifelse(AVISIT == "BASELINE", NA_real_, CHG)) %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %>%
  split_rows_by("AVISIT", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Analysis Visit") %>%
  split_cols_by_multivar(vars = c("AVAL", "CHG"), varlabels = c("Value at Visit", "Change from Baseline")) %>%
  summarize_colvars(.stats = c("n", "mean_sd", "median", "range")) %>%
  append_topleft("    Statistics")

result <- build_table(lyt, df = advs, alt_counts_df = adsl)
)--"),

    tlg("CMHT01", "Cochran-Mantel-Haenszel (CMH) Summary", "Table", topic, slug, data = c("ADSL", "ADRS"), code = paste0(
      "library(tern)\n", rsp_data, r"--(
lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  estimate_proportion(vars = "is_rsp", table_names = "est_prop") %>%
  estimate_proportion_diff(
    vars = "is_rsp",
    show_labels = "visible",
    var_labels = "Unstratified Analysis",
    table_names = "est_prop_diff"
  ) %>%
  estimate_proportion_diff(
    vars = "is_rsp",
    variables = list(strata = c("STRATA1", "STRATA2")),
    method = "cmh",
    show_labels = "visible",
    var_labels = "Stratified Analysis (CMH)",
    table_names = "est_prop_diff_strat"
  ) %>%
  test_proportion_diff(
    vars = "is_rsp",
    variables = list(strata = c("STRATA1", "STRATA2")),
    method = "cmh",
    table_names = "test_prop_diff_strat"
  ) %>%
  estimate_odds_ratio(
    vars = "is_rsp",
    variables = list(arm = "ARM", strata = c("STRATA1", "STRATA2")),
    table_names = "est_or_strat"
  )

result <- build_table(lyt, df = adrs, alt_counts_df = adsl)
)--")),

    tlg("COXT01", "Cox Regression", "Table", topic, slug, data = c("ADSL", "ADTTE"), code = r"--(
library(tern)

anl <- ADTTE %>%
  filter(ITTFL == "Y", PARAMCD == "OS", ARM %in% c("A: Drug X", "B: Placebo")) %>%
  mutate(
    AVAL = day2month(AVAL),
    EVENT = 1 - CNSR,
    ARM = factor(as.character(ARM), levels = c("B: Placebo", "A: Drug X")),
    SEX = droplevels(factor(SEX)),
    BMRKR2 = droplevels(factor(BMRKR2))
  )

variables <- list(
  time = "AVAL",
  event = "EVENT",
  arm = "ARM",
  covariates = c("SEX", "AGE", "BMRKR2")
)

lyt <- basic_table() %>%
  summarize_coxreg(variables = variables)

result <- build_table(lyt, df = anl)
)--", note = "Treatment effect (A: Drug X vs. B: Placebo) on overall survival, adjusted for each covariate separately."),

    tlg("COXT02", "Multi-Variable Cox Regression", "Table", topic, slug, data = c("ADSL", "ADTTE"), code = r"--(
library(tern)

anl <- ADTTE %>%
  filter(ITTFL == "Y", PARAMCD == "OS", ARM %in% c("A: Drug X", "B: Placebo")) %>%
  mutate(
    AVAL = day2month(AVAL),
    EVENT = 1 - CNSR,
    ARM = factor(as.character(ARM), levels = c("B: Placebo", "A: Drug X")),
    SEX = droplevels(factor(SEX)),
    BMRKR2 = droplevels(factor(BMRKR2))
  )

variables <- list(
  time = "AVAL",
  event = "EVENT",
  arm = "ARM",
  covariates = c("SEX", "AGE", "BMRKR2")
)

lyt <- basic_table() %>%
  summarize_coxreg(
    variables = variables,
    multivar = TRUE,
    control = control_coxreg(conf_level = 0.95)
  )

result <- build_table(lyt, df = anl)
)--"),

    tlg("DORT01", "Duration of Response", "Table", topic, slug, data = c("ADSL", "ADTTE"),
        code = tte_summary("CRSD", "Duration of Confirmed Response"),
        note = "Patients with a confirmed response (parameter CRSD)."),

    tlg("LGRT02", "Multi-Variable Logistic Regression", "Table", topic, slug, data = c("ADSL", "ADRS"), code = r"--(
library(tern)

adrs <- ADRS %>%
  filter(ITTFL == "Y", PARAMCD == "BESRSPI", ARMCD %in% c("ARM A", "ARM B")) %>%
  mutate(
    Response = as.integer(AVALC %in% c("CR", "PR")),
    ARMCD = factor(as.character(ARMCD), levels = c("ARM B", "ARM A")),
    SEX = droplevels(factor(SEX))
  ) %>%
  var_relabel(ARMCD = "Planned Arm Code", SEX = "Sex", AGE = "Age")

model <- fit_logistic(
  adrs,
  variables = list(response = "Response", arm = "ARMCD", covariates = c("AGE", "SEX"))
)

# empty strings (e.g. no interaction) become "_NA_", rows that summarize_logistic() drops
df <- broom::tidy(model, conf_level = 0.95) %>% df_explicit_na(na_level = "_NA_")

result <- basic_table() %>%
  summarize_logistic(conf_level = 0.95, drop_and_remove_str = "_NA_") %>%
  build_table(df = df)
)--"),

    tlg("MMRMT01", "Tables for Mixed-Effect Model Repeated Measures Analysis", "Table", topic, slug,
        data = c("ADSL", "ADQS"), packages = c("tern", "tern.mmrm"), code = paste0(
      "library(tern)\nlibrary(tern.mmrm)\n", mmrm_fit, r"--(
df <- broom::tidy(mmrm_results)

result <- basic_table() %>%
  split_cols_by("ARMCD", ref_group = mmrm_results$ref_level) %>%
  add_colcounts() %>%
  split_rows_by("AVISIT") %>%
  summarize_lsmeans(show_relative = "increase") %>%
  append_topleft("  Population Mean") %>%
  build_table(df, alt_counts_df = adsl)
)--")),

    tlg("ONCT05", "Objective Response Rate by Subgroup", "Table", topic, slug, data = c("ADSL", "ADRS"), code = r"--(
library(tern)

anl <- ADRS %>%
  filter(ITTFL == "Y", PARAMCD == "BESRSPI", ARM %in% c("A: Drug X", "B: Placebo")) %>%
  mutate(
    is_rsp = AVALC %in% c("CR", "PR"),
    ARM = factor(as.character(ARM), levels = c("B: Placebo", "A: Drug X")),
    SEX = droplevels(factor(SEX)),
    BMRKR2 = droplevels(factor(BMRKR2)),
    STRATA1 = droplevels(factor(STRATA1))
  ) %>%
  var_relabel(SEX = "Sex", BMRKR2 = "Categorical Level Biomarker 2", STRATA1 = "Stratification Factor 1")

df <- extract_rsp_subgroups(
  variables = list(rsp = "is_rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2", "STRATA1")),
  data = anl
)

result <- basic_table() %>%
  tabulate_rsp_subgroups(df, vars = c("n_tot", "n", "n_rsp", "prop", "or", "ci"))
)--"),

    tlg("RATET01", "Event Rate Summary for Recurrent Events", "Table", topic, slug, data = c("ADSL", "ADTTE"), code = r"--(
library(tern)

adsl <- ADSL %>% filter(ITTFL == "Y") %>% df_explicit_na()
# TNE: total number of exacerbations; lgTMATRSK: log of the time at risk
anl <- ADTTE %>%
  filter(ITTFL == "Y", PARAMCD == "TNE") %>%
  mutate(ARM = factor(as.character(ARM), levels = c("B: Placebo", "A: Drug X", "C: Combination"))) %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM", ref_group = "B: Placebo") %>%
  analyze_vars("AVAL", var_labels = "Number of events", .stats = c("n", "mean_sd", "median", "range"), show_labels = "visible", table_names = "events") %>%
  summarize_glm_count(
    vars = "AVAL",
    variables = list(arm = "ARM", offset = "lgTMATRSK", covariates = NULL),
    conf_level = 0.95,
    distribution = "poisson",
    rate_mean_method = "emmeans",
    var_labels = "Unadjusted rate (per year)",
    table_names = "unadjusted",
    .stats = c("rate", "rate_ci", "rate_ratio", "rate_ratio_ci", "pval")
  )

result <- build_table(lyt, df = anl, alt_counts_df = adsl)
)--"),

    tlg("RBMIT01", "Reference-Based Multiple Imputation (rbmi)", "Table", topic, slug, data = c("ADSL", "ADQS"),
        packages = c("tern", "rbmi"), code = r"--(
library(tern)
library(rbmi)

adqs <- ADQS %>%
  filter(ITTFL == "Y", PARAMCD == "FKSI-FWB") %>%
  mutate(
    ARMCD = factor(as.character(ARMCD), levels = c("ARM B", "ARM A", "ARM C")),
    AVISIT = droplevels(factor(AVISIT)),
    USUBJID = factor(USUBJID),
    STRATA1 = droplevels(factor(STRATA1))
  ) %>%
  select(USUBJID, ARMCD, AVISIT, STRATA1, BASE, CHG) %>%
  as.data.frame()

vars <- set_vars(
  outcome = "CHG", visit = "AVISIT", subjid = "USUBJID", group = "ARMCD",
  covariates = c("BASE", "STRATA1")
)
method <- method_condmean(type = "jackknife")

set.seed(2025)
draws_obj <- draws(data = adqs, vars = vars, method = method)
impute_obj <- impute(draws_obj, references = c("ARM A" = "ARM B", "ARM B" = "ARM B", "ARM C" = "ARM B"))
analyse_obj <- analyse(impute_obj, ancova, vars = vars)
pool_obj <- pool(analyse_obj)

result <- as.data.frame(pool_obj)
)--", note = "Jump-to-reference imputation with conditional mean (jackknife); pooled treatment effects by visit."),

    tlg("RSPT01", "Best Overall Response", "Table", topic, slug, data = c("ADSL", "ADRS"), code = paste0(
      "library(tern)\n", rsp_data, r"--(
lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by(var = "ARM", ref_group = "B: Placebo") %>%
  estimate_proportion(vars = "is_rsp", table_names = "est_prop") %>%
  estimate_proportion_diff(
    vars = "is_rsp",
    show_labels = "visible",
    var_labels = "Unstratified Analysis",
    table_names = "est_prop_diff"
  ) %>%
  test_proportion_diff(vars = "is_rsp", table_names = "test_prop_diff") %>%
  estimate_odds_ratio(vars = "is_rsp", table_names = "est_or") %>%
  estimate_multinomial_response(var = "AVALC")

result <- build_table(lyt, df = adrs, alt_counts_df = adsl)
)--")),

    tlg("TTET01", "Time-To-Event Summary", "Table", topic, slug, data = c("ADSL", "ADTTE"),
        code = tte_summary("OS", "Time to Event"), note = "Overall survival."),

    tlg("ONCL01", "Listing of Individual Efficacy Data", "Listing", topic, slug, data = c("ADSL", "ADRS", "ADTTE"), code = r"--(
library(tern)
library(rlistings)

adsl <- ADSL %>% filter(ITTFL == "Y")
bor <- ADRS %>%
  filter(ITTFL == "Y", PARAMCD == "BESRSPI") %>%
  transmute(USUBJID, BOR = as.character(AVALC))
tte <- ADTTE %>%
  filter(ITTFL == "Y", PARAMCD %in% c("OS", "PFS")) %>%
  transmute(USUBJID, PARAMCD, VALUE = paste0(round(day2month(AVAL), 1), ifelse(CNSR == 1, "+", ""))) %>%
  tidyr::pivot_wider(names_from = PARAMCD, values_from = VALUE, values_fn = dplyr::first)

out <- adsl %>%
  mutate(CPID = paste(SITEID, SUBJID, sep = "/")) %>%
  select(ARM, CPID, USUBJID, AGE, SEX) %>%
  left_join(bor, by = "USUBJID") %>%
  left_join(tte, by = "USUBJID") %>%
  select(-USUBJID) %>%
  arrange(ARM, CPID) %>%
  var_relabel(
    ARM = "Treatment",
    CPID = "Center/Patient ID",
    AGE = "Age",
    SEX = "Sex",
    BOR = "Best Overall Response",
    OS = "Overall Survival (Months; + censored)",
    PFS = "Progression-Free Survival (Months; + censored)"
  )

result <- as_listing(out, key_cols = c("ARM", "CPID"), disp_cols = names(out))
)--"),

    tlg("FSTG01", "Subgroup Analysis of Best Overall Response", "Graph", topic, slug, data = c("ADSL", "ADRS"), height = 520L, code = r"--(
library(tern)

anl <- ADRS %>%
  filter(ITTFL == "Y", PARAMCD == "BESRSPI", ARM %in% c("A: Drug X", "B: Placebo")) %>%
  mutate(
    is_rsp = AVALC %in% c("CR", "PR"),
    ARM = factor(as.character(ARM), levels = c("B: Placebo", "A: Drug X")),
    SEX = droplevels(factor(SEX)),
    BMRKR2 = droplevels(factor(BMRKR2)),
    STRATA1 = droplevels(factor(STRATA1))
  ) %>%
  var_relabel(SEX = "Sex", BMRKR2 = "Categorical Level Biomarker 2", STRATA1 = "Stratification Factor 1")

df <- extract_rsp_subgroups(
  variables = list(rsp = "is_rsp", arm = "ARM", subgroups = c("SEX", "BMRKR2", "STRATA1")),
  data = anl
)
tbl <- basic_table() %>%
  tabulate_rsp_subgroups(df, vars = c("n_tot", "n", "n_rsp", "prop", "or", "ci"))

result <- g_forest(tbl)
)--"),

    tlg("FSTG02", "Subgroup Analysis of Survival Duration", "Graph", topic, slug, data = c("ADSL", "ADTTE"), height = 520L, code = r"--(
library(tern)

anl <- ADTTE %>%
  filter(ITTFL == "Y", PARAMCD == "OS", ARM %in% c("A: Drug X", "B: Placebo")) %>%
  mutate(
    AVAL = day2month(AVAL),
    is_event = CNSR == 0,
    ARM = factor(as.character(ARM), levels = c("B: Placebo", "A: Drug X")),
    SEX = droplevels(factor(SEX)),
    BMRKR2 = droplevels(factor(BMRKR2)),
    STRATA1 = droplevels(factor(STRATA1))
  ) %>%
  var_relabel(SEX = "Sex", BMRKR2 = "Categorical Level Biomarker 2", STRATA1 = "Stratification Factor 1")

df <- extract_survival_subgroups(
  variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM", subgroups = c("SEX", "BMRKR2", "STRATA1")),
  data = anl
)
tbl <- basic_table() %>%
  tabulate_survival_subgroups(df, vars = c("n_tot_events", "n", "n_events", "median", "hr", "ci"), time_unit = "Months")

result <- g_forest(tbl)
)--"),

    tlg("KMG01", "Kaplan-Meier Plot", "Graph", topic, slug, data = c("ADSL", "ADTTE"), height = 640L, code = r"--(
library(tern)

anl <- ADTTE %>%
  filter(ITTFL == "Y", PARAMCD == "OS") %>%
  mutate(AVAL = day2month(AVAL), is_event = CNSR == 0) %>%
  df_explicit_na()

result <- g_km(
  df = anl,
  variables = list(tte = "AVAL", is_event = "is_event", arm = "ARM"),
  xlab = "Time (Months)",
  annot_coxph = TRUE
)
)--", note = "Overall survival."),

    tlg("MMRMG01", "Plots for Mixed-Effect Model Repeated Measures Analysis", "Graph", topic, slug,
        data = c("ADSL", "ADQS"), packages = c("tern", "tern.mmrm", "ggplot2"), code = paste0(
      "library(tern)\nlibrary(tern.mmrm)\n", mmrm_fit, r"--(
result <- g_mmrm_lsmeans(
  mmrm_results,
  select = c("estimates", "contrasts"),
  xlab = "Visit",
  ylab = "FKSI-FWB"
)
)--")),

    tlg("MMRMG02", "Forest Plot for Mixed-Effect Model Repeated Measures", "Graph", topic, slug,
        data = c("ADSL", "ADQS"), packages = c("tern", "tern.mmrm", "ggplot2"), height = 460L, code = paste0(
      "library(tern)\nlibrary(tern.mmrm)\nlibrary(ggplot2)\n", mmrm_fit, r"--(
df <- extract_mmrm_subgroups(
  fit = mmrm_results,
  visit = levels(adqs$AVISIT)[nlevels(adqs$AVISIT)],
  subgroups = c("SEX", "BMRKR2")
)

# the forest plot, drawn from the contrasts (difference in LS means and its CI)
forest <- as.data.frame(df$contrasts) %>%
  mutate(
    label = ifelse(var == "ALL", as.character(subgroup), paste0(var_label, ": ", subgroup)),
    label = factor(label, levels = rev(unique(label)))
  )

result <- ggplot(forest, aes(x = diff, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = lcl, xmax = ucl), width = 0.25, orientation = "y") +
  geom_point(size = 2.5) +
  labs(
    title = "Difference in LS means at the last visit, by subgroup",
    x = "Difference in LS means (95% CI)", y = NULL
  ) +
  theme_minimal(base_size = 12)
)--"), note = "Difference in least-squares means at the last visit, overall and by subgroup.")
  )
}

#' @noRd
tlg_entries_other_graphs <- function() {
  topic <- "Other Graphs"
  slug <- "other"
  list(
    tlg("BRG01", "Bar Chart", "Graph", topic, slug, data = c("ADSL", "ADAE"), code = r"--(
library(tern)
library(ggplot2)

adsl <- ADSL %>% filter(SAFFL == "Y")
adae <- ADAE %>% filter(SAFFL == "Y", ANL01FL == "Y")

# percentage of patients with at least one AE, by system organ class and arm
anl <- adae %>%
  distinct(USUBJID, ACTARM, AEBODSYS) %>%
  count(ACTARM, AEBODSYS, name = "n") %>%
  left_join(count(adsl, ACTARM, name = "N"), by = "ACTARM") %>%
  mutate(pct = 100 * n / N)

result <- ggplot(anl, aes(x = pct, y = reorder(AEBODSYS, pct), fill = ACTARM)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  labs(
    title = "Patients with at least one adverse event by system organ class",
    x = "Patients (%)", y = NULL, fill = "Treatment"
  ) +
  theme_minimal(base_size = 13)
)--"),

    tlg("BWG01", "Box Plot", "Graph", topic, slug, data = c("ADSL", "ADLB"), code = r"--(
library(tern)
library(ggplot2)

adlb <- ADLB %>%
  filter(SAFFL == "Y", PARAMCD == "ALT", AVISIT != "SCREENING") %>%
  mutate(AVISIT = droplevels(factor(AVISIT)))

result <- ggplot(adlb, aes(x = AVISIT, y = AVAL, fill = ACTARM)) +
  geom_boxplot(outlier.size = 0.8, width = 0.7, position = position_dodge(width = 0.8)) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, fill = "white",
               position = position_dodge(width = 0.8)) +
  labs(
    title = "Alanine Aminotransferase by visit",
    x = "Analysis Visit", y = paste0("ALT (", adlb$AVALU[1], ")"), fill = "Treatment"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
)--"),

    tlg("CIG01", "Confidence Interval Plot", "Graph", topic, slug, data = c("ADSL", "ADLB"), code = r"--(
library(tern)
library(ggplot2)

adlb <- ADLB %>%
  filter(SAFFL == "Y", PARAMCD == "ALT", AVISIT == "BASELINE")

result <- ggplot(adlb, aes(x = ACTARM, y = AVAL, colour = ACTARM)) +
  stat_summary(fun.data = stat_mean_ci, geom = "errorbar", width = 0.15, linewidth = 0.8) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(
    title = "Mean and 95% confidence interval of ALT at baseline",
    x = "Treatment", y = paste0("ALT (", adlb$AVALU[1], ")")
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
)--"),

    tlg("IPPG01", "Individual Patient Plot Over Time", "Graph", topic, slug, data = c("ADSL", "ADLB"), code = r"--(
library(tern)

adlb <- ADLB %>%
  filter(SAFFL == "Y", PARAMCD == "ALT", AVISIT != "SCREENING", ARMCD == "ARM A") %>%
  mutate(AVISIT = droplevels(factor(AVISIT)))
# first 8 patients, for readability
adlb <- adlb %>% filter(USUBJID %in% head(unique(USUBJID), 8))

result <- g_ipp(
  df = adlb,
  xvar = "AVISIT",
  yvar = "AVAL",
  xlab = "Visit",
  ylab = paste0("ALT (", adlb$AVALU[1], ")"),
  title = "Individual patient plots (A: Drug X)",
  id_var = "USUBJID",
  add_baseline_hline = TRUE,
  yvar_baseline = "BASE"
)
)--"),

    tlg("LTG01", "Lattice Graph", "Graph", topic, slug, data = c("ADSL", "ADLB"), packages = c("tern", "lattice"), height = 620L, code = r"--(
library(tern)
library(lattice)

adlb <- ADLB %>%
  filter(SAFFL == "Y", PARAMCD %in% c("ALT", "CRP"), AVISIT != "SCREENING") %>%
  group_by(ACTARM, PARAMCD, AVISITN) %>%
  summarise(MEAN = mean(AVAL, na.rm = TRUE), .groups = "drop")

result <- xyplot(
  MEAN ~ AVISITN | PARAMCD,
  groups = ACTARM,
  data = adlb,
  type = "b",
  scales = list(y = list(relation = "free")),
  auto.key = list(columns = 3, lines = TRUE, points = TRUE),
  xlab = "Analysis Visit Number",
  ylab = "Mean value",
  main = "Mean laboratory values over time"
)
)--"),

    tlg("MNG01", "Mean Plot", "Graph", topic, slug, data = c("ADSL", "ADLB"), height = 620L, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adlb <- ADLB %>%
  filter(SAFFL == "Y", PARAMCD == "ALT", AVISIT != "SCREENING") %>%
  mutate(AVISIT = droplevels(factor(AVISIT))) %>%
  df_explicit_na()

result <- g_lineplot(
  df = adlb,
  alt_counts_df = adsl,
  variables = control_lineplot_vars(group_var = "ACTARM"),
  mid = "mean",
  interval = "mean_ci",
  title = "Mean and 95% CI of ALT by visit"
)
)--")
  )
}
