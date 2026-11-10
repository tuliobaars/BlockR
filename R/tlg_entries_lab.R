# TLG Catalog entries: Lab Results (tables LBT*, listings LBL*).
# ADLB in this app has three parameters (ALT, CRP, IGA). The Hy's law outputs
# need ADHY, which is not part of the app's data: they use the
# random.cdisc.data example restricted to the filtered patients.

#' @noRd
tlg_entries_lab <- function() {
  topic <- "Lab Results"
  slug <- "lab-results"
  lb <- c("ADSL", "ADLB")
  adhy_note <- "ADHY is not part of the app's data: uses the random.cdisc.data example, restricted to the filtered patients."

  # Liver (Hy's law) criteria from ADHY: patients meeting each criterion
  hy_patients <- function(baseline, pattern) {
    sprintf(r"--(
library(tern)

adsl <- ADSL %%>%% filter(SAFFL == "Y") %%>%% df_explicit_na()
# ADHY is not part of the app's data: synthetic example data for the same patients
adhy <- BlockR::tlg_example_data("ADHY", adsl) %%>%% df_explicit_na()

# criteria relative to %s; criteria selected by parameter name
is_baseline <- grepl("baseline", adhy$PARAM, ignore.case = TRUE)
keep <- %sis_baseline & grepl("%s", adhy$PARAM, ignore.case = TRUE)
if (!any(keep)) keep <- %sis_baseline
if (!any(keep)) keep <- rep(TRUE, nrow(adhy))
liver <- adhy[keep, , drop = FALSE] %%>%%
  mutate(
    PARAM = droplevels(factor(PARAM)),
    MET = toupper(as.character(AVALC)) %%in%% c("Y", "YES", "POSITIVE")
  ) %%>%%
  var_relabel(MET = "Patients meeting the criterion post-baseline")

lyt <- basic_table(show_colcounts = TRUE) %%>%%
  split_cols_by("ARM") %%>%%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Liver laboratory criterion") %%>%%
  count_patients_with_flags("USUBJID", flag_variables = "MET", denom = "N_col")

result <- build_table(lyt, df = liver, alt_counts_df = adsl)
)--",
      if (baseline) "baseline" else "the upper limit of normal (ULN)",
      if (baseline) "" else "!", pattern,
      if (baseline) "" else "!")
  }

  # Liver (Hy's law) criteria from ADHY by visit
  hy_by_visit <- function(baseline) {
    sprintf(r"--(
library(tern)

adsl <- ADSL %%>%% filter(SAFFL == "Y") %%>%% df_explicit_na()
# ADHY is not part of the app's data: synthetic example data for the same patients
adhy <- BlockR::tlg_example_data("ADHY", adsl) %%>%% df_explicit_na()

is_baseline <- grepl("baseline", adhy$PARAM, ignore.case = TRUE)
keep <- %sis_baseline
if (!any(keep)) keep <- rep(TRUE, nrow(adhy))
liver <- adhy[keep, , drop = FALSE] %%>%%
  mutate(PARAM = droplevels(factor(PARAM)), AVALC = droplevels(factor(AVALC)))

lyt <- basic_table(show_colcounts = TRUE) %%>%%
  split_cols_by("ARM") %%>%%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Liver laboratory criterion") %%>%%
  split_rows_by("AVISIT", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Visit") %%>%%
  count_occurrences(vars = "AVALC") %%>%%
  append_topleft("    Result")

result <- build_table(lyt, df = liver, alt_counts_df = adsl) %%>%% prune_table()
)--", if (baseline) "" else "!")
  }

  # Time to first Hy's law elevation (ADAETTE)
  hy_tte <- function(paramcd) {
    sprintf(r"--(
library(tern)

adsl <- ADSL %%>%% filter(SAFFL == "Y") %%>%% df_explicit_na()
anl <- ADAETTE %%>%%
  filter(SAFFL == "Y", PARAMCD == "%s") %%>%%
  mutate(is_event = CNSR == 0) %%>%%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %%>%%
  split_cols_by("ACTARM", ref_group = "B: Placebo") %%>%%
  analyze_vars(
    "is_event",
    .stats = "count_fraction",
    .labels = c(count_fraction = "Patients meeting Hy's law criteria (%%)"),
    show_labels = "hidden",
    table_names = "events"
  ) %%>%%
  surv_time(vars = "AVAL", var_labels = "Time to first increase", is_event = "is_event", table_names = "time") %%>%%
  coxph_pairwise(
    vars = "AVAL", is_event = "is_event", var_labels = "Unstratified analysis",
    control = control_coxph(pval_method = "log-rank"), table_names = "coxph"
  )

result <- build_table(lyt, df = anl, alt_counts_df = adsl)
)--", paramcd)
  }

  # NCI-CTCAE grade shift: baseline versus worst post-baseline, one direction
  shift <- function(flag, levels, direction) {
    sprintf(r"--(
library(tern)

adsl <- ADSL %%>%% filter(SAFFL == "Y") %%>%% df_explicit_na()
# worst post-baseline record per patient and parameter (%s direction)
adlb <- ADLB %%>%%
  filter(SAFFL == "Y", %s == "Y") %%>%%
  mutate(
    BTOXGR = factor(as.character(BTOXGR), levels = c(%s)),
    ATOXGR = factor(as.character(ATOXGR), levels = c(%s))
  ) %%>%%
  var_relabel(BTOXGR = "Baseline NCI-CTCAE grade", ATOXGR = "Worst post-baseline NCI-CTCAE grade") %%>%%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %%>%%
  split_cols_by("ACTARM") %%>%%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %%>%%
  split_rows_by("BTOXGR", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Baseline NCI-CTCAE Grade") %%>%%
  summarize_num_patients(var = "USUBJID", .stats = "unique_count", unique_count_suffix = FALSE) %%>%%
  count_occurrences(vars = "ATOXGR", drop = FALSE) %%>%%
  append_topleft("    Worst Post-Baseline NCI-CTCAE Grade")

result <- build_table(lyt, df = adlb, alt_counts_df = adsl)
)--", direction, flag, levels, levels)
  }

  list(
    tlg("LBT01", "Laboratory Test Results and Change from Baseline by Visit", "Table", topic, slug, data = lb, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adlb <- ADLB %>%
  filter(SAFFL == "Y", AVISIT != "SCREENING") %>%
  mutate(CHG = ifelse(AVISIT == "BASELINE", NA_real_, CHG)) %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %>%
  split_rows_by("AVISIT", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Analysis Visit") %>%
  split_cols_by_multivar(vars = c("AVAL", "CHG"), varlabels = c("Value at Visit", "Change from Baseline")) %>%
  summarize_colvars(.stats = c("n", "mean_sd", "median", "range")) %>%
  append_topleft("    Statistics")

result <- build_table(lyt, df = adlb, alt_counts_df = adsl)
)--"),

    tlg("LBT02", "Laboratory Test Results by Visit", "Table", topic, slug, data = lb, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adlb <- ADLB %>% filter(SAFFL == "Y", AVISIT != "SCREENING") %>% df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %>%
  split_rows_by("AVISIT", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Analysis Visit") %>%
  analyze_vars(vars = "AVAL", .stats = c("n", "mean_sd", "median", "range"), show_labels = "hidden") %>%
  append_topleft("    Statistics")

result <- build_table(lyt, df = adlb, alt_counts_df = adsl)
)--"),

    tlg("LBT03", "Laboratory Test Results Change from Baseline by Visit", "Table", topic, slug, data = lb, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
# summarize_change() needs the baseline flag as TRUE / FALSE
adlb <- ADLB %>%
  filter(SAFFL == "Y", AVISIT != "SCREENING") %>%
  mutate(ABLFL = ABLFL %in% "Y") %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %>%
  split_rows_by("AVISIT", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Analysis Visit") %>%
  summarize_change(
    "CHG",
    variables = list(value = "AVAL", baseline_flag = "ABLFL"),
    .stats = c("n", "mean_sd", "median", "range")
  ) %>%
  append_topleft("    Value at baseline / change from baseline")

result <- build_table(lyt, df = adlb, alt_counts_df = adsl)
)--"),

    tlg("LBT04", "Laboratory Abnormalities Not Present at Baseline", "Table", topic, slug, data = lb, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adlb <- ADLB %>% filter(SAFFL == "Y", ONTRTFL == "Y") %>% df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by("LBCAT", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Laboratory Category") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Parameter") %>%
  count_abnormal(var = "ANRIND", abnormal = list(Low = "LOW", High = "HIGH"), exclude_base_abn = TRUE) %>%
  append_topleft("    Direction of Abnormality")

result <- build_table(lyt, df = adlb, alt_counts_df = adsl)
)--"),

    tlg("LBT05", "Laboratory Abnormalities with Single and Replicated Marked", "Table", topic, slug, data = lb, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()

# ADLB has no marked-abnormality category: simulate AVALCAT1 as in the catalog
set.seed(1, kind = "Mersenne-Twister")
adlb <- ADLB %>%
  filter(SAFFL == "Y", ONTRTFL == "Y", ANRIND %in% c("LOW", "HIGH")) %>%
  mutate(
    abn_dir = factor(ifelse(ANRIND == "LOW", "Low", "High"), levels = c("Low", "High")),
    AVALCAT1 = factor(sample(c("LAST", "REPLICATED", "SINGLE"), n(), replace = TRUE),
                      levels = c("LAST", "REPLICATED", "SINGLE"))
  ) %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %>%
  summarize_num_patients(var = "USUBJID", .stats = "unique_count") %>%
  split_rows_by("abn_dir", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Direction of Abnormality") %>%
  count_abnormal_by_marked(
    var = "AVALCAT1",
    variables = list(id = "USUBJID", param = "PARAM", direction = "abn_dir")
  )

result <- build_table(lyt, df = adlb, alt_counts_df = adsl)
)--", note = "The marked-abnormality category (AVALCAT1) is simulated, as in the catalog: the example ADLB does not have it."),

    tlg("LBT06", "Laboratory Abnormalities by Visit and Baseline Status", "Table", topic, slug, data = lb, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adlb <- ADLB %>% filter(SAFFL == "Y", ONTRTFL == "Y") %>% df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %>%
  split_rows_by("AVISIT", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Visit") %>%
  count_abnormal_by_baseline(var = "ANRIND", abnormal = c(Low = "LOW", High = "HIGH")) %>%
  append_topleft("    Abnormality at Visit / Baseline Status")

result <- build_table(lyt, df = adlb, alt_counts_df = adsl)
)--"),

    tlg("LBT07", "Laboratory Test Results with Highest Grade Post-Baseline", "Table", topic, slug, data = lb, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adlb <- ADLB %>% filter(SAFFL == "Y")

# worst grade in each direction (LOW/HIGH) per patient and parameter
adlb_out <- h_adlb_abnormal_by_worst_grade(adlb) %>%
  filter(GRADE_DIR %in% c("LOW", "HIGH")) %>%
  mutate(
    GRADE_DIR = factor(GRADE_DIR, levels = c("LOW", "HIGH")),
    GRADE_ANL = factor(GRADE_ANL, levels = c("0", "1", "2", "3", "4"))
  ) %>%
  var_relabel(PARAM = "Parameter", GRADE_DIR = "Direction of Abnormality", GRADE_ANL = "Highest NCI CTCAE Grade") %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %>%
  split_rows_by("GRADE_DIR", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Direction of Abnormality") %>%
  count_abnormal_by_worst_grade(
    var = "GRADE_ANL",
    variables = list(id = "USUBJID", param = "PARAM", grade_dir = "GRADE_DIR")
  ) %>%
  append_topleft("    Highest NCI CTCAE Grade")

result <- build_table(lyt, df = adlb_out, alt_counts_df = adsl)
)--"),

    tlg("LBT08", "Laboratory Test Results with Highest Grade Post-Baseline (Worsened from Baseline)", "Table", topic, slug, data = lb, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
# direction of clinical interest per parameter: B = both, L = low, H = high
adlb <- ADLB %>%
  filter(SAFFL == "Y") %>%
  mutate(GRADDR = case_when(PARAMCD == "ALT" ~ "B", PARAMCD == "CRP" ~ "L", PARAMCD == "IGA" ~ "H")) %>%
  filter(!is.na(GRADDR))

# the direction ("High" / "Low") must be text for count_abnormal_lab_worsen_by_baseline()
df <- h_adlb_worsen(
  adlb,
  worst_flag_low = c("WGRLOFL" = "Y"),
  worst_flag_high = c("WGRHIFL" = "Y"),
  direction_var = "GRADDR"
) %>%
  mutate(GRADDR = as.character(GRADDR))

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by("PARAMCD", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Laboratory Test") %>%
  # no split_fun here: drop_split_levels would turn GRADDR back into a factor
  split_rows_by("GRADDR", label_pos = "topleft", split_label = "  Direction of Abnormality") %>%
  count_abnormal_lab_worsen_by_baseline(
    var = "ATOXGR",
    variables = list(id = "USUBJID", baseline_var = "BTOXGR", direction_var = "GRADDR")
  ) %>%
  append_topleft("    Highest NCI CTCAE Grade")

result <- build_table(lyt, df = df, alt_counts_df = adsl)
)--"),

    tlg("LBT09", "Liver Laboratory Tests \u2013 Patients with Elevated Post-Baseline AST or ALT Levels", "Table", topic, slug,
        data = "ADSL", extra = "ADHY", note = adhy_note, code = hy_patients(FALSE, "ALT|AST")),
    tlg("LBT10", "Liver Laboratory Tests \u2013 Patients Meeting Hy's Law Criteria (with Respect to ULN)", "Table", topic, slug,
        data = "ADSL", extra = "ADHY", note = adhy_note, code = hy_patients(FALSE, "BILI|Hy")),
    tlg("LBT10_BL", "Liver Laboratory Tests \u2013 Patients Meeting Hy's Law Criteria (with Respect to Baseline)", "Table", topic, slug,
        data = "ADSL", extra = "ADHY", note = adhy_note, code = hy_patients(TRUE, "BILI|Hy")),
    tlg("LBT11", "Time-to-First Increase in Liver Laboratory Test Result Meeting Hy's Law Criteria (with Respect to ULN)", "Table", topic, slug,
        data = c("ADSL", "ADAETTE"), code = hy_tte("HYSTTEUL")),
    tlg("LBT11_BL", "Time-to-First Increase in Liver Laboratory Test Result Meeting Hy's Law Criteria (with Respect to Baseline)", "Table", topic, slug,
        data = c("ADSL", "ADAETTE"), code = hy_tte("HYSTTEBL")),
    tlg("LBT12", "Liver Laboratory Test Results by Visit (with Respect to ULN)", "Table", topic, slug,
        data = "ADSL", extra = "ADHY", note = adhy_note, code = hy_by_visit(FALSE)),
    tlg("LBT12_BL", "Liver Laboratory Test Results by Visit (with Respect to Baseline)", "Table", topic, slug,
        data = "ADSL", extra = "ADHY", note = adhy_note, code = hy_by_visit(TRUE)),

    tlg("LBT13", "NCI-CTCAE Grade Laboratory Abnormality Shift Table \u2013 Baseline versus Worst Post-Baseline (High)", "Table", topic, slug,
        data = lb, code = shift("WGRHIFL", '"0", "1", "2", "3", "4"', "high")),
    tlg("LBT14", "NCI-CTCAE Grade Laboratory Abnormality Shift Table \u2013 Baseline versus Worst Post-Baseline (Low)", "Table", topic, slug,
        data = lb, code = shift("WGRLOFL", '"0", "-1", "-2", "-3", "-4"', "low")),

    tlg("LBT15", "Laboratory Test Shifts to NCI-CTCAE Grade 3-4 Post-Baseline", "Table", topic, slug, data = lb, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adlb <- ADLB %>%
  filter(SAFFL == "Y", ONTRTFL == "Y") %>%
  mutate(
    SHIFT_LOW = BTOXGR %in% c("0", "-1", "-2") & ATOXGR %in% c("-3", "-4"),
    SHIFT_HIGH = BTOXGR %in% c("0", "1", "2") & ATOXGR %in% c("3", "4")
  ) %>%
  var_relabel(
    SHIFT_LOW = "Low: baseline grade 0-2 to grade 3-4",
    SHIFT_HIGH = "High: baseline grade 0-2 to grade 3-4"
  ) %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %>%
  count_patients_with_flags("USUBJID", flag_variables = c("SHIFT_LOW", "SHIFT_HIGH"), denom = "N_col") %>%
  append_topleft("  Shift")

result <- build_table(lyt, df = adlb, alt_counts_df = adsl)
)--"),

    tlg("LBL01", "Listing of Laboratory Test Results", "Listing", topic, slug, data = lb, code = r"--(
library(tern)
library(rlistings)

adlb <- ADLB %>% filter(SAFFL == "Y") %>% df_explicit_na()

out <- adlb %>%
  mutate(
    CPID = paste(SITEID, SUBJID, sep = "/"),
    AVAL = round(AVAL, 2),
    RANGE = paste(round(ANRLO, 2), "-", round(ANRHI, 2))
  ) %>%
  select(ACTARM, CPID, PARAM, AVISIT, ADY, AVAL, AVALU, RANGE, ANRIND) %>%
  arrange(ACTARM, CPID, PARAM, ADY) %>%
  var_relabel(
    ACTARM = "Treatment",
    CPID = "Center/Patient ID",
    PARAM = "Laboratory Test",
    AVISIT = "Analysis Visit",
    ADY = "Study Day",
    AVAL = "Result",
    AVALU = "Unit",
    RANGE = "Normal Range",
    ANRIND = "Reference Range Indicator"
  )

result <- as_listing(out, key_cols = c("ACTARM", "CPID", "PARAM"), disp_cols = names(out))
)--"),

    tlg("LBL01_RLS", "Listing of Laboratory Test Results with NCI CTCAE Grades (Safety Lab Standardization, RLS)", "Listing", topic, slug,
        data = lb, code = r"--(
library(tern)
library(rlistings)

adlb <- ADLB %>%
  filter(SAFFL == "Y", !as.character(ATOXGR) %in% c("0", NA)) %>%
  df_explicit_na()

out <- adlb %>%
  mutate(CPID = paste(SITEID, SUBJID, sep = "/"), AVAL = round(AVAL, 2)) %>%
  select(ACTARM, CPID, PARAM, AVISIT, ADY, AVAL, AVALU, BTOXGR, ATOXGR) %>%
  arrange(ACTARM, CPID, PARAM, ADY) %>%
  var_relabel(
    ACTARM = "Treatment",
    CPID = "Center/Patient ID",
    PARAM = "Laboratory Test",
    AVISIT = "Analysis Visit",
    ADY = "Study Day",
    AVAL = "Result",
    AVALU = "Unit",
    BTOXGR = "Baseline NCI CTCAE Grade",
    ATOXGR = "NCI CTCAE Grade"
  )

result <- as_listing(out, key_cols = c("ACTARM", "CPID", "PARAM"), disp_cols = names(out))
)--"),

    tlg("LBL02A", "Listing of Laboratory Abnormalities (constant units)", "Listing", topic, slug, data = lb, code = r"--(
library(tern)
library(rlistings)

adlb <- ADLB %>%
  filter(SAFFL == "Y", ONTRTFL == "Y", ANRIND %in% c("LOW", "HIGH")) %>%
  df_explicit_na()

out <- adlb %>%
  mutate(
    CPID = paste(SITEID, SUBJID, sep = "/"),
    AVAL = round(AVAL, 2),
    RANGE = paste(round(ANRLO, 2), "-", round(ANRHI, 2))
  ) %>%
  select(ACTARM, CPID, PARAM, AVISIT, ADY, AVAL, AVALU, RANGE, ANRIND) %>%
  arrange(ACTARM, CPID, PARAM, ADY) %>%
  var_relabel(
    ACTARM = "Treatment",
    CPID = "Center/Patient ID",
    PARAM = "Laboratory Test",
    AVISIT = "Analysis Visit",
    ADY = "Study Day",
    AVAL = "Result",
    AVALU = "Unit",
    RANGE = "Normal Range",
    ANRIND = "Abnormality"
  )

result <- as_listing(out, key_cols = c("ACTARM", "CPID", "PARAM"), disp_cols = names(out))
)--"),

    tlg("LBL02A_RLS", "Listing of Laboratory Abnormalities Defined by Safety Lab Standardization (RLS)", "Listing", topic, slug,
        data = lb, code = r"--(
library(tern)
library(rlistings)

# marked abnormalities: NCI CTCAE grade 3 or 4 (either direction)
adlb <- ADLB %>%
  filter(SAFFL == "Y", ONTRTFL == "Y", as.character(ATOXGR) %in% c("3", "4", "-3", "-4")) %>%
  df_explicit_na()

out <- adlb %>%
  mutate(CPID = paste(SITEID, SUBJID, sep = "/"), AVAL = round(AVAL, 2)) %>%
  select(ACTARM, CPID, PARAM, AVISIT, ADY, AVAL, AVALU, ATOXGR, ANRIND) %>%
  arrange(ACTARM, CPID, PARAM, ADY) %>%
  var_relabel(
    ACTARM = "Treatment",
    CPID = "Center/Patient ID",
    PARAM = "Laboratory Test",
    AVISIT = "Analysis Visit",
    ADY = "Study Day",
    AVAL = "Result",
    AVALU = "Unit",
    ATOXGR = "NCI CTCAE Grade",
    ANRIND = "Reference Range Indicator"
  )

result <- as_listing(out, key_cols = c("ACTARM", "CPID", "PARAM"), disp_cols = names(out))
)--"),

    tlg("LBL02B", "Listing of Laboratory Abnormalities (variable units)", "Listing", topic, slug, data = lb, code = r"--(
library(tern)
library(rlistings)

adlb <- ADLB %>%
  filter(SAFFL == "Y", ONTRTFL == "Y", ANRIND %in% c("LOW", "HIGH")) %>%
  df_explicit_na()

out <- adlb %>%
  mutate(
    CPID = paste(SITEID, SUBJID, sep = "/"),
    RESULT = paste(LBSTRESC, AVALU),
    RANGE = paste(round(ANRLO, 2), "-", round(ANRHI, 2), AVALU)
  ) %>%
  select(ACTARM, CPID, LBCAT, PARAM, AVISIT, ADY, RESULT, RANGE, ANRIND) %>%
  arrange(ACTARM, CPID, LBCAT, PARAM, ADY) %>%
  var_relabel(
    ACTARM = "Treatment",
    CPID = "Center/Patient ID",
    LBCAT = "Category",
    PARAM = "Laboratory Test",
    AVISIT = "Analysis Visit",
    ADY = "Study Day",
    RESULT = "Result (Unit)",
    RANGE = "Normal Range (Unit)",
    ANRIND = "Abnormality"
  )

result <- as_listing(out, key_cols = c("ACTARM", "CPID", "LBCAT", "PARAM"), disp_cols = names(out))
)--")
  )
}
