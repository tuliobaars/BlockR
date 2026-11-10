# TLG Catalog entries: Concomitant Medications, Deaths, Demography,
# Disclosures, Disposition, ECG, Exposure, Risk Management Plan, Medical
# History and Vital Signs. Code follows the TLG Catalog, adapted to the app's
# datasets (safety population and actual arm unless stated otherwise).

#' @noRd
tlg_entries_conmed <- function() {
  topic <- "Concomitant Medications"
  slug <- "concomitant-medications"
  cm <- c("ADSL", "ADCM")

  cm_table <- function(class_var, class_label) {
    sprintf(r"--(
library(tern)

adsl <- ADSL %%>%% filter(SAFFL == "Y") %%>%% df_explicit_na()
adcm <- ADCM %%>%% filter(SAFFL == "Y", ATIREL == "CONCOMITANT") %%>%% df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %%>%%
  split_cols_by("ACTARM") %%>%%
  add_overall_col(label = "All Patients") %%>%%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one treatment",
      nonunique = "Total number of treatments"
    )
  ) %%>%%
  split_rows_by(
    "%s",
    child_labels = "visible", nested = FALSE, split_fun = drop_split_levels,
    label_pos = "topleft", split_label = "%s"
  ) %%>%%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one treatment",
      nonunique = "Total number of treatments"
    )
  ) %%>%%
  count_occurrences(vars = "CMDECOD", .indent_mods = -1L) %%>%%
  append_topleft("  Other Treatment")

result <- build_table(lyt, df = adcm, alt_counts_df = adsl) %%>%%
  prune_table() %%>%%
  sort_at_path(path = "%s", scorefun = cont_n_allcols) %%>%%
  sort_at_path(path = c("%s", "*", "CMDECOD"), scorefun = score_occurrences)
)--", class_var, class_label, class_var, class_var)
  }

  list(
    tlg("CMT01", "Concomitant Medications (GNEDrug Legacy Dictionary)", "Table", topic, slug, data = cm,
        code = cm_table("CMCLAS", "Medication Class")),
    tlg("CMT01A", "Concomitant Medications by Medication Class and Preferred Name", "Table", topic, slug, data = cm,
        code = cm_table("ATC2", "ATC Class Level 2")),
    tlg("CMT01B", "Concomitant Medications by Medication Class and Preferred Name (ATC Level 1)", "Table", topic, slug, data = cm,
        code = cm_table("ATC1", "ATC Class Level 1")),

    tlg("CMT02_PT", "Concomitant Medications by Preferred Name", "Table", topic, slug, data = cm, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adcm <- ADCM %>% filter(SAFFL == "Y", ATIREL == "CONCOMITANT") %>% df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one treatment",
      nonunique = "Total number of treatments"
    )
  ) %>%
  count_occurrences(vars = "CMDECOD") %>%
  append_topleft("Other Treatment")

result <- build_table(lyt, df = adcm, alt_counts_df = adsl) %>%
  sort_at_path(path = "CMDECOD", scorefun = score_occurrences)
)--"),

    tlg("CML01", "Listing of Previous and Concomitant Medications", "Listing", topic, slug, data = cm, code = r"--(
library(tern)
library(rlistings)

adcm <- ADCM %>% filter(SAFFL == "Y") %>% df_explicit_na()

out <- adcm %>%
  mutate(
    CPID = paste(SITEID, SUBJID, sep = "/"),
    ASR = paste(AGE, substr(as.character(SEX), 1, 1), substr(as.character(RACE), 1, 1), sep = "/")
  ) %>%
  select(ACTARM, CPID, ASR, CMDECOD, CMTRT, ASTDY, AENDY, CMDOSE, CMDOSU, CMROUTE, CMDOSFRQ, ATIREL) %>%
  arrange(ACTARM, CPID, ASTDY) %>%
  var_relabel(
    ACTARM = "Treatment",
    CPID = "Center/Patient ID",
    ASR = "Age/Sex/Race",
    CMDECOD = "Medication Preferred Name",
    CMTRT = "Medication Name Given by Investigator",
    ASTDY = "Study Day of Start",
    AENDY = "Study Day of Stop",
    CMDOSE = "Dose",
    CMDOSU = "Dose Unit",
    CMROUTE = "Route",
    CMDOSFRQ = "Frequency",
    ATIREL = "Previous or Concomitant"
  )

result <- as_listing(out, key_cols = c("ACTARM", "CPID", "ASR"), disp_cols = names(out))
)--"),

    tlg("CML02A_GL", "Listing of Concomitant Medication Class Level 2, Preferred Name, and Investigator-Specified Terms", "Listing", topic, slug,
        data = cm, code = r"--(
library(tern)
library(rlistings)

adcm <- ADCM %>% filter(SAFFL == "Y") %>% df_explicit_na()

out <- adcm %>%
  select(ATC2, CMDECOD, CMTRT) %>%
  distinct() %>%
  arrange(ATC2, CMDECOD, CMTRT) %>%
  var_relabel(
    ATC2 = "ATC Class Level 2",
    CMDECOD = "WHODrug Preferred Name",
    CMTRT = "Investigator-Specified Treatment Term"
  )

result <- as_listing(out, key_cols = c("ATC2", "CMDECOD"), disp_cols = names(out))
)--"),

    tlg("CML02B_GL", "Listing of Concomitant Medication Class, Level 1, Level 2, Level 3, Preferred Name, and Investigator-Specified Terms", "Listing", topic, slug,
        data = cm, code = r"--(
library(tern)
library(rlistings)

adcm <- ADCM %>% filter(SAFFL == "Y") %>% df_explicit_na()

out <- adcm %>%
  select(ATC1, ATC2, ATC3, CMDECOD, CMTRT) %>%
  distinct() %>%
  arrange(ATC1, ATC2, ATC3, CMDECOD, CMTRT) %>%
  var_relabel(
    ATC1 = "ATC Class Level 1",
    ATC2 = "ATC Class Level 2",
    ATC3 = "ATC Class Level 3",
    CMDECOD = "WHODrug Preferred Name",
    CMTRT = "Investigator-Specified Treatment Term"
  )

result <- as_listing(out, key_cols = c("ATC1", "ATC2", "ATC3", "CMDECOD"), disp_cols = names(out))
)--")
  )
}

#' @noRd
tlg_entries_deaths_demography <- function() {
  list(
    tlg("DTHT01", "Death Table", "Table", "Deaths", "deaths", data = "ADSL", code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
deaths <- adsl %>%
  filter(DTHFL == "Y") %>%
  mutate(
    DTHCAT = droplevels(factor(DTHCAT)),
    LDDTHGR1 = droplevels(factor(LDDTHGR1))
  ) %>%
  var_relabel(DTHCAT = "Primary Cause of Death", LDDTHGR1 = "Days from Last Drug Administration")

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(var = "USUBJID", .stats = "unique", .labels = c(unique = "Total number of deaths")) %>%
  count_occurrences(vars = "DTHCAT", var_labels = "Primary Cause of Death", show_labels = "visible") %>%
  count_occurrences(vars = "LDDTHGR1", var_labels = "Days from Last Drug Administration", show_labels = "visible")

result <- build_table(lyt, df = deaths, alt_counts_df = adsl)
)--"),

    tlg("DSUR4", "Listing of Patients Who Died During Reporting Period", "Listing", "Deaths", "development-safety-update-report",
        data = "ADSL", code = r"--(
library(tern)
library(rlistings)

adsl <- ADSL %>% filter(SAFFL == "Y", DTHFL == "Y") %>% df_explicit_na()

out <- adsl %>%
  mutate(
    CPID = paste(SITEID, SUBJID, sep = "/"),
    ASR = paste(AGE, substr(as.character(SEX), 1, 1), substr(as.character(RACE), 1, 1), sep = "/"),
    DTHDT = as.Date(DTHDT)
  ) %>%
  select(ACTARM, CPID, ASR, DTHDT, DTHADY, DTHCAUS, DTHCAT) %>%
  arrange(ACTARM, CPID) %>%
  var_relabel(
    ACTARM = "Treatment",
    CPID = "Center/Patient ID",
    ASR = "Age/Sex/Race",
    DTHDT = "Date of Death",
    DTHADY = "Study Day of Death",
    DTHCAUS = "Cause of Death",
    DTHCAT = "Death Category"
  )

result <- as_listing(out, key_cols = c("ACTARM", "CPID"), disp_cols = names(out))
)--"),

    tlg("DMT01", "Demographics and Baseline Characteristics", "Table", "Demography", "demography", data = "ADSL", code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()

vars <- c("AGE", "AGEGR1", "SEX", "ETHNIC", "RACE", "COUNTRY", "BMRKR1", "BMRKR2")
var_labels <- c(
  AGE = "Age (yr)", AGEGR1 = "Age group", SEX = "Sex", ETHNIC = "Ethnicity", RACE = "Race",
  COUNTRY = "Country", BMRKR1 = "Continuous biomarker 1", BMRKR2 = "Categorical biomarker 2"
)

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  analyze_vars(
    vars = vars,
    var_labels = var_labels[vars],
    .stats = c("n", "mean_sd", "median", "range", "count_fraction")
  )

result <- build_table(lyt, df = adsl)
)--")
  )
}

#' @noRd
tlg_entries_disclosures_disposition <- function() {
  list(
    tlg("DISCLOSUREST01", "Disclosures Outputs", "Table", "Disclosures", "disclosures", data = "ADSL", code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  analyze_vars(
    vars = c("AGE", "AGEGR1", "SEX", "RACE", "ETHNIC", "EOSSTT", "DCSREAS"),
    var_labels = c(
      "Age (yr)", "Age group", "Sex", "Race", "Ethnicity",
      "End of study status", "Reason for discontinuation from study"
    ),
    .stats = c("n", "mean_sd", "median", "range", "count_fraction")
  )

result <- build_table(lyt, df = adsl)
)--"),

    tlg("EUDRAT01", "Non-Serious Adverse Events Reported in \u2265 5% of Patients in Any Treatment Group", "Table", "Disclosures", "disclosures",
        data = c("ADSL", "ADAE"), code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adae <- ADAE %>% filter(SAFFL == "Y", ANL01FL == "Y", AESER == "N") %>% df_explicit_na()

# non-serious preferred terms reported in at least 5% of the patients of any arm
n_arm <- adsl %>% count(ACTARM, name = "N")
pt_5 <- adae %>%
  distinct(USUBJID, ACTARM, AEDECOD) %>%
  count(ACTARM, AEDECOD, name = "n") %>%
  left_join(n_arm, by = "ACTARM") %>%
  filter(n / N >= 0.05) %>%
  pull(AEDECOD) %>%
  as.character() %>%
  unique()
adae_5 <- adae %>% filter(AEDECOD %in% pt_5)

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one non-serious AE (\u2265 5%)",
      nonunique = "Total number of non-serious AEs"
    )
  ) %>%
  split_rows_by(
    "AEBODSYS",
    child_labels = "visible", nested = FALSE, split_fun = drop_split_levels,
    indent_mod = -1L, label_pos = "topleft", split_label = "Body System or Organ Class"
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(unique = "Patients with at least one event", nonunique = "Number of events")
  ) %>%
  count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
  append_topleft("  Dictionary-Derived Term")

result <- build_table(lyt, df = adae_5, alt_counts_df = adsl) %>%
  prune_table() %>%
  sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)
)--"),

    tlg("EUDRAT02", "Serious Adverse Events, Fatal Serious Adverse Events, and Serious Adverse Events Related to Study Medication", "Table",
        "Disclosures", "disclosures", data = c("ADSL", "ADAE"), code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adae <- ADAE %>%
  filter(SAFFL == "Y", ANL01FL == "Y", AESER == "Y") %>%
  mutate(
    SER = TRUE,
    SERFATAL = AESDTH == "Y",
    SERREL = AEREL == "Y"
  ) %>%
  var_relabel(
    SER = "Serious AEs",
    SERFATAL = "Fatal serious AEs",
    SERREL = "Serious AEs related to study medication"
  ) %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by(
    "AEBODSYS",
    child_labels = "visible", split_fun = drop_split_levels,
    label_pos = "topleft", split_label = "Body System or Organ Class"
  ) %>%
  split_rows_by(
    "AEDECOD",
    child_labels = "visible", split_fun = drop_split_levels,
    label_pos = "topleft", split_label = "  Dictionary-Derived Term"
  ) %>%
  count_patients_with_flags(
    "USUBJID",
    flag_variables = c("SER", "SERFATAL", "SERREL"),
    denom = "N_col"
  )

result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
  prune_table()
)--"),

    tlg("DST01", "Patient Disposition", "Table", "Disposition", "safety", data = "ADSL", code = r"--(
library(tern)

adsl <- ADSL %>% df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  add_overall_col(label = "All Patients") %>%
  count_occurrences(vars = "EOSSTT", var_labels = "Study status", show_labels = "visible") %>%
  count_occurrences(vars = "EOTSTT", var_labels = "Treatment status", show_labels = "visible") %>%
  split_rows_by("EOSSTT", split_fun = keep_split_levels("DISCONTINUED"), child_labels = "hidden") %>%
  count_occurrences(
    vars = "DCSREAS",
    var_labels = "Reason for study discontinuation",
    show_labels = "visible"
  )

result <- build_table(lyt, df = adsl) %>% prune_table()
)--"),

    tlg("ENROLLMENT01", "Enrollment by Region, Country, and Investigator Number", "Table", "Disposition", "safety", data = "ADSL", code = r"--(
library(tern)

adsl <- ADSL %>% filter(ITTFL == "Y") %>% df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  add_overall_col(label = "All Patients") %>%
  split_rows_by("REGION1", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Region") %>%
  summarize_row_groups(label_fstr = "%s") %>%
  split_rows_by("COUNTRY", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Country") %>%
  summarize_row_groups(label_fstr = "%s") %>%
  count_occurrences(vars = "INVID", .indent_mods = 0L) %>%
  append_topleft("    Investigator Number")

result <- build_table(lyt, df = adsl)
)--"),

    tlg("DSL01", "Listing of Patients with Study Drug Withdrawn Due to Adverse Events", "Listing", "Disposition", "disposition",
        data = c("ADSL", "ADAE"), code = r"--(
library(tern)
library(rlistings)

adae <- ADAE %>% filter(SAFFL == "Y", AEACN == "DRUG WITHDRAWN") %>% df_explicit_na()

out <- adae %>%
  mutate(
    CPID = paste(SITEID, SUBJID, sep = "/"),
    ASR = paste(AGE, substr(as.character(SEX), 1, 1), substr(as.character(RACE), 1, 1), sep = "/"),
    TRTSDT = as.Date(TRTSDTM)
  ) %>%
  select(ACTARM, CPID, ASR, TRTSDT, AEDECOD, ASTDY, AETOXGR, AEOUT) %>%
  arrange(ACTARM, CPID, ASTDY) %>%
  var_relabel(
    ACTARM = "Treatment",
    CPID = "Center/Patient ID",
    ASR = "Age/Sex/Race",
    TRTSDT = "Date of First Study Drug Administration",
    AEDECOD = "Adverse Event Preferred Term",
    ASTDY = "Study Day of Onset",
    AETOXGR = "NCI CTCAE Grade",
    AEOUT = "Outcome"
  )

result <- as_listing(out, key_cols = c("ACTARM", "CPID", "ASR"), disp_cols = names(out))
)--"),

    tlg("DSL02", "Listing of Patients Who Discontinued Early from Study", "Listing", "Disposition", "disposition", data = "ADSL", code = r"--(
library(tern)
library(rlistings)

adsl <- ADSL %>% filter(EOSSTT == "DISCONTINUED") %>% df_explicit_na()

out <- adsl %>%
  mutate(
    CPID = paste(SITEID, SUBJID, sep = "/"),
    ASR = paste(AGE, substr(as.character(SEX), 1, 1), substr(as.character(RACE), 1, 1), sep = "/"),
    EOSDT = as.Date(EOSDT)
  ) %>%
  select(ARM, CPID, ASR, EOSDT, EOSDY, DCSREAS) %>%
  arrange(ARM, CPID) %>%
  var_relabel(
    ARM = "Treatment",
    CPID = "Center/Patient ID",
    ASR = "Age/Sex/Race",
    EOSDT = "Date of Discontinuation",
    EOSDY = "Study Day of Discontinuation",
    DCSREAS = "Reason for Discontinuation"
  )

result <- as_listing(out, key_cols = c("ARM", "CPID", "ASR"), disp_cols = names(out))
)--")
  )
}

#' @noRd
tlg_entries_ecg <- function() {
  topic <- "ECG"
  eg <- c("ADSL", "ADEG")
  list(
    tlg("EGT01", "ECG Results and Change from Baseline by Visit", "Table", topic, "ECG", data = eg, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adeg <- ADEG %>%
  filter(
    SAFFL == "Y", PARAMCD %in% c("HR", "QT", "RR"),
    !AVISIT %in% c("SCREENING", "POST-BASELINE MINIMUM", "POST-BASELINE MAXIMUM")
  ) %>%
  mutate(CHG = ifelse(AVISIT == "BASELINE", NA_real_, CHG)) %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %>%
  split_rows_by("AVISIT", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Analysis Visit") %>%
  split_cols_by_multivar(vars = c("AVAL", "CHG"), varlabels = c("Value at Visit", "Change from Baseline")) %>%
  summarize_colvars(.stats = c("n", "mean_sd", "median", "range")) %>%
  append_topleft("    Statistics")

result <- build_table(lyt, df = adeg, alt_counts_df = adsl)
)--"),

    tlg("EGT02", "ECG Abnormalities (Regardless of Abnormality at Baseline)", "Table", topic, "ECG", data = eg, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adeg <- ADEG %>%
  filter(SAFFL == "Y", ONTRTFL == "Y", PARAMCD %in% c("HR", "QT", "RR")) %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Assessment") %>%
  count_abnormal(var = "ANRIND", abnormal = list(Low = "LOW", High = "HIGH"), exclude_base_abn = FALSE) %>%
  append_topleft("    Abnormality")

result <- build_table(lyt, df = adeg, alt_counts_df = adsl)
)--"),

    tlg("EGT03", "Shift Table of ECG Interval Data \u2013 Baseline versus Minimum/Maximum Post-Baseline", "Table", topic, "ECG",
        data = eg, code = r"--(
library(tern)

adeg <- ADEG %>%
  filter(SAFFL == "Y", PARAMCD == "HR", AVISIT == "POST-BASELINE MINIMUM") %>%
  mutate(
    ANRIND = factor(as.character(ANRIND), levels = c("LOW", "NORMAL", "HIGH")),
    BNRIND = factor(as.character(BNRIND), levels = c("LOW", "NORMAL", "HIGH"))
  ) %>%
  var_relabel(BNRIND = "Baseline Reference Range Indicator") %>%
  df_explicit_na()

# rows: baseline category; columns: minimum post-baseline category
lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ANRIND") %>%
  split_rows_by("ARM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Treatment arm") %>%
  add_rowcounts() %>%
  analyze_vars("BNRIND", denom = "N_row", .stats = "count_fraction", show_labels = "hidden") %>%
  append_topleft("  Baseline Reference Range Indicator")

result <- build_table(lyt, df = adeg)
)--", note = "Heart rate: baseline (rows) versus minimum post-baseline (columns) reference range indicator."),

    tlg("EGT04", "Shift Table of Qualitative ECG Assessments", "Table", topic, "ECG", data = eg, code = r"--(
library(tern)

# worst post-baseline interpretation per patient versus baseline interpretation
adeg <- ADEG %>%
  filter(SAFFL == "Y", PARAMCD == "ECGINTP", ONTRTFL == "Y") %>%
  group_by(USUBJID) %>%
  mutate(WORST = ifelse(any(AVALC == "ABNORMAL"), "ABNORMAL", "NORMAL")) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    BASEC = factor(as.character(BASEC), levels = c("NORMAL", "ABNORMAL")),
    WORST = factor(WORST, levels = c("NORMAL", "ABNORMAL"))
  ) %>%
  var_relabel(BASEC = "Baseline", WORST = "Worst post-baseline") %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  split_cols_by("WORST") %>%
  analyze_vars("BASEC", .stats = "count_fraction", var_labels = "Baseline", show_labels = "visible") %>%
  append_topleft("ECG interpretation")

result <- build_table(lyt, df = adeg)
)--", note = "Columns: worst post-baseline ECG interpretation within each arm."),

    tlg("EGT05_QTCAT", "ECG Actual Values and Changes from Baseline by Visit (QT Categories)", "Table", topic, "ECG", data = eg, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adeg <- ADEG %>%
  filter(SAFFL == "Y", PARAMCD == "QT", ONTRTFL == "Y", !grepl("POST-BASELINE", AVISIT)) %>%
  mutate(
    AVALCAT1 = cut(AVAL, breaks = c(-Inf, 450, 480, 500, Inf),
                   labels = c("<=450 msec", ">450 to <=480 msec", ">480 to <=500 msec", ">500 msec")),
    CHGCAT1 = cut(CHG, breaks = c(-Inf, 30, 60, Inf),
                  labels = c("<=30 msec", ">30 to <=60 msec", ">60 msec"))
  ) %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by("AVISIT", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Analysis Visit") %>%
  analyze_vars(
    vars = c("AVALCAT1", "CHGCAT1"),
    var_labels = c("Value at Visit", "Change from Baseline"),
    .stats = "count_fraction"
  ) %>%
  append_topleft("  QT Duration (msec)")

result <- build_table(lyt, df = adeg, alt_counts_df = adsl)
)--"),

    tlg("EGL01", "Listing of ECG Data: Study Specific Parameters", "Listing", topic, "ECG", data = eg, code = r"--(
library(tern)
library(rlistings)

adeg <- ADEG %>%
  filter(SAFFL == "Y", PARAMCD %in% c("HR", "QT", "RR"), !grepl("POST-BASELINE", AVISIT)) %>%
  df_explicit_na()

out <- adeg %>%
  mutate(CPID = paste(SITEID, SUBJID, sep = "/"), AVAL = round(AVAL, 1)) %>%
  select(ACTARM, CPID, AVISIT, ADY, PARAMCD, AVAL) %>%
  tidyr::pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fn = dplyr::first) %>%
  arrange(ACTARM, CPID, ADY) %>%
  var_relabel(
    ACTARM = "Treatment",
    CPID = "Center/Patient ID",
    AVISIT = "Analysis Visit",
    ADY = "Study Day",
    HR = "Heart Rate (beats/min)",
    QT = "QT Duration (msec)",
    RR = "RR Duration (msec)"
  )

result <- as_listing(out, key_cols = c("ACTARM", "CPID"), disp_cols = names(out))
)--")
  )
}

#' @noRd
tlg_entries_exposure_rmp <- function() {
  ex <- c("ADSL", "ADEX")
  rmp_by <- function(var, label) {
    sprintf(r"--(
library(tern)

adsl <- ADSL %%>%% filter(SAFFL == "Y") %%>%% df_explicit_na()
adex <- ADEX %%>%%
  filter(SAFFL == "Y", PARAMCD == "TDURD") %%>%%
  var_relabel(%s = "%s") %%>%%
  df_explicit_na() %%>%%
  mutate(%s = droplevels(factor(%s)))

lyt <- basic_table(show_colcounts = TRUE, main_footer = "* Patient time is the sum of exposure across all patients in days.") %%>%%
  analyze_patients_exposure_in_cols(
    var = "%s",
    col_split = TRUE,
    add_total_level = TRUE,
    custom_label = "Total patients number/person time*"
  ) %%>%%
  append_topleft(c("", "%s"))

result <- build_table(lyt, df = adex, alt_counts_df = adsl)
)--", var, label, var, var, var, label)
  }

  list(
    tlg("EXT01", "Study Drug Exposure", "Table", "Exposure", "exposure", data = ex, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
# overall exposure: treatment duration, and total / number of doses for drug A
adex <- ADEX %>%
  filter(SAFFL == "Y", PARCAT1 == "OVERALL", PARAMCD == "TDURD" | PARCAT2 == "Drug A") %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %>%
  analyze_vars(vars = "AVAL", .stats = c("n", "mean_sd", "median", "range"), show_labels = "hidden")

result <- build_table(lyt, df = adex, alt_counts_df = adsl)
)--"),

    tlg("EXL01", "Listing of Exposure to Study Drug", "Listing", "Exposure", "exposure", data = ex, code = r"--(
library(tern)
library(rlistings)

adex <- ADEX %>% filter(SAFFL == "Y", PARCAT1 == "OVERALL") %>% df_explicit_na()

out <- adex %>%
  mutate(CPID = paste(SITEID, SUBJID, sep = "/")) %>%
  select(ACTARM, CPID, PARCAT2, PARAM, AVAL, AVALU) %>%
  arrange(ACTARM, CPID, PARCAT2, PARAM) %>%
  var_relabel(
    ACTARM = "Treatment",
    CPID = "Center/Patient ID",
    PARCAT2 = "Drug",
    PARAM = "Parameter",
    AVAL = "Value",
    AVALU = "Unit"
  )

result <- as_listing(out, key_cols = c("ACTARM", "CPID"), disp_cols = names(out))
)--"),

    tlg("RMPT01", "Duration of Exposure for Risk Management Plan", "Table", "Risk Management Plan", "risk-management-plan",
        data = ex, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adex <- ADEX %>%
  filter(SAFFL == "Y", PARAMCD == "TDURD") %>%
  mutate(
    AVALCAT1 = cut(AVAL, breaks = c(0, 30, 90, 180, Inf), right = FALSE,
                   labels = c("< 1 month", "1 to <3 months", "3 to <6 months", ">=6 months"))
  ) %>%
  var_relabel(AVALCAT1 = "Duration of exposure") %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE, main_footer = "* Patient time is the sum of exposure across all patients in days.") %>%
  analyze_patients_exposure_in_cols(
    var = "AVALCAT1",
    col_split = TRUE,
    add_total_level = TRUE,
    custom_label = "Total patients number/person time*"
  ) %>%
  append_topleft(c("", "Duration of exposure"))

result <- build_table(lyt, df = adex, alt_counts_df = adsl)
)--"),

    tlg("RMPT03", "Extent of Exposure by Age Group and Gender for Risk Management Plan", "Table", "Risk Management Plan",
        "risk-management-plan", data = ex, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adex <- ADEX %>%
  filter(SAFFL == "Y", PARAMCD == "TDURD") %>%
  mutate(
    AGEGR = cut(AGE, breaks = c(0, 18, 65, Inf), right = FALSE, labels = c("<18", "18 - 64", ">=65")),
    SEX = droplevels(factor(SEX))
  ) %>%
  var_relabel(AGEGR = "Age group (years)") %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE, main_footer = "* Patient time is the sum of exposure across all patients in days.") %>%
  split_cols_by("SEX", split_fun = add_overall_level("Total", first = FALSE)) %>%
  analyze_patients_exposure_in_cols(
    var = "AGEGR",
    col_split = TRUE,
    add_total_level = TRUE,
    custom_label = "Total patients number/person time*"
  ) %>%
  append_topleft(c("", "Age group (years)"))

result <- build_table(lyt, df = adex, alt_counts_df = adsl)
)--"),

    tlg("RMPT04", "Extent of Exposure by Ethnic Origin for Risk Management Plan", "Table", "Risk Management Plan",
        "risk-management-plan", data = ex, code = rmp_by("ETHNIC", "Ethnicity")),

    tlg("RMPT05", "Extent of Exposure by Race for Risk Management Plan", "Table", "Risk Management Plan",
        "risk-management-plan", data = ex, code = rmp_by("RACE", "Race")),

    tlg("RMPT06", "Seriousness, Outcomes, Severity, Frequency with 95% CI for Risk Management Plan", "Table",
        "Risk Management Plan", "risk-management-plan", data = c("ADSL", "ADAE"), code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y")
adae <- ADAE %>% filter(SAFFL == "Y", ANL01FL == "Y")

# one record per patient with AE flags (patients without AE: FALSE)
ae_flags <- adae %>%
  group_by(USUBJID) %>%
  summarise(
    ANY = TRUE,
    SER = any(AESER == "Y"),
    FATAL = any(AESDTH == "Y"),
    RESOLVED = all(AEOUT %in% c("RECOVERED/RESOLVED", "RECOVERED/RESOLVED WITH SEQUELAE")),
    WORSTGR = max(as.integer(as.character(AETOXGR)), na.rm = TRUE),
    .groups = "drop"
  )
anl <- adsl %>%
  left_join(ae_flags, by = "USUBJID") %>%
  mutate(
    across(c(ANY, SER, FATAL), ~ coalesce(.x, FALSE)),
    RESOLVED = coalesce(ANY & RESOLVED, FALSE),
    WORSTGR = factor(WORSTGR, levels = 1:5, labels = paste("Grade", 1:5))
  ) %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  estimate_proportion(vars = "ANY", method = "clopper-pearson", table_names = "any",
                      .labels = c(n_prop = "Patients with at least one AE")) %>%
  estimate_proportion(vars = "SER", method = "clopper-pearson", table_names = "ser",
                      .labels = c(n_prop = "Patients with at least one serious AE")) %>%
  estimate_proportion(vars = "FATAL", method = "clopper-pearson", table_names = "fatal",
                      .labels = c(n_prop = "Patients with at least one fatal AE")) %>%
  estimate_proportion(vars = "RESOLVED", method = "clopper-pearson", table_names = "resolved",
                      .labels = c(n_prop = "Patients with all AEs resolved")) %>%
  analyze_vars("WORSTGR", var_labels = "Highest NCI CTCAE grade", .stats = "count_fraction", show_labels = "visible")

result <- build_table(lyt, df = anl)
)--", note = "Any AE; seriousness, outcome and severity, each with a 95% Clopper-Pearson confidence interval.")
  )
}

#' @noRd
tlg_entries_medical_history <- function() {
  topic <- "Medical History"
  list(
    tlg("MHT01", "Medical History", "Table", topic, "medical-history", data = "ADSL", extra = "ADMH", code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
# ADMH is not part of the app's data: synthetic example data for the same patients
admh <- BlockR::tlg_example_data("ADMH", adsl) %>% df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one condition",
      nonunique = "Total number of conditions"
    )
  ) %>%
  split_rows_by(
    "MHBODSYS",
    child_labels = "visible", nested = FALSE, split_fun = drop_split_levels,
    indent_mod = -1L, label_pos = "topleft", split_label = "MedDRA System Organ Class"
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one condition",
      nonunique = "Total number of conditions"
    )
  ) %>%
  count_occurrences(vars = "MHDECOD", .indent_mods = -1L) %>%
  append_topleft("  MedDRA Preferred Term")

result <- build_table(lyt, df = admh, alt_counts_df = adsl) %>%
  prune_table()
)--", note = "ADMH is not part of the app's data: uses the random.cdisc.data example, restricted to the filtered patients."),

    tlg("MHL01", "Listing of Medical History and Concurrent Diseases", "Listing", topic, "medical-history",
        data = "ADSL", extra = "ADMH", code = r"--(
library(tern)
library(rlistings)

adsl <- ADSL %>% filter(SAFFL == "Y")
admh <- BlockR::tlg_example_data("ADMH", adsl) %>% df_explicit_na()

out <- admh %>%
  mutate(CPID = paste(SITEID, SUBJID, sep = "/")) %>%
  select(any_of(c("ARM", "CPID", "MHBODSYS", "MHDECOD", "MHTERM", "ASTDY", "AENDY", "ATIREL"))) %>%
  arrange(ARM, CPID) %>%
  var_relabel(ARM = "Treatment", CPID = "Center/Patient ID")

result <- as_listing(out, key_cols = c("ARM", "CPID"), disp_cols = names(out))
)--", note = "ADMH is not part of the app's data: uses the random.cdisc.data example, restricted to the filtered patients.")
  )
}

#' @noRd
tlg_entries_vital_signs <- function() {
  topic <- "Vital Signs"
  vs <- c("ADSL", "ADVS")
  list(
    tlg("VST01", "Vital Sign Results and Change from Baseline by Visit", "Table", topic, "vital-signs", data = vs, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
advs <- ADVS %>%
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

result <- build_table(lyt, df = advs, alt_counts_df = adsl)
)--"),

    tlg("VST02", "Vital Sign Abnormalities", "Table", topic, "vital-signs", data = vs, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
advs <- ADVS %>%
  filter(SAFFL == "Y", ONTRTFL == "Y") %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Assessment") %>%
  count_abnormal(var = "ANRIND", abnormal = list(Low = "LOW", High = "HIGH"), exclude_base_abn = FALSE) %>%
  append_topleft("    Abnormality")

result <- build_table(lyt, df = advs, alt_counts_df = adsl)
)--", note = "Regardless of abnormality at baseline."),

    tlg("VSL01", "Listing of Vital Signs: Study Specific Parameters", "Listing", topic, "vital-signs", data = vs, code = r"--(
library(tern)
library(rlistings)

advs <- ADVS %>% filter(SAFFL == "Y") %>% df_explicit_na()

out <- advs %>%
  mutate(CPID = paste(SITEID, SUBJID, sep = "/"), AVAL = round(AVAL, 1)) %>%
  select(ACTARM, CPID, AVISIT, ADY, PARAMCD, AVAL) %>%
  tidyr::pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fn = dplyr::first) %>%
  arrange(ACTARM, CPID, ADY) %>%
  var_relabel(ACTARM = "Treatment", CPID = "Center/Patient ID", AVISIT = "Analysis Visit", ADY = "Study Day")

result <- as_listing(out, key_cols = c("ACTARM", "CPID"), disp_cols = names(out))
)--")
  )
}
