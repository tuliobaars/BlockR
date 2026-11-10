# TLG Catalog entries: Adverse Events (tables AET*, listings AEL*).
# Code follows the TLG Catalog, adapted to the app's datasets: safety
# population, actual arm (ACTARM), analysis records (ANL01FL == "Y").

#' @noRd
tlg_entries_adverse_events <- function() {
  topic <- "Adverse Events"
  slug <- "adverse-events"
  ae <- c("ADSL", "ADAE")
  list(
    tlg("AET01", "Safety Summary", "Table", topic, slug, data = ae, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adae <- ADAE %>%
  filter(SAFFL == "Y", ANL01FL == "Y") %>%
  mutate(
    FATAL = AESDTH == "Y",
    SER = AESER == "Y",
    SERWD = AESER == "Y" & AEACN == "DRUG WITHDRAWN",
    SERDSM = AESER == "Y" & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
    RELSER = AESER == "Y" & AEREL == "Y",
    WD = AEACN == "DRUG WITHDRAWN",
    DSM = AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
    REL = AEREL == "Y",
    RELWD = AEREL == "Y" & AEACN == "DRUG WITHDRAWN",
    RELDSM = AEREL == "Y" & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
    SEV = AESEV == "SEVERE"
  ) %>%
  var_relabel(
    FATAL = "AE with fatal outcome",
    SER = "Serious AE",
    SERWD = "Serious AE leading to withdrawal from treatment",
    SERDSM = "Serious AE leading to dose modification/interruption",
    RELSER = "Related Serious AE",
    WD = "AE leading to withdrawal from treatment",
    DSM = "AE leading to dose modification/interruption",
    REL = "Related AE",
    RELWD = "Related AE leading to withdrawal from treatment",
    RELDSM = "Related AE leading to dose modification/interruption",
    SEV = "Severe AE (at greatest intensity)"
  ) %>%
  df_explicit_na()

aesi_vars <- c("FATAL", "SER", "SERWD", "SERDSM", "RELSER", "WD", "DSM", "REL", "RELWD", "RELDSM", "SEV")

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one AE",
      nonunique = "Total number of AEs"
    )
  ) %>%
  count_patients_with_flags(
    "USUBJID",
    flag_variables = aesi_vars,
    denom = "N_col",
    var_labels = "Total number of patients with at least one",
    show_labels = "visible"
  )

result <- build_table(lyt, df = adae, alt_counts_df = adsl)
)--"),

    tlg("AET01_AESI", "Safety Summary (Adverse Events of Special Interest)", "Table", topic, slug, data = ae, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adae <- ADAE %>%
  filter(SAFFL == "Y", ANL01FL == "Y") %>%
  mutate(
    AESI = TMP_SMQ01 | TMP_SMQ02 | TMP_CQ01,
    AESI_FATAL = AESI & AESDTH == "Y",
    AESI_SER = AESI & AESER == "Y",
    AESI_REL = AESI & AEREL == "Y",
    AESI_WD = AESI & AEACN == "DRUG WITHDRAWN",
    AESI_DSM = AESI & AEACN %in% c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED"),
    AESI_GR34 = AESI & AETOXGR %in% c("3", "4")
  ) %>%
  var_relabel(
    AESI = "Any AE of special interest (AESI)",
    AESI_FATAL = "AESI with fatal outcome",
    AESI_SER = "Serious AESI",
    AESI_REL = "Related AESI",
    AESI_WD = "AESI leading to withdrawal from treatment",
    AESI_DSM = "AESI leading to dose modification/interruption",
    AESI_GR34 = "Grade 3-4 AESI"
  ) %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  count_patients_with_flags(
    "USUBJID",
    flag_variables = c("AESI", "AESI_FATAL", "AESI_SER", "AESI_REL", "AESI_WD", "AESI_DSM", "AESI_GR34"),
    denom = "N_col",
    var_labels = "Total number of patients with at least one",
    show_labels = "visible",
    table_names = "aesi_overall"
  ) %>%
  count_patients_with_flags(
    "USUBJID",
    flag_variables = c("TMP_SMQ01", "TMP_SMQ02", "TMP_CQ01"),
    denom = "N_col",
    var_labels = "Patients with AESI by basket",
    show_labels = "visible",
    table_names = "aesi_baskets"
  )

result <- build_table(lyt, df = adae, alt_counts_df = adsl)
)--"),

    tlg("AET02", "Adverse Events", "Table", topic, slug, data = ae, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adae <- ADAE %>% filter(SAFFL == "Y", ANL01FL == "Y") %>% df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one adverse event",
      nonunique = "Overall total number of events"
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
    .labels = c(
      unique = "Total number of patients with at least one adverse event",
      nonunique = "Total number of events"
    )
  ) %>%
  count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
  append_topleft("  Dictionary-Derived Term")

result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
  prune_table() %>%
  sort_at_path(path = "AEBODSYS", scorefun = cont_n_allcols) %>%
  sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)
)--"),

    tlg("AET02_SMQ", "Adverse Events by Standardized MedDRA Query", "Table", topic, slug, data = ae, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adae <- ADAE %>% filter(SAFFL == "Y", ANL01FL == "Y")

# one record per AE and basket (SMQs and customised queries)
adae_smq <- bind_rows(
  adae %>% filter(!is.na(SMQ01NAM)) %>% mutate(SMQ = as.character(SMQ01NAM)),
  adae %>% filter(!is.na(SMQ02NAM)) %>% mutate(SMQ = as.character(SMQ02NAM)),
  adae %>% filter(!is.na(CQ01NAM)) %>% mutate(SMQ = as.character(CQ01NAM))
) %>%
  mutate(SMQ = factor(SMQ)) %>%
  var_relabel(SMQ = "Standardized MedDRA Query") %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one adverse event",
      nonunique = "Total number of events"
    )
  ) %>%
  split_rows_by(
    "SMQ",
    child_labels = "visible", nested = FALSE, split_fun = drop_split_levels,
    indent_mod = -1L, label_pos = "topleft", split_label = "Standardized MedDRA Query"
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one adverse event",
      nonunique = "Total number of events"
    )
  ) %>%
  count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
  append_topleft("  Dictionary-Derived Term")

result <- build_table(lyt, df = adae_smq, alt_counts_df = adsl) %>%
  prune_table() %>%
  sort_at_path(path = c("SMQ", "*", "AEDECOD"), scorefun = score_occurrences)
)--"),

    tlg("AET03", "Adverse Events by Greatest Intensity", "Table", topic, slug, data = ae, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adae <- ADAE %>%
  filter(SAFFL == "Y", ANL01FL == "Y") %>%
  mutate(AESEV = factor(as.character(AESEV), levels = c("MILD", "MODERATE", "SEVERE"))) %>%
  df_explicit_na()

grade_groups <- list("- Any Intensity -" = c("MILD", "MODERATE", "SEVERE"))

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  count_occurrences_by_grade(var = "AESEV", grade_groups = grade_groups) %>%
  split_rows_by(
    "AEBODSYS",
    child_labels = "visible", nested = TRUE, split_fun = drop_split_levels,
    label_pos = "topleft", split_label = "Body System or Organ Class"
  ) %>%
  summarize_occurrences_by_grade(var = "AESEV", grade_groups = grade_groups) %>%
  split_rows_by(
    "AEDECOD",
    child_labels = "visible", nested = TRUE, indent_mod = -1L, split_fun = drop_split_levels,
    label_pos = "topleft", split_label = "Dictionary-Derived Term"
  ) %>%
  summarize_occurrences_by_grade(var = "AESEV", grade_groups = grade_groups) %>%
  append_topleft("                            Intensity")

# the groups hold summary rows only: prune_table() would remove them all
# (drop_split_levels already leaves out the terms that do not occur)
result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
  sort_at_path(path = "AEBODSYS", scorefun = cont_n_allcols, decreasing = TRUE) %>%
  sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = cont_n_allcols, decreasing = TRUE)
)--"),

    tlg("AET04", "Adverse Events by Highest NCI CTCAE Grade", "Table", topic, slug, data = ae, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adae <- ADAE %>%
  filter(SAFFL == "Y", ANL01FL == "Y") %>%
  mutate(AETOXGR = factor(as.character(AETOXGR), levels = c("1", "2", "3", "4", "5"))) %>%
  df_explicit_na()

grade_groups <- list(
  "- Any Grade -" = c("1", "2", "3", "4", "5"),
  "Grade 1-2" = c("1", "2"),
  "Grade 3-4" = c("3", "4"),
  "Grade 5" = "5"
)

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  count_occurrences_by_grade(var = "AETOXGR", grade_groups = grade_groups) %>%
  split_rows_by(
    "AEBODSYS",
    child_labels = "visible", nested = TRUE, split_fun = drop_split_levels,
    label_pos = "topleft", split_label = "Body System or Organ Class"
  ) %>%
  summarize_occurrences_by_grade(var = "AETOXGR", grade_groups = grade_groups) %>%
  split_rows_by(
    "AEDECOD",
    child_labels = "visible", nested = TRUE, indent_mod = -1L, split_fun = drop_split_levels,
    label_pos = "topleft", split_label = "Dictionary-Derived Term"
  ) %>%
  summarize_occurrences_by_grade(var = "AETOXGR", grade_groups = grade_groups) %>%
  append_topleft("                            Grade")

# the groups hold summary rows only: prune_table() would remove them all
# (drop_split_levels already leaves out the terms that do not occur)
result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
  sort_at_path(path = "AEBODSYS", scorefun = cont_n_allcols, decreasing = TRUE) %>%
  sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = cont_n_allcols, decreasing = TRUE)
)--"),

    tlg("AET04_PI", "Adverse Events Reported in \u2265 10% of Patients by Highest NCI CTCAE Grade", "Table", topic, slug, data = ae, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adae <- ADAE %>%
  filter(SAFFL == "Y", ANL01FL == "Y") %>%
  mutate(AETOXGR = factor(as.character(AETOXGR), levels = c("1", "2", "3", "4", "5"))) %>%
  df_explicit_na()

# preferred terms reported in at least 10% of the patients of any arm
n_arm <- adsl %>% count(ACTARM, name = "N")
pt_10 <- adae %>%
  distinct(USUBJID, ACTARM, AEDECOD) %>%
  count(ACTARM, AEDECOD, name = "n") %>%
  left_join(n_arm, by = "ACTARM") %>%
  filter(n / N >= 0.10) %>%
  pull(AEDECOD) %>%
  as.character() %>%
  unique()
adae_pi <- adae %>% filter(AEDECOD %in% pt_10)

grade_groups <- list(
  "- Any Grade -" = c("1", "2", "3", "4", "5"),
  "Grade 1-2" = c("1", "2"),
  "Grade 3-4" = c("3", "4"),
  "Grade 5" = "5"
)

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  split_rows_by(
    "AEBODSYS",
    child_labels = "visible", nested = TRUE, split_fun = drop_split_levels,
    label_pos = "topleft", split_label = "Body System or Organ Class"
  ) %>%
  split_rows_by(
    "AEDECOD",
    child_labels = "visible", nested = TRUE, split_fun = drop_split_levels,
    label_pos = "topleft", split_label = "Dictionary-Derived Term"
  ) %>%
  summarize_occurrences_by_grade(var = "AETOXGR", grade_groups = grade_groups) %>%
  append_topleft("                            Grade")

# the groups hold summary rows only: prune_table() would remove them all
# (drop_split_levels already leaves out the terms that do not occur)
result <- build_table(lyt, df = adae_pi, alt_counts_df = adsl)
)--"),

    tlg("AET05", "Adverse Event Rate Adjusted for Patient-Years at Risk - First Occurrence", "Table", topic, slug,
        data = c("ADSL", "ADAETTE"), code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adaette <- ADAETTE %>%
  filter(SAFFL == "Y", PARAMCD %in% c("AETTE1", "AETTE2", "AETTE3")) %>%
  mutate(is_event = CNSR == 0, n_events = as.integer(is_event)) %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %>%
  estimate_incidence_rate(
    vars = "AVAL",
    n_events = "n_events",
    control = control_incidence_rate(num_pt_year = 100)
  )

result <- build_table(lyt, df = adaette, alt_counts_df = adsl)
)--"),

    tlg("AET05_ALL", "Adverse Event Rate Adjusted for Patient-Years at Risk - All Occurrences", "Table", topic, slug,
        data = c("ADSL", "ADAETTE"), code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
# AETOT* records: number of occurrences (n_events) over the AE reporting period (AVAL)
adaette <- ADAETTE %>%
  filter(SAFFL == "Y", PARAMCD %in% c("AETOT1", "AETOT2", "AETOT3")) %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %>%
  estimate_incidence_rate(
    vars = "AVAL",
    n_events = "n_events",
    control = control_incidence_rate(num_pt_year = 100)
  )

result <- build_table(lyt, df = adaette, alt_counts_df = adsl)
)--"),

    tlg("AET06", "Adverse Events by Baseline Characteristic", "Table", topic, slug, data = ae, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% mutate(SEX = droplevels(factor(SEX))) %>% df_explicit_na()
adae <- ADAE %>%
  filter(SAFFL == "Y", ANL01FL == "Y") %>%
  mutate(SEX = factor(as.character(SEX), levels = levels(adsl$SEX))) %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_cols_by("SEX") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one adverse event",
      nonunique = "Overall total number of events"
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
    .labels = c(
      unique = "Total number of patients with at least one adverse event",
      nonunique = "Total number of events"
    )
  ) %>%
  count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
  append_topleft("  Dictionary-Derived Term")

result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
  prune_table() %>%
  sort_at_path(path = "AEBODSYS", scorefun = cont_n_allcols) %>%
  sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)
)--"),

    tlg("AET06_SMQ", "Adverse Events by Baseline Characteristic, by SMQ and Preferred Term", "Table", topic, slug, data = ae, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% mutate(SEX = droplevels(factor(SEX))) %>% df_explicit_na()
adae <- ADAE %>%
  filter(SAFFL == "Y", ANL01FL == "Y") %>%
  mutate(SEX = factor(as.character(SEX), levels = levels(adsl$SEX)))

adae_smq <- bind_rows(
  adae %>% filter(!is.na(SMQ01NAM)) %>% mutate(SMQ = as.character(SMQ01NAM)),
  adae %>% filter(!is.na(SMQ02NAM)) %>% mutate(SMQ = as.character(SMQ02NAM)),
  adae %>% filter(!is.na(CQ01NAM)) %>% mutate(SMQ = as.character(CQ01NAM))
) %>%
  mutate(SMQ = factor(SMQ)) %>%
  var_relabel(SMQ = "Standardized MedDRA Query") %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  split_cols_by("SEX") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one adverse event",
      nonunique = "Total number of events"
    )
  ) %>%
  split_rows_by(
    "SMQ",
    child_labels = "visible", nested = FALSE, split_fun = drop_split_levels,
    indent_mod = -1L, label_pos = "topleft", split_label = "Standardized MedDRA Query"
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one adverse event",
      nonunique = "Total number of events"
    )
  ) %>%
  count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
  append_topleft("  Dictionary-Derived Term")

result <- build_table(lyt, df = adae_smq, alt_counts_df = adsl) %>%
  prune_table() %>%
  sort_at_path(path = c("SMQ", "*", "AEDECOD"), scorefun = score_occurrences)
)--"),

    tlg("AET07", "Adverse Events Resulting in Death", "Table", topic, slug, data = ae, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adae <- ADAE %>%
  filter(SAFFL == "Y", ANL01FL == "Y", AESDTH == "Y") %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = "unique",
    .labels = c(unique = "Total number of deaths")
  ) %>%
  split_rows_by(
    "AEBODSYS",
    child_labels = "visible", nested = FALSE, split_fun = drop_split_levels,
    indent_mod = -1L, label_pos = "topleft", split_label = "Body System or Organ Class"
  ) %>%
  count_occurrences(vars = "AEDECOD", .indent_mods = 0L) %>%
  append_topleft("  Dictionary-Derived Term")

result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
  prune_table() %>%
  sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)
)--"),

    tlg("AET09", "Adverse Events Related to Study Drug", "Table", topic, slug, data = ae, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adae <- ADAE %>%
  filter(SAFFL == "Y", ANL01FL == "Y", AEREL == "Y") %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one adverse event related to study drug",
      nonunique = "Overall total number of events related to study drug"
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
    .labels = c(
      unique = "Total number of patients with at least one adverse event related to study drug",
      nonunique = "Total number of events related to study drug"
    )
  ) %>%
  count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
  append_topleft("  Dictionary-Derived Term")

result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
  prune_table() %>%
  sort_at_path(path = "AEBODSYS", scorefun = cont_n_allcols) %>%
  sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)
)--"),

    tlg("AET09_SMQ", "Adverse Events Related to Study Drug by Standardized MedDRA Query", "Table", topic, slug, data = ae, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adae <- ADAE %>% filter(SAFFL == "Y", ANL01FL == "Y", AEREL == "Y")

adae_smq <- bind_rows(
  adae %>% filter(!is.na(SMQ01NAM)) %>% mutate(SMQ = as.character(SMQ01NAM)),
  adae %>% filter(!is.na(SMQ02NAM)) %>% mutate(SMQ = as.character(SMQ02NAM)),
  adae %>% filter(!is.na(CQ01NAM)) %>% mutate(SMQ = as.character(CQ01NAM))
) %>%
  mutate(SMQ = factor(SMQ)) %>%
  var_relabel(SMQ = "Standardized MedDRA Query") %>%
  df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one adverse event related to study drug",
      nonunique = "Total number of events related to study drug"
    )
  ) %>%
  split_rows_by(
    "SMQ",
    child_labels = "visible", nested = FALSE, split_fun = drop_split_levels,
    indent_mod = -1L, label_pos = "topleft", split_label = "Standardized MedDRA Query"
  ) %>%
  summarize_num_patients(
    var = "USUBJID",
    .stats = c("unique", "nonunique"),
    .labels = c(
      unique = "Total number of patients with at least one adverse event related to study drug",
      nonunique = "Total number of events related to study drug"
    )
  ) %>%
  count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
  append_topleft("  Dictionary-Derived Term")

result <- build_table(lyt, df = adae_smq, alt_counts_df = adsl) %>%
  prune_table() %>%
  sort_at_path(path = c("SMQ", "*", "AEDECOD"), scorefun = score_occurrences)
)--"),

    tlg("AET10", "Most Common (\u22655%) Adverse Events Preferred Terms", "Table", topic, slug, data = ae, code = r"--(
library(tern)

adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
adae <- ADAE %>% filter(SAFFL == "Y", ANL01FL == "Y") %>% df_explicit_na()

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ACTARM") %>%
  add_overall_col(label = "All Patients") %>%
  count_occurrences(vars = "AEDECOD") %>%
  append_topleft("Dictionary-Derived Term")

result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
  prune_table(keep_rows(has_fraction_in_any_col(atleast = 0.05))) %>%
  sort_at_path(path = "AEDECOD", scorefun = score_occurrences)
)--"),

    tlg("AEL01", "Listing of Preferred Terms, Lowest Level Terms, and Investigator-Specified Adverse Event Terms", "Listing", topic, slug,
        data = ae, code = r"--(
library(tern)
library(rlistings)

adae <- ADAE %>% filter(SAFFL == "Y") %>% df_explicit_na()

out <- adae %>%
  select(AEBODSYS, AEDECOD, AELLT, AETERM) %>%
  distinct() %>%
  arrange(AEBODSYS, AEDECOD, AELLT, AETERM) %>%
  var_relabel(
    AEBODSYS = "MedDRA System Organ Class",
    AEDECOD = "MedDRA Preferred Term",
    AELLT = "MedDRA Lowest Level Term",
    AETERM = "Investigator-Specified Adverse Event Term"
  )

result <- as_listing(out, key_cols = c("AEBODSYS", "AEDECOD", "AELLT"), disp_cols = names(out))
)--"),

    tlg("AEL01_NOLLT", "Listing of Preferred Terms and Investigator-Specified Adverse Event Terms", "Listing", topic, slug,
        data = ae, code = r"--(
library(tern)
library(rlistings)

adae <- ADAE %>% filter(SAFFL == "Y") %>% df_explicit_na()

out <- adae %>%
  select(AEBODSYS, AEDECOD, AETERM) %>%
  distinct() %>%
  arrange(AEBODSYS, AEDECOD, AETERM) %>%
  var_relabel(
    AEBODSYS = "MedDRA System Organ Class",
    AEDECOD = "MedDRA Preferred Term",
    AETERM = "Investigator-Specified Adverse Event Term"
  )

result <- as_listing(out, key_cols = c("AEBODSYS", "AEDECOD"), disp_cols = names(out))
)--"),

    tlg("AEL02", "Listing of Adverse Events", "Listing", topic, slug, data = ae, code = r"--(
library(tern)
library(rlistings)

adae <- ADAE %>% filter(SAFFL == "Y") %>% df_explicit_na()

out <- adae %>%
  mutate(
    CPID = paste(SITEID, SUBJID, sep = "/"),
    ASR = paste(AGE, substr(as.character(SEX), 1, 1), substr(as.character(RACE), 1, 1), sep = "/"),
    DURATION = as.numeric(AENDY) - as.numeric(ASTDY) + 1,
    SERIOUS = ifelse(AESER == "Y", "Yes", "No"),
    RELATED = ifelse(AEREL == "Y", "Yes", "No")
  ) %>%
  select(ACTARM, CPID, ASR, AEDECOD, ASTDY, DURATION, SERIOUS, AESEV, RELATED, AEOUT, AEACN, AETOXGR) %>%
  arrange(ACTARM, CPID, ASTDY) %>%
  var_relabel(
    ACTARM = "Treatment",
    CPID = "Center/Patient ID",
    ASR = "Age/Sex/Race",
    AEDECOD = "Adverse Event MedDRA Preferred Term",
    ASTDY = "Study Day of Onset",
    DURATION = "AE Duration in Days",
    SERIOUS = "Serious",
    AESEV = "Most Extreme Intensity",
    RELATED = "Caused by Study Drug",
    AEOUT = "Outcome",
    AEACN = "Action Taken with Study Drug",
    AETOXGR = "NCI CTCAE Grade"
  )

result <- as_listing(out, key_cols = c("ACTARM", "CPID", "ASR"), disp_cols = names(out))
)--"),

    tlg("AEL02_ED", "Listing of Adverse Events (for Studies with Extra Design Variables)", "Listing", topic, slug, data = ae, code = r"--(
library(tern)
library(rlistings)

adae <- ADAE %>% filter(SAFFL == "Y") %>% df_explicit_na()

out <- adae %>%
  mutate(
    CPID = paste(SITEID, SUBJID, sep = "/"),
    ASR = paste(AGE, substr(as.character(SEX), 1, 1), substr(as.character(RACE), 1, 1), sep = "/"),
    DURATION = as.numeric(AENDY) - as.numeric(ASTDY) + 1,
    SERIOUS = ifelse(AESER == "Y", "Yes", "No"),
    RELATED = ifelse(AEREL == "Y", "Yes", "No")
  ) %>%
  select(ACTARM, CPID, ASR, TRT01A, TRT02A, AEDECOD, ASTDY, DURATION, SERIOUS, AESEV, RELATED, AEOUT, AEACN) %>%
  arrange(ACTARM, CPID, ASTDY) %>%
  var_relabel(
    ACTARM = "Treatment",
    CPID = "Center/Patient ID",
    ASR = "Age/Sex/Race",
    TRT01A = "Actual Treatment, Period 1",
    TRT02A = "Actual Treatment, Period 2",
    AEDECOD = "Adverse Event MedDRA Preferred Term",
    ASTDY = "Study Day of Onset",
    DURATION = "AE Duration in Days",
    SERIOUS = "Serious",
    AESEV = "Most Extreme Intensity",
    RELATED = "Caused by Study Drug",
    AEOUT = "Outcome",
    AEACN = "Action Taken with Study Drug"
  )

result <- as_listing(out, key_cols = c("ACTARM", "CPID", "ASR"), disp_cols = names(out))
)--"),

    tlg("AEL03", "Listing of Serious Adverse Events", "Listing", topic, slug, data = ae, code = r"--(
library(tern)
library(rlistings)

adae <- ADAE %>% filter(SAFFL == "Y", AESER == "Y") %>% df_explicit_na()

out <- adae %>%
  mutate(
    CPID = paste(SITEID, SUBJID, sep = "/"),
    ASR = paste(AGE, substr(as.character(SEX), 1, 1), substr(as.character(RACE), 1, 1), sep = "/"),
    DURATION = as.numeric(AENDY) - as.numeric(ASTDY) + 1,
    SERCRIT = trimws(paste(
      ifelse(AESDTH == "Y", "1", ""), ifelse(AESLIFE == "Y", "2", ""), ifelse(AESHOSP == "Y", "3", ""),
      ifelse(AESDISAB == "Y", "4", ""), ifelse(AESCONG == "Y", "5", ""), ifelse(AESMIE == "Y", "6", "")
    )),
    RELATED = ifelse(AEREL == "Y", "Yes", "No")
  ) %>%
  select(ACTARM, CPID, ASR, AEDECOD, ASTDY, DURATION, AESEV, AETOXGR, SERCRIT, RELATED, AEOUT, AEACN) %>%
  arrange(ACTARM, CPID, ASTDY) %>%
  var_relabel(
    ACTARM = "Treatment",
    CPID = "Center/Patient ID",
    ASR = "Age/Sex/Race",
    AEDECOD = "Adverse Event MedDRA Preferred Term",
    ASTDY = "Study Day of Onset",
    DURATION = "AE Duration in Days",
    AESEV = "Most Extreme Intensity",
    AETOXGR = "NCI CTCAE Grade",
    SERCRIT = "Serious criteria (1 death, 2 life-threatening, 3 hospitalisation, 4 disability, 5 congenital anomaly, 6 other)",
    RELATED = "Caused by Study Drug",
    AEOUT = "Outcome",
    AEACN = "Action Taken with Study Drug"
  )

result <- as_listing(out, key_cols = c("ACTARM", "CPID", "ASR"), disp_cols = names(out))
)--"),

    tlg("AEL04", "Listing of Patient Deaths", "Listing", topic, slug, data = "ADSL", code = r"--(
library(tern)
library(rlistings)

adsl <- ADSL %>% filter(SAFFL == "Y", DTHFL == "Y") %>% df_explicit_na()

out <- adsl %>%
  mutate(
    CPID = paste(SITEID, SUBJID, sep = "/"),
    ASR = paste(AGE, substr(as.character(SEX), 1, 1), substr(as.character(RACE), 1, 1), sep = "/"),
    TRTSDT = as.Date(TRTSDTM)
  ) %>%
  select(ACTARM, CPID, ASR, TRTSDT, DTHADY, LDDTHELD, DTHCAUS) %>%
  arrange(ACTARM, CPID) %>%
  var_relabel(
    ACTARM = "Treatment",
    CPID = "Center/Patient ID",
    ASR = "Age/Sex/Race",
    TRTSDT = "Date of First Study Drug Administration",
    DTHADY = "Day of Death",
    LDDTHELD = "Days from Last Dose to Death",
    DTHCAUS = "Cause of Death"
  )

result <- as_listing(out, key_cols = c("ACTARM", "CPID", "ASR"), disp_cols = names(out))
)--")
  )
}
