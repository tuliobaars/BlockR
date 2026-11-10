# TLG Catalog entries: Pharmacokinetics (PK tables, listings, graphs) and
# Anti-Drug Antibodies (ADA). ADPC, ADPP and ADAB are not part of the app's
# data: the code uses the random.cdisc.data examples restricted to the
# filtered patients (tlg_example_data()). Parameters and specimens are
# selected by name, so the code also adapts to other versions of these data.

#' @noRd
tlg_entries_pk <- function() {
  topic <- "Pharmacokinetics"
  slug <- "pharmacokinetic"
  note <- "ADPC/ADPP are not part of the app's data: uses the random.cdisc.data examples, restricted to the filtered patients."

  pc_setup <- r"--(
adsl <- ADSL %>% filter(SAFFL == "Y")
# ADPC is not part of the app's data: synthetic example data for the same patients
adpc <- BlockR::tlg_example_data("ADPC", adsl) %>% df_explicit_na()
is_urine <- grepl("urine", adpc$PARAM, ignore.case = TRUE)
adpc_plasma <- adpc[!is_urine, , drop = FALSE]
adpc_plasma <- adpc_plasma %>% filter(PARAM == PARAM[1]) %>% mutate(PARAM = droplevels(factor(PARAM)))
adpc_urine <- adpc[is_urine, , drop = FALSE]
if (nrow(adpc_urine)) adpc_urine <- adpc_urine %>% filter(PARAM == PARAM[1]) %>% mutate(PARAM = droplevels(factor(PARAM)))
)--"

  pp_setup <- r"--(
adsl <- ADSL %>% filter(SAFFL == "Y")
# ADPP is not part of the app's data: synthetic example data for the same patients
adpp <- BlockR::tlg_example_data("ADPP", adsl) %>% df_explicit_na()
specimen <- if ("PPSPEC" %in% names(adpp)) as.character(adpp$PPSPEC) else as.character(adpp$PARAM)
adpp_plasma <- adpp[!grepl("urine", specimen, ignore.case = TRUE), , drop = FALSE] %>% mutate(PARAM = droplevels(factor(PARAM)))
adpp_urine <- adpp[grepl("urine", specimen, ignore.case = TRUE), , drop = FALSE] %>% mutate(PARAM = droplevels(factor(PARAM)))
)--"

  pk_stats <- r"--(.stats = c("n", "mean", "sd", "cv", "geom_mean", "geom_cv", "median", "min", "max"),
    .labels = c(
      n = "n", mean = "Mean", sd = "SD", cv = "CV (%)", geom_mean = "Geometric Mean",
      geom_cv = "CV % Geometric Mean", median = "Median", min = "Minimum", max = "Maximum"
    ))--"

  pp_table <- function(data, rows, stats = pk_stats) {
    split_rows <- paste0(vapply(rows, function(r) {
      sprintf('  split_rows_by("%s", split_fun = drop_split_levels, label_pos = "topleft", split_label = "%s") %%>%%\n', r[1], r[2])
    }, character(1)), collapse = "")
    paste0("library(tern)\n", pp_setup, sprintf(r"--(
lyt <- basic_table() %%>%%
%s  analyze_vars_in_cols(
    vars = "AVAL",
    %s
  )

result <- build_table(lyt, df = %s)
)--", split_rows, stats, data))
  }

  list(
    tlg("PKCT01", "Summary Concentration Table", "Table", topic, slug, data = "ADSL", extra = "ADPC", note = note,
        code = paste0("library(tern)\n", pc_setup, sprintf(r"--(
adpc_plasma <- adpc_plasma %%>%% mutate(NFRLT = factor(NFRLT))

lyt <- basic_table() %%>%%
  split_rows_by("ARM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Treatment Group") %%>%%
  split_rows_by("NFRLT", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Nominal Time") %%>%%
  analyze_vars_in_cols(
    vars = "AVAL",
    %s
  )

result <- build_table(lyt, df = adpc_plasma)
)--", pk_stats))),

    tlg("PKPT02", "Pharmacokinetic Parameter Summary of Plasma PK Parameters by Treatment", "Table", topic, slug,
        data = "ADSL", extra = "ADPP", note = note,
        code = pp_table("adpp_plasma", list(c("PARAM", "PK Parameter"), c("ARM", "  Treatment Arm")))),
    tlg("PKPT03", "Summary of Plasma Pharmacokinetic Parameters by Treatment Arm", "Table", topic, slug,
        data = "ADSL", extra = "ADPP", note = note,
        code = pp_table("adpp_plasma", list(c("ARM", "Treatment Arm"), c("PARAM", "  PK Parameter")))),
    tlg("PKPT04", "Summary of Urinary Pharmacokinetic Parameters by Treatment Arm", "Table", topic, slug,
        data = "ADSL", extra = "ADPP", note = note,
        code = pp_table("adpp_urine", list(c("ARM", "Treatment Arm"), c("PARAM", "  PK Parameter")))),
    tlg("PKPT05", "Summary of Urinary Pharmacokinetic Parameters by Visit", "Table", topic, slug,
        data = "ADSL", extra = "ADPP", note = note,
        code = pp_table("adpp_urine", list(c("AVISIT", "Visit"), c("PARAM", "  PK Parameter")))),
    tlg("PKPT06", "Summary of Urinary Pharmacokinetic Parameters (Normalized by Dose)", "Table", topic, slug,
        data = "ADSL", extra = "ADPP", note = paste(note, "Dose-normalised parameters are selected by name (containing \"norm\" or \"dose\"); all urinary parameters otherwise."),
        code = sub(
          "result <- build_table(lyt, df = adpp_urine)",
          paste0(
            "adpp_norm <- adpp_urine[grepl(\"norm|dose\", adpp_urine$PARAM, ignore.case = TRUE), , drop = FALSE]\n",
            "if (!nrow(adpp_norm)) adpp_norm <- adpp_urine\n",
            "result <- build_table(lyt, df = adpp_norm %>% mutate(PARAM = droplevels(factor(PARAM))))"
          ),
          pp_table("adpp_urine", list(c("ARM", "Treatment Arm"), c("PARAM", "  PK Parameter"))),
          fixed = TRUE
        )),
    tlg("PKPT07", "Mean Summary of Urinary Pharmacokinetic Parameters by Treatment Arm", "Table", topic, slug,
        data = "ADSL", extra = "ADPP", note = note,
        code = pp_table("adpp_urine", list(c("PARAM", "PK Parameter"), c("ARM", "  Treatment Arm")),
                        stats = r"--(.stats = c("n", "mean", "sd", "cv"),
    .labels = c(n = "n", mean = "Mean", sd = "SD", cv = "CV (%)"))--")),

    tlg("PKPT08", "Summary of Cumulative Amount of Drug Excreted in Urine", "Table", topic, slug, data = "ADSL", extra = "ADPC", note = note,
        code = paste0("library(tern)\n", pc_setup, sprintf(r"--(
urine <- if (nrow(adpc_urine)) adpc_urine else adpc_plasma
cumulative <- urine %%>%%
  arrange(USUBJID, NFRLT) %%>%%
  group_by(USUBJID) %%>%%
  mutate(AVAL = cumsum(coalesce(AVAL, 0))) %%>%%
  ungroup() %%>%%
  mutate(NFRLT = factor(NFRLT))

lyt <- basic_table() %%>%%
  split_rows_by("ARM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Treatment Group") %%>%%
  split_rows_by("NFRLT", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Nominal Time") %%>%%
  analyze_vars_in_cols(
    vars = "AVAL",
    %s
  )

result <- build_table(lyt, df = cumulative)
)--", pk_stats))),

    tlg("PKPT11", "Summary of Pharmacokinetic Parameter Estimates (Geometric Means for Ratios)", "Table", topic, slug,
        data = "ADSL", extra = "ADPP", note = note,
        code = pp_table("adpp_plasma", list(c("PARAM", "PK Parameter"), c("ARM", "  Treatment Arm")),
                        stats = r"--(.stats = c("n", "geom_mean", "geom_cv", "geom_mean_ci"),
    .labels = c(n = "n", geom_mean = "Geometric Mean", geom_cv = "CV % Geometric Mean", geom_mean_ci = "95% CI"))--")),

    tlg("PKCL01", "Listing of Drug Concentration by Treatment Group, Patient and Nominal Time", "Listing", topic, slug,
        data = "ADSL", extra = "ADPC", note = note, code = paste0("library(tern)\nlibrary(rlistings)\n", pc_setup, r"--(
out <- adpc_plasma %>%
  select(any_of(c("ARM", "USUBJID", "AVISIT", "NFRLT", "AFRLT", "AVAL", "AVALU"))) %>%
  arrange(ARM, USUBJID, NFRLT) %>%
  var_relabel(ARM = "Treatment Group", USUBJID = "Patient ID", AVAL = paste("Concentration:", adpc_plasma$PARAM[1]))

result <- as_listing(out, key_cols = c("ARM", "USUBJID"), disp_cols = names(out))
)--")),

    tlg("PKCL02", "Listing of Drug Urine Concentration and Volumes", "Listing", topic, slug,
        data = "ADSL", extra = "ADPC", note = note, code = paste0("library(tern)\nlibrary(rlistings)\n", pc_setup, r"--(
urine <- if (nrow(adpc_urine)) adpc_urine else adpc_plasma
out <- urine %>%
  select(any_of(c("ARM", "USUBJID", "AVISIT", "NFRLT", "AFRLT", "PARAM", "AVAL", "AVALU"))) %>%
  arrange(ARM, USUBJID, NFRLT) %>%
  var_relabel(ARM = "Treatment Group", USUBJID = "Patient ID")

result <- as_listing(out, key_cols = c("ARM", "USUBJID"), disp_cols = names(out))
)--")),

    tlg("PKPL01", "Listing of Drug Plasma PK Parameters", "Listing", topic, slug,
        data = "ADSL", extra = "ADPP", note = note, code = paste0("library(tern)\nlibrary(rlistings)\n", pp_setup, r"--(
out <- adpp_plasma %>%
  mutate(AVAL = round(AVAL, 3)) %>%
  select(any_of(c("ARM", "USUBJID", "AVISIT", "PARAM", "AVAL", "AVALU"))) %>%
  arrange(ARM, USUBJID, PARAM) %>%
  var_relabel(ARM = "Treatment Group", USUBJID = "Patient ID", PARAM = "PK Parameter", AVAL = "Value")

result <- as_listing(out, key_cols = c("ARM", "USUBJID"), disp_cols = names(out))
)--")),

    tlg("PKPL02", "Listing of Drug Urine PK Parameters", "Listing", topic, slug,
        data = "ADSL", extra = "ADPP", note = note, code = paste0("library(tern)\nlibrary(rlistings)\n", pp_setup, r"--(
urine <- if (nrow(adpp_urine)) adpp_urine else adpp_plasma
out <- urine %>%
  mutate(AVAL = round(AVAL, 3)) %>%
  select(any_of(c("ARM", "USUBJID", "AVISIT", "PARAM", "AVAL", "AVALU"))) %>%
  arrange(ARM, USUBJID, PARAM) %>%
  var_relabel(ARM = "Treatment Group", USUBJID = "Patient ID", PARAM = "PK Parameter", AVAL = "Value")

result <- as_listing(out, key_cols = c("ARM", "USUBJID"), disp_cols = names(out))
)--")),

    tlg("PKPL04", "Listing of Individual AUC and Cmax Values", "Listing", topic, slug,
        data = "ADSL", extra = "ADPP", note = note, code = paste0("library(tern)\nlibrary(rlistings)\n", pp_setup, r"--(
# one column per exposure parameter (AUC and Cmax), one row per patient and visit
out <- adpp_plasma %>%
  filter(grepl("^AUC|^CMAX", PARAMCD, ignore.case = TRUE)) %>%
  mutate(AVAL = round(AVAL, 3), PARAMCD = as.character(PARAMCD)) %>%
  select(any_of(c("ARM", "USUBJID", "AVISIT", "PARAMCD", "AVAL"))) %>%
  tidyr::pivot_wider(names_from = PARAMCD, values_from = AVAL, values_fn = dplyr::first) %>%
  arrange(ARM, USUBJID) %>%
  var_relabel(ARM = "Treatment Group", USUBJID = "Patient ID")

result <- as_listing(out, key_cols = c("ARM", "USUBJID"), disp_cols = names(out))
)--")),

    tlg("PKCG01", "Plot of PK Concentration Over Time by Subject", "Graph", topic, slug, data = "ADSL", extra = "ADPC", note = note,
        code = paste0("library(tern)\nlibrary(ggplot2)\n", pc_setup, r"--(
result <- ggplot(adpc_plasma, aes(x = NFRLT, y = AVAL, group = USUBJID, colour = ARM)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 0.8, alpha = 0.6) +
  labs(
    title = paste("Individual concentration over time:", adpc_plasma$PARAM[1]),
    x = "Nominal time (hr)", y = paste0("Concentration (", adpc_plasma$AVALU[1], ")"), colour = "Treatment"
  ) +
  theme_minimal(base_size = 13)
)--")),

    tlg("PKCG02", "Plot of PK Concentration Over Time by Cohort/Treatment Group/Dose", "Graph", topic, slug, data = "ADSL", extra = "ADPC", note = note,
        height = 620L, code = paste0("library(tern)\nlibrary(ggplot2)\n", pc_setup, r"--(
result <- ggplot(adpc_plasma, aes(x = NFRLT, y = AVAL, group = USUBJID)) +
  geom_line(colour = "#0a84ff", alpha = 0.35) +
  facet_wrap(~ARM) +
  scale_y_log10() +
  labs(
    title = paste("Concentration over time by treatment group (log scale):", adpc_plasma$PARAM[1]),
    x = "Nominal time (hr)", y = paste0("Concentration (", adpc_plasma$AVALU[1], ")")
  ) +
  theme_minimal(base_size = 13)
)--")),

    tlg("PKCG03", "Plot of Mean PK Concentration Over Time by Cohort", "Graph", topic, slug, data = "ADSL", extra = "ADPC", note = note,
        code = paste0("library(tern)\nlibrary(ggplot2)\n", pc_setup, r"--(
means <- adpc_plasma %>%
  group_by(ARM, NFRLT) %>%
  summarise(MEAN = mean(AVAL, na.rm = TRUE), SD = sd(AVAL, na.rm = TRUE), .groups = "drop")

result <- ggplot(means, aes(x = NFRLT, y = MEAN, colour = ARM)) +
  geom_errorbar(aes(ymin = MEAN - SD, ymax = MEAN + SD), width = 0.3, alpha = 0.6) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = paste("Mean (\u00b1 SD) concentration over time:", adpc_plasma$PARAM[1]),
    x = "Nominal time (hr)", y = paste0("Concentration (", adpc_plasma$AVALU[1], ")"), colour = "Treatment"
  ) +
  theme_minimal(base_size = 13)
)--")),

    tlg("PKPG01", "Plot of Mean Cumulative Percentage (%) of Recovered Drug in Urine", "Graph", topic, slug, data = "ADSL", extra = "ADPC", note = note,
        code = paste0("library(tern)\nlibrary(ggplot2)\n", pc_setup, r"--(
urine <- if (nrow(adpc_urine)) adpc_urine else adpc_plasma
recovered <- urine %>%
  arrange(USUBJID, NFRLT) %>%
  group_by(USUBJID) %>%
  mutate(PCT = 100 * cumsum(coalesce(AVAL, 0)) / sum(AVAL, na.rm = TRUE)) %>%
  group_by(ARM, NFRLT) %>%
  summarise(PCT = mean(PCT, na.rm = TRUE), .groups = "drop")

result <- ggplot(recovered, aes(x = NFRLT, y = PCT, colour = ARM)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Mean cumulative percentage of recovered drug",
    x = "Nominal time (hr)", y = "Cumulative recovered (%)", colour = "Treatment"
  ) +
  theme_minimal(base_size = 13)
)--")),

    tlg("PKPG02", "Pharmacokinetic Parameter Summary of Plasma PK Parameters by Treatment Arm", "Graph", topic, slug, data = "ADSL", extra = "ADPP",
        note = note, height = 620L, code = paste0("library(tern)\nlibrary(ggplot2)\n", pp_setup, r"--(
result <- ggplot(adpp_plasma, aes(x = ARM, y = AVAL, fill = ARM)) +
  geom_boxplot(outlier.size = 0.8, width = 0.6) +
  facet_wrap(~PARAM, scales = "free_y") +
  labs(title = "Plasma PK parameters by treatment arm", x = NULL, y = "Value", fill = "Treatment") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank())
)--")),

    tlg("PKPG03", "Box Plot of Pharmacokinetic Parameters by Visit - Plasma", "Graph", topic, slug, data = "ADSL", extra = "ADPP",
        note = note, height = 620L, code = paste0("library(tern)\nlibrary(ggplot2)\n", pp_setup, r"--(
result <- ggplot(adpp_plasma, aes(x = AVISIT, y = AVAL, fill = ARM)) +
  geom_boxplot(outlier.size = 0.8, width = 0.7) +
  facet_wrap(~PARAM, scales = "free_y") +
  labs(title = "Plasma PK parameters by visit", x = "Visit", y = "Value", fill = "Treatment") +
  theme_minimal(base_size = 12)
)--")),

    tlg("PKPG04", "Bar Chart of Pharmacokinetic Parameters", "Graph", topic, slug, data = "ADSL", extra = "ADPP",
        note = note, height = 620L, code = paste0("library(tern)\nlibrary(ggplot2)\n", pp_setup, r"--(
means <- adpp_plasma %>%
  group_by(PARAM, ARM) %>%
  summarise(MEAN = mean(AVAL, na.rm = TRUE), SD = sd(AVAL, na.rm = TRUE), .groups = "drop")

result <- ggplot(means, aes(x = ARM, y = MEAN, fill = ARM)) +
  geom_col(width = 0.65) +
  geom_errorbar(aes(ymin = MEAN, ymax = MEAN + SD), width = 0.2) +
  facet_wrap(~PARAM, scales = "free_y") +
  labs(title = "Mean (+ SD) plasma PK parameters", x = NULL, y = "Mean", fill = "Treatment") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank())
)--")),

    tlg("PKPG06", "Box Plot of Pharmacokinetic Parameters by Visit - Urine", "Graph", topic, slug, data = "ADSL", extra = "ADPP",
        note = note, height = 620L, code = paste0("library(tern)\nlibrary(ggplot2)\n", pp_setup, r"--(
urine <- if (nrow(adpp_urine)) adpp_urine else adpp_plasma
result <- ggplot(urine, aes(x = AVISIT, y = AVAL, fill = ARM)) +
  geom_boxplot(outlier.size = 0.8, width = 0.7) +
  facet_wrap(~PARAM, scales = "free_y") +
  labs(title = "Urinary PK parameters by visit", x = "Visit", y = "Value", fill = "Treatment") +
  theme_minimal(base_size = 12)
)--"))
  )
}

#' @noRd
tlg_entries_ada <- function() {
  topic <- "Anti-Drug Antibodies"
  slug <- "ADA"
  note <- "ADAB is not part of the app's data: uses the random.cdisc.data example, restricted to the filtered patients; parameters are selected by name."

  ab_setup <- r"--(
adsl <- ADSL %>% filter(SAFFL == "Y") %>% df_explicit_na()
# ADAB is not part of the app's data: synthetic example data for the same patients
adab <- BlockR::tlg_example_data("ADAB", adsl) %>% df_explicit_na()
)--"

  # patient-level ADA results whose parameter name matches `pattern`
  ada_table <- function(pattern, label) {
    paste0("library(tern)\n", ab_setup, sprintf(r"--(
keep <- grepl("%s", adab$PARAM, ignore.case = TRUE)
if (!any(keep)) keep <- rep(TRUE, nrow(adab))
anl <- adab[keep, , drop = FALSE] %%>%%
  mutate(PARAM = droplevels(factor(PARAM)), AVALC = droplevels(factor(AVALC))) %%>%%
  group_by(USUBJID, PARAM) %%>%%
  slice(1) %%>%%
  ungroup()

lyt <- basic_table(show_colcounts = TRUE) %%>%%
  split_cols_by("ARM") %%>%%
  add_overall_col(label = "All Patients") %%>%%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "%s") %%>%%
  count_occurrences(vars = "AVALC") %%>%%
  append_topleft("  Result")

result <- build_table(lyt, df = anl, alt_counts_df = adsl) %%>%% prune_table()
)--", pattern, label))
  }

  list(
    tlg("ADAT01", "Baseline Prevalence and Incidence of Treatment Emergent ADA", "Table", topic, slug,
        data = "ADSL", extra = "ADAB", note = note, code = ada_table("baseline|emergent|incidence|prevalence", "ADA parameter")),
    tlg("ADAT02", "Summary of Patients with Treatment-Induced ADA", "Table", topic, slug,
        data = "ADSL", extra = "ADAB", note = note, code = ada_table("induced|enhanced|persistent|transient", "ADA parameter")),

    tlg("ADAT03", "Summary of Serum Concentrations at Timepoints Where ADA Samples Were Collected and Analyzed", "Table", topic, slug,
        data = "ADSL", extra = c("ADAB", "ADPC"), note = note, code = paste0("library(tern)\n", ab_setup, r"--(
adpc <- BlockR::tlg_example_data("ADPC", adsl) %>% df_explicit_na()
adpc <- adpc[!grepl("urine", adpc$PARAM, ignore.case = TRUE), , drop = FALSE] %>% filter(PARAM == PARAM[1])

# the example data may have no AVISIT: use VISIT, else one group for all visits
if (!"AVISIT" %in% names(adab)) adab$AVISIT <- if ("VISIT" %in% names(adab)) adab$VISIT else "All visits"
if (!"AVISIT" %in% names(adpc)) adpc$AVISIT <- if ("VISIT" %in% names(adpc)) adpc$VISIT else "All visits"

# concentrations at the visits with an ADA sample
ada_visits <- adab %>% distinct(USUBJID, AVISIT) %>% mutate(AVISIT = as.character(AVISIT))
anl <- adpc %>%
  mutate(AVISIT = as.character(AVISIT)) %>%
  semi_join(ada_visits, by = c("USUBJID", "AVISIT")) %>%
  mutate(AVISIT = factor(AVISIT))
if (!nrow(anl)) anl <- adpc %>% mutate(AVISIT = factor(AVISIT))

lyt <- basic_table() %>%
  split_rows_by("ARM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Treatment Group") %>%
  split_rows_by("AVISIT", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Visit") %>%
  analyze_vars_in_cols(
    vars = "AVAL",
    .stats = c("n", "mean", "sd", "median", "min", "max", "cv", "geom_mean"),
    .labels = c(
      n = "n", mean = "Mean", sd = "SD", median = "Median", min = "Minimum", max = "Maximum",
      cv = "CV (%)", geom_mean = "Geometric Mean"
    )
  )

result <- build_table(lyt, df = anl)
)--")),

    tlg("ADAT04A", "Baseline Prevalence and Incidence of Treatment Emergent Neutralizing Antibodies", "Table", topic, slug,
        data = "ADSL", extra = "ADAB", note = note, code = ada_table("neutraliz|NAb", "Neutralizing antibody parameter")),
    tlg("ADAT04B", "Baseline Prevalence and Incidence of Treatment Emergent Neutralizing Antibodies (by Visit)", "Table", topic, slug,
        data = "ADSL", extra = "ADAB", note = note, code = paste0("library(tern)\n", ab_setup, r"--(
# the example data may have no AVISIT: use VISIT, else one group for all visits
if (!"AVISIT" %in% names(adab)) adab$AVISIT <- if ("VISIT" %in% names(adab)) adab$VISIT else "All visits"
keep <- grepl("neutraliz|NAb", adab$PARAM, ignore.case = TRUE)
if (!any(keep)) keep <- rep(TRUE, nrow(adab))
anl <- adab[keep, , drop = FALSE] %>%
  mutate(PARAM = droplevels(factor(PARAM)), AVALC = droplevels(factor(AVALC)), AVISIT = droplevels(factor(AVISIT)))

lyt <- basic_table(show_colcounts = TRUE) %>%
  split_cols_by("ARM") %>%
  split_rows_by("PARAM", split_fun = drop_split_levels, label_pos = "topleft", split_label = "Parameter") %>%
  split_rows_by("AVISIT", split_fun = drop_split_levels, label_pos = "topleft", split_label = "  Visit") %>%
  count_occurrences(vars = "AVALC") %>%
  append_topleft("    Result")

result <- build_table(lyt, df = anl, alt_counts_df = adsl) %>% prune_table()
)--")),

    tlg("ADAL01", "Listing of Anti-Drug Antibody Data", "Listing", topic, slug, data = "ADSL", extra = "ADAB", note = note,
        code = paste0("library(tern)\nlibrary(rlistings)\n", ab_setup, r"--(
out <- adab %>%
  select(any_of(c("ARM", "USUBJID", "AVISIT", "PARAM", "AVALC", "AVAL", "AVALU"))) %>%
  arrange(ARM, USUBJID) %>%
  var_relabel(ARM = "Treatment Group", USUBJID = "Patient ID")

result <- as_listing(out, key_cols = c("ARM", "USUBJID"), disp_cols = names(out))
)--")),

    tlg("ADAL02", "Listing of Anti-Drug Antibody Data for Treatment Emergent ADA Positive Patients", "Listing", topic, slug,
        data = "ADSL", extra = "ADAB", note = note, code = paste0("library(tern)\nlibrary(rlistings)\n", ab_setup, r"--(
# patients with a positive treatment-emergent ADA result
positive <- adab %>%
  filter(
    grepl("emergent", PARAM, ignore.case = TRUE),
    toupper(as.character(AVALC)) %in% c("Y", "YES", "POSITIVE")
  ) %>%
  distinct(USUBJID)
anl <- if (nrow(positive)) semi_join(adab, positive, by = "USUBJID") else adab

out <- anl %>%
  select(any_of(c("ARM", "USUBJID", "AVISIT", "PARAM", "AVALC", "AVAL", "AVALU"))) %>%
  arrange(ARM, USUBJID) %>%
  var_relabel(ARM = "Treatment Group", USUBJID = "Patient ID")

result <- as_listing(out, key_cols = c("ARM", "USUBJID"), disp_cols = names(out))
)--"))
  )
}
