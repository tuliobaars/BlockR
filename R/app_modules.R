# Module configuration. The home page is the canvas (R/canvas.R); then the
# TLG Catalog (one page, every output), the shared data pages, and the
# exploratory and safety teal.gallery templates (inst/templates/) as tab
# groups. Module calls are kept as in the templates; see the NOTE comments
# for the intentional changes.
#
# The exploratory plot modules are created through blockr_plot_module()
# (R/plot_theme.R), which only adds the app's default ggplot2 theme
# (option blockr.ggtheme) to modules that support it.

#' All app modules
#' @param data The app's `teal_data` (see [build_blockr_data()]).
#' @noRd
blockr_modules <- function(data) {
  source <- data[[".blockr"]]$source
  all_modules <- modules(
    # the Tutorial is a button on the canvas page (a second canvas, with lessons)
    tm_blockr_canvas("Canvas", datanames = names(data)),
    module_tlg_catalog(datanames = names(data)),
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser"),
    modules_exploratory(blockr_file_viewer_paths(source)),
    modules_safety(data)
  )
  labels <- leaf_labels(all_modules)
  if (anyDuplicated(labels)) {
    stop("Module labels must be unique: ", toString(unique(labels[duplicated(labels)])), call. = FALSE)
  }
  all_modules
}

#' Files shown by the File viewer: the parquet data folder when that source is
#' used, otherwise the installed package files (always present).
#' @noRd
blockr_file_viewer_paths <- function(source) {
  if (identical(source$source, "parquet")) {
    list("Parquet data folder" = source$parquet_dir)
  } else {
    list("BlockR package files" = system.file(package = "BlockR"))
  }
}

#' Exploratory Analysis (from inst/templates/app_exploratory.R)
#' @noRd
modules_exploratory <- function(file_viewer_paths) {
  pick_adsl_age <- picks(
    datasets("ADSL", "ADSL"),
    variables(selected = "AGE", multiple = FALSE)
  )
  pick_adsl_bmrkr1 <- picks(
    datasets("ADSL", "ADSL"),
    variables(selected = "BMRKR1", multiple = FALSE)
  )
  pick_adsl_armcd <- picks(
    datasets("ADSL", "ADSL"),
    variables(selected = "ARMCD", multiple = FALSE)
  )
  pick_adsl_strata2 <- picks(
    datasets("ADSL", "ADSL"),
    variables(is.factor, selected = "STRATA2", multiple = FALSE)
  )
  pick_adsl_armcd_multi <- picks(
    datasets("ADSL", "ADSL"),
    variables(selected = "ARMCD", multiple = TRUE)
  )
  pick_adsl_numeric_bmrkr1 <- picks(
    datasets("ADSL", "ADSL"),
    variables(is.numeric, selected = "BMRKR1", multiple = FALSE)
  )
  pick_adsl_factor <- picks(
    datasets("ADSL", "ADSL"),
    variables(is.factor, selected = NULL, multiple = FALSE)
  )
  pick_adsl_multi <- picks(
    datasets("ADSL", "ADSL"),
    variables(selected = c("AGE", "BMRKR1"), multiple = TRUE)
  )

  pick_adrs_response <- picks(
    datasets("ADRS", "ADRS"),
    variables(choices = c("AVALC", "AVAL"), selected = "AVALC", multiple = FALSE)
  )
  pick_adrs_response_fct <- picks(
    datasets("ADRS", "ADRS"),
    variables(choices = is.factor, selected = "AVALC", multiple = FALSE)
  )

  pick_adlb_aval <- picks(
    datasets("ADLB", "ADLB"),
    variables(choices = c("AVAL", "CHG", "PCHG", "ANRIND", "BASE"), selected = "AVAL", multiple = FALSE)
  )
  pick_adlb_outlier <- picks(
    datasets("ADLB", "ADLB"),
    variables(choices = c("AVAL", "CHG", "PCHG", "BASE"), selected = "AVAL", multiple = FALSE)
  )
  pick_adlb_categorical <- picks(
    datasets("ADLB", "ADLB"),
    variables(choices = c("PARAM", "PARAMCD"), selected = NULL, multiple = FALSE)
  )

  pick_adlbpca <- picks(
    datasets("ADLBPCA", "ADLBPCA"),
    variables(
      choices = is.numeric,
      selected = c("ALT - WEEK 5 DAY 36", "CRP - WEEK 5 DAY 36", "IGA - WEEK 5 DAY 36"),
      multiple = TRUE
    )
  )

  adrs_endpoint_filter <- teal_transform_filter(
    picks(
      datasets("ADRS", "ADRS"),
      variables(choices = "PARAMCD", selected = "PARAMCD", multiple = FALSE, fixed = TRUE),
      values(choices = c("BESRSPI", "INVET"), selected = "BESRSPI", multiple = FALSE)
    ),
    label = "Choose endpoint"
  )
  adlb_lab_filter <- teal_transform_filter(
    picks(
      datasets("ADLB", "ADLB"),
      variables(choices = "PARAMCD", selected = "PARAMCD", multiple = FALSE, fixed = TRUE),
      values(selected = "ALT", multiple = FALSE)
    ),
    label = "Select lab"
  )
  adlb_visit_filter <- teal_transform_filter(
    picks(
      datasets("ADLB", "ADLB"),
      variables(choices = "AVISIT", selected = "AVISIT", multiple = FALSE, fixed = TRUE),
      values(selected = "SCREENING", multiple = FALSE)
    ),
    label = "Select visit"
  )

  modules(
    label = "Exploratory Analysis",
    # NOTE: the template pointed at 3 remote demo files (checked over the network
    # at every start-up) and a non-existent ./packrat/desc/ folder.
    tm_file_viewer(
      label = "File viewer",
      input_path = file_viewer_paths
    ),
    tm_missing_data("Missing Data"),
    blockr_plot_module(
      tm_g_distribution,
      "Distribution",
      dist_var = pick_adsl_numeric_bmrkr1,
      strata_var = pick_adsl_factor,
      group_var = pick_adsl_factor
    ),
    blockr_plot_module(
      tm_outliers,
      "Outliers",
      outlier_var = pick_adlb_outlier,
      categorical_var = pick_adlb_categorical
    ),
    blockr_plot_module(
      tm_g_association,
      label = "Association",
      ref = pick_adsl_age,
      vars = pick_adsl_armcd_multi
    ),
    # NOTE: the template fixed ggtheme = "gray"; the app-wide default is used instead
    blockr_plot_module(
      tm_g_bivariate,
      label = "Bivariate Plots",
      x = pick_adsl_age,
      y = pick_adlb_aval,
      row_facet = pick_adsl_factor,
      col_facet = pick_adsl_factor,
      use_density = FALSE,
      plot_height = c(600L, 200L, 2000L),
      transformators = list(adlb_lab_filter, adlb_visit_filter)
    ),
    blockr_plot_module(
      tm_a_regression,
      label = "Regression",
      response = pick_adsl_numeric_bmrkr1,
      regressor = pick_adrs_response,
      transformators = list(adrs_endpoint_filter)
    ),
    blockr_plot_module(
      tm_g_response,
      label = "Response Plot",
      response = pick_adrs_response_fct,
      x = pick_adsl_strata2,
      row_facet = pick_adsl_factor,
      col_facet = pick_adsl_factor,
      coord_flip = FALSE,
      transformators = list(adrs_endpoint_filter)
    ),
    tm_g_scatterplotmatrix(
      label = "Scatterplot Matrix",
      variables = list(pick_adsl_multi)
    ),
    # NOTE: smaller, slightly transparent points (template: size = 3, alpha = 1)
    # so overlapping subjects stay visible
    blockr_plot_module(
      tm_g_scatterplot,
      "Scatterplot",
      x = pick_adsl_age,
      y = pick_adsl_bmrkr1,
      row_facet = pick_adsl_factor,
      col_facet = pick_adsl_factor,
      color_by = pick_adsl_factor,
      size = 2.5, alpha = 0.8,
      plot_height = c(600L, 200L, 2000L)
    ),
    tm_t_crosstable(
      "Table Choices",
      x = pick_adsl_strata2,
      y = pick_adsl_armcd
    ),
    blockr_plot_module(
      tm_a_pca,
      "Principal Component Analysis",
      dat = pick_adlbpca,
      plot_height = c(600L, 200L, 2000L),
      plot_width = c(600L, 200L, 2000L)
    )
  )
}

#' Safety Analysis (from inst/templates/app_safety.R)
#' @noRd
modules_safety <- function(data) {
  ADSL <- data[["ADSL"]]
  ADAE <- data[["ADAE"]]

  # Safety uses the *actual* arm
  arm_vars_safety <- c("ACTARMCD", "ACTARM")
  date_vars_adsl <- names(ADSL)[vapply(ADSL, function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")), logical(1))]
  demog_vars_adsl <- names(ADSL)[!(names(ADSL) %in% c("USUBJID", "STUDYID", date_vars_adsl))]

  ae_anl_vars <- names(ADAE)[startsWith(names(ADAE), "TMPFL_")]
  # flag variables for AE baskets; set to NULL if not applicable to study
  aesi_vars <- names(ADAE)[startsWith(names(ADAE), "TMP_SMQ") | startsWith(names(ADAE), "TMP_CQ")]

  modules(
    label = "Safety Analysis",
    tm_t_summary(
      label = "Demographic Table (Safety)",
      dataname = "ADSL",
      arm_var = variables(arm_vars_safety, "ACTARM"),
      summarize_vars = variables(demog_vars_adsl, c("SEX", "AGE", "RACE"))
    ),
    modules(
      label = "Adverse Events",
      tm_t_events_summary(
        label = "AE Summary",
        dataname = "ADAE",
        arm_var = variables(arm_vars_safety, "ACTARM"),
        flag_var_anl = variables(ae_anl_vars, ae_anl_vars, multiple = TRUE),
        flag_var_aesi = variables(aesi_vars, aesi_vars, multiple = TRUE),
        add_total = TRUE
      ),
      tm_t_events(
        label = "AE by Term",
        dataname = "ADAE",
        arm_var = variables(arm_vars_safety, "ACTARM"),
        llt = variables(c("AETERM", "AEDECOD"), "AEDECOD"),
        hlt = variables(c("AEBODSYS", "AESOC"), "AEBODSYS"),
        add_total = TRUE,
        event_type = "adverse event"
      ),
      tm_t_events_by_grade(
        label = "AE Table by Grade",
        dataname = "ADAE",
        arm_var = variables(arm_vars_safety, "ACTARM"),
        llt = variables("AEDECOD", "AEDECOD"),
        hlt = variables(c("AEBODSYS", "AESOC"), "AEBODSYS"),
        grade = variables("AETOXGR", "AETOXGR"),
        add_total = TRUE
      ),
      tm_t_events_patyear(
        label = "AE Rates Adjusted for Patient-Years at Risk",
        dataname = "ADAETTE",
        arm_var = variables(arm_vars_safety, "ACTARM"),
        paramcd = picks(
          variables("PARAMCD", "PARAMCD"),
          values(selected = "AETTE1", multiple = FALSE),
          check_dataset = FALSE
        ),
        events_var = variables("n_events", "n_events")
      ),
      tm_t_smq(
        label = "Adverse Events by SMQ Table",
        dataname = "ADAE",
        arm_var = variables(c(arm_vars_safety, "SEX"), "ACTARM"),
        add_total = FALSE,
        baskets = variables(
          function(x) grepl("^(SMQ|CQ).*NAM$", names(parent.frame()$X)[[parent.frame()$i]]),
          dplyr::everything(),
          multiple = TRUE
        ),
        scopes = variables(
          function(x) grepl("^SMQ.*SC$", names(parent.frame()$X)[[parent.frame()$i]]),
          dplyr::everything(),
          multiple = TRUE,
          fixed = TRUE
        ),
        llt = variables("AEDECOD", "AEDECOD")
      )
    ),
    modules(
      label = "Lab Tables",
      tm_t_summary_by(
        label = "Labs Summary",
        dataname = "ADLB",
        arm_var = variables(arm_vars_safety, "ACTARM"),
        by_vars = variables(c("PARAM", "AVISIT"), c("PARAM", "AVISIT"), multiple = TRUE, fixed = TRUE),
        summarize_vars = variables(c("AVAL", "CHG"), "AVAL"),
        paramcd = picks(
          variables("PARAMCD", "PARAMCD"),
          values(selected = "ALT", multiple = TRUE),
          check_dataset = FALSE
        )
      ),
      tm_t_shift_by_grade(
        label = "Grade Laboratory Abnormality Table",
        dataname = "ADLB",
        arm_var = variables(c("ACTARMCD", "ACTARM"), "ACTARM"),
        paramcd = picks(
          variables("PARAMCD", "PARAMCD"),
          values(selected = "ALT", multiple = TRUE),
          check_dataset = FALSE
        ),
        worst_flag_var = variables(c("WGRLOVFL", "WGRLOFL", "WGRHIVFL", "WGRHIFL"), "WGRLOVFL"),
        worst_flag_indicator = teal.picks::values("Y", "Y", fixed = TRUE, multiple = FALSE),
        anl_toxgrade_var = variables("ATOXGR", "ATOXGR"),
        base_toxgrade_var = variables("BTOXGR", "BTOXGR"),
        add_total = FALSE
      ),
      tm_t_abnormality_by_worst_grade(
        label = "Laboratory test results with highest grade post-baseline",
        dataname = "ADLB",
        arm_var = variables(c("ARM", "ARMCD"), "ARM"),
        paramcd = picks(
          variables("PARAMCD", "PARAMCD"),
          values(selected = c("ALT", "CRP", "IGA")),
          check_dataset = FALSE
        ),
        add_total = FALSE
      )
    ),
    modules(
      label = "Exposure",
      tm_t_summary_by(
        label = "Exposure Summary",
        dataname = "ADEX",
        arm_var = variables(arm_vars_safety, "ACTARM"),
        by_vars = variables(c("PARCAT2", "PARAM"), c("PARCAT2", "PARAM"), fixed = TRUE),
        summarize_vars = variables("AVAL", "AVAL", fixed = TRUE),
        paramcd = picks(
          variables("PARAMCD", "PARAMCD"),
          values(selected = "TDOSE", multiple = TRUE),
          check_dataset = FALSE
        ),
        denominator = values(c("n", "N", "omit"), "n")
      ),
      tm_t_exposure(
        label = "Duration of Exposure Table",
        dataname = "ADEX",
        paramcd = picks(
          variables("PARAMCD", "PARAMCD"),
          values(selected = "TDURD", multiple = FALSE),
          check_dataset = FALSE
        ),
        col_by_var = variables(c(arm_vars_safety, "SEX"), "SEX"),
        row_by_var = variables(c("RACE", "REGION1", "STRATA1", "SEX"), "RACE"),
        parcat = picks(
          variables("PARCAT2", "PARCAT2"),
          values(selected = "Drug A"),
          check_dataset = FALSE
        ),
        add_total = FALSE
      )
    ),
    tm_t_abnormality(
      label = "Vital Signs Abnormality",
      dataname = "ADVS",
      arm_var = variables(arm_vars_safety, "ACTARM"),
      id_var = variables("USUBJID", "USUBJID", fixed = TRUE),
      by_vars = variables(c("PARAM", "AVISIT"), "PARAM", multiple = TRUE),
      grade = variables("ANRIND", "ANRIND", fixed = TRUE),
      abnormal = list(low = "LOW", high = "HIGH")
    ),
    tm_t_mult_events(
      label = "Concomitant Medication",
      dataname = "ADCM",
      arm_var = variables(arm_vars_safety, "ACTARM"),
      seq_var = variables("CMSEQ", "CMSEQ", fixed = TRUE),
      hlt = variables("ATC2", "ATC2", fixed = TRUE),
      llt = variables("CMDECOD", "CMDECOD", fixed = TRUE),
      add_total = TRUE,
      event_type = "treatment"
    ),
    tm_t_shift_by_arm(
      label = "ECG Shift Table by Arm",
      dataname = "ADEG",
      arm_var = variables(arm_vars_safety, "ACTARM"),
      paramcd = picks(
        variables("PARAMCD", "PARAMCD"),
        values(selected = "HR", multiple = FALSE),
        check_dataset = FALSE
      ),
      visit_var = picks(
        variables("AVISIT", "AVISIT"),
        values(selected = "POST-BASELINE MINIMUM"),
        check_dataset = FALSE
      ),
      aval_var = variables("ANRIND", "ANRIND", fixed = TRUE),
      baseline_var = variables("BNRIND", "BNRIND", fixed = TRUE)
    ),
    tm_g_lineplot(
      label = "Line Plot",
      dataname = "ADLB",
      group_var = variables(arm_vars_safety, "ACTARM"),
      x = variables("AVISIT", "AVISIT", fixed = TRUE),
      y = variables(c("AVAL", "BASE", "CHG", "PCHG"), "AVAL"),
      y_unit = variables("AVALU", "AVALU", fixed = TRUE),
      paramcd = picks(
        variables("PARAMCD", "PARAMCD", fixed = TRUE),
        values(selected = "ALT", multiple = FALSE),
        check_dataset = FALSE
      ),
      plot_height = c(1000L, 200L, 4000L)
    )
  )
}

#' Labels of all leaf modules inside a (nested) teal_modules object
#' @noRd
leaf_labels <- function(x) {
  if (inherits(x, "teal_modules")) {
    unlist(lapply(x$children, leaf_labels), use.names = FALSE)
  } else {
    x$label
  }
}
