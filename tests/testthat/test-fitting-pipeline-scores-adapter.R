#===============================================================================
#
#                       test-fitting-pipeline-scores-adapter.R
#
#===============================================================================

#  Checkpoint 4 tests (plan §5/§6): bind_fitting_scores() (the legacy
#  all_fitting_scores_df adapter) and run_output_error_fit() (the
#  orchestrator: fit + bind scores + the save_final_model gate).
#
#  Run via testthat::test_file() after sourcing R/provenance_helpers.R,
#  R/v1_paper_3_fitting_functions.R, R/v1_paper_3_plotting_and_evaluation_functions.R,
#  R/v1_paper_3_utility_functions.R, and R/v1_paper_9_fitting_and_eval_pipeline.R,
#  plus library(dataprov) / library(DBI) / library(here) / library(tidymodels),
#  per the convention in
#  "planning/dataprov integration/DONE - dataprov_planning_decisions_checkpoint - 2026 07 13.md".

proj_dir_for_tests = here::here ()

golden_registry_dir_for_tests =
    file.path (proj_dir_for_tests, "tests", "fixtures", "dataprov_registry")

body_rmd_path_for_tests =
    file.path (proj_dir_for_tests, "Paper_9_heavily_abridged_version_of_p8",
              "p9_v01_all_combined__body.Rmd")

resolve_golden_fixture <- function (reg, tib_name, ext = "rds")
    {
    cand = dataprov::prov_list (reg, tags = list (tib = tib_name,
                                                  file_extension = ext))
    cand = cand [cand$status == "finalized", ]

    if (nrow (cand) != 1)
        stop (paste0 ("Expected exactly one finalized golden record for tib '",
                      tib_name, "', found ", nrow (cand), "."))

    resolve_prov_file (reg, cand$session_uuid [1], tib_name, ext = ext,
                       verify = TRUE)
    }

extract_rmd_chunk <- function (rmd_path, label)
    {
    lines = readLines (rmd_path, warn = FALSE)

    trimmed = trimws (lines)
    start_idx = which (startsWith (trimmed, paste0 ("```{r ", label, ",")) |
                       (trimmed == paste0 ("```{r ", label, "}")))

    if (length (start_idx) != 1)
        stop (paste0 ("Expected exactly one chunk labeled '", label,
                      "' in ", rmd_path, ", found ", length (start_idx), "."))

    close_offset = which (lines [(start_idx + 1):length (lines)] == "```") [1]
    end_idx = start_idx + close_offset

    paste (lines [(start_idx + 1):(end_idx - 1)], collapse = "\n")
    }

extract_feature_set <- function (rmd_path, chunk_label)
    {
    chunk_env = new.env ()
    chunk_env$fitting_model_str = "lm"

    eval (parse (text = extract_rmd_chunk (rmd_path, chunk_label)), envir = chunk_env)

    list (vars_used_str = chunk_env$vars_used_str, inVars = chunk_env$inVars)
    }

legacy_cols_for_tests =
    c ("train_or_test", "fitting_model_str", "vars_used_str", "measure_name_str",
      "rs_method_name", "rmse", "R2", "adj_R2")

sort_key_for_tests <- function (df)
    order (df$vars_used_str, df$measure_name_str, df$rs_method_name, df$train_or_test)

#===============================================================================

test_that ("bind_fitting_scores() projects bundle$metrics onto the exact legacy column shape",
    {
    fake_metrics = data.frame (
        rs_name = c ("ILP", "SA"), ds_label = c ("TRAIN", "TEST"), fold_id = c (1, 1),
        adj_R2 = c (0.1, 0.2), rmse = c (0.3, 0.4), R2 = c (0.5, 0.6),
        num_predictors = c (2, 2), feature_set_label = "PUsAndSppOnly",
        error_type_label = "abs_rep_shortfall_resid", fitting_model_str = "lm",
        stringsAsFactors = FALSE)
    fake_bundle = structure (list (metrics = fake_metrics), class = "bdpg_fit_result")

    out = bind_fitting_scores (NULL, fake_bundle)

    expect_equal (names (out), legacy_cols_for_tests)
    expect_equal (nrow (out), 2)
    expect_equal (out$train_or_test, c ("TRAIN", "TEST"))
    expect_equal (out$rs_method_name, c ("ILP", "SA"))
    expect_equal (out$vars_used_str, c ("PUsAndSppOnly", "PUsAndSppOnly"))
    expect_equal (out$measure_name_str, c ("abs_rep_shortfall_resid", "abs_rep_shortfall_resid"))
    expect_equal (out$rmse, c (0.3, 0.4))
    expect_equal (out$R2, c (0.5, 0.6))
    expect_equal (out$adj_R2, c (0.1, 0.2))
    })

test_that ("bind_fitting_scores() accumulates across repeated calls",
    {
    fake_metrics_1 = data.frame (
        rs_name = "ILP", ds_label = "TRAIN", fold_id = 1, adj_R2 = 0.1, rmse = 0.3, R2 = 0.5,
        num_predictors = 2, feature_set_label = "PUsAndSppOnly",
        error_type_label = "abs_rep_shortfall_resid", fitting_model_str = "lm",
        stringsAsFactors = FALSE)
    fake_metrics_2 = data.frame (
        rs_name = "SA", ds_label = "TEST", fold_id = 1, adj_R2 = 0.2, rmse = 0.4, R2 = 0.6,
        num_predictors = 5, feature_set_label = "ProbSizeAndDensity",
        error_type_label = "abs_sol_cost_err_resid", fitting_model_str = "lm",
        stringsAsFactors = FALSE)
    bundle_1 = structure (list (metrics = fake_metrics_1), class = "bdpg_fit_result")
    bundle_2 = structure (list (metrics = fake_metrics_2), class = "bdpg_fit_result")

    scores = bind_fitting_scores (NULL, bundle_1)
    scores = bind_fitting_scores (scores, bundle_2)

    expect_equal (nrow (scores), 2)
    expect_equal (scores$rs_method_name, c ("ILP", "SA"))
    })

test_that ("bind_fitting_scores() treats a 0-row starting frame the same as NULL",
    {
    fake_metrics = data.frame (
        rs_name = "ILP", ds_label = "TRAIN", fold_id = 1, adj_R2 = 0.1, rmse = 0.3, R2 = 0.5,
        num_predictors = 2, feature_set_label = "PUsAndSppOnly",
        error_type_label = "abs_rep_shortfall_resid", fitting_model_str = "lm",
        stringsAsFactors = FALSE)
    fake_bundle = structure (list (metrics = fake_metrics), class = "bdpg_fit_result")

    zero_row_legacy_df = data.frame (test_or_train = character (0))    #  mirrors the Rmd's own init

    out = bind_fitting_scores (zero_row_legacy_df, fake_bundle)

    expect_equal (names (out), legacy_cols_for_tests)
    expect_equal (nrow (out), 1)
    })

#===============================================================================

reg_for_tests = dataprov::prov_registry_open (golden_registry_dir_for_tests)

p3_working_train_df_golden = readRDS (resolve_golden_fixture (reg_for_tests, "p3_working_train_df"))
p3_working_test_df_golden  = readRDS (resolve_golden_fixture (reg_for_tests, "p3_working_test_df"))
p3_train_aux_df_golden     = readRDS (resolve_golden_fixture (reg_for_tests, "p3_train_aux_df"))
p3_test_aux_df_golden      = readRDS (resolve_golden_fixture (reg_for_tests, "p3_test_aux_df"))
all_fitting_scores_df_golden = readRDS (resolve_golden_fixture (reg_for_tests, "all_fitting_scores_df"))

DBI::dbDisconnect (reg_for_tests$conn)

rs_method_names_list_for_tests = c ("ILP", "SA", "UR_Forward", "SA_SS")

feature_set_chunk_labels_for_tests =
    c ("setPUsAndSppOnlyParams", "setProbSizeAndDensityParams",
      "setNonLatapyGraphParams", "setEverythingParams")

feature_sets_for_tests =
    lapply (feature_set_chunk_labels_for_tests,
           function (lbl) extract_feature_set (body_rmd_path_for_tests, lbl))

error_types_for_tests =
    list (
        list (target_col_name  = "rsr_COR_spp_rep_shortfall",
             measure_name_str = "abs_rep_shortfall_resid"),
        list (target_col_name  = "rs_solution_cost_err_frac",
             measure_name_str = "abs_sol_cost_err_resid")
        )

build_params_for_tests = list (write_tibs_to_csv = FALSE, VERBOSE_LM_CATS = FALSE)

test_that ("run_output_error_fit() with save_final_model = FALSE does not error and skips silently",
    {
    xy = build_feature_set_specific_test_and_train (
        working_train_df = p3_working_train_df_golden,
        working_test_df  = p3_working_test_df_golden,
        feature_sets_for_tests [[1]]$inVars, build_params_for_tests,
        include_median_redundancies = FALSE)

    recipe_template_df = xy$p3_train_x_df %>% dplyr::select (-rs_method_name)
    recipe_template_df [["rsr_COR_spp_rep_shortfall"]] =
        p3_working_train_df_golden [["rsr_COR_spp_rep_shortfall"]]

    result =
        run_output_error_fit (
            all_fitting_scores_df = NULL,
            rs_method_names_list_for_tests,
            train_x_df = xy$p3_train_x_df, test_x_df = xy$p3_test_x_df,
            working_train_df = p3_working_train_df_golden,
            working_test_df  = p3_working_test_df_golden,
            train_aux_df = p3_train_aux_df_golden, test_aux_df = p3_test_aux_df_golden,
            target_col_name = "rsr_COR_spp_rep_shortfall",
            recipe       = make_bdpg_recipe (recipe_template_df, "rsr_COR_spp_rep_shortfall"),
            learner_spec = make_bdpg_learner ("lm"), learner_id = "lm",
            vars_used_str = "PUsAndSppOnly", measure_name_str = "abs_rep_shortfall_resid",
            fitting_model_str = "lm",
            save_final_model = FALSE)

    expect_named (result, c ("bundle", "all_fitting_scores_df"))
    expect_s3_class (result$bundle, "bdpg_fit_result")
    expect_equal (names (result$all_fitting_scores_df), legacy_cols_for_tests)
    expect_equal (nrow (result$all_fitting_scores_df), 8)
    })

test_that ("run_output_error_fit() with save_final_model = TRUE hits the stubbed gate without error",
    {
    xy = build_feature_set_specific_test_and_train (
        working_train_df = p3_working_train_df_golden,
        working_test_df  = p3_working_test_df_golden,
        feature_sets_for_tests [[1]]$inVars, build_params_for_tests,
        include_median_redundancies = FALSE)

    recipe_template_df = xy$p3_train_x_df %>% dplyr::select (-rs_method_name)
    recipe_template_df [["rsr_COR_spp_rep_shortfall"]] =
        p3_working_train_df_golden [["rsr_COR_spp_rep_shortfall"]]

    expect_no_error (
        run_output_error_fit (
            all_fitting_scores_df = NULL,
            rs_method_names_list_for_tests,
            train_x_df = xy$p3_train_x_df, test_x_df = xy$p3_test_x_df,
            working_train_df = p3_working_train_df_golden,
            working_test_df  = p3_working_test_df_golden,
            train_aux_df = p3_train_aux_df_golden, test_aux_df = p3_test_aux_df_golden,
            target_col_name = "rsr_COR_spp_rep_shortfall",
            recipe       = make_bdpg_recipe (recipe_template_df, "rsr_COR_spp_rep_shortfall"),
            learner_spec = make_bdpg_learner ("lm"), learner_id = "lm",
            vars_used_str = "PUsAndSppOnly", measure_name_str = "abs_rep_shortfall_resid",
            fitting_model_str = "lm",
            save_final_model = TRUE)
        )
    })

#===============================================================================

test_that ("run_output_error_fit() + bind_fitting_scores() reproduce the full all_fitting_scores_df column-for-column",
    {
    accumulated_scores = NULL

    for (fs in feature_sets_for_tests)
        {
        xy = build_feature_set_specific_test_and_train (
            working_train_df = p3_working_train_df_golden,
            working_test_df  = p3_working_test_df_golden,
            fs$inVars, build_params_for_tests, include_median_redundancies = FALSE)

        for (et in error_types_for_tests)
            {
            recipe_template_df = xy$p3_train_x_df %>% dplyr::select (-rs_method_name)
            recipe_template_df [[et$target_col_name]] =
                p3_working_train_df_golden [[et$target_col_name]]

            result =
                run_output_error_fit (
                    all_fitting_scores_df = accumulated_scores,
                    rs_method_names_list_for_tests,
                    train_x_df = xy$p3_train_x_df, test_x_df = xy$p3_test_x_df,
                    working_train_df = p3_working_train_df_golden,
                    working_test_df  = p3_working_test_df_golden,
                    train_aux_df = p3_train_aux_df_golden, test_aux_df = p3_test_aux_df_golden,
                    target_col_name = et$target_col_name,
                    recipe       = make_bdpg_recipe (recipe_template_df, et$target_col_name),
                    learner_spec = make_bdpg_learner ("lm"), learner_id = "lm",
                    vars_used_str = fs$vars_used_str, measure_name_str = et$measure_name_str,
                    fitting_model_str = "lm")

            accumulated_scores = result$all_fitting_scores_df
            }
        }

    expect_equal (names (accumulated_scores), legacy_cols_for_tests)
    expect_equal (names (accumulated_scores), names (all_fitting_scores_df_golden))
    expect_equal (nrow (accumulated_scores), nrow (all_fitting_scores_df_golden))
    expect_equal (nrow (accumulated_scores), 64)

        #  Row order is NOT preserved (see bind_fitting_scores()'s header
        #  comment) -- sort both sides onto a canonical key before comparing.
    new_sorted    = accumulated_scores [sort_key_for_tests (accumulated_scores), ]
    golden_sorted = all_fitting_scores_df_golden [sort_key_for_tests (all_fitting_scores_df_golden), ]

    expect_equal (new_sorted$train_or_test, golden_sorted$train_or_test)
    expect_equal (new_sorted$fitting_model_str, golden_sorted$fitting_model_str)
    expect_equal (new_sorted$vars_used_str, golden_sorted$vars_used_str)
    expect_equal (new_sorted$measure_name_str, golden_sorted$measure_name_str)
    expect_equal (new_sorted$rs_method_name, golden_sorted$rs_method_name)
    expect_equal (new_sorted$rmse, golden_sorted$rmse)
    expect_equal (new_sorted$R2, golden_sorted$R2)
    expect_equal (new_sorted$adj_R2, golden_sorted$adj_R2)
    })
