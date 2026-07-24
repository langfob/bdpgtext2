#===============================================================================
#
#                       test-fitting-pipeline-lm-equivalence.R
#
#===============================================================================

#  Checkpoint 2 golden-master equivalence test (see
#  planning/bdpg_fitting_refactor_plan.md §5, §6 Checkpoint 2).  For every
#  (feature set x error type) call site in
#  Paper_9_heavily_abridged_version_of_p8/p9_v01_all_combined__body.Rmd,
#  reproduces fit_output_error_for_feature_set()'s metrics against the
#  committed full-batch metric golden (tests/fixtures/golden/all_fitting_scores_df.rds,
#  captured from the OLD pipeline -- see the 2026-07-23 DECISIONS.md entries)
#  within testthat's default tolerance.
#
#  The 4 feature sets' inVars are NOT hand-transcribed here: they are
#  extracted and eval()'d directly from the Rmd's own
#  set<FeatureSet>Params chunks, so this test cannot silently drift from the
#  actual document (plan §12: "read them from the Rmd call sites... rather
#  than hardcoding").
#
#  Run via testthat::test_file() after sourcing R/provenance_helpers.R,
#  R/v1_paper_3_fitting_functions.R, R/v1_paper_3_plotting_and_evaluation_functions.R,
#  and R/v1_paper_9_fitting_and_eval_pipeline.R, plus library(dataprov) /
#  library(DBI) / library(here) / library(tidymodels), per the convention in
#  "planning/dataprov integration/DONE - dataprov_planning_decisions_checkpoint - 2026 07 13.md".

proj_dir_for_tests = here::here ()

golden_registry_dir_for_tests =
    file.path (proj_dir_for_tests, "tests", "fixtures", "dataprov_registry")

body_rmd_path_for_tests =
    file.path (proj_dir_for_tests, "Paper_9_heavily_abridged_version_of_p8",
              "p9_v01_all_combined__body.Rmd")

    #  Same single-finalized-record assumption as
    #  test-golden-master-capture.R -- see that file's comment for why it is
    #  safe for this dedicated registry.
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

        #  A chunk header is either "```{r label, <options>}" or, with no
        #  options, bare "```{r label}" -- e.g. setPUsAndSppOnlyParams.
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

    #  Evaluates one set<FeatureSet>Params chunk in an isolated environment
    #  and pulls out vars_used_str / inVars.  fitting_model_str is stubbed
    #  because the chunk also computes source_string = paste0(vars_used_str,
    #  "_", fitting_model_str), which this test does not use.
extract_feature_set <- function (rmd_path, chunk_label)
    {
    chunk_env = new.env ()
    chunk_env$fitting_model_str = "lm"

    eval (parse (text = extract_rmd_chunk (rmd_path, chunk_label)), envir = chunk_env)

    list (vars_used_str = chunk_env$vars_used_str, inVars = chunk_env$inVars)
    }

#===============================================================================

#  Load fixtures and derive the 4 feature sets once, shared across all
#  per-combination tests below.

golden_reg_for_tests = dataprov::prov_registry_open (golden_registry_dir_for_tests)

p3_working_train_df_golden =
    readRDS (resolve_golden_fixture (golden_reg_for_tests, "p3_working_train_df"))
p3_working_test_df_golden =
    readRDS (resolve_golden_fixture (golden_reg_for_tests, "p3_working_test_df"))
p3_train_aux_df_golden =
    readRDS (resolve_golden_fixture (golden_reg_for_tests, "p3_train_aux_df"))
p3_test_aux_df_golden =
    readRDS (resolve_golden_fixture (golden_reg_for_tests, "p3_test_aux_df"))
all_fitting_scores_df_golden =
    readRDS (resolve_golden_fixture (golden_reg_for_tests, "all_fitting_scores_df"))

DBI::dbDisconnect (golden_reg_for_tests$conn)

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

#===============================================================================

test_that ("the 4 feature sets extracted from the Rmd have the expected sizes",
    {
    sizes = setNames (sapply (feature_sets_for_tests, function (fs) length (fs$inVars)),
                      sapply (feature_sets_for_tests, function (fs) fs$vars_used_str))

    expect_equal (sizes [["PUsAndSppOnly"]], 2)
    expect_equal (sizes [["ProbSizeAndDensity"]], 5)
    expect_equal (sizes [["Graph"]], 27)
    expect_equal (sizes [["All"]], 42)
    })

#===============================================================================

    #  One test per (feature set x error type) call site in the Rmd -- 8
    #  total, matching the 8 fit_and_predict_output_error_using_feature_set()
    #  calls read from the document.

for (cur_feature_set in feature_sets_for_tests)
    {
    for (cur_error_type in error_types_for_tests)
        {
        local (
            {
            fs = cur_feature_set
            et = cur_error_type

            test_that (
                paste0 ("LM equivalence: ", fs$vars_used_str, " / ", et$measure_name_str,
                       " matches the golden within testthat's default tolerance"),
                {
                xy = build_feature_set_specific_test_and_train (
                    working_train_df = p3_working_train_df_golden,
                    working_test_df  = p3_working_test_df_golden,
                    fs$inVars, build_params_for_tests,
                    include_median_redundancies = FALSE)

                recipe_template_df = xy$p3_train_x_df %>% dplyr::select (-rs_method_name)
                recipe_template_df [[et$target_col_name]] =
                    p3_working_train_df_golden [[et$target_col_name]]

                bundle = fit_output_error_for_feature_set (
                    rs_method_names_list_for_tests,
                    train_x_df = xy$p3_train_x_df, test_x_df = xy$p3_test_x_df,
                    working_train_df = p3_working_train_df_golden,
                    working_test_df  = p3_working_test_df_golden,
                    train_aux_df = p3_train_aux_df_golden,
                    test_aux_df  = p3_test_aux_df_golden,
                    target_col_name = et$target_col_name,
                    recipe       = make_bdpg_recipe (recipe_template_df, et$target_col_name),
                    learner_spec = make_bdpg_learner ("lm"),
                    learner_id   = "lm",
                    vars_used_str     = fs$vars_used_str,
                    measure_name_str  = et$measure_name_str,
                    fitting_model_str = "lm")

                expect_s3_class (bundle, "bdpg_fit_result")
                expect_equal (nrow (bundle$metrics), 8)    #  4 RS x TRAIN/TEST

                golden_subset =
                    all_fitting_scores_df_golden %>%
                    dplyr::filter (vars_used_str == fs$vars_used_str,
                                  measure_name_str == et$measure_name_str)

                expect_equal (nrow (golden_subset), 8)

                merged =
                    dplyr::inner_join (
                        bundle$metrics, golden_subset,
                        by = c (rs_name = "rs_method_name", ds_label = "train_or_test"))

                expect_equal (nrow (merged), 8)

                expect_equal (merged$rmse.x, merged$rmse.y)
                expect_equal (merged$R2.x, merged$R2.y)
                expect_equal (merged$adj_R2.x, merged$adj_R2.y)
                }
                )
            }
            )
        }
    }
