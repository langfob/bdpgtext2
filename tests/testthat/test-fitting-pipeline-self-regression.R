#===============================================================================
#
#                       test-fitting-pipeline-self-regression.R
#
#===============================================================================

#  Checkpoint 2 fast-inner-loop self-regression test (see
#  planning/bdpg_fitting_refactor_plan.md §5, "Golden-master specifics", and
#  the 2026-07-24 entries in DECISIONS.md / FUTURE_CHATS.md FC-6).
#
#  Unlike test-fitting-pipeline-lm-equivalence.R (which compares against the
#  OLD pipeline's captured output), this test compares the NEW pipeline
#  against its OWN frozen output on a small, deterministic COR-subsample --
#  a fast regression guard for future changes to this pipeline, not a
#  correctness proof against the old code.
#
#  The subsample covers only PUsAndSppOnly / ProbSizeAndDensity / Graph.
#  "All" (42 vars) is excluded: it is structurally rank-deficient by 1 at ANY
#  sample size (ig_num_edges_m = edge_frac_of_possible x sppPUprod, an exact
#  identity present in the full dataset too -- not a subsampling artifact;
#  see FUTURE_CHATS.md FC-6). "All" is still fully covered by the full-batch
#  LM equivalence test.
#
#  The COR-group selection (8 groups) and frozen metrics/predictions were
#  generated once and committed via the same dataprov idiom as the
#  old-pipeline golden, into the same dedicated
#  tests/fixtures/dataprov_registry (session
#  3010097a-e206-4496-8e8d-3436eb767484). This test re-derives the subsample
#  from the frozen COR-group UUIDs (not by re-running the selection search),
#  re-runs the new pipeline, and expects to reproduce the frozen values
#  exactly (LM is deterministic).
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

COR_COL_for_tests = "rsp_UUID_of_COR_Base_problem_that_is_wrapped"

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

#===============================================================================

reg_for_tests = dataprov::prov_registry_open (golden_registry_dir_for_tests)

p3_working_train_df_golden =
    readRDS (resolve_golden_fixture (reg_for_tests, "p3_working_train_df"))
p3_working_test_df_golden =
    readRDS (resolve_golden_fixture (reg_for_tests, "p3_working_test_df"))
p3_train_aux_df_golden =
    readRDS (resolve_golden_fixture (reg_for_tests, "p3_train_aux_df"))
p3_test_aux_df_golden =
    readRDS (resolve_golden_fixture (reg_for_tests, "p3_test_aux_df"))

cor_subsample_selection_df =
    readRDS (resolve_golden_fixture (reg_for_tests, "cor_subsample_selection"))
frozen_self_regression_metrics_df =
    readRDS (resolve_golden_fixture (reg_for_tests, "self_regression_metrics"))
frozen_self_regression_predictions_df =
    readRDS (resolve_golden_fixture (reg_for_tests, "self_regression_predictions"))

DBI::dbDisconnect (reg_for_tests$conn)

rs_method_names_list_for_tests = c ("ILP", "SA", "UR_Forward", "SA_SS")

train_cor_uuids = cor_subsample_selection_df$cor_uuid [cor_subsample_selection_df$dataset == "train"]
test_cor_uuids  = cor_subsample_selection_df$cor_uuid [cor_subsample_selection_df$dataset == "test"]

train_idx = which (p3_working_train_df_golden [[COR_COL_for_tests]] %in% train_cor_uuids)
test_idx  = which (p3_working_test_df_golden [[COR_COL_for_tests]] %in% test_cor_uuids)

sub_working_train_df = p3_working_train_df_golden [train_idx, ]
sub_working_test_df  = p3_working_test_df_golden [test_idx, ]
sub_train_aux_df = p3_train_aux_df_golden [train_idx, ]
sub_test_aux_df  = p3_test_aux_df_golden [test_idx, ]

feature_set_chunk_labels_for_tests =
    c ("setPUsAndSppOnlyParams", "setProbSizeAndDensityParams", "setNonLatapyGraphParams")

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

test_that ("the re-derived COR-subsample matches the frozen selection's shape",
    {
    expect_equal (length (train_cor_uuids), 8)
    expect_equal (length (test_cor_uuids), 8)

    expect_true (all (table (sub_working_train_df$rs_method_name) >= 29))
    expect_true (all (table (sub_working_test_df$rs_method_name) >= 29))

    expect_setequal (unique (sub_train_aux_df$dom_err_type), c ("FN", "FP"))
    expect_setequal (unique (sub_test_aux_df$dom_err_type), c ("FN", "FP"))
    })

#===============================================================================

for (cur_feature_set in feature_sets_for_tests)
    {
    for (cur_error_type in error_types_for_tests)
        {
        local (
            {
            fs = cur_feature_set
            et = cur_error_type

            test_that (
                paste0 ("self-regression: ", fs$vars_used_str, " / ", et$measure_name_str,
                       " reproduces the frozen new-pipeline output exactly"),
                {
                xy = build_feature_set_specific_test_and_train (
                    working_train_df = sub_working_train_df,
                    working_test_df  = sub_working_test_df,
                    fs$inVars, build_params_for_tests,
                    include_median_redundancies = FALSE)

                recipe_template_df = xy$p3_train_x_df %>% dplyr::select (-rs_method_name)
                recipe_template_df [[et$target_col_name]] =
                    sub_working_train_df [[et$target_col_name]]

                bundle = fit_output_error_for_feature_set (
                    rs_method_names_list_for_tests,
                    train_x_df = xy$p3_train_x_df, test_x_df = xy$p3_test_x_df,
                    working_train_df = sub_working_train_df,
                    working_test_df  = sub_working_test_df,
                    train_aux_df = sub_train_aux_df,
                    test_aux_df  = sub_test_aux_df,
                    target_col_name = et$target_col_name,
                    recipe       = make_bdpg_recipe (recipe_template_df, et$target_col_name),
                    learner_spec = make_bdpg_learner ("lm"),
                    learner_id   = "lm",
                    vars_used_str     = fs$vars_used_str,
                    measure_name_str  = et$measure_name_str,
                    fitting_model_str = "lm")

                expect_true (all (is.finite (bundle$metrics$adj_R2)))
                expect_true (all (is.finite (bundle$metrics$rmse)))

                frozen_subset =
                    frozen_self_regression_metrics_df %>%
                    dplyr::filter (feature_set_label == fs$vars_used_str,
                                  error_type_label == et$measure_name_str)

                merged =
                    dplyr::inner_join (
                        bundle$metrics, frozen_subset,
                        by = c ("rs_name", "ds_label", "fold_id"))

                expect_equal (nrow (merged), 8)
                expect_equal (merged$rmse.x, merged$rmse.y)
                expect_equal (merged$R2.x, merged$R2.y)
                expect_equal (merged$adj_R2.x, merged$adj_R2.y)

                    #  frozen_self_regression_predictions_df pools all 6
                    #  combos; filter to this one first.  Both sides are then
                    #  built by the identical deterministic construction (same
                    #  RS loop order, same TRAIN-then-TEST order, same
                    #  original row order within each) on the identical
                    #  subsample data, so a row-position match is safe here
                    #  (unlike an arbitrary pair of data frames).
                frozen_pred_subset =
                    frozen_self_regression_predictions_df %>%
                    dplyr::filter (feature_set_label == fs$vars_used_str,
                                  error_type_label == et$measure_name_str)

                expect_equal (nrow (frozen_pred_subset), nrow (bundle$predictions))
                expect_equal (as.character (frozen_pred_subset$rs_name),
                             as.character (bundle$predictions$rs_name))
                expect_equal (as.character (frozen_pred_subset$ds_label),
                             as.character (bundle$predictions$ds_label))
                expect_equal (frozen_pred_subset$true_values, bundle$predictions$true_values)
                expect_equal (frozen_pred_subset$pred_values, bundle$predictions$pred_values)
                }
                )
            }
            )
        }
    }
