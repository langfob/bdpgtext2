#===============================================================================
#
#                       test-fitting-pipeline-rf-smoke.R
#
#===============================================================================

#  Checkpoint 2 RF smoke test (plan §5/§6): the same workflow path with a
#  rand_forest spec runs end-to-end through fit_output_error_for_feature_set()
#  and returns a well-formed bdpg_fit_result.  Structure only -- RF cannot be
#  golden-matched to the old RF path (different engine/seed/defaults; see
#  plan §11).
#
#  Also covers bdpg_fit_result's return shape/type generally (the "return
#  value and type" test category), and the keep_workflow flag's two branches,
#  using the small committed COR-subsample so this stays fast.
#
#  Run via testthat::test_file() after sourcing R/provenance_helpers.R,
#  R/v1_paper_3_fitting_functions.R, R/v1_paper_3_plotting_and_evaluation_functions.R,
#  and R/v1_paper_9_fitting_and_eval_pipeline.R, plus library(dataprov) /
#  library(DBI) / library(here) / library(tidymodels), per the convention in
#  "planning/dataprov integration/DONE - dataprov_planning_decisions_checkpoint - 2026 07 13.md".

proj_dir_for_tests = here::here ()

golden_registry_dir_for_tests =
    file.path (proj_dir_for_tests, "tests", "fixtures", "dataprov_registry")

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

reg_for_tests = dataprov::prov_registry_open (golden_registry_dir_for_tests)

p3_working_train_df_golden = readRDS (resolve_golden_fixture (reg_for_tests, "p3_working_train_df"))
p3_working_test_df_golden  = readRDS (resolve_golden_fixture (reg_for_tests, "p3_working_test_df"))
p3_train_aux_df_golden     = readRDS (resolve_golden_fixture (reg_for_tests, "p3_train_aux_df"))
p3_test_aux_df_golden      = readRDS (resolve_golden_fixture (reg_for_tests, "p3_test_aux_df"))
cor_subsample_selection_df = readRDS (resolve_golden_fixture (reg_for_tests, "cor_subsample_selection"))

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

    #  Smallest feature set (PUsAndSppOnly, 2 vars) -- structure is all this
    #  test cares about, so keep it fast.
smoke_inVars = c ("rsp_num_spp", "rsp_num_occupied_PUs")
smoke_target_col_name = "rsr_COR_spp_rep_shortfall"
build_params_for_tests = list (write_tibs_to_csv = FALSE, VERBOSE_LM_CATS = FALSE)

xy_for_tests = build_feature_set_specific_test_and_train (
    working_train_df = sub_working_train_df, working_test_df = sub_working_test_df,
    smoke_inVars, build_params_for_tests, include_median_redundancies = FALSE)

recipe_template_df_for_tests = xy_for_tests$p3_train_x_df %>% dplyr::select (-rs_method_name)
recipe_template_df_for_tests [[smoke_target_col_name]] =
    sub_working_train_df [[smoke_target_col_name]]

fit_rf_bundle <- function (keep_workflow = FALSE)
    {
    fit_output_error_for_feature_set (
        rs_method_names_list_for_tests,
        train_x_df = xy_for_tests$p3_train_x_df, test_x_df = xy_for_tests$p3_test_x_df,
        working_train_df = sub_working_train_df, working_test_df = sub_working_test_df,
        train_aux_df = sub_train_aux_df, test_aux_df = sub_test_aux_df,
        target_col_name = smoke_target_col_name,
        recipe       = make_bdpg_recipe (recipe_template_df_for_tests, smoke_target_col_name),
        learner_spec = make_bdpg_learner ("rf"),
        learner_id   = "rf",
        vars_used_str     = "PUsAndSppOnly",
        measure_name_str  = "abs_rep_shortfall_resid",
        fitting_model_str = "rf",
        keep_workflow = keep_workflow)
    }

#===============================================================================

test_that ("RF smoke: fit_output_error_for_feature_set() runs end-to-end and returns a bdpg_fit_result",
    {
    bundle = fit_rf_bundle ()

    expect_s3_class (bundle, "bdpg_fit_result")
    expect_named (bundle, c ("metrics", "predictions", "meta", "workflow"))
    })

test_that ("RF smoke: metrics has the expected shape, columns, and types",
    {
    bundle = fit_rf_bundle ()

    expect_s3_class (bundle$metrics, "data.frame")
    expect_equal (nrow (bundle$metrics), 8)    #  4 RS x TRAIN/TEST

    expect_named (bundle$metrics,
                 c ("rs_name", "ds_label", "fold_id", "adj_R2", "rmse", "R2",
                   "num_predictors", "feature_set_label", "error_type_label",
                   "fitting_model_str"))

    expect_setequal (unique (bundle$metrics$rs_name), rs_method_names_list_for_tests)
    expect_setequal (unique (bundle$metrics$ds_label), c ("TRAIN", "TEST"))
    expect_true (all (bundle$metrics$fold_id == 1))
    expect_true (all (bundle$metrics$num_predictors == 2))
    expect_true (is.numeric (bundle$metrics$rmse))
    expect_true (all (bundle$metrics$rmse >= 0))
    })

test_that ("RF smoke: predictions has the expected shape and columns",
    {
    bundle = fit_rf_bundle ()

    expect_s3_class (bundle$predictions, "data.frame")
    expect_true (nrow (bundle$predictions) > 0)

    expect_named (bundle$predictions,
                 c ("ds_label", "true_values", "pred_values", "dom_err_type",
                   "rs_name", "fold_id"))

    expect_true (is.numeric (bundle$predictions$true_values))
    expect_true (is.numeric (bundle$predictions$pred_values))
    expect_true (all (is.finite (bundle$predictions$pred_values)))
    })

test_that ("RF smoke: meta carries learner_id and the recipe object",
    {
    bundle = fit_rf_bundle ()

    expect_equal (bundle$meta$learner_id, "rf")
    expect_equal (bundle$meta$feature_set_label, "PUsAndSppOnly")
    expect_equal (bundle$meta$target_col_name, smoke_target_col_name)
    expect_s3_class (bundle$meta$recipe, "recipe")
    })

test_that ("keep_workflow = FALSE (the default) omits fitted workflows",
    {
    bundle = fit_rf_bundle (keep_workflow = FALSE)

    expect_null (bundle$workflow)
    })

test_that ("keep_workflow = TRUE carries one fitted workflow per reserve selector",
    {
    bundle = fit_rf_bundle (keep_workflow = TRUE)

    expect_type (bundle$workflow, "list")
    expect_setequal (names (bundle$workflow), rs_method_names_list_for_tests)

    for (rs in rs_method_names_list_for_tests)
        expect_s3_class (bundle$workflow [[rs]], "workflow")
    })
