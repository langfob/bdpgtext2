#===============================================================================
#
#                       test-fitting-pipeline-plot-structure.R
#
#===============================================================================

#  Checkpoint 3 plot structural test (plan §5/§6): plot_output_error_fit()
#  returns a ggplot with one facet per reserve selector and the expected
#  annotation/diagonal layers. Structure only -- no vdiffr pixel snapshot
#  (plan §5 explicitly makes that optional, to avoid coupling to
#  ggplot/font/OS versions).
#
#  Uses the small committed COR-subsample fixture (see
#  test-fitting-pipeline-self-regression.R) so this stays fast.
#
#  Run via testthat::test_file() after sourcing R/provenance_helpers.R,
#  R/v1_paper_3_fitting_functions.R, R/v1_paper_3_plotting_and_evaluation_functions.R,
#  R/v1_paper_3_utility_functions.R (for convert_rs_method_name_to_ordered_factor(),
#  a new dependency introduced by this checkpoint's plot function), and
#  R/v1_paper_9_fitting_and_eval_pipeline.R, plus library(dataprov) /
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

build_params_for_tests = list (write_tibs_to_csv = FALSE, VERBOSE_LM_CATS = FALSE)

make_test_bundle <- function (target_col_name, measure_name_str)
    {
    inVars = c ("rsp_num_spp", "rsp_num_occupied_PUs", "links_per_PUsAndSpp",
               "edge_frac_of_possible", "sppPUprod")

    xy = build_feature_set_specific_test_and_train (
        working_train_df = sub_working_train_df, working_test_df = sub_working_test_df,
        inVars, build_params_for_tests, include_median_redundancies = FALSE)

    recipe_template_df = xy$p3_train_x_df %>% dplyr::select (-rs_method_name)
    recipe_template_df [[target_col_name]] = sub_working_train_df [[target_col_name]]

    fit_output_error_for_feature_set (
        rs_method_names_list_for_tests,
        train_x_df = xy$p3_train_x_df, test_x_df = xy$p3_test_x_df,
        working_train_df = sub_working_train_df, working_test_df = sub_working_test_df,
        train_aux_df = sub_train_aux_df, test_aux_df = sub_test_aux_df,
        target_col_name = target_col_name,
        recipe       = make_bdpg_recipe (recipe_template_df, target_col_name),
        learner_spec = make_bdpg_learner ("lm"),
        learner_id   = "lm",
        vars_used_str     = "ProbSizeAndDensity",
        measure_name_str  = measure_name_str,
        fitting_model_str = "lm")
    }

rep_shortfall_bundle_for_tests =
    make_test_bundle ("rsr_COR_spp_rep_shortfall", "abs_rep_shortfall_resid")
cost_err_bundle_for_tests =
    make_test_bundle ("rs_solution_cost_err_frac", "abs_sol_cost_err_resid")

layer_geom_classes <- function (a_plot)
    sapply (a_plot$layers, function (l) class (l$geom) [1])

#===============================================================================

test_that ("plot_output_error_fit() returns a ggplot object, not shown, not saved",
    {
    p = plot_output_error_fit (rep_shortfall_bundle_for_tests)

    expect_s3_class (p, "ggplot")
    })

test_that ("plot_output_error_fit() has one facet panel per reserve selector",
    {
    p = plot_output_error_fit (rep_shortfall_bundle_for_tests)

    built = ggplot2::ggplot_build (p)
    point_layer_idx = which (layer_geom_classes (p) == "GeomPoint")

    expect_length (point_layer_idx, 1)
    expect_equal (length (unique (built$data [[point_layer_idx]]$PANEL)), 4)
    })

test_that ("plot_output_error_fit() includes the point, diagonal, and both annotation layers",
    {
    p = plot_output_error_fit (rep_shortfall_bundle_for_tests)

    geoms = layer_geom_classes (p)

    expect_true ("GeomPoint" %in% geoms)
    expect_true ("GeomAbline" %in% geoms)
    expect_equal (sum (geoms == "GeomText"), 2)    #  adj-R2 label + rmse label
    })

test_that ("plot_output_error_fit() labels the axes as in the old figure",
    {
    p = plot_output_error_fit (rep_shortfall_bundle_for_tests)

    expect_equal (p$labels$x, "Predicted value")
    expect_equal (p$labels$y, "Correct value")
    })

test_that ("plot_output_error_fit() renders without error for both ds_label values",
    {
    expect_no_error (ggplot2::ggplot_build (plot_output_error_fit (rep_shortfall_bundle_for_tests, ds_label = "TRAIN")))
    expect_no_error (ggplot2::ggplot_build (plot_output_error_fit (rep_shortfall_bundle_for_tests, ds_label = "TEST")))
    })

test_that ("plot_output_error_fit() derives the title from measure_name_str when not supplied",
    {
    p_rep  = plot_output_error_fit (rep_shortfall_bundle_for_tests)
    p_cost = plot_output_error_fit (cost_err_bundle_for_tests)

    expect_match (p_rep$labels$title, "^Representation Shortfall predictions")
    expect_match (p_cost$labels$title, "^Solution Cost Error predictions")
    })

test_that ("plot_output_error_fit() honors an explicit pred_value_name_display_str override",
    {
    p = plot_output_error_fit (rep_shortfall_bundle_for_tests,
                              pred_value_name_display_str = "Custom Label")

    expect_match (p$labels$title, "^Custom Label predictions")
    })

test_that ("plot_output_error_fit() applies the Solution Cost Error ylim(NA, 1.5) quirk only for that error type",
    {
    p_cost = plot_output_error_fit (cost_err_bundle_for_tests)
    p_rep  = plot_output_error_fit (rep_shortfall_bundle_for_tests)

    y_scale_cost = p_cost$scales$get_scales ("y")
    y_scale_rep  = p_rep$scales$get_scales ("y")

    expect_false (is.null (y_scale_cost))
    expect_equal (y_scale_cost$limits, c (NA, 1.5))

    expect_true (is.null (y_scale_rep))    #  no ylim() layer added for rep shortfall
    })
