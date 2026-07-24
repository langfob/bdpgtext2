#===============================================================================
#
#                       test-fitting-pipeline-seams.R
#
#===============================================================================

#  Unit tests for the three seam-builder functions in
#  R/v1_paper_9_fitting_and_eval_pipeline.R: make_bdpg_resampling_plan(),
#  make_bdpg_recipe(), make_bdpg_learner().  See
#  planning/bdpg_fitting_refactor_plan.md §4/§10.
#
#  Run via testthat::test_file() after sourcing
#  R/v1_paper_9_fitting_and_eval_pipeline.R plus library(tidymodels) (or at
#  least parsnip/recipes/workflows), per the convention in
#  "planning/dataprov integration/DONE - dataprov_planning_decisions_checkpoint - 2026 07 13.md".

test_that ("make_bdpg_resampling_plan() returns a one-element fold_id = 1 plan",
    {
    analysis_df   = data.frame (x = 1:3, y = 4:6)
    assessment_df = data.frame (x = 7:8, y = 9:10)

    plan = make_bdpg_resampling_plan (analysis_df, assessment_df)

    expect_type (plan, "list")
    expect_length (plan, 1)
    expect_equal (plan [[1]]$fold_id, 1)
    expect_equal (plan [[1]]$analysis, analysis_df)
    expect_equal (plan [[1]]$assessment, assessment_df)
    })

test_that ("make_bdpg_resampling_plan() passes through zero-row data frames unchanged",
    {
    empty_df = data.frame (x = numeric (0), y = numeric (0))

    plan = make_bdpg_resampling_plan (empty_df, empty_df)

    expect_length (plan, 1)
    expect_equal (nrow (plan [[1]]$analysis), 0)
    expect_equal (nrow (plan [[1]]$assessment), 0)
    })

#===============================================================================

test_that ("make_bdpg_recipe() builds a pass-through recipe with the right outcome/predictor roles",
    {
    template_df = data.frame (x1 = 1:5, x2 = 6:10, y = 11:15)

    rec = make_bdpg_recipe (template_df, "y")

    expect_s3_class (rec, "recipe")

    var_info = rec$var_info
    expect_equal (var_info$role [var_info$variable == "y"], "outcome")
    expect_setequal (var_info$role [var_info$variable %in% c ("x1", "x2")],
                     c ("predictor", "predictor"))
    expect_length (rec$steps, 0)    #  pass-through: no step_*() preprocessing
    })

#===============================================================================

test_that ("make_bdpg_learner() returns a linear_reg spec for 'lm' (the default)",
    {
    learner = make_bdpg_learner ("lm")

    expect_s3_class (learner, "linear_reg")
    expect_equal (learner$engine, "lm")

    expect_equal (make_bdpg_learner ()$engine, "lm")    #  default argument
    })

test_that ("make_bdpg_learner() returns a ranger-engine regression rand_forest spec for 'rf'",
    {
    learner = make_bdpg_learner ("rf")

    expect_s3_class (learner, "rand_forest")
    expect_equal (learner$engine, "ranger")
    expect_equal (learner$mode, "regression")
    })

test_that ("make_bdpg_learner() aborts on an unrecognized learner_id",
    {
    expect_error (make_bdpg_learner ("xgboost"), "Must be 'lm' or 'rf'")
    expect_error (make_bdpg_learner (""), "Must be 'lm' or 'rf'")
    })
