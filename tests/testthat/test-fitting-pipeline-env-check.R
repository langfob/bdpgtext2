#===============================================================================
#
#                       test-fitting-pipeline-env-check.R
#
#===============================================================================

#  Checkpoint 0 environment check for the fitting/eval refactor (see
#  planning/bdpg_fitting_refactor_plan.md).  Confirms the OLD pipeline's
#  metric kernel (eval_model_on_train_or_test_data(), in
#  R/v1_paper_3_plotting_and_evaluation_functions.R) is sourceable and
#  produces sane values on a trivial LM fit.  This does not touch plotting
#  (plot_train_and_test_stuff_for_one_RS() depends on functions defined in
#  files this checkpoint does not source).
#
#  Run via testthat::test_file() after sourcing
#  R/v1_paper_3_fitting_functions.R and
#  R/v1_paper_3_plotting_and_evaluation_functions.R, per the convention
#  documented in planning/dataprov integration/DONE - dataprov_planning_decisions_checkpoint - 2026 07 13.md.

test_that ("required packages for the old and new fitting pipelines are installed",
    {
    needed_pkgs = c ("tidyverse", "caret", "party", "ranger", "glmnet",
                     "parsnip", "recipes", "workflows", "rsample",
                     "yardstick", "dataprov", "bdpg")

    installed = vapply (needed_pkgs, requireNamespace, logical (1), quietly = TRUE)

    expect_true (all (installed),
                info = paste0 ("Missing packages: ",
                               paste (needed_pkgs [! installed], collapse = ", ")))
    })

test_that ("eval_model_on_train_or_test_data() reproduces a trivial LM fit",
    {
    set.seed (1)
    n = 12
    train_x_df = data.frame (x1 = rnorm (n), x2 = rnorm (n))
    train_y_vec = 2 * train_x_df$x1 - train_x_df$x2 + rnorm (n, sd = 0.1)

    test_x_df = data.frame (x1 = rnorm (6), x2 = rnorm (6))
    test_y_vec = 2 * test_x_df$x1 - test_x_df$x2 + rnorm (6, sd = 0.1)

    fit = lm (train_y_vec ~ ., data = train_x_df)
    train_pred = predict (fit, train_x_df)
    test_pred  = predict (fit, test_x_df)

    train_eval = eval_model_on_train_or_test_data (
        "TRAIN", train_y_vec, train_pred,
        num_predictors = ncol (train_x_df),
        dom_err_type = rep ("FN", n),
        must_specify_predictions_vector = FALSE)

    test_eval = eval_model_on_train_or_test_data (
        "TEST", test_y_vec, test_pred,
        num_predictors = ncol (train_x_df),
        dom_err_type = rep ("FN", 6),
        must_specify_predictions_vector = FALSE)

    expect_true (train_eval$adj_R2 > 0.9)
    expect_true (test_eval$adj_R2 > 0.9)
    expect_true (train_eval$rmse_value >= 0)
    expect_true (test_eval$rmse_value >= 0)
    })
