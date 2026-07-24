#===============================================================================
#
#                 v1_paper_9_fitting_and_eval_pipeline.R
#
#===============================================================================

#  History

#  2026 07 23 - BTL - v1
#     - Created as the parallel-build target of the fitting/eval refactor
#       described in planning/bdpg_fitting_refactor_plan.md.  The "v1_paper_9"
#       prefix records when/where this code was first added (Paper 9 /
#       2026-07-23), per the naming convention approved in that plan (§10).
#       It does NOT imply a version relationship to v1_paper_3_fitting_functions.R
#       or v2_paper_3_cv_test_train_splitting_functions.R.

#  Relationship to existing files (see plan §2, §9 do-not-touch list)

#  - R/v1_paper_3_fitting_functions.R and
#    R/v1_paper_3_plotting_and_evaluation_functions.R are the OLD pipeline.
#    This file is a parallel build alongside them, not a replacement (yet).
#    eval_model_on_train_or_test_data() and force_dom_err_type_colors() (both
#    in the plotting/eval file) are reused UNCHANGED, as is
#    convert_rs_method_name_to_ordered_factor() (in
#    R/v1_paper_3_utility_functions.R) -- including that function's known
#    facet-ordering quirk (CLEANUP_GOALS.MD Priority 1), which this file does
#    not attempt to fix.
#  - R/v2_paper_3_cv_test_train_splitting_functions.R is the upstream
#    COR-disjoint splitting logic.  It is not consumed by this file yet, but
#    the resampling-plan seam here (make_bdpg_resampling_plan()) is built so
#    that a future k-fold / grouped-CV plan produced by that file can be
#    substituted at the call boundary without restructuring this file.

#  Architecture: three seams (resampling plan, recipe, learner), only the
#  simplest case of each exercised this round (single holdout, pass-through
#  recipe, LM).  See planning/bdpg_fitting_refactor_plan.md §4 and §10 for
#  the full design and approved function names.

#===============================================================================

#  SEAM: resampling plan.  This round returns exactly one fold (fold_id = 1)
#  built from a pre-split (analysis, assessment) pair.  A future k-fold /
#  grouped-CV plan (see R/v2_paper_3_cv_test_train_splitting_functions.R, the
#  upstream COR-disjoint splitting) returns a longer list of same-shaped
#  elements without requiring any change to the fit/evaluate loop that
#  consumes it.

make_bdpg_resampling_plan <- function (analysis_df, assessment_df)
    {
    list (
        list (fold_id    = 1,
             analysis   = analysis_df,
             assessment = assessment_df)
        )
    }

#===============================================================================

#  SEAM: recipe.  Pass-through this round -- formula + role assignment only,
#  no step_*() preprocessing -- because data arrives already Box-Cox'd and
#  standardized upstream (see p9_v01_prep_data_for_p8_to_load_from_files.Rmd).
#
#  `data` is a TEMPLATE only: recipes::recipe() uses it solely to record
#  variable names/types/roles at creation time.  The actual analysis data for
#  each RS/fold is supplied later, to generics::fit() on a workflow built from
#  this recipe, which re-preps the recipe on that data.  So moving
#  step_BoxCox() / step_normalize() into this recipe later re-estimates them
#  per fold (leakage-safe) with no change to callers.

make_bdpg_recipe <- function (data, target_col_name)
    {
    formula = stats::reformulate (termlabels = ".", response = target_col_name)

    recipes::recipe (formula, data = data)
    }

#===============================================================================

#  SEAM: learner.  "lm" (default) is the only equivalence-tested learner this
#  round; "rf" is a structure-only smoke test through the identical workflow
#  path (see plan §11 -- RF cannot be golden-matched to the old RF path:
#  different engine/seed/defaults).

make_bdpg_learner <- function (learner_id = "lm")
    {
    if (learner_id == "lm")
        {
        learner = parsnip::linear_reg () %>% parsnip::set_engine ("lm")
        } else if (learner_id == "rf")
        {
        learner = parsnip::rand_forest () %>%
            parsnip::set_engine ("ranger") %>%
            parsnip::set_mode ("regression")
        } else
        {
        stop (paste0 ("In make_bdpg_learner(), \n",
                      "learner_id is '", learner_id, "'.  ",
                      "Must be 'lm' or 'rf'.\n"))
        }

    return (learner)
    }

#===============================================================================

#  Pure fit/evaluate core (Checkpoint 2).  Loops over rs_method_names_list --
#  one fitted model per reserve selector, matching the old pipeline's
#  fit_one_rs() in R/v1_paper_3_fitting_functions.R -- builds a one-element
#  resampling plan per RS via make_bdpg_resampling_plan(), fits the given
#  recipe + learner_spec as a tidymodels workflow, predicts on analysis and
#  assessment via broom::augment(), and calls
#  eval_model_on_train_or_test_data() UNCHANGED (the existing metric kernel in
#  R/v1_paper_3_plotting_and_evaluation_functions.R) to produce
#  rmse/R2/adj_R2.  Returns data only -- no ggplot, no show().
#
#  num_predictors is computed as ncol() of the RS-filtered, feature-only x
#  data frame (rs_method_name dropped, target not yet added), matching the
#  legacy p in compute_adj.r.squared() exactly (plan §3).
#
#  rs_method_names_list  - character vector of reserve selector names
#  train_x_df, test_x_df - multi-RS: rs_method_name + feature columns only
#                          (no target column), as returned by
#                          build_feature_set_specific_test_and_train()
#  working_train_df,
#  working_test_df       - full working frames, for pulling target_col_name
#                          values (same role as in fit_one_rs())
#  train_aux_df,
#  test_aux_df            - rs_method_name + dom_err_type
#  target_col_name        - e.g. "rsr_COR_spp_rep_shortfall"
#  recipe                 - SEAM: a recipes::recipe(), from make_bdpg_recipe()
#  learner_spec            - SEAM: a parsnip model_spec, from make_bdpg_learner()
#  learner_id              - e.g. "lm" / "rf" -- carried into meta only
#  vars_used_str           - feature_set_label, e.g. "PUsAndSppOnly"
#  measure_name_str        - error_type_label, e.g. "abs_rep_shortfall_resid"
#                          (legacy perf_metric_name_for_file_name_str)
#  fitting_model_str       - legacy label, e.g. params$fitting_model_str
#  x_min_on_plot, ...      - plot annotation bounds, carried into meta only
#  seed                    - if not NULL, set.seed() before each RS's fit
#                          (no-op for lm; matters once a stochastic learner
#                          such as rf is actually reported -- see plan §11)
#  keep_workflow           - if TRUE, bundle$workflow is a named list
#                          (rs_name -> fitted workflow); default FALSE
#                          (plan §4: off by default, since the eventual CV
#                          case has k fitted models per cell)
#
#  Returns a bdpg_fit_result: list (metrics, predictions, meta, workflow).

fit_output_error_for_feature_set <- function (
    rs_method_names_list,
    train_x_df, test_x_df,
    working_train_df, working_test_df,
    train_aux_df, test_aux_df,
    target_col_name,
    recipe,
    learner_spec,
    learner_id,
    vars_used_str,
    measure_name_str,
    fitting_model_str,

    x_min_on_plot = NA, x_max_on_plot = NA,
    y_min_on_plot = NA, y_max_on_plot = NA,

    seed = NULL,
    keep_workflow = FALSE
    )
    {
    all_metrics_rows    = list ()
    all_prediction_rows = list ()
    all_workflows        = list ()

    for (cur_rs_name in rs_method_names_list)
        {
            #  Reduce data down to just the current RS, matching fit_one_rs()
            #  exactly (same filter, same column drop), so num_predictors and
            #  row alignment match the golden.

        train_x_df %>%
            dplyr::ungroup () %>%
            dplyr::filter (rs_method_name == cur_rs_name) %>%
            dplyr::select (-rs_method_name) ->
            cur_train_x_df

        test_x_df %>%
            dplyr::ungroup () %>%
            dplyr::filter (rs_method_name == cur_rs_name) %>%
            dplyr::select (-rs_method_name) ->
            cur_test_x_df

        working_train_df %>%
            dplyr::ungroup () %>%
            dplyr::filter (rs_method_name == cur_rs_name) ->
            cur_working_train_df

        working_test_df %>%
            dplyr::ungroup () %>%
            dplyr::filter (rs_method_name == cur_rs_name) ->
            cur_working_test_df

        train_aux_df %>%
            dplyr::ungroup () %>%
            dplyr::filter (rs_method_name == cur_rs_name) ->
            cur_train_aux_df

        test_aux_df %>%
            dplyr::ungroup () %>%
            dplyr::filter (rs_method_name == cur_rs_name) ->
            cur_test_aux_df

        cur_train_y_vec = cur_working_train_df [[target_col_name]]
        cur_test_y_vec  = cur_working_test_df [[target_col_name]]

        num_predictors = ncol (cur_train_x_df)

        cur_train_analysis_df = cur_train_x_df
        cur_train_analysis_df [[target_col_name]] = cur_train_y_vec

        cur_test_assessment_df = cur_test_x_df
        cur_test_assessment_df [[target_col_name]] = cur_test_y_vec

        cur_fold_plan = make_bdpg_resampling_plan (cur_train_analysis_df,
                                                   cur_test_assessment_df)
        cur_fold = cur_fold_plan [[1]]    #  single holdout this round

        if (! is.null (seed))
            set.seed (seed)

        cur_workflow =
            workflows::workflow () %>%
            workflows::add_recipe (recipe) %>%
            workflows::add_model (learner_spec)

        cur_fitted_workflow = generics::fit (cur_workflow, data = cur_fold$analysis)

        cur_train_augmented = broom::augment (cur_fitted_workflow, new_data = cur_fold$analysis)
        cur_test_augmented  = broom::augment (cur_fitted_workflow, new_data = cur_fold$assessment)

        cur_train_eval =
            eval_model_on_train_or_test_data ("TRAIN",
                                              cur_train_y_vec,
                                              cur_train_augmented$.pred,
                                              num_predictors,
                                              cur_train_aux_df$dom_err_type,
                                              must_specify_predictions_vector = FALSE)

        cur_test_eval =
            eval_model_on_train_or_test_data ("TEST",
                                              cur_test_y_vec,
                                              cur_test_augmented$.pred,
                                              num_predictors,
                                              cur_test_aux_df$dom_err_type,
                                              must_specify_predictions_vector = FALSE)

        all_metrics_rows [[length (all_metrics_rows) + 1]] =
            data.frame (rs_name           = cur_rs_name,
                        ds_label          = "TRAIN",
                        fold_id           = cur_fold$fold_id,
                        adj_R2            = cur_train_eval$adj_R2,
                        rmse              = cur_train_eval$rmse_value,
                        R2                = cur_train_eval$R2,
                        num_predictors    = num_predictors,
                        feature_set_label = vars_used_str,
                        error_type_label  = measure_name_str,
                        fitting_model_str = fitting_model_str,
                        stringsAsFactors  = FALSE)

        all_metrics_rows [[length (all_metrics_rows) + 1]] =
            data.frame (rs_name           = cur_rs_name,
                        ds_label          = "TEST",
                        fold_id           = cur_fold$fold_id,
                        adj_R2            = cur_test_eval$adj_R2,
                        rmse              = cur_test_eval$rmse_value,
                        R2                = cur_test_eval$R2,
                        num_predictors    = num_predictors,
                        feature_set_label = vars_used_str,
                        error_type_label  = measure_name_str,
                        fitting_model_str = fitting_model_str,
                        stringsAsFactors  = FALSE)

        all_prediction_rows [[length (all_prediction_rows) + 1]] =
            cur_train_eval$true_vs_pred_df %>%
            dplyr::mutate (rs_name = cur_rs_name, fold_id = cur_fold$fold_id)

        all_prediction_rows [[length (all_prediction_rows) + 1]] =
            cur_test_eval$true_vs_pred_df %>%
            dplyr::mutate (rs_name = cur_rs_name, fold_id = cur_fold$fold_id)

        if (keep_workflow)
            all_workflows [[cur_rs_name]] = cur_fitted_workflow
        }

    metrics_df     = do.call (rbind, all_metrics_rows)
    predictions_df = do.call (rbind, all_prediction_rows)

    meta = list (feature_set_label = vars_used_str,
                error_type         = measure_name_str,
                target_col_name    = target_col_name,
                learner_id         = learner_id,
                recipe             = recipe,
                seed               = seed,
                x_min_on_plot      = x_min_on_plot,
                x_max_on_plot      = x_max_on_plot,
                y_min_on_plot      = y_min_on_plot,
                y_max_on_plot      = y_max_on_plot)

    bundle = list (metrics     = metrics_df,
                  predictions = predictions_df,
                  meta        = meta,
                  workflow    = if (keep_workflow) all_workflows else NULL)

    class (bundle) = "bdpg_fit_result"

    return (bundle)
    }

#===============================================================================

#  Plot function (Checkpoint 3), separated from fitting.  Consumes a
#  bdpg_fit_result and returns a ggplot object; never calls show().  Borrows
#  its layout from plot_full_fits() (R/v1_paper_3_fitting_functions.R) --
#  faceted true-vs-predicted scatter colored by dom_err_type, perfect-fit
#  diagonal, per-facet adj-R2/rmse annotation -- WITHOUT modifying that
#  function. Preserves plot_full_fits()'s known facet-ordering quirk
#  (convert_rs_method_name_to_ordered_factor(), in
#  R/v1_paper_3_utility_functions.R, reused unchanged) rather than fixing it;
#  that is CLEANUP_GOALS.MD Priority 1, out of scope here.
#
#  Two gaps between what fit_output_error_for_feature_set()'s meta carries and
#  what the old plot needs are resolved here, not by reopening the
#  already-tested fit function's signature:
#
#  - Old code hardcodes per-error-type text-annotation coordinates
#    (R2_x_loc/R2_y_loc/rmse_x_loc/rmse_y_loc) inside fit_rep_shortfall() /
#    fit_cost_err_frac(), collapsed away by this refactor's single generic fit
#    function. Replaced with ONE shared anchor point per plot (matching old
#    code's own behavior of reusing the same location across every facet, via
#    rep(loc, len)) computed from the plotted data's own range -- an inset
#    from meta$x_min_on_plot/y_min_on_plot etc. when set, else from the
#    predictions' own range. This is a structural/layout simplification, not
#    a metrics change; flagged for author review.
#  - Old code's title and the "Solution Cost Error" y-axis special-case use a
#    pred_value_name_display_str ("Representation Shortfall" / "Solution Cost
#    Error") that fit_output_error_for_feature_set() does not carry (only the
#    internal measure_name_str, e.g. "abs_rep_shortfall_resid"). A caller may
#    pass pred_value_name_display_str explicitly (matching how the Rmd will
#    eventually supply it); if omitted, it is derived from
#    meta$error_type_label for the two known error types.
#
#  bundle                       - a bdpg_fit_result from fit_output_error_for_feature_set()
#  ds_label                     - "TRAIN" or "TEST"; which subset to plot.
#                                Default "TEST", matching the old
#                                display_train_as_final_pred_using_plot's
#                                default (FALSE -> show test)
#  pred_value_name_display_str  - e.g. "Representation Shortfall"; NULL derives
#                                it from meta$error_type_label
#  num_facet_wrap_rows          - default 2, matching the Rmd's actual default
#                                (params$exclude_greedy_rs_in_fit_plots = FALSE)
#  force_colors                 - manually scale dom_err_type colors via
#                                force_dom_err_type_colors() (matches old
#                                params$force_colors = TRUE default)
#
#  Returns a ggplot object.

plot_output_error_fit <- function (
    bundle,
    ds_label = "TEST",
    pred_value_name_display_str = NULL,
    num_facet_wrap_rows = 2,
    force_colors = TRUE
    )
    {
    meta = bundle$meta

    if (is.null (pred_value_name_display_str))
        {
        pred_value_name_display_str =
            switch (meta$error_type,
                   "abs_rep_shortfall_resid" = "Representation Shortfall",
                   "abs_sol_cost_err_resid"  = "Solution Cost Error",
                   meta$error_type)
        }

    predictions_subset = bundle$predictions [bundle$predictions$ds_label == ds_label, ]
    metrics_subset      = bundle$metrics [bundle$metrics$ds_label == ds_label, ]

    predictions_subset$rs_method_name_fac =
        convert_rs_method_name_to_ordered_factor (predictions_subset$rs_name)

    rs_names_vec_fac = convert_rs_method_name_to_ordered_factor (metrics_subset$rs_name)
    num_facets = length (unique (predictions_subset$rs_name))

        #  Shared annotation anchor (one point, reused across every facet --
        #  matches old code's rep(loc, len) pattern), inset from the plotted
        #  x/y bounds if given, else from the plotted data's own range.
    x_bounds = c (meta$x_min_on_plot, meta$x_max_on_plot)
    y_bounds = c (meta$y_min_on_plot, meta$y_max_on_plot)

    x_range = if (all (! is.na (x_bounds))) x_bounds else
        range (predictions_subset$pred_values, na.rm = TRUE)
    y_range = if (all (! is.na (y_bounds))) y_bounds else
        range (predictions_subset$true_values, na.rm = TRUE)

    anchor_x      = x_range [1] + 0.05 * (x_range [2] - x_range [1])
    anchor_y_R2   = y_range [2] - 0.08 * (y_range [2] - y_range [1])
    anchor_y_rmse = y_range [2] - 0.16 * (y_range [2] - y_range [1])

    locs_R2 = data.frame (x = anchor_x, y = anchor_y_R2,
                          rs_method_name_fac = rs_names_vec_fac,
                          R2_label = paste ("adj~R^2 ==", round (metrics_subset$adj_R2, digits = 2)))

    locs_rmse = data.frame (x = anchor_x, y = anchor_y_rmse,
                            rs_method_name_fac = rs_names_vec_fac,
                            rmse_label = paste ("rmse ==", round (metrics_subset$rmse, digits = 2)))

    color_breaks_and_values = force_dom_err_type_colors (predictions_subset$dom_err_type)

    the_plot =
        ggplot2::ggplot (data = predictions_subset) +
        ggplot2::geom_point (ggplot2::aes (x = pred_values, y = true_values,
                                          color = dom_err_type),
                            shape = 15, size = 0.5, alpha = 0.5) +
        ggplot2::labs (x = "Predicted value", y = "Correct value") +
        ggplot2::facet_wrap (~ rs_method_name_fac, nrow = num_facet_wrap_rows) +
        ggplot2::geom_abline (intercept = 0, slope = 1) +
        ggplot2::ggtitle (paste0 (pred_value_name_display_str, " predictions ", "\n",
                                 "using ", meta$feature_set_label, " features")) +
        ggplot2::theme (plot.title = ggplot2::element_text (hjust = 0.5)) +
        { if (force_colors) ggplot2::scale_color_manual (breaks = color_breaks_and_values$breaks,
                                                         values = color_breaks_and_values$values,
                                                         name = "Dominant\nerror type") } +
        { if (num_facets == 5) ggplot2::theme (legend.position = c (0.85, 0.25),
                                              legend.direction = "vertical") } +
        { if (num_facets == 4) ggplot2::theme (legend.position = "right",
                                              legend.direction = "vertical") } +
        ggplot2::guides (color = ggplot2::guide_legend (
            override.aes = list (size = 4, alpha = 1, fill = NA))) +
        ggplot2::geom_text (ggplot2::aes (x, y, label = R2_label), data = locs_R2,
                           family = "Times", fontface = "italic", size = 3, hjust = 0,
                           parse = TRUE) +
        ggplot2::geom_text (ggplot2::aes (x, y, label = rmse_label), data = locs_rmse,
                           family = "Times", fontface = "italic", size = 3, hjust = 0,
                           parse = TRUE) +
        { if (pred_value_name_display_str == "Solution Cost Error") ggplot2::ylim (NA, 1.5) }

        #  plot_train_and_test_stuff_for_one_RS() (per-selector diagnostic
        #  plot, in R/v1_paper_3_plotting_and_evaluation_functions.R) is
        #  intentionally NOT called here -- it is a side-effect-only,
        #  discarded-return-value diagnostic in the old path with nothing
        #  downstream depending on it (plan §7). Left commented out so it is
        #  easy to revive if needed:
        # plot_train_and_test_stuff_for_one_RS (rs_name, train_pred_true_df, test_pred_true_df, ...)

    return (the_plot)
    }

#===============================================================================

#  Legacy scores adapter (Checkpoint 4).  Projects bundle$metrics onto the
#  exact legacy all_fitting_scores_df column shape (plan §3, produced by
#  add_to_full_fitting_scores() in R/v1_paper_3_fitting_functions.R) and
#  row-binds onto the running scores frame, so downstream table/summary code
#  in the Rmd stays untouched.
#
#  Column VALUES are reproduced exactly (see the LM equivalence test); row
#  ORDER is not: add_to_full_fitting_scores() appends all-RS-TRAIN-then-
#  all-RS-TEST per call, while fit_output_error_for_feature_set() appends
#  TRAIN-then-TEST per RS (interleaved). Nothing downstream depends on row
#  order (all_fitting_scores_df is only ever consumed via
#  filter()/group_by()/summarize() for tables and bar charts) -- flagged for
#  author review as an intentional, low-stakes divergence rather than
#  reworking Checkpoint 2's already-tested accumulation order to match.
#
#  all_fitting_scores_df - the running legacy-shaped frame, or NULL/0-row to
#                          start a new one
#  bundle                 - a bdpg_fit_result from fit_output_error_for_feature_set()
#
#  Returns the updated legacy-shaped data frame.

bind_fitting_scores <- function (all_fitting_scores_df, bundle)
    {
    legacy_rows =
        data.frame (train_or_test     = bundle$metrics$ds_label,
                    fitting_model_str = bundle$metrics$fitting_model_str,
                    vars_used_str     = bundle$metrics$feature_set_label,
                    measure_name_str  = bundle$metrics$error_type_label,
                    rs_method_name    = bundle$metrics$rs_name,
                    rmse              = bundle$metrics$rmse,
                    R2                = bundle$metrics$R2,
                    adj_R2            = bundle$metrics$adj_R2,
                    stringsAsFactors  = FALSE)

    if (is.null (all_fitting_scores_df) || nrow (all_fitting_scores_df) == 0)
        return (legacy_rows)

    return (rbind (all_fitting_scores_df, legacy_rows))
    }

#===============================================================================

#  Orchestrator (Checkpoint 4): fit + bind scores + the save_final_model
#  gate. No plotting here (plan §6) -- plot_output_error_fit() is called
#  separately by the caller on the returned bundle.
#
#  The save_final_model gate is consumed here as an already-resolved boolean
#  (plan §8: "Is this a final-model run?" is a property of the whole run, not
#  of any individual fit, so it is resolved ONCE, globally, by the caller --
#  e.g. params$save_final_model in the eventual Rmd -- and this function only
#  acts on it mechanically). TRUE fits the final workflow on the full
#  training pool and saves a versioned artifact; FALSE (default) skips
#  silently. The save body is a stub this round -- see the "save_final_model"
#  / "Fitted-model carriage and final-model persistence" entries in
#  DECISIONS.md (2026-06-28) and FUTURE_CHATS.md FC-4.
#
#  all_fitting_scores_df - the running legacy-shaped frame to bind onto
#                          (NULL/0-row to start a new one)
#  ...                    - all fit_output_error_for_feature_set() arguments,
#                          passed through unchanged (see that function's
#                          header comment)
#  save_final_model       - resolved boolean; see above
#
#  Returns list (bundle, all_fitting_scores_df).

run_output_error_fit <- function (
    all_fitting_scores_df,
    rs_method_names_list,
    train_x_df, test_x_df,
    working_train_df, working_test_df,
    train_aux_df, test_aux_df,
    target_col_name,
    recipe,
    learner_spec,
    learner_id,
    vars_used_str,
    measure_name_str,
    fitting_model_str,

    x_min_on_plot = NA, x_max_on_plot = NA,
    y_min_on_plot = NA, y_max_on_plot = NA,

    seed = NULL,
    keep_workflow = FALSE,
    save_final_model = FALSE
    )
    {
    bundle =
        fit_output_error_for_feature_set (
            rs_method_names_list,
            train_x_df, test_x_df,
            working_train_df, working_test_df,
            train_aux_df, test_aux_df,
            target_col_name,
            recipe, learner_spec, learner_id,
            vars_used_str, measure_name_str, fitting_model_str,

            x_min_on_plot = x_min_on_plot, x_max_on_plot = x_max_on_plot,
            y_min_on_plot = y_min_on_plot, y_max_on_plot = y_max_on_plot,

            seed = seed, keep_workflow = keep_workflow)

    all_fitting_scores_df = bind_fitting_scores (all_fitting_scores_df, bundle)

    if (save_final_model)
        {
            #  TODO (deferred to the final-model round; see DECISIONS.md
            #  2026-06-28 "save_final_model is a single global gate..." and
            #  FUTURE_CHATS.md FC-4): fit the final workflow on the FULL
            #  training pool (not just this call's train_x_df/test_x_df split)
            #  and saveRDS() a versioned artifact. Not implemented -- the
            #  final model is not trained until the pipeline is frozen and
            #  the sequestered batches are unlocked.
        }

    return (list (bundle = bundle, all_fitting_scores_df = all_fitting_scores_df))
    }

#===============================================================================
