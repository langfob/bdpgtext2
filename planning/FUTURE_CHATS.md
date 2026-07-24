# FUTURE_CHATS.md

A running list of topics that have been deliberately deferred to their **own dedicated chats** because they are too large or detailed to fold into the chat that raised them. Each entry records what the topic is, why it deserves a standalone chat, what it depends on (its trigger), and where it was first flagged. Add to this file whenever a chat surfaces something that "needs its own conversation later," so these don't get lost inside placeholder/deferred-decision lists.

Status key: **open** (not yet started), **scheduled**, **in progress**, **done** (with a pointer to where it landed).

---

## FC-1 — Train/test evaluation methodology (group-aware CV, conformal intervals, coverage, paired comparisons)

- **Status:** open  
- **What:** Design the full evaluation methodology the fitting pipeline will eventually support: group-aware k-fold cross-validation with the COR base problem as the grouping unit (`group_vfold_cv(group = rsp_UUID_of_COR_Base_problem_that_is_wrapped)`), split-conformal prediction intervals, interval-coverage diagnostics, paired feature-set comparison across folds with a paired test, and the sequestration discipline that keeps held-out batches untouched until the pipeline is frozen.  
- **Why its own chat:** This is the substance of `bdpg_methodology_handoff_summary.md` and is a methodological design problem in its own right, with many interacting options. Folding it into the implementation chat would overwhelm it.  
- **Depends on / trigger:** The fitting refactor (with its resampling-plan, recipe, and learner seams) must exist first. Then this chat decides which schemes to instantiate and how preprocessing moves into the per-fold recipe.  
- **First flagged:** fitting-refactor planning chat, 2026-06-28 (`bdpg_fitting_refactor_plan.md` §11, "Evaluation scheme").  

## FC-2 — R² and adjusted-R² for non-linear models

- **Status:** open  
- **What:** Decide what goodness-of-fit value (if any) to display for non-linear learners such as random forest or glmnet. Adjusted R² with `p = number of features` is the current placeholder for all methods; this chat revisits whether adjusted R² — or any R² — is appropriate for non-linear fits, and what to show instead.  
- **Why its own chat:** It is a statistics/interpretation question with paper-facing consequences, separate from plumbing.  
- **Depends on / trigger:** Becomes relevant once a non-linear learner is actually reported (not just smoke-tested).  
- **First flagged:** fitting-refactor planning chat, 2026-06-28 (plan §11, "adjusted-R² predictor count"; original answer to design question 2).  

## FC-3 — Displaying multi-evaluation (multi-fold) results

- **Status:** open  
- **What:** How to present results when there are many evaluations per cell (e.g. k folds): pooling held-out predictions across folds into one scatter (each problem appears once); reporting metrics as mean ± SD; placing error bars on the summary bar chart rather than the scatter; printing the cross-validated mean in the per-facet annotation; using per-fold paired differences for the headline feature-set claim.  
- **Why its own chat:** Display choices interact with the methodology (FC-1) and with what is statistically defensible; needs deliberate design rather than an ad hoc default.  
- **Depends on / trigger:** FC-1 (a multi-fold scheme must exist to display).  
- **First flagged:** fitting-refactor planning chat, 2026-06-28 (plan §11, "Multi-evaluation display").  

## FC-4 — Final-model persistence

- **Status:** open  
- **What:** Implement the deferred save step: fit the final reported model on the full training pool and `saveRDS()` a versioned fitted workflow as a durable artifact, so it can later be applied to new test batches (e.g. a reader generating their own data with the bdpg package). The pipeline already has the `save_final_model` gate and a stubbed save body.  
- **Why its own chat:** Tied to the sequestration discipline — the final model is not trained until the pipeline is frozen and the sequestered batches are unlocked — so it is a distinct, later milestone with its own correctness concerns (versioning, leakage discipline).  
- **Depends on / trigger:** Pipeline frozen; sequestered batches unlocked; FC-1 settled.  
- **First flagged:** fitting-refactor planning chat, 2026-06-28 (plan §8 and §11, "Fitted-model carriage and final-model persistence").  

## FC-5 (optional) — Promoting RF / glmnet to reported, validated learners

- **Status:** open  
- **What:** Move random forest (and possibly glmnet) from smoke-test-only to fully validated, reported options: weakened-equivalence testing for the stochastic RF, and penalty (λ) tuning for glmnet (pulling in `tune` + resampling-for-tuning).  
- **Why its own chat:** Adds a tuning/validation surface and test-matrix growth that is separable from the core refactor.  
- **Depends on / trigger:** Core refactor done; a decision that a non-linear learner will actually be reported (overlaps FC-2).  
- **First flagged:** fitting-refactor planning chat, 2026-06-28 (plan §11, "Learners available day one").  

## FC-6 — "All" feature set exact collinearity (`ig_num_edges_m`)

- **Status:** open  
- **What:** `ig_num_edges_m` is an exact linear combination of two other "All"-feature-set predictors (`edge_frac_of_possible x sppPUprod`), confirmed via `caret::findLinearCombos()`. This makes the "All" feature set's design matrix rank-deficient by exactly 1, for every reserve selector, at full data scale -- not a subsampling artifact. It is pre-existing OLD-pipeline behavior (the old `lm()` call already emits "prediction from rank-deficient fit" warnings on "All"; the new pipeline reproduces this bit-for-bit). Decide whether/how to address it -- likely via a `recipes::step_lincomb()` or `step_zv()` step once the recipe seam takes on real preprocessing (see the "Preprocessing ownership" placeholder in the plan §11), rather than manually editing the `inVars` list, since a recipe step would also catch any other near-collinear variables in "All" that have not been specifically checked.  
- **Why its own chat:** Removing or transforming a variable in "All" changes that feature set's actual reported `adj_R2`/`rmse`/`R2` in the current manuscript -- a paper-content / methodology decision, not code-refactor plumbing. It also requires re-deriving and re-reviewing the full-batch golden master. Out of scope for the "do not modify working code" refactor discipline (plan §9).  
- **Depends on / trigger:** None strictly -- can be picked up whenever the author wants to revisit "All" feature set methodology, or naturally alongside FC-1 (evaluation methodology) once the recipe seam gets real preprocessing steps.  
- **First flagged:** Checkpoint 2 implementation session, 2026-07-24, while building the fast-inner-loop self-regression fixture (see DECISIONS.md 2026-07-24 entry). The fast fixture works around this by excluding "All" (covered instead by the full-batch LM equivalence test, which is unaffected).  

## FC-7 — Retire the old fitting/plotting path

- **Status:** open  
- **What:** Checkpoint 6 (2026-07-24) kept the old path as a rollback safety net rather than removing it (author's explicit choice). This entry is the checklist for whenever that changes. Not a methodology question like the others above -- purely mechanical, but detailed enough to be worth writing down now rather than reconstructing later:

    1. **In `Paper_9_heavily_abridged_version_of_p8/p9_v01_all_combined__body.Rmd`:**
        - For each of the 8 wired `predict*` chunks (see DECISIONS.md Checkpoints 5-6 entries for the full list), delete the `if (params$use_new_fitting_pipeline) { ... } else { ... }` wrapper and the `else` branch's old `fit_and_predict_output_error_using_feature_set()` call, keeping only the (now unconditional) new-path body.  
        - Remove the `use_new_fitting_pipeline` param from the params block. Keep `save_final_model` -- that gate is unrelated to old-vs-new and still applies.  
        - Retire the golden-master capture instrumentation, since its only purpose was confirming the new pipeline against the old one, which will no longer exist to capture from: the `capture_golden_master` / `force_golden_master` params, the `goldenMasterOverwriteGuard` chunk, and the `captureGoldenMasterInputsAndScores` chunk (plan §5's own framing: "once the new pipeline is confirmed, the old pipeline and this capture instrumentation are retired").  

    2. **In `R/`, do NOT delete `v1_paper_3_fitting_functions.R`, `v1_paper_3_plotting_and_evaluation_functions.R`, or `v1_paper_3_utility_functions.R` wholesale** -- the new pipeline (`R/v1_paper_9_fitting_and_eval_pipeline.R`) directly calls and still needs, from those files: `build_feature_set_specific_test_and_train()` (and transitively `select_cols_for_learning()`) from the fitting-functions file; `eval_model_on_train_or_test_data()` (and transitively `compute_r.squared()` / `compute_adj.r.squared()` / `bdpg.regr.eval()`) and `force_dom_err_type_colors()` from the plotting/eval file; `convert_rs_method_name_to_ordered_factor()` from the utility-functions file. Everything else in those files that was only reachable from the now-deleted old Rmd calls (`fit_and_predict_output_error_using_feature_set()`, `fit_rep_shortfall()`, `fit_cost_err_frac()`, `fit_to_target_var()`, `fit_one_rs()`, `fit_and_plot()`, `f_lm()`, `f_rf_party()`, `f_rf_ranger()`, `f_glmnet_caret()`, `f_glmnet_UC()`, `gen_full_fits()`, `plot_full_fits()`, `ppe_for_train_and_test_for_one_RS()`, `ppe_for_train_and_test_given_preds_for_one_RS()`, `plot_train_and_test_stuff_for_one_RS()`, `save_this_ggplot()`, etc.) becomes dead code -- but confirm with a real usage grep across the whole repo (not just this Rmd) before deleting any of it, since some of these files' other functions may be used elsewhere (e.g. bar-chart/summary code downstream of `all_fitting_scores_df` is a separate consumer, not part of the fit-call chain, and needs its own check).  

    3. **Tests:** `test-fitting-pipeline-lm-equivalence.R` and the equivalence part of `test-fitting-pipeline-scores-adapter.R` compare the new pipeline against the committed golden *data* (RDS files), not against live old-pipeline execution -- they can stay as pure regression tests against the frozen golden even after the old R functions that originally produced it are gone. `test-golden-master-capture.R` and the "OLD branch parses"/byte-identity-reliant parts of `test-fitting-pipeline-rmd-wiring.R` specifically exercise the capture instrumentation and old-path chunk text and should be removed or rewritten once that instrumentation and those branches are gone.  
        - `tests/fixtures/` (the golden master + self-regression fixtures) do not need to be deleted -- the self-regression fixture keeps protecting the new pipeline regardless of the old path's existence, and the golden `all_fitting_scores_df` can keep serving as a frozen reference for the equivalence-turned-regression tests above.  

    4. **Optional, cosmetic:** once `R/v1_paper_9_fitting_and_eval_pipeline.R` is no longer a parallel build but *the* pipeline, consider whether its name / header comment (currently framed as "the parallel-build target... not a replacement (yet)") should be updated to say so plainly. Author's call, not required.  

- **Why its own item (not a full dedicated chat):** Mechanical follow-through on an already-made architectural decision, not a new design question -- doesn't need the weight of FC-1 through FC-6, just a checklist so nothing gets missed or silently broken (in particular item 2's dependency trap: naively deleting the "old" files would break the new pipeline).  
- **Depends on / trigger:** Author decides to stop keeping the old path as a rollback safety net.  
- **First flagged:** Checkpoint 6 implementation session, 2026-07-24, in response to the author asking whether removal steps were written down anywhere (they weren't, until this entry).  
