<!--
  Append-ready DECISIONS.md entry, formatted to match the existing
  ProblemDifficulty/_claude_project_guide/DECISIONS.md house style
  (H1 "Decisions Log Entry: <date>", "## Session:", "###" subsections).
  Paste the block below (from the H1 line down) after the last existing entry.
-->

# Decisions Log Entry: 2026-06-28

## Session: Plan for refactoring the error-prediction fitting & evaluation pipeline (paper 9 / bdpgtext2)

### Context

Planning-only chat in claude.ai (no code written). Goal: design a stepwise, test-driven
plan for a later Claude Code session to rebuild the fitting / evaluation / plotting of the
output-error prediction models in the "Results - Learning to predict output errors" section
of `p9_v01_all_combined__body.Rmd`. The two error targets are representation shortfall
(column `rsr_COR_spp_rep_shortfall`) and solution cost error (column
`rs_solution_cost_err_frac`), fitted for four reserve selectors across several input feature
sets. Output of the chat: `bdpg_fitting_refactor_plan.md`, plus a `FUTURE_CHATS.md` running
file.

### Enabling insight

The learning-method dependency in the current code collapses to a single `predict()` call;
everything downstream (`eval_model_on_train_or_test_data()` and the plotting) is already
method-agnostic and consumes prediction vectors. So the refactor reuses the existing metric
kernel unchanged and replaces only what produces the prediction vectors.

### Decisions made

- Parallel build, not in-place rewrite: the new tidymodels pipeline is built alongside the
  old code and validated by golden-master equivalence before the Rmd is rewired; old path
  kept callable for rollback 

- Reuse `eval_model_on_train_or_test_data()` (in `v1_paper_3_plotting_and_evaluation_functions.R`)
  unchanged; this makes LM equivalence near-exact and isolates the refactor to the fit/predict step 

- Three seams, simplest case only this round: resampling plan (single holdout as a
  one-element plan, `fold_id = 1`), recipe (pass-through; Box-Cox / standardization stay
  upstream in the prep Rmd), learner (parsnip spec) 

- LM is the only equivalence-tested learner; random forest gets a structure-only smoke test
  through the identical workflow path; glmnet and others out of scope 

- Separate plotting from fitting: the fit function returns a data bundle (`metrics`,
  `predictions`, `meta`, optional `workflow`); a separate plot function returns the ggplot;
  the Rmd prints it. Note: the current figure renders only because `show()` runs inside the
  old function, so each Rmd site must explicitly print the returned plot to keep the figure
  under its `fig.cap` 

- Preserve `all_fitting_scores_df` column-for-column so downstream table/summary code is
  untouched 

- adjusted-R² uses `p = number of features` for every learner (placeholder); flagged for a
  future discussion on whether R² is meaningful for non-linear models 

- Preserve the R² definition: `compute_r.squared()` is out-of-sample `1 - SSE/SST` around
  the eval set's own mean — the tidymodels match is `yardstick::rsq_trad()`, NOT the default
  `rsq()` (squared correlation). Likewise do not replace `compute_adj.r.squared()` with
  `summary(lm)$adj.r.squared` or `broom::glance()`, which compute the in-sample form 

- `save_final_model` is a single global gate in the top-of-Rmd params block, default quiet
  `FALSE`; the ~16 per-call fits consume the resolved boolean silently. Real `saveRDS` of the
  final versioned workflow is stubbed with a TODO and deferred to the final-model round 

- Per-RS diagnostic plot (`plot_train_and_test_stuff_for_one_RS()`) dropped from the new path
  but preserved as a commented-out call with a note; nothing downstream depends on it 

- Golden master: two things are frozen — inputs (a committed snapshot copy of the already-
  existing `(train_df, test_df)` under `tests/fixtures/inputs/`) and outputs (the old
  pipeline's adj-R²/RMSE/R²/predictions under `tests/fixtures/golden/`). Claude Code drafts
  the capture script; the author runs it on the full batches and commits the canonical
  golden; Claude Code regenerates the fast-subset golden 

- Naming / location: new file `R/v1_paper_9_fitting_and_eval_pipeline.R` (the `v1_paper_9`
  prefix records when/where the code was added); its relationship to
  `v2_paper_3_cv_test_train_splitting_functions.R` is stated in a header comment, not implied
  by prefix. `bdpgtext2` is a plain project repo (not a package), so fixtures live in
  `tests/fixtures/` 

### Deferred to their own future chats (tracked in FUTURE_CHATS.md)

- Full train/test evaluation methodology: group-aware k-fold CV (COR as group), split
  conformal intervals, coverage diagnostics, paired feature-set comparison, sequestration
  discipline (the substance of `bdpg_methodology_handoff_summary.md`) 

- R² / adjusted-R² appropriateness for non-linear models 

- How to display multi-fold results (pooled held-out scatter; mean ± SD; error bars on the
  summary bar chart; paired differences for the headline claim) 

- Final-model persistence implementation (versioned `saveRDS` of the fitted workflow; gated
  on frozen pipeline + unlocked sequestered batches) 

- Promoting RF / glmnet to fully validated, reported learners 

### Concept established

- "Seam" (Michael Feathers, *Working Effectively with Legacy Code*, 2004): a place where
  behavior can be changed without editing the code at that spot, via an enabling point (e.g.
  a parameter). Distinct from an abstraction: an abstraction is the thing that varies; a seam
  is the place it varies plus the guarantee no surgery is needed there 

### Next steps

- Author to do a careful read of `bdpg_fitting_refactor_plan.md` and flag edits (revise the
  file in place rather than pasting into chat) 

- Approve (or amend) the proposed function names in §10 of the plan (file name and location
  already approved) 

- Continue in a fresh chat (context limit approaching) using
  `SEED_next_chat_fitting_refactor.md`; first topic is remaining golden-master questions 
