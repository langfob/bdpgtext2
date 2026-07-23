# SEED — continuation chat: bdpg fitting/eval refactor planning

**Purpose of this file.** This is a handoff seed for a *new* chat that continues a long planning conversation which was approaching its context limit. Paste or upload this at the start of the new chat. It records what was decided, why, and what is still open — so the new chat does not re-litigate settled questions or re-derive conclusions.

**Nature of the work:** planning only. **No code has been written and none should be written** unless the author explicitly asks. The deliverable of the original chat was a plan for a *later* Claude Code session.

---

## 1. Read these first

Three artifacts were produced by the original chat. They are the source of truth; this seed is the connective tissue around them.

- **`bdpg_fitting_refactor_plan.md`** — the main deliverable. Stepwise, checkpointed, test-driven plan for Claude Code. Contains: context/goal, file map, invariants, target architecture (the three seams), test strategy, Checkpoints 0–6 with Definitions of Done, do-not-touch list, naming, deferred decisions/placeholders, runtime open questions.  
- **`DECISIONS_entry_2026-06-28_fitting_refactor.md`** — append-ready entry for the author's `ProblemDifficulty/_claude_project_guide/DECISIONS.md`.  
- **`FUTURE_CHATS.md`** — new running file; five topics (FC-1…FC-5) deferred to their own dedicated chats.  

**Project files that were examined** (available in the project; re-read as needed rather than trusting summaries): `p9_v01_all_combined__body.Rmd`, `v1_paper_3_fitting_functions.R`, `v1_paper_3_plotting_and_evaluation_functions.R`, `v2_paper_3_cv_test_train_splitting_functions.R`, `p9_v01_prep_data_for_p8_to_load_from_files.Rmd`, `v1_Paper_3_func_defns_for_tidymodels.R`, `bdpg_methodology_handoff_summary.md`, plus `CLAUDE.MD`, `PROJECT_OVERVIEW.md`, `CLEANUP_GOALS.md`, `DIRECTORY_INVENTORY.md`.

---

## 2. Author's working preferences (important — follow these)

- **Do not rewrite or refactor working code** without explicit permission. No opportunistic cleanup ("while I'm here"). Additions must be small, surgical, testable increments.  
- **Prefers output written to `.md` files** rather than pasted inline in chat (copy-paste mangles formatting). Inline copies are fine *in addition* to the file.  
- Markdown style: blank line before the first item and after the last item of every list (including nested); 4-space indents for nested levels; trailing space at the end of each list element.  
- Background: strong scientific and object-oriented programming experience; recent work mostly R/RStudio; limited web/mobile knowledge.  
- Asked for a token-usage estimate at the top of each reply. Note honestly that this can only be a rough estimate.  
- Verify citations actually exist; do not hallucinate references.  
- Only responds to / expects replies on messages ending in "READY" or "✓".  

---

## 3. The problem in one paragraph

In the paper's section "Results - Learning to predict output errors" (`p9_v01_all_combined__body.Rmd`), predictive models of two output-error types (representation shortfall, column `rsr_COR_spp_rep_shortfall`; and solution cost error, column `rs_solution_cost_err_frac`) are fitted for four reserve selectors across several input feature sets. Each subsection calls `build_feature_set_specific_test_and_train()` then `fit_and_predict_output_error_using_feature_set()`. The current implementation is deeply nested and abstract (originally built to allow swapping learners), hard to read and extend. The refactor aims to: make learners swappable via tidymodels; make train/test evaluation schemes swappable later; make the code legible; and separate plotting from fitting so the plot object and metrics are returned rather than `show()`n as a side effect.

**The key insight that makes it tractable** (do not lose this): the learning-method dependency collapses to a *single* `predict()` call. Everything downstream — `eval_model_on_train_or_test_data()` and the plotting — is already method-agnostic and consumes prediction vectors. So the refactor reuses the existing metric kernel unchanged and replaces only what produces the prediction vectors. That is why the old layers of abstraction bought so little.

---

## 4. Settled decisions (do NOT re-open without cause)

Architecture and scope:

- **Parallel build.** New pipeline alongside old; old path stays callable; Rmd rewired only at late checkpoints behind a flag.  
- **Reuse `eval_model_on_train_or_test_data()` unchanged** (in `v1_paper_3_plotting_and_evaluation_functions.R`) — this is what makes LM golden-master equivalence near-exact.  
- **Three seams, simplest case only this round:** resampling plan (single holdout as a one-element plan, `fold_id = 1`), recipe (pass-through; Box-Cox/standardization stay upstream in the prep Rmd), learner (parsnip spec).  
- **Scope boundary:** input contract is the existing pre-built, pre-transformed `(train_df, test_df)` pair. No splitting, no CV this round.  
- **LM only as equivalence-tested learner**; random forest gets a structure-only smoke test through the identical workflow path; glmnet out of scope.  
- **Plotting separated from fitting.** Fit function returns a bundle: `metrics`, `predictions`, `meta`, plus optional `workflow` (default off). A separate plot function returns the ggplot. Proposed S3 class `bdpg_fit_result`.  
- **Subtle catch:** the current figure renders under its `fig.cap` *only because* `show()` runs inside the old function. The new Rmd chunks must explicitly print the returned plot or the figure silently disappears from the knit.  
- **`save_final_model`:** one global gate in the top-of-Rmd params block, default quiet `FALSE`; the ~16 per-call fits consume the resolved boolean silently (a per-call tripwire would fire 16× and halt). Real `saveRDS` body is stubbed with a TODO, deferred to the final-model round.  
- **Per-RS diagnostic plot** (`plot_train_and_test_stuff_for_one_RS()`) dropped from the new path but preserved as a commented-out call with a note; nothing downstream depends on it.  

Invariants to preserve exactly:

- `all_fitting_scores_df` column-for-column: `train_or_test`, `fitting_model_str`, `vars_used_str`, `measure_name_str`, `rs_method_name`, `rmse`, `R2`, `adj_R2`.  
- **R² definition:** `compute_r.squared()` is out-of-sample `1 − SSE/SST` with SST around the **eval set's own mean** — the tidymodels match is `yardstick::rsq_trad()`, **not** the default `rsq()` (squared correlation). A known trap.  
- **adjusted R²:** `1 − (1 − R2)·((n−1)/(n−p−1))` via `compute_adj.r.squared()`, with `p` = number of feature columns (legacy `ncol(train_x_df)`).  
- The faceted figure: one subplot per reserve selector, scatter colored by `dom_err_type`, per-facet adj-R²/rmse annotation upper-left, perfect-fit diagonal.  

Environment and naming:

- **`bdpgtext2` is a plain project repo, NOT an R package.** Fixtures live in `tests/fixtures/` (`inputs/` and `golden/`).  
- **New file:** `R/v1_paper_9_fitting_and_eval_pipeline.R`. The `v1_paper_9` prefix records when/where the code was added (author's `v?_paper_?` convention). Its relationship to `v2_paper_3_cv_test_train_splitting_functions.R` goes in a header comment, **not** implied via a shared prefix.  
- **Golden master ownership:** the **author** generates and commits the canonical full-batch golden from the *old* code in their trusted environment; Claude Code may freely regenerate the *fast-subset* golden (deterministic subsample of COR groups, all 4 selectors × 2 error types).  
- **Claude Code's ability to run R** is governed entirely by the author's local environment (`Rscript` on PATH + packages installed in the library that interpreter sees) — nothing Anthropic-side gates it. The plan's Checkpoint 0 opens with an environment check.  
- **COR grouping key** (for later CV): `rsp_UUID_of_COR_Base_problem_that_is_wrapped`.  
- Seeds: irrelevant this round (LM single-holdout is deterministic); matter when CV/RF/bootstrap land.  

Assessed and closed:

- `v1_Paper_3_func_defns_for_tidymodels.R` was examined and is **not useful** for this refactor — it is abandoned splitting-stage plumbing (contains a live `browser()`), with tidymodels usage limited to `initial_split`/`vfold_cv`; no recipe/parsnip/workflow/yardstick code was ever written. One idea worth keeping for FC-1: assign fold membership once and join it back so every reserve selector is evaluated on the identical problem partition — but express it via `group_vfold_cv(group = rsp_UUID_of_COR_Base_problem_that_is_wrapped)`, not the manual `cv_set_ID` loop.  

---

## 5. Concepts established in the original chat (reuse this vocabulary)

- **Seam** (Michael Feathers, *Working Effectively with Legacy Code*, 2004): a place where behavior can be changed *without editing the code at that spot*. Metaphor from sewing — the joint where a garment is altered. Requires an **enabling point** (the place where you choose which behavior takes effect, e.g. a parameter).  
- **Seam vs. abstraction:** an abstraction is a *thing* (concept/type/interface — what varies); a seam is a *property of a place* (where it varies, plus the guarantee no surgery is needed there). You can have an abstraction with no seam (no enabling point) and a seam with almost no abstraction (the one-element resampling plan). Abstractions are judged by domain fit; seams by *leverage*.  
- **Golden master / characterization testing:** capture the OLD pipeline's outputs as frozen fixtures, then assert the new code reproduces them. Must be captured from the old code, before the new code exists, in a trusted environment — otherwise the test proves only that the new code agrees with itself.  

---

## 6. OPEN — what the new chat is for

The author explicitly said they **have more questions, including about the golden master**. Nothing below was resolved.

Known open threads:

- **Golden master mechanics** — the author's stated next topic. Already resolved in the chat that produced this seed: (a) two distinct things are frozen — *inputs* (a committed snapshot copy of the already-existing `(train_df, test_df)` under `tests/fixtures/inputs/`) and *outputs* = the golden (the old pipeline's numbers under `tests/fixtures/golden/`); (b) division of labor — Claude Code drafts the capture script, the author writes no capture code but runs it on the full batches and commits the canonical golden, Claude Code regenerates the fast-subset golden. Still genuinely open: what exactly to capture (metrics, predictions, plot); what tolerance counts as "equivalent"; how the fast subsample is chosen and whether it is representative; what to do if full-batch and subset goldens disagree; how goldens are refreshed when a *legitimate* change occurs; committed-fixture size; whether to draft the capture script now.  
- **Author's careful read of `bdpg_fitting_refactor_plan.md`** was still pending; edits may follow. Revise the file in place rather than pasting a new version into chat.  
- **`DECISIONS.md` formatting** — the author's existing `ProblemDifficulty/_claude_project_guide/DECISIONS.md` was never uploaded. The entry currently uses a generic dated H2 format. If the file is uploaded, reformat the entry to match its conventions.  
- **Function names in §10 of the plan** were proposed but not explicitly approved (file name and location *were* approved). Proposed: `make_bdpg_resampling_plan()`, `make_bdpg_recipe()`, `make_bdpg_learner()`, `fit_output_error_for_feature_set()`, `plot_output_error_fit()`, `bind_fitting_scores()`, `run_output_error_fit()`; class `bdpg_fit_result`.  

Deferred to their own chats (tracked in `FUTURE_CHATS.md` — do not fold these into this one):

- **FC-1** Train/test evaluation methodology: group-aware k-fold CV (COR as group), split conformal intervals, coverage diagnostics, paired feature-set comparison, sequestration discipline. This is the substance of `bdpg_methodology_handoff_summary.md`.  
- **FC-2** R²/adjusted-R² appropriateness for non-linear models.  
- **FC-3** Displaying multi-fold results (pooled held-out scatter; mean ± SD; error bars on the summary bar chart, not the scatter; paired differences for the headline claim).  
- **FC-4** Final-model persistence implementation (versioned `saveRDS` of the fitted workflow; gated on frozen pipeline + unlocked sequestered batches). Note the distinction established: reusing a *fitted* model = "predict on a new test batch"; "re-evaluate under new folds" *refits* per fold and reuses only the spec — keeping these separate avoids reintroducing train-on-test leakage.  
- **FC-5** Promoting RF/glmnet to fully validated, reported learners (RF cannot be golden-matched to the old RF path — different engines/seeds/defaults plus a 2024 bug-fix; glmnet pulls in penalty tuning).  

---

## 7. Suggested opening move for the new chat

Confirm the artifacts above are available, then go straight to the author's golden-master questions. Do not re-derive the architecture; it is settled. Do not write code.
