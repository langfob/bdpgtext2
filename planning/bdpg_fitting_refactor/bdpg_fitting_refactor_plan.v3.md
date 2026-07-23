# Plan: refactor of the error-prediction fitting & evaluation pipeline (bdpg paper)

**Audience:** a Claude Code session working in the `bdpgtext2` repo.
**Status:** design-complete handoff. Implement in the staged, checkpointed order below. Stop at the end of each checkpoint and wait for the human (referred to here as "the author") to review before proceeding.
**Hard rule:** this is a *parallel* build. Do not modify working code or the analysis Rmd's existing behavior until the explicit wiring checkpoints (5–6), and only then behind a switch with the old path still callable.

---

## 1. Context and goal

The paper fits per-problem predictive models of two reserve-selection output-error types (representation shortfall, the column `rsr_COR_spp_rep_shortfall`; and solution cost error, the column `rs_solution_cost_err_frac`) for four reserve selectors, across several input feature sets. Fitting, evaluation, and plotting currently live in a deeply nested set of functions that are hard to read, hard to extend with new learners, and hard to re-evaluate under different train/test schemes.

The refactor has four goals:

- Make the **learning method swappable** (LM now; random forest and others later) via a tidymodels workflow.  
- Make the **train/test evaluation scheme swappable** later (k-fold CV, grouped CV, etc.), by structuring the code around a resampling plan even though only single-holdout runs in this round.  
- Make the code **legible** by collapsing the abstraction layers down to the one place that actually varies (the fit/predict step).  
- **Separate plotting from fitting** and return structured results (plot object, metrics, predictions) so they can be displayed in the Rmd and reused elsewhere.  

The single enabling insight: in the current code, the learning-method dependency collapses to one `predict()` call. Everything downstream (`eval_model_on_train_or_test_data()` and the plotting) is already method-agnostic and consumes prediction vectors. So the refactor reuses the existing metric kernel unchanged and only replaces what produces the prediction vectors.

### Scope boundary for this round

- Input contract is the existing **pre-built, pre-transformed** `(train_df, test_df)` pair produced upstream by the data-prep Rmd. The new code treats this pair as a **one-element resampling plan** (`fold_id = 1`). It does not split data and does not do cross-validation.  
- Preprocessing (Box-Cox, standardization) stays **upstream** in the data-prep Rmd for this round. The new pipeline carries a **pass-through recipe** as a seam so preprocessing can later move inside the pipeline.  
- **LM** is the only equivalence-tested learner. **Random forest** gets a single smoke test through the identical workflow path (runs and returns a well-formed result; no numeric equivalence). glmnet and others are out of scope.  

---

## 2. Files involved

### Read / depend on (DO NOT MODIFY — see do-not-touch list, §9)

- `R/v1_paper_3_fitting_functions.R` — current fitting orchestration. Entry points: `build_feature_set_specific_test_and_train()`, `fit_and_predict_output_error_using_feature_set()`. Read to understand current behavior and to call when capturing golden masters.  
- `R/v1_paper_3_plotting_and_evaluation_functions.R` — contains the **metric kernel** `eval_model_on_train_or_test_data()` (and helpers `compute_r.squared()`, `compute_adj.r.squared()`, `bdpg.regr.eval()`). The new pipeline **calls `eval_model_on_train_or_test_data()` unchanged**. Also contains `plot_train_and_test_stuff_for_one_RS()` (a per-reserve-selector diagnostic plot — see §7).  
- `R/v2_paper_3_cv_test_train_splitting_functions.R` — upstream COR-disjoint splitting. Not used in this round; relevant later for CV.  
- `p9_v01_prep_data_for_p8_to_load_from_files.Rmd` — upstream data prep; produces `p3_working_train_df` / `p3_working_test_df` and does Box-Cox + standardization. Reference only.  
- `p9_v01_all_combined__body.Rmd` — the analysis document. Section "Results - Learning to predict output errors". Modified only at checkpoints 5–6.  

### Create (new)

- `R/v1_paper_9_fitting_and_eval_pipeline.R` — the new pipeline. The `v1_paper_9` prefix marks when/where this block of code was first added; include a header comment in the file noting its relationship to `R/v2_paper_3_cv_test_train_splitting_functions.R` (the upstream COR-disjoint splitting it will eventually consume) rather than implying that relationship through a shared prefix.  
- Test files and fixtures under the test tree (paths resolved in Checkpoint 0).  

---

## 3. Current behavior to preserve (the invariants / equivalence targets)

These are what the golden-master and structural tests assert against. The refactor is correct only if all hold.

- **Per-cell metrics match.** For each `(reserve selector, feature set, error type, TRAIN/TEST)`, the new pipeline reproduces the existing `adj_R2`, `rmse`, and `R2`. LM is deterministic, so this should match to floating-point tolerance.  
- **`all_fitting_scores_df` is reproduced column-for-column.** The existing accumulator (`add_to_full_fitting_scores()`) produces a data frame with exactly these columns, one row per reserve selector per TRAIN/TEST call:  

    - `train_or_test` (`"TRAIN"` / `"TEST"`)  
    - `fitting_model_str` (from `params$fitting_model_str`)  
    - `vars_used_str`  
    - `measure_name_str` (the `perf_metric_name_for_file_name_str` argument, e.g. `"abs_rep_shortfall_resid"`)  
    - `rs_method_name`  
    - `rmse`  
    - `R2`  
    - `adj_R2`  

  The new path must produce rows bindable into this identical shape and values, so downstream table/summary code in the Rmd (which factors `vars_used` and builds bar charts) is untouched.  
- **The faceted figure is visually preserved.** One subplot per reserve selector; true-vs-predicted scatter colored by `dom_err_type`; per-facet text annotation showing adjusted R² and rmse in the upper-left; the perfect-fit diagonal reference line. The metric printed per facet is the **TEST** value by default (controlled today by `display_train_as_final_pred_using_plot`).  
- **adjusted R² definition is preserved exactly.** `adj_R2 = 1 - (1 - R2) * ((n - 1) / (n - p - 1))`, with `p` = number of feature columns of the x data frame at the fit point (the legacy `ncol(train_x_df)` count), via the existing `compute_adj.r.squared()`. The golden values encode this; reproduce them rather than re-deriving `p` independently.  
- **R² definition is preserved exactly.** `compute_r.squared()` is the out-of-sample form `1 - SSE/SST`, where SST is taken **around the evaluation set's own mean**, not the squared-correlation form. (See the preserved-behavior note in §11 about `yardstick::rsq_trad()` vs `rsq()`.)  

---

## 4. Target architecture (the seams)

Three substitution points ("seams"), each built now even though only the simplest case is exercised this round. Each seam is an **enabling point** (a parameter the caller supplies), not just an abstraction.

- **Resampling-plan seam.** The pipeline consumes a list of `(analysis, assessment)` pairs. This round the list has exactly one element built from the incoming `(train_df, test_df)`, tagged `fold_id = 1`. The fit/evaluate loop already iterates over the list, so swapping in a k-fold or grouped plan later is a data change at the call boundary, not a restructure. Use `rsample` to represent the plan where natural (e.g. a one-row `manual_rset`-style object, or a thin internal list with the same shape).  
- **Recipe seam.** The pipeline takes a `recipe` object. This round it is **pass-through** (formula + role assignment only; no transform steps), because data arrives pre-transformed. Later, `step_BoxCox()` / `step_normalize()` move into this recipe so preprocessing is re-estimated per fold inside the resampling loop. A tidymodels `workflow()` carries the prepped recipe together with the fitted model, so applying a saved workflow to new data re-applies the training-derived preprocessing (leakage-safe).  
- **Learner seam.** The pipeline takes a parsnip model spec. Default `linear_reg() |> set_engine("lm")`. The RF smoke test passes `rand_forest() |> set_engine("ranger") |> set_mode("regression")` through the identical path. `augment()`/`predict()` on the fitted workflow normalizes prediction output, which removes the legacy `must_specify_predictions_vector` special-casing (ranger's `$predictions` quirk).  

### Data flow (new path)

```
(train_df, test_df)
    -> make resampling plan  [one element, fold_id = 1]
    -> for each fold:
         build workflow(recipe, learner spec)
         fit on analysis set
         predict on analysis and assessment sets   (augment)
         call eval_model_on_train_or_test_data() UNCHANGED on each
           -> {true_vs_pred_df, rmse_value, R2, adj_R2}
    -> assemble result bundle (metrics, predictions, meta, [optional workflow])
```

Fitting produces **data only** — no ggplot, no `show()`. Plotting is a separate function the Rmd calls on the bundle.

### Result bundle (returned by the fit function)

A named list (proposed S3 class `bdpg_fit_result`, pending approval):

- `metrics` — tidy data frame, one row per `(rs_name, ds_label ∈ {TRAIN, TEST}, fold_id)`. Columns include `adj_R2`, `rmse`, `R2`, `num_predictors`, plus the labels needed downstream (`feature_set_label` / `vars_used_str`, `error_type_label` / `measure_name_str`, `fitting_model_str`). Superset of the legacy `all_fitting_scores_df` rows; an adapter projects it to the exact legacy columns (§3).  
- `predictions` — tidy data frame, one row per `(problem, rs_name, ds_label, fold_id)`, columns `true_values`, `pred_values`, `dom_err_type`. This is the pooled `true_vs_pred_df` the scatter consumes; fold-aware so CV pooling later is automatic.  
- `meta` — small list: `feature_set_label`, `error_type`, `target_col_name`, `learner_id`, `recipe`, `seed`, and the annotation label x/y locations (`x_min_on_plot` etc.) so the plot function can place per-facet text.  
- `workflow` — **optional, default off** (flag-controlled). The fitted workflow(s). Useful for inspection and for the single-holdout case where exactly one model exists. Off by default because the eventual CV case has k per cell. Persisting the *final reported* model is a separate, deferred step (§6 gate + §11).  

---

## 5. Test strategy (golden-master / characterization TDD)

The correctness strategy is **golden-master equivalence**: capture the current pipeline's numeric outputs as frozen fixtures, then assert the new pipeline reproduces them.

Two distinct things get frozen, and it is important not to conflate them:

- **Frozen *inputs*** (Checkpoint 0) — a stable copy of the working frames the pipeline consumes: `p3_working_train_df` / `p3_working_test_df` plus the aux frames `p3_train_aux_df` / `p3_test_aux_df`. These already exist (the prep Rmd produces them and the analysis Rmd reads them); the only new work is placing a snapshot copy under version control in the test tree, because the project's data is otherwise not committed. See the capture-point notes in Checkpoint 0 and in "Golden-master specifics" below — the snapshot must be taken **in memory at the fit call site as RDS**, not copied from the on-disk CSVs (details there).  
- **Frozen *outputs* = the golden master** (Checkpoint 1) — the `adj_R2` / `rmse` / `R2` and predictions produced by running the **old** pipeline on those frozen inputs. This is the reference the new code must reproduce.  

Division of labor: **Claude Code drafts the capture script; the author does not hand-write any capture code.** The author *runs* the drafted script once on the full batches (trusted environment, untouched old code) and commits the result as the canonical golden; Claude Code runs the same script on the fast subset.


- **Fixtures are captured from the OLD code, before the new code exists**, in the author's trusted environment. A golden that reflects new-code behavior proves nothing.  
- **Two tiers:**  

    - **Fast fixture** — a deterministic subsample of the prepared frames down to a handful of COR groups (fixed seed; filter on `rsp_UUID_of_COR_Base_problem_that_is_wrapped`) that still contains all 4 reserve selectors × 2 error types. Claude Code may generate this subsample and its golden (by running the old code on the same subsample) freely. This is the inner-loop test.  
    - **Full-batch canonical golden** — the same capture on the full prepared batches. **The author generates and commits this once** as the frozen reference (insulated from environment drift; the paper's numbers trace to the author's environment). Claude Code drafts the capture script; the author runs it.  

- **LM equivalence test:** new core reproduces golden `adj_R2` / `rmse` / `R2` / predictions within tight tolerance (LM via parsnip wraps `stats::lm`, so expect near-exact).  
- **RF smoke test:** the same workflow path with a `rand_forest` spec runs end-to-end and returns a well-formed `bdpg_fit_result`. Assert structure only, not values.  
- **Plot structural test:** the plot function returns a ggplot with one facet per reserve selector and the expected annotation/diagonal layers. (Optional, heavier: a `vdiffr` snapshot test — propose but do not require.)  

### Golden-master specifics (resolved 2026-07-21)

These settle the mechanics that the two-tier and equivalence bullets above left open. They refine, not replace, those bullets.

- **What is captured (and what is not).** Freeze two *data* artifacts per tier: (a) the final `all_fitting_scores_df` (the metric golden), and (b) the per-cell true-vs-predicted frame (the predictions golden, with the keys needed to join per selector / feature set / error type / TRAIN-TEST). Do **not** freeze a rendered plot. The predictions frame is the real ground truth — metrics are a deterministic function of it, and the figure is a deterministic function of both — so freezing predictions lets everything downstream be reconstructed and re-checked. A frozen image would instead couple the test to ggplot/font/OS versions and produce loud, meaningless failures; the figure is guarded by the structural test above (with `vdiffr` optional only).  

- **Equivalence tolerance.** Assert both metrics and predictions at testthat's default tolerance (`~1.5e-8`, absolute+relative). LM through parsnip wraps `stats::lm`, so with an identically built design matrix agreement should be near machine precision. **If the tolerance has to be loosened to pass, do not loosen it — investigate.** The one place equivalence can quietly break is design-matrix construction at the recipe seam: intercept handling, factor contrasts, or column ordering differing between the pass-through recipe/hardhat path and the old code's manual `as.matrix`/`model.matrix`. A tight tolerance is doing double duty as a correctness probe on that seam.  

- **Fast-subset selection rule.** Subsample by COR group, not by row: fixed seed, then take the *smallest* set of distinct `rsp_UUID_of_COR_Base_problem_that_is_wrapped` values that still (a) populates all 4 reserve selectors × 2 error types, (b) retains both `dom_err_type` levels, and (c) leaves every cell non-degenerate for an LM fit (comfortably `n > p + 1`, not rank-deficient). The fast fixture is a plumbing/equivalence gate, **not** an estimate of the science — statistical representativeness of the full-batch numbers is explicitly *not* a goal, because the test asserts new-code-vs-its-own-golden, not new-code-vs-truth. Selecting on the COR group (rather than random rows) also keeps the fixture usable for FC-1's grouped CV.  

- **The two goldens are two independent gates, not two measurements of one quantity.** They describe different inputs, so their *values* are expected to differ and must never be reconciled against each other (no averaging, no relaxing one to match the other). The fast-subset golden is the inner-loop gate (Claude Code runs it constantly); the full-batch canonical is the release gate (the author runs it). The only meaningful failure is the **new code passing one tier but failing the other** — that signals a scale/edge-case bug (e.g. a cell degenerate at subset size but not at full size, or a numerical/branching path reached only at full n); investigate that specific divergence.  

- **Refresh protocol.** Goldens are frozen artifacts, regenerated only by a deliberate, reviewed act — never hand-edited, never auto-refreshed. When a *legitimate* behavior change lands: make the change, re-run the committed capture script, review the diff in the golden files as the explicit record of what changed, and commit it alongside a `DECISIONS.md` entry stating why. The author regenerates the full-batch canonical in the trusted environment; Claude Code may regenerate the fast subset. Keeping the capture script in the repo is what makes a refresh reproducible; forcing it through a reviewed diff is what keeps unintended drift loud.  

---

## 6. Staged implementation with checkpoints

Stop after each checkpoint; wait for author review. Each lists a **Definition of Done (DoD)**.

### Checkpoint 0 — Environment, repo detection, scaffolding

- **Environment check (first action of the whole effort).** Confirm R runs and the existing code is sourceable: source `R/v1_paper_3_fitting_functions.R` and `R/v1_paper_3_plotting_and_evaluation_functions.R`, and run one trivial LM fit on a few rows. If a package is missing, report exactly which, and stop. Required for old path: tidyverse, caret, plus whatever the sourced files `library()`. Required additionally for new path: tidymodels (parsnip, recipes, workflows, rsample, yardstick); ranger for the RF smoke test.  
- **Fixture location.** `bdpgtext2` is a plain project repo, **not** an R package, so use `tests/fixtures/` as `FIXTURE_ROOT`. (No `DESCRIPTION`-based detection is needed.)  
- **Create fixture directories:** `FIXTURE_ROOT/inputs/` and `FIXTURE_ROOT/golden/`.  
- **Freeze the input snapshot (Checkpoint 0 = inputs only, not the golden).** Snapshot the **working frames plus aux** — `p3_working_train_df` / `p3_working_test_df` and `p3_train_aux_df` / `p3_test_aux_df` — as **RDS** into `FIXTURE_ROOT/inputs/`, and commit them, so the tests have a fixed input that does not move when data is regenerated. Snapshotting the *working* frames (rather than the already-derived `p3_train_x_df` / `p3_test_x_df`) is deliberate: one snapshot serves every feature set, and the fixture's own `build_feature_set_specific_test_and_train()` derives the per-feature-set x-frames, keeping that derivation inside the tested path.  

    Two correctness details govern *how* the snapshot is taken:  

    - **Capture in memory, at the fit call site, as RDS — not by copying the on-disk CSVs.** The frames live on disk as CSV (`p3_working_train_df.gurobi__all.exclude_imperfect_wraps__FALSE.csv`, etc.), which has already lost R types and factor levels, and the analysis body *munges the frames after loading* (the Gurobi→ILP string replacement). So a raw CSV copy would be both lossy and pre-munge. The snapshot must be `saveRDS()` of the in-memory frames as they enter `fit_and_predict_output_error_using_feature_set()` (after the ILP replacement).  
    - **These working frames are already post-Box-Cox / post-standardization** (preprocessing happened upstream in the prep Rmd), which is exactly the pass-through-recipe input this round expects. The separate `__before_any_preprocessing` frames are *not* the learning input (they feed only the disabled Matilda-file chunk) and are not snapshotted.  

    **Commit the full batch.** Measured sizes: each working frame is ~13 MB as CSV (smaller as compressed RDS) and each aux frame ~100 KB — comfortably small enough to commit directly (no Git LFS, no out-of-repo artifact). So both tiers' inputs are committed: the full-batch snapshot for the author's release gate and the fast-subset snapshot for the inner loop. This step freezes inputs only — the golden outputs come in Checkpoint 1.  
- **Set up testthat scaffolding** and create an empty `R/v1_paper_9_fitting_and_eval_pipeline.R` (with the header comment described in §2 and §10).  
- **DoD:** environment-check test green; `FIXTURE_ROOT` resolved and reported; input RDS files present; empty new R file created; test harness runs.  

### Checkpoint 1 — Capture golden masters from the OLD pipeline

This checkpoint freezes the pipeline's **outputs** (the golden master) — distinct from Checkpoint 0, which froze the **inputs**. Same frozen inputs go in; the old code's numbers come out and become the reference.

- Draft a **capture script** (Claude Code writes it; the author runs it — the author writes no capture code) that sources the old fitting + eval code, loads the frozen input fixtures from `FIXTURE_ROOT/inputs/`, and runs the existing `build_feature_set_specific_test_and_train()` + `fit_and_predict_output_error_using_feature_set()` for the **LM** cases across the reported feature sets × error types (all four reserve selectors), capturing into `FIXTURE_ROOT/golden/`:  

    - the final `all_fitting_scores_df` (canonical metric golden), and  
    - the per-cell true-vs-predicted data (predictions golden), for the scatter.  

- **The author runs the capture on the full batches** (trusted environment, old code untouched) and commits the result as the canonical golden — because the paper's authoritative numbers should trace to the author's environment, captured before any new code exists.  
- **Claude Code generates the fast-subset golden** by running the same script on the deterministic subsample.  
- Note: the capture script temporarily needs the old `show()` side effect suppressed or redirected; capture data, not figures, at this stage.  
- Note (resolved 2026-07-21): the capture script is authored **here, by Claude Code**, not drafted during planning — an untested script written in the planning chat could drift from the real return shapes of `eval_model_on_train_or_test_data()` and the true-vs-pred frame, and it belongs in the session that can execute and verify it. What the script captures, the equivalence tolerance, the fast-subset selection rule, and the refresh protocol are fixed in "Golden-master specifics" (§5); follow those.  
- **DoD:** golden files exist for both tiers; a test loads them; the fast-subset golden is reproducible by re-running the capture (determinism check).  

### Checkpoint 2 — Fit/evaluate core (LM), seams in place, no plotting

- Implement the pure fit/evaluate function: inputs `(train_x_df, test_x_df, train_aux_df, test_aux_df, target values, recipe, learner spec, label/meta)`; builds the one-element resampling plan; builds and fits the workflow; predicts on analysis + assessment; calls **`eval_model_on_train_or_test_data()` unchanged**; returns the `bdpg_fit_result` bundle (`metrics`, `predictions`, `meta`, optional `workflow`). `fold_id = 1` throughout.  
- Compute `num_predictors` to match the legacy `ncol()` of the x data frame at the fit point; rely on the golden to verify.  
- **LM equivalence test** against the fast-subset golden (tight tolerance).  
- **RF smoke test** through the same path (structure only).  
- **DoD:** LM equivalence green on the fast subset; RF smoke green; no plotting code present in this function.  

### Checkpoint 3 — Plot function (separated)

- Implement `plot_output_error_fit(bundle, ...)`: consumes `predictions` (scatter, faceted by reserve selector, colored by `dom_err_type`, perfect-fit diagonal) and `metrics` (per-facet adj-R² / rmse annotation, using the label locations in `meta`). Returns the ggplot object; does **not** call `show()`.  
- Reproduce the current faceted figure's structure (borrow layout logic from the existing `plot_full_fits()` behavior, but in the new function; do not modify the old one).  
- Place a **commented-out** call to `plot_train_and_test_stuff_for_one_RS()` at the spot it would belong, with a one-line note explaining what it is and that it is intentionally disabled (see §7).  
- **Plot structural test** (facet count, layers). Optional `vdiffr` snapshot.  
- **DoD:** plot function returns a correctly-structured ggplot; author does a visual eyeball check against the current figure and approves.  

### Checkpoint 4 — Orchestrator, scores assembly, final-model gate

- Implement the orchestrator that calls the fit core, then an **adapter** that projects `bundle$metrics` to the exact legacy `all_fitting_scores_df` columns (§3) and binds them onto the running scores frame. **No plotting inside the orchestrator.**  
- Wire the **`save_final_model` gate** (consumed here, value resolved once globally — see §8): on `TRUE`, fit the final workflow on the full training pool and `saveRDS()` a versioned artifact; on `FALSE` (default), skip silently. For this round the save body is a **stub with a `TODO`** pointing at the Decisions entry (§11); do not implement the real persistence yet.  
- **Equivalence test:** the new path reproduces the full `all_fitting_scores_df` column-for-column against the golden.  
- **DoD:** full `all_fitting_scores_df` reproduced exactly; gate wired with stubbed save; tests green.  

### Checkpoint 5 — Wire ONE Rmd subsection (parallel, behind a switch)

- Choose one subsection (suggest "Representation shortfall using PUsAndSppOnly"). Add the new calls **alongside** the old, behind a param/flag (default = old path). The new sequence: call the fit function → bind scores via the adapter → call the plot function → **print the returned plot** so knitr captures it under the existing `fig.cap`. (The current figure renders only because `show()` runs inside the old function; the new path must explicitly print/return the plot at the chunk top level.)  
- **DoD:** with the flag on, the chosen subsection renders an identical figure and identical scores via the new path; with the flag off, behavior is unchanged; author approves.  

### Checkpoint 6 — Roll out and flip default (explicit, last)

- After approval, convert the remaining subsections the same way; flip the default to the new path; keep the old path callable for one cycle (rollback).  
- **DoD:** the full document knits; `all_fitting_scores_df` matches the canonical golden; author approves whether to retain or remove the old path.  

---

## 7. Per-reserve-selector diagnostic plot

`plot_train_and_test_stuff_for_one_RS()` (in the plotting/eval file) is a per-selector diagnostic distinct from the faceted paper figure. It is currently called for side effect inside the old `ppe_*` wrapper and its return value is discarded; nothing downstream depends on it. The new path calls the metric kernel directly and so never invokes it. **Action:** in the new plot function, include a commented-out call at the spot it would belong, with a short note, so it is easy to revive. Commenting it out is harmless and the old file is not touched.

---

## 8. The `save_final_model` gate (global, quiet default)

Fitting is called once per `(reserve selector × feature set × error type)` — ~16 calls in the current Rmd. "Is this a final-model run?" is a property of the **run**, not of any individual fit, so the decision is made **once, globally**, and the per-call functions only consume the resolved value.

- **Global setting** `params$save_final_model` lives in the top-of-Rmd params block, default **`FALSE`** (quiet; routine exploratory runs are silent).  
- **Per-call functions consume the resolved boolean** and act mechanically: `TRUE` → fit final workflow on the full training pool + `saveRDS()` versioned artifact; `FALSE` → skip silently. No prompting or stopping inside the loop.  
- The reminder that final-model persistence still needs doing rides on **documentation, not runtime noise**: the Decisions entry (§11), a visible comment in the params block next to the flag, and a short note at the fitting call site — all cross-referencing the same Decisions entry.  
- For this round the persistence body is **stubbed with a `TODO`**; real `saveRDS` logic lands in the final-model round (after the pipeline is frozen and sequestered batches are unlocked).  

---

## 9. Do-not-touch list

The new path is built alongside the old. Until checkpoints 5–6 (and only behind a switch), do not modify:

- `R/v1_paper_3_plotting_and_evaluation_functions.R` — depended on unchanged (the metric kernel especially).  
- `R/v1_paper_3_fitting_functions.R` — read and call for golden capture; do not edit.  
- `R/v2_paper_3_cv_test_train_splitting_functions.R` — upstream splitting; not in scope.  
- `p9_v01_prep_data_for_p8_to_load_from_files.Rmd` — upstream prep, incl. Box-Cox; not in scope.  
- The existing behavior of `p9_v01_all_combined__body.Rmd` — modified only at 5–6, additively, behind a flag, old path preserved.  

No opportunistic cleanup of nearby code. Additions are minimal, surgical, testable increments.

---

## 10. Naming convention (APPROVED 2026-07-21)

- **New file:** `R/v1_paper_9_fitting_and_eval_pipeline.R`. The `v1_paper_9` prefix records when and in what context this code was first added (the author's convention for locating new blocks). Do **not** reuse the `v2_paper_3` prefix to signal the connection to `R/v2_paper_3_cv_test_train_splitting_functions.R`; instead state that relationship explicitly in a header comment inside the new file (it is the upstream COR-disjoint splitting the pipeline will eventually consume when CV lands).  
- **Functions:**  

    - `make_bdpg_resampling_plan(train_df, test_df)` → one-element plan, `fold_id = 1`.  
    - `make_bdpg_recipe(...)` → pass-through recipe (seam).  
    - `make_bdpg_learner(learner_id = "lm")` → parsnip spec (`"lm"` default, `"rf"` for smoke).  
    - `fit_output_error_for_feature_set(...)` → returns `bdpg_fit_result` (pure; no plot).  
    - `plot_output_error_fit(bundle, ...)` → returns ggplot.  
    - `bind_fitting_scores(all_fitting_scores_df, bundle)` → legacy-shaped rows appended.  
    - `run_output_error_fit(...)` → orchestrator (fit + bind scores + gate); returns bundle (+ updated scores).  

- **Result class:** `bdpg_fit_result` (S3 list).  

---

## 11. Deferred decisions and placeholders

Each entry: the placeholder/decision, why it is acceptable now, and what would trigger revisiting. Two kinds are tracked: **placeholders** (simplest-thing-now, expected to change) and **preserved behaviors** (match the old code exactly; do not "improve").

> The subset of these items that are large enough to warrant their **own dedicated chats** (rather than being folded into implementation) are also tracked separately in `FUTURE_CHATS.md`, so they are not lost inside this list. Entries tagged *"separate chat"* below correspond to items in that file.


### Placeholders (expected to revisit)

- **adjusted-R² predictor count.** Placeholder: `p = number of features` for every learning method. Acceptable: LM is the expected model; keeps the metric kernel untouched. Revisit: whether adjusted R² (or any R²) is meaningful for non-linear models, and what to display instead. *Needs a dedicated discussion.*  
- **Evaluation scheme.** Placeholder: single pre-built holdout treated as a one-element resampling plan (`fold_id = 1`). Acceptable: matches current behavior; the seam is cheap. Revisit: full group-aware k-fold (COR as group via `group_vfold_cv`) + split conformal + coverage diagnostics + paired feature-set comparison — deferred to a **separate methodology chat**.  
- **Multi-evaluation display.** Placeholder: structure is fold-aware but no aggregation/error-bars/paired-test logic is built. Acceptable: nothing beyond single-holdout runs this round. Revisit (separate chat): pool held-out (assessment) predictions across folds into one scatter (each problem appears once); report metrics as mean ± SD across folds; error bars belong on the summary bar chart, not the scatter; per-facet annotation prints the cross-validated mean; headline feature-set claim uses per-fold paired differences with a paired test.  
- **Preprocessing ownership.** Placeholder: pass-through recipe; Box-Cox + standardization stay upstream in the prep Rmd. Acceptable: correct for single-holdout (lambdas learned on train, applied to test). Revisit: move `step_BoxCox` / `step_normalize` into the recipe so they re-estimate per fold once CV exists.  
- **Learners available day one.** Placeholder: LM equivalence-tested; RF a structure-only smoke test; glmnet absent. Acceptable: LM is what the paper reports; the seam proves extensibility. Revisit: RF/glmnet as reported, validated options — note RF cannot be golden-matched to the old RF path (different engines/seeds/defaults; a 2024 bug-fix), so its test would be sanity-only; glmnet pulls in penalty tuning.  
- **Fitted-model carriage and final-model persistence.** Placeholder: bundle can optionally carry the fitted workflow (default off); durable save-to-disk is a stubbed `TODO` behind the `save_final_model` gate. Acceptable: the final model is not trained until the pipeline is frozen and sequestered batches unlocked. Revisit: implement `saveRDS` of the versioned final workflow in the final-model round. (Note the distinction: reusing a *fitted* model applies to "predict on a new test batch"; "re-evaluate under new folds" instead *refits* per fold and reuses only the spec, not a fitted object — keep these separate to avoid reintroducing train-on-test leakage.)  
- **Seed handling.** Placeholder: not needed this round (LM single-holdout is deterministic). Revisit: when CV / RF / bootstrap land, fix seeds for reproducible resampling.  

### Preserved behaviors (match exactly; do not "fix")

- **R² definition.** `compute_r.squared()` is out-of-sample `1 - SSE/SST` with SST around the **eval set's own mean** — *not* the squared-correlation R². If tidymodels metrics are ever substituted, the matching function is `yardstick::rsq_trad()`, **not** the default `rsq()`. Do not swap in `rsq()`.  
- **adjusted-R² formula.** `1 - (1 - R2) * ((n - 1)/(n - p - 1))` via `compute_adj.r.squared()` (df.int = 1). Reproduce, do not "modernize" — i.e. do not replace it with `summary(lm_fit)$adj.r.squared` or `broom::glance()$adj.r.squared`, which compute the *in-sample* adjusted R² from the model's own residual degrees of freedom and would silently produce a different number than this out-of-sample form.  
- **`all_fitting_scores_df` column shape.** Exactly as in §3; downstream factoring/plotting depends on it.  

---

## 12. Runtime open questions for Claude Code (surface, don't assume)

Raise these during implementation rather than guessing:

- `FIXTURE_ROOT` is `tests/fixtures/` (project repo, not a package); create it if absent and report the path used.  
- Confirm the required packages are installed before relying on them (env check, Checkpoint 0); if not, report which and stop.  
- Confirm the exact set of feature sets × error types to include in the golden capture (read them from the Rmd call sites in the "Results - Learning to predict output errors" section rather than hardcoding).  
- Confirm the new file name and function names against §10 once the author approves.  
- Flag any place where reproducing the golden requires a `num_predictors` count that differs from `ncol(train_x_df)`; do not silently adjust.  

---

## 13. One-screen summary

Build a new, legible fitting/evaluation pipeline in `R/v1_paper_9_fitting_and_eval_pipeline.R`, parallel to the old code, structured around three seams (resampling plan, recipe, learner) but exercising only the simplest case (single holdout, pass-through recipe, LM). Reuse the existing metric kernel `eval_model_on_train_or_test_data()` unchanged so golden-master equivalence is exact for LM. Separate plotting from fitting: the fit function returns a data bundle; a separate plot function returns the ggplot; the Rmd prints it. Preserve `all_fitting_scores_df` exactly. Add an RF smoke test through the same path. Gate final-model persistence behind a quiet global `save_final_model = FALSE`, stubbed for now. Proceed checkpoint by checkpoint, stopping for review, touching the Rmd only at the end and only behind a switch with the old path intact.
