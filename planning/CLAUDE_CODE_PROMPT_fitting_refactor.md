# Claude Code prompt — implement the bdpg fitting/eval refactor

Give this file to Claude Code at the start of the implementation session.
Written 2026-07-21, from two planning chats (2026-06-28 and 2026-07-21).
Planning is finished. This session writes code.

---

## 0. Read these first, before doing anything

Read, in this order, and do not begin work until you have:

- `planning/bdpg_fitting_refactor_plan.md` — the authoritative plan. Everything below is a summary of it; **where this prompt and the plan disagree, the plan wins**, except for the "superseding" items called out in §2 of this prompt.  
- `planning/DECISIONS.md` — read at least the 2026-06-28 and 2026-07-21 entries. Note that the 2026-07-21 entry explicitly supersedes an earlier same-session draft; only the final decisions stand.  
- `/Users/bill/D/Projects/ProblemDifficulty/CLAUDE.MD` (repo root) and anything in `/Users/bill/D/Projects/ProblemDifficulty/_claude_project_guide/`.  
- `planning/FUTURE_CHATS.md` — the list of things deliberately **out of scope**. Do not implement any of it.  

Then read, to understand current behavior (**read only — do not edit**):

- `R/v1_paper_3_fitting_functions.R` — entry points `build_feature_set_specific_test_and_train()` and `fit_and_predict_output_error_using_feature_set()`.  
- `R/v1_paper_3_plotting_and_evaluation_functions.R` — in particular `eval_model_on_train_or_test_data()`, which is reused **unchanged**.  
- `R/provenance_helpers.R` — `open_or_init_registry()`, `write_a_tib_with_provenance()`, `resolve_prov_file()`.  
- `p9_v01_all_combined__body.Rmd` — the fit call sites in the "Results - Learning to predict output errors" section.  
- `p9_v01_prep_data_for_p8_to_load_from_files.Rmd` — for the dataprov session idiom to copy.  

---

## 1. Working agreement (non-negotiable)

- **Stop at every checkpoint and wait for author review.** Do not run ahead. Each checkpoint below has a Definition of Done; when you hit it, stop and report.  
- **Surgical, minimal, incremental changes only.** No opportunistic cleanup. Never "improve" working code you happen to be reading. If you spot something worth changing that is not in the plan, write it down and raise it — do not do it.  
- **Do not modify existing working code**, with exactly one scoped exception (Checkpoint 1, see below). Outside that exception, the old pipeline is read-only.  
- **Parallel build.** New work goes in a new file; the old path keeps working until the author decides to retire it.  
- **Test-driven.** Write the failing test first, then the code to pass it.  
- If you find yourself reinterpreting a decision to make it easier to implement, stop and ask instead.  
- Ask before installing packages or changing anything about the environment.  

The author's background: strong scientific and OO programming experience, R/RStudio; limited web/front-end. Explain in those terms.

---

## 2. What was settled (and what supersedes what)

Architecture:

- New file: `R/v1_paper_9_fitting_and_eval_pipeline.R`. All new code goes here.  
- Three seams (resampling plan, recipe, learner); this round exercises only the simplest case of each — single holdout, pass-through recipe, LM.  
- The fitting function returns a **data bundle** (`bdpg_fit_result`: `metrics`, `predictions`, `meta`, optional `workflow`). A **separate** function returns the ggplot. Fitting never calls `show()`.  
- `eval_model_on_train_or_test_data()` is reused **unchanged** as the metric kernel.  
- `all_fitting_scores_df` column structure is preserved **exactly**.  
- Adjusted-R² keeps its existing definition, with `p` = number of feature columns of the x data frame at the fit point (legacy `ncol()`).  
- Out-of-sample R² keeps its existing definition (not squared correlation).  
- Function names in plan §10 are approved as written; use them.  

Golden master (these **supersede** anything earlier that says otherwise):

- **Metrics only from the old side.** Capture `all_fitting_scores_df` only. Old-side predictions are deliberately not captured — reaching them would require editing old fitting logic. Do not attempt it.  
- **Capture is in-Rmd and flag-gated**, not an external capture script. Any earlier mention of an external capture script is superseded.  
- **The old side has a single tier** — the full-batch metric golden. There is **no old-side subset golden**. Any earlier mention of a two-tier old-side golden is superseded.  
- The fast TDD inner loop uses a **new-side self-regression fixture** (the new pipeline vs its own frozen output) on a deterministic COR-subsample.  
- Golden files are written with `write_a_tib_with_provenance()` as **RDS**, into the dataprov registry idiom already used by the prep Rmd.  

Scope discipline:

- This whole golden-master facility is a **one-shot bootstrap**, not durable infrastructure. The old pipeline stays live only long enough to confirm the new one. Do not over-engineer it.  
- The gate is a sanity check — "the new pipeline isn't doing something obviously wrong" — not a proof.  

---

## 3. Checkpoints

Stop after each. Report what you did, what the tests say, and anything you had to decide.

### Checkpoint 0 — Environment, scaffolding, frozen inputs

- **First action of the entire session:** confirm the environment. Source `R/v1_paper_3_fitting_functions.R` and `R/v1_paper_3_plotting_and_evaluation_functions.R`, and run one trivial LM fit on a few rows. If any package is missing, report exactly which and **stop** — do not install anything without permission. Old path needs tidyverse, caret, plus whatever the sourced files `library()`. New path additionally needs tidymodels (parsnip, recipes, workflows, rsample, yardstick) and ranger.  
- `bdpgtext2` is a plain project repo, **not** an R package. `FIXTURE_ROOT` is `tests/fixtures/`. Create `FIXTURE_ROOT/inputs/` and `FIXTURE_ROOT/golden/`.  
- **Freeze the input snapshot.** Snapshot `p3_working_train_df`, `p3_working_test_df`, `p3_train_aux_df`, `p3_test_aux_df` as RDS into `FIXTURE_ROOT/inputs/`, and commit them. Two details govern how:  

    - **Capture in memory at the fit call site, after the Gurobi→ILP munging** — not by copying the on-disk CSVs, which have lost R types and factor levels and are pre-munge.  
    - Snapshot the **working frames**, not the derived `p3_train_x_df` / `p3_test_x_df`: one snapshot then serves every feature set, and `build_feature_set_specific_test_and_train()` derives the x-frames inside the tested path.  

- These working frames are already post-Box-Cox and post-standardization (preprocessing happens upstream in the prep Rmd) — which is exactly what the pass-through recipe expects this round. The `__before_any_preprocessing` frames are **not** the learning input; do not snapshot them.  
- Create an empty `R/v1_paper_9_fitting_and_eval_pipeline.R` with the header comment described in plan §2/§10, and set up testthat scaffolding.  
- **DoD:** environment check green; `FIXTURE_ROOT` resolved and reported; the four input RDS files present and committed; empty new R file created; test harness runs.  

### Checkpoint 1 — Capture the metric golden from the OLD pipeline

**This checkpoint contains the one scoped exception to the no-modify rule.** You may add flag-gated instrumentation to the working analysis Rmd. You may **not** alter any existing logic in it, and you may not touch the old fitting functions at all.

- Draft a `capture_golden_master` params flag (default `FALSE`) plus a `force` param (default `FALSE`), and one additive capture chunk after the last fit chunk. When the flag is off, the chunk must be completely inert — a normal knit must behave exactly as it does today.  
- When on, the chunk writes `all_fitting_scores_df` via `write_a_tib_with_provenance(sess, ...)` as **RDS** into `FIXTURE_ROOT/golden/`, reusing the prep Rmd's dataprov session/registry idiom (`Data/dataprov_registry`).  
- **Overwrite guard, evaluated at the very start of the run, before any fitting or writing:** if `capture_golden_master` is TRUE and the golden already exists and `force` is not set, `stop()` immediately with a clear message. Fail fast; never silently skip generation (a run recorded as "generate = yes" that produced nothing is a self-contradicting record).  
    Do **not** implement this as an interactive prompt: `interactive()` is FALSE under `rmarkdown::render()` and `readline()` would not block, so a prompt would silently fall through. An `if (interactive())` confirmation may be layered on top as a second guard, never as the only one.  
- The capture chunk needs the old `show()` side effect suppressed or redirected — capture the scores tibble, not figures.  
- Read the exact set of feature sets × error types to capture **from the Rmd call sites**, not from memory or hardcoding.  
- **You draft this; the author reviews it and runs it.** Do not run the full-batch capture yourself — the canonical golden must come from the author's trusted environment, from the old code, before the new code exists.  
- Log the scoped no-modify exception in `DECISIONS.md`.  
- **DoD:** the flag-gated chunk exists and is inert when off; the author has run it once and committed the full-batch metric golden RDS with its provenance record; a test loads the golden (hash-verified via `resolve_prov_file()`); the overwrite guard demonstrably aborts a re-run when `force` is unset.  

### Checkpoint 2 — Fit/evaluate core (LM), seams in place, no plotting

- Implement the pure fit/evaluate function per plan §4/§10: builds the one-element resampling plan, builds and fits the workflow, predicts on analysis + assessment, calls `eval_model_on_train_or_test_data()` **unchanged**, returns the `bdpg_fit_result` bundle. `fold_id = 1` throughout.  
- `num_predictors` must match the legacy `ncol()` of the x data frame at the fit point. If reproducing the golden ever seems to require a different count, **stop and report it** — do not silently adjust.  
- **LM equivalence test:** the new pipeline's metrics reproduce the old-side full-batch metric golden within testthat's default tolerance (`~1.5e-8`).  
    If the tolerance has to be loosened to pass, **do not loosen it — investigate**. The likely cause is design-matrix construction at the recipe seam: intercept handling, factor contrasts, or column ordering differing between the pass-through recipe/hardhat path and the old code's manual `as.matrix` / `model.matrix`. The tight tolerance is deliberately doubling as a correctness probe on that seam.  
- **New-side self-regression fixture** for the fast inner loop. Build the COR-subsample by group, not by row: fixed seed, then the smallest set of distinct `rsp_UUID_of_COR_Base_problem_that_is_wrapped` values that still (a) populates all 4 reserve selectors × 2 error types, (b) retains both `dom_err_type` levels, and (c) leaves every cell non-degenerate for an LM fit (`n > p + 1`, not rank-deficient). Freeze the **new** pipeline's own output on that subsample as the fixture. Statistical representativeness of the science is explicitly **not** a goal.  
- **RF smoke test:** the same workflow path with a `rand_forest` / ranger spec runs end to end and returns a well-formed `bdpg_fit_result`. Assert **structure only, never values**. RF cannot be golden-matched to the old RF path.  
- **DoD:** LM metric equivalence green against the full-batch golden; new-side self-regression green on the subset; RF smoke green; **no plotting code anywhere in this function**.  

### Checkpoint 3 — Plot function (separated)

- Implement `plot_output_error_fit(bundle, ...)`: consumes `predictions` (scatter, faceted by reserve selector, colored by `dom_err_type`, perfect-fit diagonal) and `metrics` (per-facet adj-R²/rmse annotation using the label locations in `meta`). **Returns** the ggplot object; does **not** call `show()`.  
- Reproduce the current figure's structure by borrowing layout logic into the new function. Do not modify the old plotting function.  
- Place a **commented-out** call to `plot_train_and_test_stuff_for_one_RS()` where it would belong, with a one-line note saying what it is and that it is intentionally disabled (see plan §7).  
- **Plot structural test:** one facet per reserve selector, expected annotation and diagonal layers present. A `vdiffr` pixel snapshot is optional only — do not make it a required test, since it couples to ggplot/font/OS versions.  
- **DoD:** plot structural test green; the returned object renders correctly when printed.  

### Checkpoints 4+ — Adapter and single-subsection Rmd integration

Follow plan §6 for the remaining checkpoints. Two things to keep in mind:

- The scores adapter must reproduce `all_fitting_scores_df` **column for column** against the golden.  
- When wiring the new path into one Rmd subsection, add it **alongside** the old path behind a flag (default = old path). The new sequence must **explicitly print** the returned plot at chunk top level — the current figure only renders because `show()` runs inside the old function.  

---

## 4. Explicitly out of scope

Do not implement, and do not "prepare the ground" for, any of the following. They are tracked in `FUTURE_CHATS.md` for dedicated future sessions:

- Group-aware CV, conformal intervals, coverage, paired comparisons (single holdout only this round).  
- R² / adjusted-R² appropriateness for non-linear models.  
- Multi-fold result display, aggregation, error bars.  
- Final-model persistence — leave the global quiet-`FALSE` gate with its stubbed TODO body.  
- Promoting RF or glmnet to fully validated, reported learners.  

The structure should be fold-aware where the plan says so, but no aggregation logic gets built this round.

---

## 5. Reporting

At each checkpoint stop, report concisely:

- What you implemented and which tests are green or red.  
- Any decision you had to make that the plan did not cover, flagged clearly as a decision needing author confirmation.  
- Anything you noticed but deliberately did not touch.  

Append settled decisions to `DECISIONS.md` in its existing house style (dated `# Decisions Log Entry:` heading, `## Session:`, `### Context` / `### Decisions made` / `### Next steps`, one blank line before and after lists, trailing space at the end of each list item).
