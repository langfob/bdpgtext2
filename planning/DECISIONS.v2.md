# Decisions Log Entry: 2026-06-24

## Session: Claude Code session recovery and workflow improvements

### Context

This planning session was conducted in claude.ai Chat (not Claude Code) after discovering
that nearly all Claude Code session transcripts from late 2025 through mid-June 2026 were
lost due to a known bug (see https://github.com/anthropics/claude-code/issues/61608 and
related issues). Session metadata (sidebar titles) survived but `.jsonl` transcript content
was never durably written to disk. Time Machine backups confirmed the files never existed
on disk — this was not a migration loss but a write failure.

### Decisions made

- Adopted workflow of doing planning discussions in claude.ai Chat Projects (cloud-stored,
  persistent) rather than Claude Code (transcript storage unreliable), then writing `.md`
  instruction files for Claude Code to execute 

- Will maintain a running `DECISIONS.md` log in `_claude_project_guide/` to record what
  was decided and why, providing durable rationale that survives transcript loss 

- Established that the root `CLAUDE.md` (at `ProblemDifficulty/CLAUDE.md`) is the correct
  location for content that Claude Code should load automatically at every session start;
  `_claude_project_guide/` is for supporting reference files that Claude reads only when
  directed 

- Continuity section drafted and added to `CLAUDE.md` — reconstructed from git history,
  session sidebar titles, and running notes; saved also as standalone file
  `ProblemDifficulty/claudes guess at continuity after lost claude conversations.md`
  (a user-edited revision will be saved as
  `ProblemDifficulty/claudes guess at continuity after lost claude conversations - my revision.md`) 

- Established backup script at `~/backup_claude_sessions.sh` with weekly cron job and
  macOS native notification, backing up both `~/.claude/projects/` and
  `~/Library/Application Support/Claude/claude-code-sessions/` to timestamped folders
  under `~/D/Backups/claude_sessions/` 

- Note: cron job requires Full Disk Access permission for `/usr/sbin/cron` in
  System Settings → Privacy & Security → Full Disk Access to read
  `~/Library/Application Support/` 

### Markdown formatting rule established

The following rule was tested and confirmed working for pandoc/RStudio PDF rendering and
was added to both claude.ai Settings → Profile and `~/.claude/CLAUDE.md`:

> When writing markdown, always insert a blank line before the first item of any bulleted
> or numbered list, including nested lists, and a blank line after the last item before
> any following paragraph. Never let a list immediately follow a paragraph or heading with
> no blank line between them. Use 4-space indentation for nested list levels, not 2-space.
> Always include a trailing space at the end of each list element to force proper text
> wrapping of the element.

Note: some editors (RStudio, VS Code) auto-strip trailing whitespace on save. Verify
trailing spaces survive after pasting into any `.md` file.

### GitHub issues to monitor for Claude Code session storage bugs

- https://github.com/anthropics/claude-code/issues/61608 (sessions not saved to disk)
- https://github.com/anthropics/claude-code/issues/63839 (May 28 migration broke history)
- https://github.com/anthropics/claude-code/issues/48334 (app update deletes sessions)
- https://github.com/anthropics/claude-code/issues/29373 (earlier migration bug)

### Next steps

- User to finish editing continuity file and save as
  `ProblemDifficulty/claudes guess at continuity after lost claude conversations - my revision.md` 

- Open new claude.ai Chat session in this Project to plan next phase of ProblemDifficulty
  work 

- At start of next Claude Code session, instruct Claude Code to read both the root
  `CLAUDE.md` and the revised continuity file before beginning any work 

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

# Decisions Log Entry: 2026-07-21

## Session: Golden-master mechanics for the fitting/eval refactor (paper 9 / bdpgtext2)

### Context

Continuation of the 2026-06-28 planning chat (still planning only; no code written), opened
from `SEED_next_chat_fitting_refactor.md`. Purpose: resolve the golden-master questions the
prior chat left open. Result: `bdpg_fitting_refactor_plan.md` revised in place — new
"Golden-master specifics (resolved 2026-07-21)" subsection in §5, corrected Checkpoint 0
input-snapshot step, and §10 function names marked approved.

### Decisions made

- Function names in plan §10 approved as proposed (`make_bdpg_resampling_plan()`,
  `make_bdpg_recipe()`, `make_bdpg_learner()`, `fit_output_error_for_feature_set()`,
  `plot_output_error_fit()`, `bind_fitting_scores()`, `run_output_error_fit()`; class
  `bdpg_fit_result`) 

- Golden master captures two data artifacts per tier — the `all_fitting_scores_df` metric
  golden and the per-cell true-vs-predicted frame — and does NOT freeze a rendered plot.
  Predictions are the ground truth (metrics and the figure are deterministic functions of
  them), and the figure is guarded by a structural test instead (`vdiffr` optional only) 

- Equivalence tolerance is testthat's default (~1.5e-8) for both metrics and predictions; if
  it must be loosened to pass, investigate rather than loosen — the likely cause is
  design-matrix construction at the recipe seam (intercept / contrasts / column order), so
  the tight tolerance doubles as a correctness probe on that seam 

- Fast-subset fixture is selected by COR group (fixed seed; the smallest set of distinct
  `rsp_UUID_of_COR_Base_problem_that_is_wrapped` that still covers all 4 selectors × 2 error
  types, retains both `dom_err_type` levels, and keeps every cell non-degenerate for LM,
  `n > p + 1`, not rank-deficient). Statistical representativeness of the full-batch science
  is explicitly NOT a goal, since the test asserts new-code-vs-its-own-golden 

- The fast-subset and full-batch goldens are two independent gates, not two measurements of
  one quantity — their values describe different inputs and are never reconciled (no
  averaging, no relaxing one to match the other). Fast subset = Claude Code's inner-loop
  gate; full batch = the author's release gate. The only meaningful failure is the new code
  passing one tier but failing the other (a scale/edge-case bug to investigate) 

- Golden refresh protocol: goldens are frozen and regenerated only by a deliberate, reviewed
  act — make the change, re-run the committed capture script, review the diff, commit with a
  DECISIONS entry; never hand-edited or auto-refreshed. Author regenerates the full-batch
  canonical; Claude Code the fast subset 

- Input snapshot = the working frames `p3_working_train_df` / `p3_working_test_df` plus aux
  `p3_train_aux_df` / `p3_test_aux_df` (not the derived x-frames), so one snapshot serves
  every feature set and the fixture's own `build_feature_set_specific_test_and_train()`
  derives the x-frames inside the tested path 

- Input snapshot must be captured in memory at the fit call site as RDS, AFTER the
  Gurobi→ILP munging — not by copying the on-disk CSVs, which are lossy (types / factor
  levels) and pre-munge. The working frames are already post-Box-Cox / standardization (the
  correct pass-through-recipe input); the `__before_any_preprocessing` frames are not the
  learning input and are not snapshotted 

- Commit the full-batch input snapshot directly, no Git LFS: measured ~13 MB per working
  frame as CSV (smaller as compressed RDS) and ~100 KB per aux frame — acceptably small. Both
  tiers' inputs are committed 

- The capture script is authored by Claude Code at Checkpoint 1 (the session that can run and
  verify it), not drafted during planning; planning fixes only its contract 

### Next steps

- Author's careful read of the revised `bdpg_fitting_refactor_plan.md`; revise in place if
  further edits arise 

- Begin the Claude Code implementation session at Checkpoint 0 when ready 
