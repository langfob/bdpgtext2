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
from `SEED_next_chat_fitting_refactor.md`. Resolved the open golden-master questions and then
the golden-master *generation* mechanics. `bdpg_fitting_refactor_plan.md` was revised in place
(new "Golden-master specifics" subsection in §5, Checkpoints 0-2 updated, §10 approved).

This entry records the *final* settled decisions. It supersedes an earlier same-session draft
(never implemented) that had the old pipeline capture *predictions* as ground truth via an
external capture script with a two-tier old-side golden. That approach is replaced below by
metrics-only, captured in-Rmd, one-shot.

### Decisions made

- Function names in plan §10 approved as proposed (`make_bdpg_resampling_plan()`, `make_bdpg_recipe()`, `make_bdpg_learner()`, `fit_output_error_for_feature_set()`, `plot_output_error_fit()`, `bind_fitting_scores()`, `run_output_error_fit()`; class `bdpg_fit_result`) 

- Golden master is a **one-shot bootstrap** to confirm the new pipeline against the old, not durable infrastructure. The old pipeline stays live only long enough to validate the new one, then it (and the capture instrumentation) are retired. The gate is a sanity check ("the new pipeline isn't doing something obviously wrong"), not a proof 

- **Metrics only, from the old side.** Capture just `all_fitting_scores_df` (rmse / R² / adj-R² per selector × feature set × error type, TRAIN and TEST). Old-side predictions are deliberately NOT captured: at Rmd scope only the scores survive (the per-cell true/pred values are discarded inside `fit_and_predict_output_error_using_feature_set()`), so reaching them would need editing old fitting logic (a real modification) or a redundant re-fit. This supersedes the earlier "capture old-side predictions" decision 

- Any predictions golden is a **new-side self-regression fixture** (free, since the new fitting function returns the bundle), guarding new-code changes only — not part of the old-vs-new gate 

- **The old side has a single tier** — the full-batch metric golden. The fast TDD inner loop is served by a new-side self-regression fixture on a deterministic COR-subsample, not by an old-side subset golden (which metrics-only + one-shot made redundant and awkward, since running the old Rmd on subset inputs would need more old-code modification) 

- **Capture mechanism: in-Rmd, params-flag-gated, additive and inert.** A `capture_golden_master` flag (default FALSE) plus one capture chunk after the last fit chunk; off = no behavior change to normal builds. On = write `all_fitting_scores_df` via `write_a_tib_with_provenance(sess, ...)` as RDS, reusing the prep run's dataprov session/registry. This supersedes the earlier external-capture-script (Mechanism B) decision 

- **Overwrite guard: existence check + `force`, not an interactive prompt.** Evaluate at the start of the run; if the flag is on and the golden exists and `force` is unset, `stop()` before any fitting or writes (fail-fast). An interactive prompt is avoided because `interactive()` is FALSE under `rmarkdown::render()` and `readline()` would not block. Abort rather than silently skip generation, so no "generate = yes but nothing generated" contradictory record is ever written 

- **dataprov for golden files.** Provenance is a strong fit for goldens (their whole value is trust-as-reference). `write_a_tib_with_provenance()` (in `R/provenance_helpers.R`) writes the tib as-is and supports RDS; provenance lives in the `Data/dataprov_registry`, so the compared payload stays clean and diff-deterministic, and reads are hash-verified via `resolve_prov_file()` 

- **Scoped exception to the no-modify rule.** Setting up the old-side capture requires touching the working analysis Rmd, but only as additive, flag-gated instrumentation that is inert when off (no existing logic altered). This is documented as a deliberate one-time exception; the rest of the refactor resumes no-modify. Claude Code may draft the flag-gated chunk; the author reviews and runs it 

- Equivalence tolerance is testthat's default (~1.5e-8) on the metrics; if it must be loosened to pass, investigate the design-matrix seam (intercept / contrasts / column order) rather than loosen 

- Fast-subset (new-side) fixture selected by COR group: fixed seed; smallest set of distinct `rsp_UUID_of_COR_Base_problem_that_is_wrapped` still covering all 4 selectors × 2 error types, both `dom_err_type` levels, every cell non-degenerate for LM (`n > p + 1`) 

- Input snapshot = working frames `p3_working_train_df` / `p3_working_test_df` plus aux `p3_train_aux_df` / `p3_test_aux_df`, captured in memory at the fit call site as RDS after the Gurobi→ILP munging (not the lossy pre-munge CSVs). Commit the full batch directly (no Git LFS): ~13 MB per working frame as CSV, smaller as RDS; ~100 KB per aux frame 

### Next steps

- Author's careful read of the revised `bdpg_fitting_refactor_plan.md`; revise in place if further edits arise 

- Begin the Claude Code implementation session at Checkpoint 0 when ready 
