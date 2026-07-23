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
