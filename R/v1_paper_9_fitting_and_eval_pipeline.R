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
#    eval_model_on_train_or_test_data() (in the plotting/eval file) is reused
#    UNCHANGED here as the metric kernel.
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
