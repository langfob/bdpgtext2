#===============================================================================
#
#                       test-golden-master-capture.R
#
#===============================================================================

#  Tests for the golden-master capture instrumentation added to
#  Paper_9_heavily_abridged_version_of_p8/p9_v01_all_combined__body.Rmd (see
#  planning/bdpg_fitting_refactor_plan.md, "Golden-master specifics", and the
#  2026-07-23 entry in DECISIONS.md).  Two things are tested:
#
#    - the committed golden-master fixtures in tests/fixtures/ load and
#      hash-verify via resolve_prov_file()
#    - the goldenMasterOverwriteGuard chunk -- extracted verbatim from the
#      Rmd, so these tests exercise the ACTUAL chunk rather than a
#      reimplementation that could drift from it -- is inert when
#      capture_golden_master is FALSE, aborts when a finalized golden
#      already exists and force_golden_master is unset, and is bypassed
#      when force_golden_master is TRUE
#
#  Run via testthat::test_file() after sourcing R/provenance_helpers.R and
#  library(dataprov) / library(DBI) / library(here), per the convention
#  documented in
#  "planning/dataprov integration/DONE - dataprov_planning_decisions_checkpoint - 2026 07 13.md".

proj_dir_for_tests = here::here ()

golden_registry_dir_for_tests =
    file.path (proj_dir_for_tests, "tests", "fixtures", "dataprov_registry")

body_rmd_path_for_tests =
    file.path (proj_dir_for_tests, "Paper_9_heavily_abridged_version_of_p8",
              "p9_v01_all_combined__body.Rmd")

    #  This dedicated registry (tests/fixtures/dataprov_registry) is meant to
    #  hold at most one finalized golden generation at a time -- that is
    #  exactly the invariant the overwrite guard enforces -- so resolving
    #  "the" finalized record for a tib name without pinning a session UUID
    #  is safe here, unlike for the shared production registry.
resolve_golden_fixture <- function (reg, tib_name, ext = "rds")
    {
    cand = dataprov::prov_list (reg, tags = list (tib = tib_name,
                                                  file_extension = ext))
    cand = cand [cand$status == "finalized", ]

    if (nrow (cand) != 1)
        stop (paste0 ("Expected exactly one finalized golden record for tib '",
                      tib_name, "', found ", nrow (cand), "."))

    resolve_prov_file (reg, cand$session_uuid [1], tib_name, ext = ext,
                       verify = TRUE)
    }

    #  Extracts the R source of one fenced chunk from the Rmd by label.
extract_rmd_chunk <- function (rmd_path, label)
    {
    lines = readLines (rmd_path, warn = FALSE)

    start_idx = which (startsWith (trimws (lines), paste0 ("```{r ", label, ",")))

    if (length (start_idx) != 1)
        stop (paste0 ("Expected exactly one chunk labeled '", label,
                      "' in ", rmd_path, ", found ", length (start_idx), "."))

    close_offset = which (lines [(start_idx + 1):length (lines)] == "```") [1]
    end_idx = start_idx + close_offset

    paste (lines [(start_idx + 1):(end_idx - 1)], collapse = "\n")
    }

#===============================================================================

test_that ("committed golden-master fixtures load and hash-verify",
    {
    reg = dataprov::prov_registry_open (golden_registry_dir_for_tests)
    on.exit (DBI::dbDisconnect (reg$conn), add = TRUE)

    train_df  = readRDS (resolve_golden_fixture (reg, "p3_working_train_df"))
    test_df   = readRDS (resolve_golden_fixture (reg, "p3_working_test_df"))
    train_aux = readRDS (resolve_golden_fixture (reg, "p3_train_aux_df"))
    test_aux  = readRDS (resolve_golden_fixture (reg, "p3_test_aux_df"))
    scores_df = readRDS (resolve_golden_fixture (reg, "all_fitting_scores_df"))

    expect_s3_class (train_df, "data.frame")
    expect_s3_class (test_df, "data.frame")
    expect_equal (nrow (train_df), nrow (train_aux))
    expect_equal (nrow (test_df), nrow (test_aux))

    expect_equal (names (scores_df),
                 c ("train_or_test", "fitting_model_str", "vars_used_str",
                   "measure_name_str", "rs_method_name", "rmse", "R2", "adj_R2"))

    expect_setequal (unique (scores_df$vars_used_str),
                     c ("PUsAndSppOnly", "ProbSizeAndDensity", "Graph", "All"))
    expect_setequal (unique (scores_df$measure_name_str),
                     c ("abs_rep_shortfall_resid", "abs_sol_cost_err_resid"))
    expect_setequal (unique (scores_df$rs_method_name),
                     c ("ILP", "SA", "UR_Forward", "SA_SS"))
    expect_setequal (unique (scores_df$train_or_test), c ("TRAIN", "TEST"))
    expect_equal (nrow (scores_df), 64)
    })

test_that ("goldenMasterOverwriteGuard is inert when capture_golden_master is FALSE",
    {
    params = list (capture_golden_master = FALSE, force_golden_master = FALSE)
    proj_dir = proj_dir_for_tests

    guard_code = extract_rmd_chunk (body_rmd_path_for_tests, "goldenMasterOverwriteGuard")
    expect_no_error (eval (parse (text = guard_code)))
    })

test_that ("goldenMasterOverwriteGuard aborts on an existing finalized golden when force is unset",
    {
    params = list (capture_golden_master = TRUE, force_golden_master = FALSE)
    proj_dir = proj_dir_for_tests

    guard_code = extract_rmd_chunk (body_rmd_path_for_tests, "goldenMasterOverwriteGuard")
    expect_error (eval (parse (text = guard_code)), "Refusing to overwrite")
    })

test_that ("goldenMasterOverwriteGuard is bypassed when force_golden_master is TRUE",
    {
    params = list (capture_golden_master = TRUE, force_golden_master = TRUE)
    proj_dir = proj_dir_for_tests

    guard_code = extract_rmd_chunk (body_rmd_path_for_tests, "goldenMasterOverwriteGuard")
    expect_no_error (eval (parse (text = guard_code)))
    })
