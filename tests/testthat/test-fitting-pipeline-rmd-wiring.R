#===============================================================================
#
#                       test-fitting-pipeline-rmd-wiring.R
#
#===============================================================================

#  Checkpoint 5 verification (plan §6): with params$use_new_fitting_pipeline
#  TRUE, the wired subsection ("Representation shortfall using PUsAndSppOnly",
#  chunk predictRepShortfallUsingPUsAndSppOnly in
#  Paper_9_heavily_abridged_version_of_p8/p9_v01_all_combined__body.Rmd)
#  reproduces the golden scores and renders a plot without error.
#
#  This test extracts and eval()'s the ACTUAL chunks from the Rmd (not a
#  hand-reproduction of what they're supposed to do), in the same order the
#  document itself would run them, against the committed golden inputs -- so
#  it exercises the real wiring, not just the underlying pipeline functions
#  (which are already covered by the Checkpoint 2/4 equivalence tests).
#
#  The OLD (use_new_fitting_pipeline = FALSE) branch is intentionally NOT
#  executed here: `git diff` already confirms it is byte-for-byte identical
#  to the pre-Checkpoint-5 Rmd (see DECISIONS.md), and exercising it would
#  require satisfying old code's full plotting/matilda-file dependency graph
#  (save_this_ggplot(), plot_train_and_test_stuff_for_one_RS(), etc.) for no
#  additional correctness signal -- nothing in that branch's bytes changed.
#
#  Run via testthat::test_file() after sourcing R/provenance_helpers.R,
#  R/v1_paper_3_fitting_functions.R, R/v1_paper_3_plotting_and_evaluation_functions.R,
#  R/v1_paper_3_utility_functions.R, and R/v1_paper_9_fitting_and_eval_pipeline.R,
#  plus library(dataprov) / library(DBI) / library(here) / library(tidymodels),
#  per the convention in
#  "planning/dataprov integration/DONE - dataprov_planning_decisions_checkpoint - 2026 07 13.md".

proj_dir_for_tests = here::here ()

golden_registry_dir_for_tests =
    file.path (proj_dir_for_tests, "tests", "fixtures", "dataprov_registry")

body_rmd_path_for_tests =
    file.path (proj_dir_for_tests, "Paper_9_heavily_abridged_version_of_p8",
              "p9_v01_all_combined__body.Rmd")

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

extract_rmd_chunk <- function (rmd_path, label)
    {
    lines = readLines (rmd_path, warn = FALSE)

    trimmed = trimws (lines)
    start_idx = which (startsWith (trimmed, paste0 ("```{r ", label, ",")) |
                       (trimmed == paste0 ("```{r ", label, "}")))

    if (length (start_idx) != 1)
        stop (paste0 ("Expected exactly one chunk labeled '", label,
                      "' in ", rmd_path, ", found ", length (start_idx), "."))

    close_offset = which (lines [(start_idx + 1):length (lines)] == "```") [1]
    end_idx = start_idx + close_offset

    paste (lines [(start_idx + 1):(end_idx - 1)], collapse = "\n")
    }

    #  Evaluates one or more Rmd chunks, by label, in sequence, inside env.
run_rmd_chunks_in_env <- function (rmd_path, labels, env)
    {
    for (lbl in labels)
        eval (parse (text = extract_rmd_chunk (rmd_path, lbl)), envir = env)
    }

#===============================================================================

reg_for_tests = dataprov::prov_registry_open (golden_registry_dir_for_tests)

p3_working_train_df_golden = readRDS (resolve_golden_fixture (reg_for_tests, "p3_working_train_df"))
p3_working_test_df_golden  = readRDS (resolve_golden_fixture (reg_for_tests, "p3_working_test_df"))
p3_train_aux_df_golden     = readRDS (resolve_golden_fixture (reg_for_tests, "p3_train_aux_df"))
p3_test_aux_df_golden      = readRDS (resolve_golden_fixture (reg_for_tests, "p3_test_aux_df"))
all_fitting_scores_df_golden = readRDS (resolve_golden_fixture (reg_for_tests, "all_fitting_scores_df"))

DBI::dbDisconnect (reg_for_tests$conn)

    #  Builds the environment the target chunk expects, running the SAME
    #  upstream Rmd chunks it depends on (params setup, rs_method_names_list,
    #  fitting_model_str/display_train_as_final_pred_using_plot,
    #  all_fitting_scores_df init, vars_used_str/inVars, p3_train_x_df/p3_test_x_df)
    #  -- all extracted from the Rmd, none hand-reproduced.
make_wired_chunk_env <- function (use_new_fitting_pipeline)
    {
    env = new.env ()

    env$p3_working_train_df = p3_working_train_df_golden
    env$p3_working_test_df  = p3_working_test_df_golden
    env$p3_train_aux_df     = p3_train_aux_df_golden
    env$p3_test_aux_df      = p3_test_aux_df_golden

    env$params = list (
        exclude_ZL = TRUE,
        fitting_model_str = "lm",
        write_tibs_to_csv = FALSE,
        VERBOSE_LM_CATS = FALSE,
        display_train_as_final_pred_using_plot = FALSE,
        use_new_fitting_pipeline = use_new_fitting_pipeline,
        save_final_model = FALSE
        )

    run_rmd_chunks_in_env (body_rmd_path_for_tests,
                           c ("setBdpgOptionsThatAreHardToSetInParams",
                             "settingsThatApplyToAllPredPlots",
                             "creacyanlFittingScoresDF",
                             "setPUsAndSppOnlyParams",
                             "buildPUsAndSppOnlyTestAndTrain"),
                           env)

    env
    }

#===============================================================================

test_that ("the wired chunk, with the flag TRUE, runs end-to-end and matches the golden scores",
    {
    env = make_wired_chunk_env (use_new_fitting_pipeline = TRUE)

    pdf (NULL)    #  headless: absorb the print(plot_output_error_fit(...)) call
    on.exit (dev.off (), add = TRUE)

    expect_no_error (
        run_rmd_chunks_in_env (body_rmd_path_for_tests,
                               "predictRepShortfallUsingPUsAndSppOnly", env)
        )

    new_scores = env$all_fitting_scores_df

    expect_equal (names (new_scores),
                 c ("train_or_test", "fitting_model_str", "vars_used_str",
                   "measure_name_str", "rs_method_name", "rmse", "R2", "adj_R2"))
    expect_equal (nrow (new_scores), 8)

    golden_subset =
        all_fitting_scores_df_golden %>%
        dplyr::filter (vars_used_str == "PUsAndSppOnly",
                      measure_name_str == "abs_rep_shortfall_resid")

    merged =
        dplyr::inner_join (
            new_scores, golden_subset,
            by = c ("rs_method_name", "train_or_test"))

    expect_equal (nrow (merged), 8)
    expect_equal (merged$rmse.x, merged$rmse.y)
    expect_equal (merged$R2.x, merged$R2.y)
    expect_equal (merged$adj_R2.x, merged$adj_R2.y)
    })

test_that ("the wired chunk's new-pipeline branch parses the OLD branch too (both are reachable code)",
    {
        #  The old branch's bytes are asserted identical to the
        #  pre-Checkpoint-5 Rmd via git diff (see DECISIONS.md), not
        #  re-executed here (see file header). This just confirms the
        #  if/else wrapping didn't break the old branch's syntax.
    chunk_text = extract_rmd_chunk (body_rmd_path_for_tests,
                                    "predictRepShortfallUsingPUsAndSppOnly")

    expect_no_error (parse (text = chunk_text))
    expect_match (chunk_text, "fit_and_predict_output_error_using_feature_set", fixed = TRUE)
    expect_match (chunk_text, "run_output_error_fit", fixed = TRUE)
    })

test_that ("the Rmd actually sources R/v1_paper_9_fitting_and_eval_pipeline.R",
    {
        #  Regression test for a real bug caught by the author's own knit: the
        #  wired chunk called run_output_error_fit() but the Rmd's own
        #  library/source-loading chunk (loadP1andP2FunctionDefns) never
        #  sourced the file that defines it, so `Rscript`/testthat runs (which
        #  source it manually per this repo's test convention) passed while
        #  an actual knit failed with "could not find function
        #  'run_output_error_fit'". This is a STATIC check on the source
        #  chunk itself, deliberately not relying on the test runner's own
        #  manual sourcing convention -- that convention is exactly what let
        #  the missing source() call slip through in the first place.
    source_chunk_text = extract_rmd_chunk (body_rmd_path_for_tests, "loadP1andP2FunctionDefns")

    expect_match (source_chunk_text, "v1_paper_9_fitting_and_eval_pipeline\\.R")
    })
