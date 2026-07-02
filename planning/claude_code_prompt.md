# Prompt to paste into Claude Code

> Before pasting: start Claude Code from **inside the `bdpgtext2` project directory**
> (`~/D/Projects/ProblemDifficulty/RnotInPkgs/bdpgtext2`), confirm `here()` resolves to that
> directory, and adjust the plan file path below to wherever you placed the planning docs.

---

You are implementing a defined plan in this R / RStudio project (`bdpgtext2`). Work carefully
and incrementally. Read this whole prompt before acting.

**Authoritative plan.** Read `./planning/dataprov_integration_plan_for_claude_code.md` in full
before doing anything. It is the source of truth for this work. Also read
`./planning/dataprov_planning_decisions_checkpoint.md` for the decisions behind the plan. The
`./planning/dataprov_param_search_side_note.md` file is optional background and is explicitly
out of scope.

**Goal.** Integrate the `dataprov` R package into two R Markdown files so that data-file writes
are provenance-tracked and reads pin the exact intended files, exactly as the plan specifies.

**How to work — these rules override any default instinct:**

- Execute the plan **one Stage at a time**, in order (Stage 0 through Stage 6). After finishing
  a Stage, STOP: summarize what you changed, list what I should verify at that Stage's
  checkpoint, and WAIT for my explicit go-ahead before starting the next Stage. Do not chain
  Stages together.  
- Make **minimal, surgical** edits only. Do NOT refactor, rename, reformat, or "clean up"
  anything the plan does not explicitly require. Do not touch working code unnecessarily.  
- Do NOT modify shared/working functions. In particular, do not edit
  `write_a_tib_to_csv_file`, `write_a_tib_to_csv_file_using_params`, or
  `load_file_into_tibble`. You may READ them to copy their exact behavior, but not change them.  
- Do NOT invent function signatures or write options. Wherever the plan says "read the real
  source," open the actual file and copy the real behavior verbatim (e.g. the exact
  `write.csv`/`saveRDS` arguments, and the real signature of `load_file_into_tibble`).  
- Add only what the plan describes, in small testable increments, and include the `testthat`
  tests the plan specifies.  
- Use the exact fixed conventions in the plan's §1 (registry directory, output directory,
  filename convention, param name `prep_session_uuid`, and the record tags including
  `file_extension`). Do not substitute your own names or values.  

**Environment:**

- R / RStudio project; paths derive from `proj_dir <- here()`. The `dataprov` and `testthat`
  packages are installed; assume `DBI` is available.  
- Run the unit tests yourself as you build them. Do NOT knit the full `.Rmd` files — I will run
  those myself at the manual-evaluation checkpoints (Stages 3, 5, and 6) and report the results
  back to you.  

**If anything is ambiguous, missing, or conflicts with what you find in the code, STOP and ask
me — do not guess or work around it.**

Begin now with **Stage 0 only** (preconditions, restore the output path, and report back the
three facts the plan asks for). Then stop and wait for my review.
