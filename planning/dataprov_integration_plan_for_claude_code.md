# Implementation Plan: Integrate `dataprov` into the bdpg p9 Rmd files

**Audience:** Claude Code (implementation), executed later, one stage at a time.
**Author of plan:** planning session (design decisions already settled with the user).
**Nature of change:** additive and surgical. Provenance tracking is added around existing
file writes and reads. Working/shared code is NOT refactored.

---

## 0. Guardrails (read before doing anything)

- **Do NOT modify shared/working code.** In particular, do not change
  `write_a_tib_to_csv_file`, `write_a_tib_to_csv_file_using_params`, or
  `load_file_into_tibble`. You may READ them to copy exact behavior, but not edit them.  
- **Do NOT refactor, rename, reformat, or "clean up" anything** beyond the specific edits
  described here.  
- **Work in small, testable increments.** Do one Stage, then STOP at its checkpoint and wait
  for the user to review before starting the next Stage.  
- **Do not invent function signatures or write options.** Where this plan says "read the real
  source," open the actual file and copy the real behavior.  
- All new code goes in ONE new file: `R/provenance_helpers.R`. The two Rmd files get only
  minimal wiring edits.  

---

## 1. Fixed conventions (use these exact names/values)

- **New helpers file:** `R/provenance_helpers.R`  
- **Registry directory:** `file.path(proj_dir, "Data", "dataprov_registry")`  
- **Output data directory (writes) and input directory (reads):** `Data/TempOutput`
  (relative to `proj_dir`), i.e. `params$relative_path_to_data_out_loc` must be
  `"Data/TempOutput"` in BOTH Rmd files.  
- **Filename convention (produced by dataprov):** `{uuid}__{tib_name}.{ext}`  
  (no generation time, no param/option settings in the name).  
- **New main-body YAML param:** `prep_session_uuid` (holds the pinned session UUID).  
- **Tags written on every record:** `list(tib = <tib_name>, file_extension = <ext>,`
  `gurobi_problem_filter = <value>, exclude_imperfect_wraps = <value>)`.
  (`tib` and `file_extension` are REQUIRED for the resolver to return exactly one match.)  
- **Session `description`:** e.g.
  `paste0("bdpg p9 prep run; gurobi_problem_filter=", gpf, "; exclude_imperfect_wraps=", eiw)`.  
- **Session `parameters`:** the **full `params` list** from the prep Rmd, recorded verbatim
  in every sidecar. Rationale: `params` is threaded into the tib-building functions, so any
  field could influence a generated tib; recording the whole set guards against an unrecorded
  value silently affecting a file. (All params originate from YAML and are JSON-serializable,
  so no serialization issues are expected.)  
- **`code_version`:** leave to auto-detection (git commit). If the repo is not a git repo,
  accept the resulting `NULL` + warning; do not fabricate a version.  

---

## 2. Design summary (what we are building)

- **Write side:** a new wrapper `write_a_tib_with_provenance()` that, for each tib, reserves a
  dataprov record, writes the tib to the record's `$filepath`, and finalizes the record. The
  13 existing write call sites in the prep Rmd are changed by name only (kept inside their
  existing `if` guards). One flat dataprov **session per prep run** groups all records as
  siblings.  
- **Read side:** the main body pins ONE `prep_session_uuid` and uses a new resolver
  `resolve_prov_file()` to turn each `tib_name` into the exact on-disk file for that session,
  with an integrity check on load. The 8 existing read call sites are adapted minimally.  

**Key implementation note (do not skip):** dataprov owns the filename. `prov_record_new()`
returns `rec$filepath` = `{data_dir}/{uuid}__{tib_name}.{ext}`. The existing writer
`write_a_tib_to_csv_file` builds its OWN filename internally, so you cannot delegate the write
to it (it would write to the wrong name). Instead, the wrapper must write the tib DIRECTLY to
`rec$filepath` using the SAME low-level write options the existing writer uses. Before writing
the wrapper, READ `write_a_tib_to_csv_file` and copy its exact write options (e.g. for CSV the
`write.csv(...)` arguments such as `row.names` and `quote`; for RDS the `saveRDS(...)` call).
Do not guess these — copy them verbatim so output files are byte-compatible with the old ones.

---

## Stage 0 — Preconditions and path restoration (tiny, safe)

**Do:**

- In `p9_v01_prep_data_for_p8_to_load_from_files.Rmd`, restore
  `relative_path_to_data_out_loc` from `"Data/TempOutput_2026_03_dataprov"` to
  `"Data/TempOutput"`. Confirm the main body already uses `"Data/TempOutput"`; if not, set it.  
- Confirm `dataprov` loads (`library(dataprov)`), and that `DBI` is available.  
- Read and report back (do not modify):

    - the exact write options inside `write_a_tib_to_csv_file` (the CSV and RDS write calls),  
    - the signature and return type of `load_file_into_tibble` (which file defines it, what
      arguments it takes, whether it accepts a full path or a dir+filename),  
    - whether the project is a git repo (affects `code_version`).  

**Do NOT:** create any new files yet.

**Checkpoint 0:** User verifies the path is restored and reviews the three reported facts
(write options, reader signature, git status). Proceed only after sign-off.

---

## Stage 1 — New helpers file skeleton + registry helper (+ test)

**Do:**

- Create `R/provenance_helpers.R` containing:

    - `open_or_init_registry(registry_dir)` — if `registry_dir` exists and contains a registry,
      call `prov_registry_open(registry_dir)`; otherwise create the directory and call
      `prov_registry_init(registry_dir)`. Return the registry object.  
    - Empty stubs (with roxygen-style header comments) for `write_a_tib_with_provenance()` and
      `resolve_prov_file()` to be filled in later Stages.  

- Add `tests/testthat/test-provenance-registry.R` (or the project's existing test location):

    - test that `open_or_init_registry()` creates a registry when the dir is absent,  
    - test that a second call opens the existing one without error.  

**Do NOT:** wire anything into the Rmd files yet.

**Checkpoint 1:** User reviews `provenance_helpers.R` and confirms the registry test passes.

---

## Stage 2 — Write wrapper + test

**Do:**

- Implement `write_a_tib_with_provenance(sess, tib, tib_name, data_dir, params, file_type)`:

    1. Map `file_type` to extension(s): `"csv"` → `"csv"`, `"rds"` → `"rds"`,
       `"both"` → both (create one record per extension).  
    2. For each extension `ext`:

        - `rec <- prov_record_new(sess, data_dir, label = tib_name, extension = ext,`
          `description = <short per-file description>, tags = <the fixed tag list incl. file_extension>)`  
        - Write `tib` to `rec$filepath` using the exact low-level options copied from
          `write_a_tib_to_csv_file` (CSV vs RDS branch).  
        - `prov_record_finalize(rec)`  

    3. Return the record(s) invisibly (so callers could collect UUIDs if ever needed).  

- Pull the tag values (`gurobi_problem_filter`, `exclude_imperfect_wraps`) from `params`/`here()`
  the same way the existing writer obtains them (read the existing writer to match the source).  
- Add `tests/testthat/test-provenance-write.R`:

    - with a temp registry + open session, calling the wrapper writes a file named
      `{uuid}__{tib_name}.csv`,  
    - the record finalizes (status `"finalized"`, non-empty hash),  
    - `prov_list(reg, tags = list(tib = tib_name))` finds exactly the written record.  

**Do NOT:** touch the prep Rmd yet.

**Checkpoint 2:** User reviews the wrapper and confirms its tests pass.

---

## Stage 3 — Wire the write wrapper into the prep Rmd

**Do (all inside the prep Rmd, minimal edits only):**

- In the setup region (after `proj_dir`/sources are established), add:

    - `source(file.path(proj_dir, "R/provenance_helpers.R"))`  
    - `reg  <- open_or_init_registry(file.path(proj_dir, "Data", "dataprov_registry"))`  
    - `sess <- prov_session_start(reg, description = <as specified>,`
      `parameters = params)` — the full params list, per §1 (let `code_version` auto-detect).  

- Swap each of the **13 active** call sites from
  `write_a_tib_to_csv_file_using_params(tib, "<name>", params, file_type)`
  to
  `write_a_tib_with_provenance(sess, tib, "<name>", data_out_loc, params, file_type)`.

    - Keep each call inside its existing `if (params$write_tibs_to_csv | ...)` guard.  
    - Change ONLY the function name and its arguments; do not alter surrounding logic.  
    - `data_out_loc` is the same output dir the existing writer uses (`Data/TempOutput`);
      obtain it the same way the existing code does.  
    - Leave the commented-out call (previously near the "cor_tib" area) commented out.  

- At the end of the prep run, add:

    - `prov_session_close(sess)`  
    - a prominent print of the session UUID, e.g.
      `message("PREP SESSION UUID: ", <session uuid accessor>)` — read how the session object
      exposes its UUID (e.g. `sess$uuid`) and use the real accessor; also echo the value in a
      visible knit output cell so it appears in the rendered document.  
    - `DBI::dbDisconnect(reg$conn)`  

**Do NOT:** change any tib-construction logic; only the write calls and the session lifecycle.

**Checkpoint 3 (manual eval):** User runs the prep Rmd and confirms:

- 13 data files appear in `Data/TempOutput` named `{uuid}__{name}.csv`, each with a
  `{uuid}.json` sidecar,  
- the session UUID is printed/echoed,  
- a spot-checked sidecar contains the expected `parameters`, `tags`, and `code_version`.  

Proceed only after sign-off.

---

## Stage 4 — Read resolver + test

**Do:**

- Implement `resolve_prov_file(reg, session_uuid, tib_name, ext = "csv", verify = TRUE)`:

    1. `cand <- prov_list(reg, tags = list(tib = tib_name, file_extension = ext))`  
    2. Keep rows where `session_uuid == <pinned>` and `status == "finalized"`.  
    3. If not exactly one row remains, `stop()` with a clear message (name the tib, the count,
       and the session) — fail loud, never guess.  
    4. `sc <- prov_get(reg, <that uuid>)`; extract the local location path from `sc$locations`
       (the entry with `type == "local"` whose file exists). This is the authoritative path.  
    5. If `verify`, run `prov_verify(path)` (throws on hash mismatch).  
    6. Return the path.  

- Add `tests/testthat/test-provenance-resolver.R`:

    - build a temp registry with a session and two tagged records,  
    - resolver returns the correct path for a valid `(session, tib, ext)`,  
    - resolver errors when zero match and when more than one match,  
    - with `verify = TRUE`, a tampered file causes an error.  

**Do NOT:** touch the main body Rmd yet.

**Checkpoint 4:** User reviews the resolver and confirms its tests pass.

---

## Stage 5 — Wire the resolver into the main body Rmd

**Do (all inside the main body Rmd, minimal edits only):**

- Add `prep_session_uuid` to the YAML `params` (default can be a placeholder string).  
- In the setup region, add:

    - `source(file.path(proj_dir, "R/provenance_helpers.R"))`  
    - `reg <- prov_registry_open(file.path(proj_dir, "Data", "dataprov_registry"))`  

- For each of the **8** read call sites currently of the form
  `load_file_into_tibble(<base_path>, "<name>.gurobi__...csv")`, change to:

    - `path <- resolve_prov_file(reg, params$prep_session_uuid, "<tib_name>", ext = "csv")`  
    - then load via the EXISTING `load_file_into_tibble` (adapt to its real signature found in
      Stage 0 — pass the resolved full path, or split into dir + filename as that function
      expects). Do not modify `load_file_into_tibble`.  
    - Use the bare `<tib_name>` (e.g. `p2_app_wrap_tib`), NOT the old suffixed filename.  

- At the end (if appropriate for the doc), `DBI::dbDisconnect(reg$conn)`.  

**Do NOT:** change any downstream analysis logic; only the 8 loads and the setup wiring.

**Checkpoint 5:** User sets `prep_session_uuid` to the value printed by the Stage 3 run, runs
the main body, and confirms all 8 inputs load from the pinned session and verification passes.

---

## Stage 6 — End-to-end validation

**Do:**

- Run prep → capture printed `session_uuid` → set `prep_session_uuid` in the main body → run
  the main body.  
- Confirm the main body loads the intended files and that downstream results match the
  pre-change behavior (parity check — the data content should be identical to before; only the
  file naming and provenance are new).  
- Spot-check one sidecar end to end (parameters, tags, hash, code_version, session_uuid).  

**Checkpoint 6:** Final user sign-off.

---

## Appendix A — File/edit inventory (expected footprint)

- **New file:** `R/provenance_helpers.R` (registry helper, write wrapper, resolver).  
- **New tests:** registry, write wrapper, resolver.  
- **Edited (wiring only):** `p9_v01_prep_data_for_p8_to_load_from_files.Rmd`
  (path restore; setup; 13 call-site name swaps; session close + UUID print + disconnect).  
- **Edited (wiring only):** `p9_v01_all_combined__body.Rmd`
  (new param; setup; 8 read call-site adaptations; disconnect).  
- **Unchanged:** `write_a_tib_to_csv_file`, `write_a_tib_to_csv_file_using_params`,
  `load_file_into_tibble`, and all other existing/shared code.  

## Appendix B — Known limitation (accepted for this pass)

A single flat session records all tibs as siblings; intra-run derivation (e.g. p3 from p2) is
NOT expressed as a parent chain. This was a deliberate simplification. Adding stage-wise
sessions with `parent_uuids` to capture the internal pipeline lineage is a possible FUTURE
increment, to be planned separately if wanted.
