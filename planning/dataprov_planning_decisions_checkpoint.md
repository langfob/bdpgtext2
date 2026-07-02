# dataprov Integration — Planning Decisions Checkpoint

**Status:** planning complete. The step-wise implementation plan has now been drafted
(`dataprov_integration_plan_for_claude_code.md`); the "Open decisions" section below was
resolved during drafting. No code has been written yet. This file records the decisions so
work can resume in a fresh chat if needed.

## Goal

Integrate the `dataprov` R package into two bdpg Rmd files so that data-file writes are
provenance-tracked and reads pin the exact intended files:

- Prep file (writer): `p9_v01_prep_data_for_p8_to_load_from_files.Rmd`
- Main body file (reader): `p9_v01_all_combined__body.Rmd`

## Decisions locked in

- **Write-side integration = Option C (new wrapper).** Add one new wrapper function
  (e.g. `write_a_tib_with_provenance()`) that performs the dataprov calls and delegates
  to the existing, untouched `write_a_tib_to_csv_file` / `write_a_tib_to_csv_file_using_params`.
  Shared library code is left as-is. The 13 call sites in the prep Rmd change by name only.  
- **Filename convention.** Use dataprov's native naming via
  `prov_record_new(label = <tib_name>, extension = <from file_type>)`, producing
  `{uuid}__{tib_name}.csv`. No generation time in the name; no params/option settings in
  the name (those go to the sidecar).  
- **Params → sidecar.** Pass the **full `params` list** through
  `prov_session_start(parameters = params)` (stored verbatim in every sidecar for the run),
  chosen over a curated subset because `params` is threaded into tib-building functions and any
  field could affect a generated tib. `code_version` (git commit) is auto-captured.  
- **Read-side = Shape B (single session pin + tag resolver).** Pin ONE `session_uuid` in the
  main-body params. A small resolver maps each needed tib to its file:
  `prov_list(reg, tags = list(tib = <name>))` -> keep row with matching pinned `session_uuid`
  and `status == "finalized"` -> `prov_get(uuid)` -> read `locations` path -> load. Exactly one
  match or error (fail loud). Optional `prov_verify()` on load for hash integrity.  
- **Session spine = ONE flat session per prep run.** All tracked tibs are siblings under a
  single session. Intra-run lineage (e.g. p3 derived from p2) is NOT modeled for now; can be
  added later as a separate increment via stage-wise sessions if desired.  
- **Path fix.** Restore the prep file's `relative_path_to_data_out_loc` from the temporary
  `Data/TempOutput_2026_03_dataprov` back to `Data/TempOutput` (matches the main body).  

## Relevant dataprov facts confirmed (from .Rd + vignettes)

- `prov_record_new(session, data_dir, description, tags, extension="csv", label, sanitize_label)`
  -> record with `$filepath`, `$uuid`, `$status`. Filename `{uuid}__{label}.{ext}`.  
- `prov_record_finalize(record)` locks SHA-256 + `created_at`. Optional if using
  `prov_session_close(sess, finalize_all = TRUE)`.  
- `prov_session_start(registry, description, parameters, parent_uuids, code_version)` — params,
  code_version, and parent lineage are session-level (shared by all records in the run).  
- `prov_list(registry, parent_uuid, tags, since, before)` -> tibble of
  `uuid, description, created_at, hash_value, status, session_uuid`. Filters on tags/parent/date
  only (NOT on session_uuid or parameters); tags are the queryable metadata.  
- `prov_get(reg, uuid)` -> full sidecar incl. `locations[].path` (authoritative filepath).  
- `prov_siblings(reg, uuid)`, `prov_chain(reg, uuid)`, `prov_verify(filepath)`,
  `prov_verify_all(reg)` available.  
- Registry: `prov_registry_init()` (create), `prov_registry_open()` (reopen, used by reads),
  `prov_registry_rebuild()`; disconnect with `DBI::dbDisconnect(reg$conn)`. Sidecar is canonical;
  registry is a rebuildable index.  

## I/O map (established)

- Prep writes 13 active tibs via `write_a_tib_to_csv_file_using_params()`.  
- Main body has NO top-level data writes; it reads 8 of the 13 via `load_file_into_tibble()`.  
- The 8 read tibs: filtered_full_initial_exp_tib, p2_app_wrap_tib, p3_working_train_df,
  p3_working_test_df, p3_train_aux_df, p3_test_aux_df,
  p3_working_train_df__before_any_preprocessing, p3_working_test_df__before_any_preprocessing.  

## Open decisions (still to settle before drafting the plan)

- Scope: track all 13 written tibs, or only the 8 read by the main body? And do both write-tier
  flags (`write_tibs_to_csv`, `write_most_important_tibs_to_csv`) route through the wrapper?  
- Session `description` text and exactly which entries go into `parameters =`.  
- Tags per record beyond `tib = <name>` (e.g. the two disambiguating settings)?  
- Where the wrapper function lives (new sourced R file vs. setup chunk in the prep Rmd).  
- Resolver's home and exact contract; whether `prov_verify()` runs on every load.  
- Pin mechanism: prep prints its `session_uuid`; new main-body param name (e.g. `prep_session_uuid`).  
- Registry directory location and init/open/disconnect policy.  
- Whether to include light unit tests for the wrapper and resolver.  

## Working-practice notes

- `.Rd` files must be uploaded renamed to `.Rd.txt` (uploader allowlist blocks `.Rd`).  
- Interaction protocol: assistant waits for a message ending in `READY` before acting.  
- Do not modify working/shared code beyond the agreed minimal, surgical additions.  
