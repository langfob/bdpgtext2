#===============================================================================
#
#                       provenance_helpers.R
#
#===============================================================================

#  History

#  2026 07 12 - BTL - v1
#     - Created to wire the `dataprov` package into the p9 prep/body Rmd
#       pipeline (write-side provenance tracking + read-side session-pinned
#       resolution).  See planning/dataprov_integration_plan_for_claude_code.md.

#===============================================================================

    #  Opens an existing dataprov registry at registry_dir, or creates one if
    #  none exists yet there.  Returns the dataprov_registry object (with the
    #  open DBI connection in $conn).  Caller is responsible for eventually
    #  calling DBI::dbDisconnect(reg$conn).

open_or_init_registry <- function (registry_dir)
    {
    if (! dir.exists (registry_dir))
        dir.create (registry_dir, recursive = TRUE)

    reg = tryCatch (
        dataprov::prov_registry_open (registry_dir),
        error = function (e) NULL
        )

    if (is.null (reg))
        reg = dataprov::prov_registry_init (registry_dir)

    return (reg)
    }

#===============================================================================

    #  Writes tib to disk under dataprov's own filename
    #  ({uuid}__{tib_name}.{ext}), recording a finalized provenance record for
    #  each extension written (per file_type: "csv", "rds", or "both").
    #
    #  sess      - an open dataprov session (from prov_session_start())
    #  tib       - the tibble/data.frame to write
    #  tib_name  - base name used as the dataprov record label
    #  data_dir  - directory the record's file is written into
    #  params    - full params list from the calling Rmd (used to pull the
    #              gurobi_problem_filter / exclude_imperfect_wraps tag values,
    #              the same way write_a_tib_to_csv_file_using_params() does)
    #  file_type - "csv", "rds", or "both"
    #
    #  Returns the list of finalized dataprov_record object(s), one per
    #  extension written, invisibly.

write_a_tib_with_provenance <- function (sess, tib, tib_name, data_dir,
                                         params, file_type)
    {
    if (file_type == "csv")
        exts = "csv"
    else if (file_type == "rds")
        exts = "rds"
    else if (file_type == "both")
        exts = c ("csv", "rds")
    else
        stop (paste0 ("In write_a_tib_with_provenance(), \n",
                      "file_type is '", file_type, "'.  ",
                      "Must be 'csv', 'rds', or 'both'.\n"))

        #  Same derivation as write_a_tib_to_csv_file_using_params() in
        #  R/p8.unifiedDataLoading.v01.R, so the tag values match what the
        #  old writer would have encoded in its filename.
    exclude_imperfect_wraps = bdpg::vb (params$exclude_imperfect_wraps)

    gurobi_problem_filter = params$gurobi_problem_filter
    if (is.null (gurobi_problem_filter) |
                  ! ((gurobi_problem_filter == "all") |
                     (gurobi_problem_filter == "completed") |
                     (gurobi_problem_filter == "unfinished"))
        )
        stop (paste0 ("In write_a_tib_with_provenance(), \n",
                      "params$gurobi_problem_filter is '",
                      gurobi_problem_filter, "'.  ",
                      "Must be 'all' or 'completed' or 'unfinished'.\n"))

    recs = list ()

    for (ext in exts)
        {
        rec = dataprov::prov_record_new (
            sess, data_dir,
            label       = tib_name,
            extension   = ext,
            description = paste0 ("bdpg p9 prep tib '", tib_name, "' (", ext, ")"),
            tags        = list (tib                    = tib_name,
                                file_extension          = ext,
                                gurobi_problem_filter    = gurobi_problem_filter,
                                exclude_imperfect_wraps  = exclude_imperfect_wraps)
            )

            #  Exact write options copied from write_a_tib_to_csv_file() in
            #  R/p8.unifiedDataLoading.v01.R so output files are
            #  byte-compatible with the old writer.
        if (ext == "csv")
            write.csv (tib, rec$filepath, row.names = FALSE, quote = TRUE)
        else if (ext == "rds")
            saveRDS (tib, rec$filepath)

        rec = dataprov::prov_record_finalize (rec)

        recs [[ext]] = rec
        }

    invisible (recs)
    }

#===============================================================================

    #  Resolves a single tib_name (within one pinned session_uuid) to its
    #  on-disk file path via the dataprov registry, verifying the file's hash
    #  by default.  Errors (does not guess) if zero or more than one
    #  finalized record matches.
    #
    #  reg             - an open dataprov_registry (from prov_registry_open())
    #  session_uuid    - the pinned prep-run session UUID
    #  tib_name        - the tib's dataprov 'tib' tag value
    #  ext             - the tib's dataprov 'file_extension' tag value
    #  verify          - if TRUE, run prov_verify() on the resolved path
    #  verbose_verify  - if TRUE (and verify is TRUE), print a confirmation
    #                    line once prov_verify() succeeds.  Off by default
    #                    to keep normal knit output quiet; turn on for
    #                    debugging.
    #
    #  Returns the resolved file path (character scalar).

resolve_prov_file <- function (reg, session_uuid, tib_name, ext = "csv",
                               verify = TRUE, verbose_verify = FALSE)
    {
    cand = dataprov::prov_list (reg, tags = list (tib = tib_name,
                                                  file_extension = ext))

    cand = cand [(cand$session_uuid == session_uuid) &
                (cand$status == "finalized"), ]

    if (nrow (cand) != 1)
        stop (paste0 ("In resolve_prov_file(), \n",
                      "Expected exactly one finalized record for tib '", tib_name,
                      "' (file_extension '", ext, "') in session '", session_uuid,
                      "', but found ", nrow (cand), ".\n"))

    sc = dataprov::prov_get (reg, cand$uuid [1])

    local_locs = Filter (function (loc) (loc$type == "local") &&
                                        file.exists (loc$path),
                        sc$locations)

    if (length (local_locs) == 0)
        stop (paste0 ("In resolve_prov_file(), \n",
                      "No existing local file location found for tib '", tib_name,
                      "' (uuid '", cand$uuid [1], "').\n"))

    path = local_locs [[1]]$path

    if (verify)
        {
        dataprov::prov_verify (path)
        if (verbose_verify)
            cat ("VERIFIED: tib '", tib_name, "' (", ext, ") -> ", path, "\n", sep='')
        }

    return (path)
    }

#===============================================================================
