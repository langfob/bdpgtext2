#===============================================================================
#
#                       test-provenance-resolver.R
#
#===============================================================================

#  Tests for resolve_prov_file() in R/provenance_helpers.R.

    #  Shared helper: builds a temp registry with one closed session
    #  containing the two given tib records (list of list(tib_name, ext,
    #  content)).  Returns list(reg, session_uuid, data_dir, recs).

build_test_registry_with_session <- function (tmp_root, tib_specs)
    {
    registry_dir = file.path (tmp_root, "dataprov_registry")
    data_dir     = file.path (tmp_root, "data_out")
    dir.create (data_dir, recursive = TRUE)

    reg  = open_or_init_registry (registry_dir)
    sess = dataprov::prov_session_start (reg, description = "test session",
                                         parameters = list ())

    recs = list ()
    for (spec in tib_specs)
        {
        rec = dataprov::prov_record_new (sess, data_dir,
                                         label       = spec$tib_name,
                                         extension   = spec$ext,
                                         description = "test record",
                                         tags        = list (tib = spec$tib_name,
                                                             file_extension = spec$ext))
        write.csv (spec$content, rec$filepath, row.names = FALSE, quote = TRUE)
        rec = dataprov::prov_record_finalize (rec)
        recs [[length (recs) + 1]] = rec
        }

    dataprov::prov_session_close (sess)

    list (reg = reg, session_uuid = sess$uuid, data_dir = data_dir, recs = recs)
    }

#===============================================================================

test_that ("resolve_prov_file() returns the correct path for a valid (session, tib, ext)",
    {
    tmp_root = withr::local_tempdir (.local_envir = parent.frame ())

    built = build_test_registry_with_session (
        tmp_root,
        list (list (tib_name = "tibA", ext = "csv", content = data.frame (x = 1:2)),
             list (tib_name = "tibB", ext = "csv", content = data.frame (y = 3:4)))
        )
    on.exit (DBI::dbDisconnect (built$reg$conn), add = TRUE)

    path = resolve_prov_file (built$reg, built$session_uuid, "tibA", ext = "csv")

    expect_identical (normalizePath (path),
                      normalizePath (built$recs [[1]]$filepath))
    })

test_that ("resolve_prov_file() errors when zero records match",
    {
    tmp_root = withr::local_tempdir (.local_envir = parent.frame ())

    built = build_test_registry_with_session (
        tmp_root,
        list (list (tib_name = "tibA", ext = "csv", content = data.frame (x = 1)))
        )
    on.exit (DBI::dbDisconnect (built$reg$conn), add = TRUE)

    expect_error (
        resolve_prov_file (built$reg, built$session_uuid, "no_such_tib", ext = "csv"),
        "but found 0"
        )
    })

test_that ("resolve_prov_file() errors when more than one record matches",
    {
    tmp_root = withr::local_tempdir (.local_envir = parent.frame ())

        #  Two records that share the same (tib, ext) tags within the same
        #  session - an ambiguous match.
    built = build_test_registry_with_session (
        tmp_root,
        list (list (tib_name = "dup_tib", ext = "csv", content = data.frame (x = 1)),
             list (tib_name = "dup_tib", ext = "csv", content = data.frame (x = 2)))
        )
    on.exit (DBI::dbDisconnect (built$reg$conn), add = TRUE)

    expect_error (
        resolve_prov_file (built$reg, built$session_uuid, "dup_tib", ext = "csv"),
        "but found 2"
        )
    })

test_that ("resolve_prov_file() with verify=TRUE errors on a tampered file",
    {
    tmp_root = withr::local_tempdir (.local_envir = parent.frame ())

    built = build_test_registry_with_session (
        tmp_root,
        list (list (tib_name = "tibA", ext = "csv", content = data.frame (x = 1:2)))
        )
    on.exit (DBI::dbDisconnect (built$reg$conn), add = TRUE)

        #  Tamper with the data file after finalization, bypassing dataprov.
    writeLines ("tampered content", built$recs [[1]]$filepath)

    expect_error (
        resolve_prov_file (built$reg, built$session_uuid, "tibA", ext = "csv",
                          verify = TRUE),
        class = "dataprov_verify_error"
        )
    })

test_that ("resolve_prov_file() with verify=FALSE does not check the hash",
    {
    tmp_root = withr::local_tempdir (.local_envir = parent.frame ())

    built = build_test_registry_with_session (
        tmp_root,
        list (list (tib_name = "tibA", ext = "csv", content = data.frame (x = 1:2)))
        )
    on.exit (DBI::dbDisconnect (built$reg$conn), add = TRUE)

    writeLines ("tampered content", built$recs [[1]]$filepath)

    path = resolve_prov_file (built$reg, built$session_uuid, "tibA", ext = "csv",
                              verify = FALSE)

    expect_identical (normalizePath (path),
                      normalizePath (built$recs [[1]]$filepath))
    })

test_that ("resolve_prov_file() stays silent by default on successful verification",
    {
    tmp_root = withr::local_tempdir (.local_envir = parent.frame ())

    built = build_test_registry_with_session (
        tmp_root,
        list (list (tib_name = "tibA", ext = "csv", content = data.frame (x = 1:2)))
        )
    on.exit (DBI::dbDisconnect (built$reg$conn), add = TRUE)

    output = capture.output (
        invisible (resolve_prov_file (built$reg, built$session_uuid, "tibA", ext = "csv"))
        )

    expect_length (output, 0)
    })

test_that ("resolve_prov_file() prints a VERIFIED line when verbose_verify=TRUE",
    {
    tmp_root = withr::local_tempdir (.local_envir = parent.frame ())

    built = build_test_registry_with_session (
        tmp_root,
        list (list (tib_name = "tibA", ext = "csv", content = data.frame (x = 1:2)))
        )
    on.exit (DBI::dbDisconnect (built$reg$conn), add = TRUE)

    output = capture.output (
        resolve_prov_file (built$reg, built$session_uuid, "tibA", ext = "csv",
                          verbose_verify = TRUE)
        )

    expect_true (any (grepl ("VERIFIED: tib 'tibA'", output, fixed = TRUE)))
    })
