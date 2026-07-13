#===============================================================================
#
#                       test-provenance-write.R
#
#===============================================================================

#  Tests for write_a_tib_with_provenance() in R/provenance_helpers.R.

test_that ("write_a_tib_with_provenance() writes a correctly named, finalized record",
    {
    tmp_root    = withr::local_tempdir (.local_envir = parent.frame ())
    registry_dir = file.path (tmp_root, "dataprov_registry")
    data_dir     = file.path (tmp_root, "data_out")
    dir.create (data_dir, recursive = TRUE)

    reg = open_or_init_registry (registry_dir)
    on.exit (DBI::dbDisconnect (reg$conn), add = TRUE)

    sess = dataprov::prov_session_start (reg, description = "test session",
                                         parameters = list (gurobi_problem_filter = "all",
                                                            exclude_imperfect_wraps = TRUE))

    tib = data.frame (x = 1:3, y = c ("a", "b", "c"))
    params = list (gurobi_problem_filter = "all", exclude_imperfect_wraps = TRUE)

    recs = write_a_tib_with_provenance (sess, tib, "my_test_tib", data_dir,
                                        params, file_type = "csv")

    rec = recs [["csv"]]

        #  File named {uuid}__{tib_name}.csv, in data_dir.
    expect_true (file.exists (rec$filepath))
    expect_identical (normalizePath (dirname (rec$filepath)),
                      normalizePath (data_dir))
    expect_identical (basename (rec$filepath),
                      paste0 (rec$uuid, "__my_test_tib.csv"))

        #  Record finalized with a non-empty hash.
    expect_identical (rec$status, "finalized")
    expect_true (nzchar (rec$hash))

        #  Content matches what was written (row.names=FALSE, quote=TRUE
        #  same as write_a_tib_to_csv_file()).
    reloaded = read.csv (rec$filepath)
    expect_equal (reloaded$x, tib$x)
    expect_equal (reloaded$y, tib$y)

    dataprov::prov_session_close (sess)

        #  prov_list() (which needs the sidecar, written at session close)
        #  finds exactly this one record for the tib tag.
    found = dataprov::prov_list (reg, tags = list (tib = "my_test_tib"))
    expect_equal (nrow (found), 1)
    expect_identical (found$uuid, rec$uuid)
    })

test_that ("write_a_tib_with_provenance() with file_type='both' writes csv and rds records",
    {
    tmp_root    = withr::local_tempdir (.local_envir = parent.frame ())
    registry_dir = file.path (tmp_root, "dataprov_registry")
    data_dir     = file.path (tmp_root, "data_out")
    dir.create (data_dir, recursive = TRUE)

    reg = open_or_init_registry (registry_dir)
    on.exit (DBI::dbDisconnect (reg$conn), add = TRUE)

    sess = dataprov::prov_session_start (reg, description = "test session",
                                         parameters = list ())

    tib = data.frame (x = 1:3)
    params = list (gurobi_problem_filter = "completed",
                  exclude_imperfect_wraps = FALSE)

    recs = write_a_tib_with_provenance (sess, tib, "both_tib", data_dir,
                                        params, file_type = "both")

    expect_setequal (names (recs), c ("csv", "rds"))
    expect_true (file.exists (recs [["csv"]]$filepath))
    expect_true (file.exists (recs [["rds"]]$filepath))
    expect_identical (recs [["csv"]]$status, "finalized")
    expect_identical (recs [["rds"]]$status, "finalized")

    dataprov::prov_session_close (sess)
    })

test_that ("write_a_tib_with_provenance() errors on an invalid file_type",
    {
    tmp_root    = withr::local_tempdir (.local_envir = parent.frame ())
    registry_dir = file.path (tmp_root, "dataprov_registry")
    data_dir     = file.path (tmp_root, "data_out")
    dir.create (data_dir, recursive = TRUE)

    reg = open_or_init_registry (registry_dir)
    on.exit (DBI::dbDisconnect (reg$conn), add = TRUE)

    sess = dataprov::prov_session_start (reg, description = "test session",
                                         parameters = list ())

    tib = data.frame (x = 1)
    params = list (gurobi_problem_filter = "all", exclude_imperfect_wraps = FALSE)

    expect_error (
        write_a_tib_with_provenance (sess, tib, "bad_tib", data_dir,
                                     params, file_type = "xlsx"),
        "Must be 'csv', 'rds', or 'both'"
        )
    })

test_that ("write_a_tib_with_provenance() errors on an invalid gurobi_problem_filter",
    {
    tmp_root    = withr::local_tempdir (.local_envir = parent.frame ())
    registry_dir = file.path (tmp_root, "dataprov_registry")
    data_dir     = file.path (tmp_root, "data_out")
    dir.create (data_dir, recursive = TRUE)

    reg = open_or_init_registry (registry_dir)
    on.exit (DBI::dbDisconnect (reg$conn), add = TRUE)

    sess = dataprov::prov_session_start (reg, description = "test session",
                                         parameters = list ())

    tib = data.frame (x = 1)
    params = list (gurobi_problem_filter = "bogus", exclude_imperfect_wraps = FALSE)

    expect_error (
        write_a_tib_with_provenance (sess, tib, "bad_tib", data_dir,
                                     params, file_type = "csv"),
        "gurobi_problem_filter"
        )
    })
