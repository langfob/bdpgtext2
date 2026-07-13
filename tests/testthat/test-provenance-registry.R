#===============================================================================
#
#                       test-provenance-registry.R
#
#===============================================================================

#  Tests for open_or_init_registry() in R/provenance_helpers.R.

test_that ("open_or_init_registry() creates a registry when the dir is absent",
    {
    registry_dir = file.path (withr::local_tempdir (.local_envir = parent.frame ()),
                              "dataprov_registry")
    expect_false (dir.exists (registry_dir))

    reg = open_or_init_registry (registry_dir)
    on.exit (DBI::dbDisconnect (reg$conn), add = TRUE)

    expect_true (dir.exists (registry_dir))
    expect_s3_class (reg, "dataprov_registry")
    })

test_that ("open_or_init_registry() opens an existing registry on a second call",
    {
    registry_dir = file.path (withr::local_tempdir (.local_envir = parent.frame ()),
                              "dataprov_registry")

    reg1 = open_or_init_registry (registry_dir)
    DBI::dbDisconnect (reg1$conn)

    reg2 = open_or_init_registry (registry_dir)
    on.exit (DBI::dbDisconnect (reg2$conn), add = TRUE)

    expect_s3_class (reg2, "dataprov_registry")
    })
