# Extracted from test-modules.R:54

# test -------------------------------------------------------------------------
skip_heavy()
cat <- morie_dataset_catalog()
expect_true(all(c("download_url", "zip_member") %in% names(cat)))
dl <- cat[nzchar(cat$download_url), ]
expect_true(nrow(dl) > 0)
zips <- dl[grepl("\\.zip$", dl$download_url, ignore.case = TRUE), ]
expect_true(all(nzchar(zips$zip_member)))
