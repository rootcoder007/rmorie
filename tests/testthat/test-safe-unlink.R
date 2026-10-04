# Recursive deletes refuse what morie did not make (a bad cleanup once removed a checkout).

test_that("protected directories are refused", {
  d <- withr::local_tempdir()
  withr::local_dir(d)
  expect_equal(.morie_unlink_refusal("/"), "it is the filesystem root")
  expect_equal(.morie_unlink_refusal("~"), "it is the home directory")
  expect_equal(.morie_unlink_refusal("."), "it is the working directory or one of its parents")
  expect_equal(.morie_unlink_refusal(dirname(d)), "it is the working directory or one of its parents")
  expect_error(.morie_unlink_owned("."), "refusing to remove")
  expect_true(dir.exists(d))
  sub <- file.path(d, "scratch")
  dir.create(sub)
  expect_null(.morie_unlink_refusal(sub))
  .morie_unlink_owned(sub)
  expect_false(dir.exists(sub))
})

test_that("cache clearing cannot climb out of the cache", {
  withr::local_envvar(MORIE_CACHE_DIR = withr::local_tempdir())
  expect_error(morie_cache_clear("..", confirm = FALSE), "inside the cache")
  expect_error(morie_cache_clear("/etc", confirm = FALSE), "inside the cache")
  expect_error(morie_cache_clear("a/../../b", confirm = FALSE), "inside the cache")
  dir.create(file.path(Sys.getenv("MORIE_CACHE_DIR"), "x"))
  writeLines("1", file.path(Sys.getenv("MORIE_CACHE_DIR"), "x", "f.txt"))
  expect_equal(morie_cache_clear("x", confirm = FALSE), 1L)
})

test_that("overwrite only wipes an earlier DMT_Imaging clone", {
  d <- withr::local_tempdir()
  writeLines("keep", file.path(d, "work.txt"))
  expect_error(morie_entheo_clone_dmt_imaging(root = d, overwrite = TRUE), "not a clone of DMT_Imaging")
  expect_true(file.exists(file.path(d, "work.txt")))
})
