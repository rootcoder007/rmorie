# SPDX-License-Identifier: AGPL-3.0-or-later

test_that(".morie_env_true parses truthy/falsy values", {
  for (v in c("1", "true", "TRUE", "yes", "on", "On")) {
    withr::with_envvar(c(MORIE_TEST_KNOB = v), {
      expect_true(.morie_env_true("MORIE_TEST_KNOB"))
    })
  }
  for (v in c("0", "", "no", "off", "bogus")) {
    withr::with_envvar(c(MORIE_TEST_KNOB = v), {
      expect_false(.morie_env_true("MORIE_TEST_KNOB"))
    })
  }
})

test_that("MORIE_NO_EXEC kill-switch gates exec + rds", {
  withr::with_envvar(c(MORIE_NO_EXEC = "1"), {
    expect_true(.morie_exec_disabled())
    expect_error(.morie_ensure_exec_allowed("x"), "MORIE_NO_EXEC")
    tmp <- tempfile(fileext = ".rds"); saveRDS(1:3, tmp)
    expect_error(.morie_safe_readRDS(tmp), "MORIE_NO_EXEC")
  })
  withr::with_envvar(c(MORIE_NO_EXEC = ""), {
    expect_false(.morie_exec_disabled())
    expect_true(.morie_ensure_exec_allowed("x"))
  })
})

test_that(".morie_safe_readRDS round-trips when execution is allowed", {
  withr::with_envvar(c(MORIE_NO_EXEC = ""), {
    tmp <- tempfile(fileext = ".rds")
    saveRDS(list(a = 1, b = "z"), tmp)
    expect_equal(.morie_safe_readRDS(tmp), list(a = 1, b = "z"))
  })
})

test_that(".morie_valid_git_ref accepts refs, rejects injection", {
  expect_true(.morie_valid_git_ref("main"))
  expect_true(.morie_valid_git_ref("release/v1.1.1"))
  expect_true(.morie_valid_git_ref("feature_x-2"))
  expect_false(.morie_valid_git_ref("--upload-pack=touch /tmp/x"))
  expect_false(.morie_valid_git_ref("a b"))
  expect_false(.morie_valid_git_ref("a;rm -rf /"))
  expect_false(.morie_valid_git_ref(c("a", "b")))  # length > 1
})

test_that(".morie_knob_status reports MORIE_NO_EXEC state", {
  withr::with_envvar(c(MORIE_NO_EXEC = ""), {
    st <- .morie_knob_status()
    expect_true("MORIE_NO_EXEC" %in% st$name)
    expect_false(st$enabled[st$name == "MORIE_NO_EXEC"])
    expect_setequal(names(st), c("name", "enabled", "detail"))
  })
})

test_that("user-facing rds loaders are unreachable under MORIE_NO_EXEC", {
  tmp <- tempfile(fileext = ".rds"); saveRDS(mtcars, tmp)
  withr::with_envvar(c(MORIE_NO_EXEC = "1"), {
    expect_error(morie_ml_load(tmp), "MORIE_NO_EXEC")
    expect_error(morie_cache_file(tmp, "t"), "MORIE_NO_EXEC")
  })
})
