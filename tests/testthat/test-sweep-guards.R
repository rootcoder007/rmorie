# Guards found by the degenerate-input sweep: inputs that used to kill the
# R process (kernlab segfault, install.packages trap) are R errors now.

test_that("kernlab extenders refuse inputs that would crash kernlab", {
  expect_error(morie_kernel_pca(list()), "numeric matrix")
  expect_error(morie_spectral_cluster(list(), centers = 2), "numeric matrix")
  expect_error(morie_kernel_pca(matrix(numeric(0), 0, 2)), "at least two rows")
  expect_error(morie_kernel_pca(matrix(c(1, NA, 3, 4), 2)), "missing or non-finite")
  expect_error(morie_spectral_cluster(data.frame(a = c("x", "y")), centers = 1),
               "numeric matrix")
})

test_that("the extras installer refuses empty package names", {
  expect_error(morie_install_extras(""), "names no package")
  expect_error(morie_install_extras(c(NA_character_, " ")), "names no package")
})
