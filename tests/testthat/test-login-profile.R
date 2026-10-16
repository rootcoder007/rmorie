# morie_llm_login() hands the terms and who-is-asking arguments to rmoriebricklayer, which asks for
# whatever is not given before a new key is issued.
test_that("morie_llm_login passes the terms and profile arguments through", {
  seen <- NULL
  testthat::local_mocked_bindings(
    bricklayer_llm_login = function(...) {
      seen <<- list(...)
      invisible("sk-k")
    },
    .package = "rmoriebricklayer")
  expect_identical(morie_llm_login(open_browser = FALSE, accept_terms = TRUE, name = "Ada Lovelace",
                                   purpose = "Course project on court delays in Ontario.", restricted = FALSE),
                   "sk-k")
  expect_true(seen$accept_terms)
  expect_identical(seen[c("name", "purpose")],
                   list(name = "Ada Lovelace", purpose = "Course project on court delays in Ontario."))
  expect_false(seen$restricted)
})
