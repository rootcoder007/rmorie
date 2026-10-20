# morie_llm_login() hands the terms and who-is-asking arguments to rmoriebricklayer, which asks for
# whatever is not given before a new key is issued.
test_that("morie_llm_login passes the terms and profile arguments through", {
  seen <- NULL
  takes <- names(formals(rmoriebricklayer::dlogin))
  testthat::local_mocked_bindings(
    dlogin = function(...) {
      seen <<- list(...)
      # only names the installed rmoriebricklayer::dlogin() takes
      stopifnot(all(names(seen) %in% takes))
      invisible("sk-k")
    },
    .package = "rmoriebricklayer")
  expect_identical(morie_llm_login(open_browser = FALSE, accept_terms = TRUE, name = "Ada Lovelace",
                                   purpose = "Course project on court delays in Ontario.", restricted = FALSE),
                   "sk-k")
  expect_true(seen$accter)
  expect_identical(seen[c("name", "purp")],
                   list(name = "Ada Lovelace", purp = "Course project on court delays in Ontario."))
  expect_false(seen$restr)
})

test_that("morie_llm_login maps every documented long name onto rmoriebricklayer::dlogin()", {
  seen <- NULL
  takes <- names(formals(rmoriebricklayer::dlogin))
  testthat::local_mocked_bindings(
    dlogin = function(...) {
      seen <<- list(...)
      invisible("sk-k")
    },
    .package = "rmoriebricklayer")
  morie_llm_login(open_browser = FALSE, accept_terms = TRUE, affiliation = "U", purpose = "p",
                  restricted = TRUE, supervisor_name = "S", supervisor_role = "chair",
                  supervisor_email = "s@example.org")
  expect_setequal(names(seen), c("token", "email", "code", "openbro", "pollmaxsec", "accter", "affil",
                                 "purp", "restr", "supnm", "suprole", "supemail"))
  expect_true(all(names(seen) %in% takes))
})
