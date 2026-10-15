# R/llm_config.R, the lines PR #77 added that no other test reached: the settings file under the home
# directory when XDG_CONFIG_HOME is unset, a settings file that is not a JSON object, and a setting
# with no environment variable of its own. The home directory is a temporary one.

.lc <- function(name) get(name, envir = environment(morie_llm_config))

test_that("without XDG_CONFIG_HOME the settings file is ~/.config/morie/llm.json", {
  home <- withr::local_tempdir()
  withr::local_envvar(XDG_CONFIG_HOME = NA, HOME = home, R_USER = home)
  p <- .lc(".morie_llm_config_path")()
  expect_identical(p, file.path(path.expand("~"), ".config", "morie", "llm.json"))
  expect_identical(normalizePath(dirname(dirname(dirname(p))), winslash = "/"), normalizePath(home, winslash = "/"))
  withr::local_envvar(XDG_CONFIG_HOME = "  ")
  expect_identical(.lc(".morie_llm_config_path")(), p)   # blank is unset
})

test_that("a settings file that is not a JSON object reads as no settings", {
  dir <- withr::local_tempdir()
  withr::local_envvar(XDG_CONFIG_HOME = dir)
  rd <- .lc(".morie_llm_read_config")
  p <- .lc(".morie_llm_config_path")()
  dir.create(dirname(p), recursive = TRUE)
  for (txt in c("[1, 2, 3]", "\"just a string\"", "{not json")) {
    writeLines(txt, p)
    expect_identical(rd(), list(), info = txt)
  }
  writeLines("{\"route\": \"groq\"}", p)
  expect_identical(rd(), list(route = "groq"))
})

test_that("a setting without an environment variable of its own is read from the file", {
  dir <- withr::local_tempdir()
  withr::local_envvar(XDG_CONFIG_HOME = dir, OLLAMA_HOST = NA, MORIE_LLM_ROUTE = NA)
  p <- .lc(".morie_llm_config_path")()
  dir.create(dirname(p), recursive = TRUE)
  writeLines("{\"groq.model\": \" llama-x \", \"route\": \"groq\"}", p)
  st <- .lc(".morie_llm_setting")
  expect_identical(st("groq.model"), "llama-x")
  expect_null(st("mistral.model"))
  # an older spelling in `also` still wins over the file
  withr::local_envvar(MY_OLD_GROQ_MODEL = "older")
  expect_identical(st("groq.model", also = "MY_OLD_GROQ_MODEL"), "older")
})
