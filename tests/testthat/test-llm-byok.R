# Language models in rmorie: the one the user brings, through rmoriebricklayer. The model calls
# are replaced by canned replies; nothing leaves the machine.

.prog <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"  # the command is named for the package: morie in the morie mirror

.byok_sandbox <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  ps <- rmoriebricklayer::bricklayer_llm_providers()
  vars <- c(ps$key_env, "MORIE_LLM_BASE_URL", "MORIE_LLM_API_KEY", "MORIE_LLM_MODEL", "MORIE_LLM_ROUTE",
            "MORIE_HOSTED_KEY", "OLLAMA_MODEL", "OLLAMA_API_KEY")
  withr::local_envvar(c(stats::setNames(rep(NA_character_, length(vars)), vars),
                        OLLAMA_HOST = "off", XDG_CONFIG_HOME = dir), .local_envir = env)
  .morie_llm_cache$ollama_cached <- NULL
  withr::defer(.morie_llm_cache$ollama_cached <- NULL, envir = env)
  dir
}

test_that("the data key is written owner-only into the shared XDG file and forgotten at logout", {
  dir <- .byok_sandbox()
  p <- .morie_llm_write_credentials(list(hosted_key = "sk-test"))
  expect_equal(p, file.path(dir, "morie", "credentials.json"))
  if (.Platform$OS.type != "windows") expect_equal(as.character(file.info(p)$mode), "600")
  expect_equal(.morie_llm_hosted_key(), "sk-test")
  suppressMessages(morie_llm_logout())
  expect_null(.morie_llm_hosted_key())
})

test_that("only an https address of a public host is handed to the browser, and server text never carries the key", {
  ok <- function(u) .morie_llm_browsable(u)
  expect_true(ok("https://github.com/login/device"))
  expect_true(ok("https://data.rmorie.com:8443/auth/x"))
  for (u in c("http://github.com/login/device", "https://127.0.0.1/", "https://localhost/", "https://10.0.0.5/x",
              "https://192.168.1.9/", "https://172.20.0.1/", "https://169.254.169.254/latest", "https://100.64.0.1/",
              "https://metadata.google.internal/", "https://box.lan/", "https://metadata/", "file:///etc/passwd",
              "javascript:alert(1)", NA_character_, "")) {
    expect_false(ok(u), info = u)
  }
  expect_false(ok(c("https://a.example.org", "https://b.example.org")))  # one address, not a vector
  r <- .morie_llm_redact
  expect_identical(r("token sk-LEAKLEAK1234 rejected", "sk-LEAKLEAK1234"), "token <key> rejected")
  expect_identical(r("header was Authorization: Bearer abcdefgh12345678 at the gateway"),
                   "header was Authorization: Bearer <key> at the gateway")
  expect_identical(r("Invalid key. Received API key = sk-abc, key hash = xyz"), "Invalid key")
  expect_identical(r("quota exceeded"), "quota exceeded")
  expect_identical(r("the key 'my secret 42' is unknown", "my secret 42"), "the key '<key>' is unknown")
})

test_that("rmorie's route names map onto rmoriebricklayer's; hosted says it is retired", {
  expect_identical(.morie_llm_bl_route("api"), "own")
  expect_identical(.morie_llm_bl_route(provider = "own"), "own")
  expect_null(.morie_llm_bl_route("auto"))
  expect_null(.morie_llm_bl_route("local"))
  expect_null(.morie_llm_bl_route())
  expect_identical(.morie_llm_bl_route("Anthropic"), "anthropic")
  expect_error(.morie_llm_bl_route("hosted"), "hosted MORIE language-model tier is retired")
})

test_that("detect_provider names the route rmoriebricklayer would take, or local", {
  .byok_sandbox()
  expect_identical(morie_llm_detect_provider(), "local")
  expect_false(morie_llm_agent_available())
  withr::local_envvar(GROQ_API_KEY = "gsk-x", MORIE_GROQ_MODEL = "llama-x")
  expect_identical(morie_llm_detect_provider(), "groq")
  expect_true(morie_llm_agent_available())
  expect_identical(morie_llm_detect_provider("ollama"), "local")  # off: nothing there
})

test_that("ask hands the question, the context and rmorie's persona to rmoriebricklayer with the packages' help", {
  .byok_sandbox()
  seen <- NULL
  testthat::local_mocked_bindings(
    bricklayer_llm_ask = function(prompt, model = NULL, timeout = 120, system_prompt = NULL, route = NULL,
                                  docs = TRUE, packages = NULL) {
      seen <<- list(prompt = prompt, model = model, route = route, docs = docs, packages = packages,
                    system_prompt = system_prompt)
      "an answer"
    },
    .package = "rmoriebricklayer")
  expect_identical(morie_llm_ask("Which module fits DiD?", context = list(design = "staggered"),
                                 model = "m1", route = "anthropic"), "an answer")
  expect_match(seen$prompt, "^Which module fits DiD\\?\n\nContext:\ndesign: staggered$")
  expect_identical(seen$model, "m1")
  expect_identical(seen$route, "anthropic")
  expect_true(seen$docs)
  expect_true("rmorie" %in% seen$packages)
  expect_match(seen$system_prompt, "MORIE agent")
  morie_llm_ask("q", provider = "api", docs = FALSE, system_prompt = "be brief")
  expect_identical(seen$route, "own")
  expect_false(seen$docs)
  expect_match(seen$system_prompt, "be brief$")
  expect_error(morie_llm_ask(""), "single non-empty string")
  expect_error(morie_llm_ask("q", route = "hosted"), "retired")
})

test_that("with no model set up, or a failure on the automatic route, ask returns the fallback text", {
  .byok_sandbox()
  out <- morie_llm_ask("hello")
  expect_true(isTRUE(attr(out, "fallback")))
  expect_match(out, "without a language model")
  expect_match(out, "ANTHROPIC_API_KEY")
  expect_match(out, paste(.prog, "config setup"), fixed = TRUE)
  testthat::local_mocked_bindings(bricklayer_llm_ask = function(...) stop("Groq rejected your key (HTTP 401)"),
                                  .package = "rmoriebricklayer")
  out <- morie_llm_ask("hello")
  expect_true(isTRUE(attr(out, "fallback")))
  expect_match(out, "No language model answered this time: Groq rejected your key (HTTP 401)", fixed = TRUE)
  # a route the caller named is an error, not the fallback
  expect_error(morie_llm_ask("hello", route = "groq"), "rejected your key")
})

test_that("ask_multi sends the conversation as it is; it falls back like ask", {
  .byok_sandbox()
  seen <- NULL
  testthat::local_mocked_bindings(
    bricklayer_llm_chat = function(messages, model = NULL, route = NULL, timeout = 120) {
      seen <<- list(messages = messages, route = route, model = model)
      "chat reply"
    },
    .package = "rmoriebricklayer")
  msgs <- list(list(role = "user", content = "hi"), list(role = "assistant", content = "hello"),
               list(role = "user", content = "again"))
  expect_identical(morie_llm_ask_multi(msgs, providers = c("local", "api"), model = "m"), "chat reply")
  expect_identical(seen$route, "own")
  expect_identical(seen$messages, msgs)
  morie_llm_ask_multi(msgs)
  expect_null(seen$route)
  testthat::local_mocked_bindings(bricklayer_llm_chat = function(...) stop("No language-model route is set up"),
                                  .package = "rmoriebricklayer")
  out <- morie_llm_ask_multi(msgs)
  expect_true(isTRUE(attr(out, "fallback")))
  testthat::local_mocked_bindings(bricklayer_llm_chat = function(...) stop("Groq answered 500"),
                                  .package = "rmoriebricklayer")
  expect_match(morie_llm_ask_multi(msgs), "Groq answered 500", fixed = TRUE)
  expect_error(morie_llm_ask_multi(msgs, providers = "groq"), "Groq answered 500")
  expect_error(morie_llm_ask_multi("hi"))
})

test_that("login, the settings and your own endpoint are rmoriebricklayer's", {
  .byok_sandbox()
  seen <- NULL
  testthat::local_mocked_bindings(
    bricklayer_llm_login = function(token = NULL, email = NULL, code = NULL, open_browser = FALSE,
                                    poll_max_seconds = 600) {
      seen <<- list(token = token, email = email, code = code)
      invisible("sk-k")
    },
    .package = "rmoriebricklayer")
  expect_identical(morie_llm_login(email = "you@example.org", code = "123456"), "sk-k")
  expect_identical(seen[c("email", "code")], list(email = "you@example.org", code = "123456"))
  # settings: saved through rmoriebricklayer, read back by rmorie's own helpers
  # the sandbox switches Ollama off in the environment, which wins (and saving under it warns so)
  withr::local_envvar(OLLAMA_HOST = NA)
  morie_llm_config(ollama.url = "http://192.168.1.20:11434", ollama.model = "qwen3:8b")
  expect_identical(.morie_llm_setting("ollama.model"), "qwen3:8b")
  expect_identical(.morie_llm_setting("ollama.url"), "http://192.168.1.20:11434")
  withr::local_envvar(OLLAMA_MODEL = "env-model")
  expect_identical(.morie_llm_setting("ollama.model"), "env-model")  # the environment wins
  expect_null(.morie_llm_setting("own.url"))
  morie_llm_provider_set("http://localhost:1234/v1", key = "sk-own-1234", model = "local-m")
  tab <- morie_llm_provider_show()
  expect_identical(tab$value[tab$key == "own.url"], "http://localhost:1234/v1")
  expect_identical(tab$value[tab$key == "own.key"], "set")  # never the key itself
  expect_identical(morie_llm_detect_provider(), "own")
  morie_llm_provider_unset()
  expect_identical(morie_llm_provider_show()$value[1L], "(not set)")
  expect_error(morie_llm_config(hosted.model = "x"), "unknown setting 'hosted.model'")
})

test_that("the shell verbs for models, the data key and the settings are rmoriebricklayer's, named rmorie", {
  .byok_sandbox()
  buf <- character()
  st <- morie_cli("doctor", out = function(s) buf <<- c(buf, s))
  expect_identical(as.integer(st), 0L)
  expect_true(any(grepl("ask has no route yet", buf)))
  expect_true(any(grepl(sprintf("`%s config setup`", .prog), buf, fixed = TRUE)))
  buf <- character()
  morie_cli(c("config", "set", "route", "anthropic"), out = function(s) buf <<- c(buf, s))
  expect_identical(morie_llm_config()$value[morie_llm_config()$key == "route"], "anthropic")
  buf <- character()
  st <- morie_cli(c("ask", "--route", "hosted", "hi"), out = function(s) buf <<- c(buf, s))
  expect_false(identical(as.integer(st), 0L))
  expect_true(any(grepl("retired", buf)))
  buf <- character()
  morie_cli(c("help", "llm"), out = function(s) buf <<- c(buf, s))
  expect_true(any(grepl("bring your own key", buf, ignore.case = TRUE)))
  buf <- character()
  morie_cli(c("provider", "show"), out = function(s) buf <<- c(buf, s))
  expect_true(any(grepl("own.url", buf, fixed = TRUE)))
})

test_that("the verbs rmorie hands to rmoriebricklayer read rmorie's help too, and leave the environment as it was", {
  .byok_sandbox()
  seen <- NULL
  testthat::local_mocked_bindings(
    bricklayer_cli = function(args, out = cat) {
      seen <<- Sys.getenv(c("RMBL_PROG", "RMBL_PACKAGES"))
      0L
    },
    .package = "rmoriebricklayer")
  withr::local_envvar(RMBL_PROG = NA, RMBL_PACKAGES = "before")
  .cli_bricklayer(c("ask", "x"), function(...) NULL)
  expect_identical(unname(seen[["RMBL_PROG"]]), .prog)
  expect_true(.prog %in% strsplit(seen[["RMBL_PACKAGES"]], ",")[[1L]])
  expect_true("rmoriebricklayer" %in% strsplit(seen[["RMBL_PACKAGES"]], ",")[[1L]])
  expect_identical(Sys.getenv("RMBL_PACKAGES"), "before")
  expect_identical(Sys.getenv("RMBL_PROG", unset = "unset"), "unset")
})
