siu_fixture <- function() {
  pkg <- environmentName(environment(morie_siu_parse_report))
  paste(readLines(system.file("extdata", "siu_synthetic_report.html", package = pkg), warn = FALSE),
        collapse = "\n")
}

test_that("native core parses the synthetic report", {
  f <- morie_siu_parse_report(siu_fixture(), engine = "native")
  expect_equal(f[["police_service"]], "Barrie Police Service")
  expect_equal(f[["date_of_incident_iso"]], "2023-01-05")
  expect_equal(f[["date_siu_notified_iso"]], "2023-01-06")
  expect_equal(f[["number_of_subject_officials"]], "2")
  expect_equal(nrow(morie_siu_schema(engine = "native")), 16L)
  expect_equal(sum(morie_siu_schema(engine = "native")$is_count), 5L)
  expect_equal(morie_siu_to_iso_date(c("January 5, 2023", "junk", NA), engine = "native"),
               c("2023-01-05", "", NA))
  many <- morie_siu_parse_reports(list(siu_fixture(), siu_fixture()), engine = "native")
  expect_equal(nrow(many), 2L)
})

test_that("native parser handles live-page format (CRLF, signature, headlines, French)", {
  live <- paste0("<p>The Investigation</p>\r\n<p>Public Reports</p>\r\n<p>Director's Resource Committee</p>\r\n",
                 "<p>Brampton Collision Between Police</p>\r\n<p>The Peel Regional Police notified the SIU.</p>\r\n",
                 "<p>Electronically approved by</p>\r\n\r\n<p>Jane Q. Doe</p>\r\n\r\n<p>Director</p>\r\n")
  f <- morie_siu_parse_report(live, engine = "native")
  expect_equal(f[["directors_name"]], "Jane Q. Doe")
  expect_equal(f[["police_service"]], "Peel Regional Police")
  expect_false(grepl("\r", morie_siu_html_to_text(live, engine = "native"), fixed = TRUE))
  interim <- "<p>Original signed by</p><p>Jane Q. Doe<br>Interim Director<br>Special Investigations Unit</p>"
  expect_equal(morie_siu_parse_report(interim, engine = "native")[["directors_name"]], "Jane Q. Doe")
  fr <- "<p>Approuv&eacute; &eacute;lectroniquement par</p><p>Jane Q. Doe</p><p>Directrice</p>"
  expect_equal(morie_siu_parse_report(fr, engine = "native")[["directors_name"]], "Jane Q. Doe")
})

test_that("native and bricklayer engines agree", {
  skip_if_not(requireNamespace("rmoriebricklayer", quietly = TRUE) &&
                exists("bricklayer_parse_siu", envir = asNamespace("rmoriebricklayer")))
  h <- siu_fixture()
  expect_equal(morie_siu_parse_report(h, engine = "native"), morie_siu_parse_report(h, engine = "bricklayer"))
  expect_equal(morie_siu_html_to_text(h, engine = "native"), morie_siu_html_to_text(h, engine = "bricklayer"))
  expect_equal(morie_siu_schema(engine = "native")$name, morie_siu_schema(engine = "bricklayer")$name)
  cases <- c(
    "Subject Officials\nSO #1 Interviewed\nSO #2 Declined interview\nWitness Officials\n",
    "The SIU designated no subject officials in this case.",
    "The SO declined an interview. WO #1 said a witness officer is not a subject official.",
    "In the SIU Director's opinion, a witness official is not a subject official.",
    "the two subject officials were interviewed",
    "SO #1 and SO #3 were present")
  for (x in cases) {
    expect_equal(morie_siu_resolve_so(text = x, engine = "native"),
                 morie_siu_resolve_so(text = x, engine = "bricklayer"), info = x)
  }
})

test_that("resolver rules (native core)", {
  r <- function(x) morie_siu_resolve_so(text = x, engine = "native")$count
  expect_equal(r("Subject Officials\nSO #1 Interviewed\nSO #1 Declined interview\n"), 2L)
  expect_equal(r("The SIU designated no subject officials."), 0L)
  expect_equal(r("The SO declined. A witness officer is not a subject official."), 1L)
  expect_equal(r("the three subject officers"), 3L)
  expect_true(is.na(r("Nothing about officers here.")))
  expect_error(morie_siu_resolve_so(), "supply `text`")
})

test_that("bring-your-own backend and the native panel", {
  b <- morie_llm_backend(api = "openai", base = "https://openrouter.ai/api")
  expect_equal(b$base, "https://openrouter.ai/api/v1")
  expect_equal(morie_llm_backend(api = "ollama", base = "gpu-box:11434/")$base, "http://gpu-box:11434")
  expect_error(morie_llm_backend(api = "grpc"))
  seen <- character(0)
  fake <- function(model, prompt) {
    seen <<- c(seen, model)
    if (startsWith(prompt, "Reply with")) return(if (model == "dead") "" else "OK")
    if (grepl("You are the AUDITOR", prompt, fixed = TRUE)) return('{"police_service": "Barrie Police Service"}')
    '{"police_service": {"value": "Barrie", "quote": "Barrie", "confidence": "high"}}'
  }
  expect_equal(morie_llm_chat("hi", model = "m", chat = fake), "{\"police_service\": {\"value\": \"Barrie\", \"quote\": \"Barrie\", \"confidence\": \"high\"}}")
  p1 <- morie_siu_audit_panel("The Barrie Police Service ...", mode = 1L, readers = c("dead", "r1"), chat = fake)
  expect_equal(p1$fields$police_service, "Barrie")
  seen <- character(0)
  p4 <- morie_siu_audit_panel("The Barrie Police Service ...", mode = 4L, readers = c("r1", "r2"),
                              auditors = c("a1", "a2"), chat = fake)
  expect_equal(p4$fields$police_service, "Barrie Police Service")
  expect_equal(sum(seen %in% c("r1", "r2")), 3L + 2L)  # 3 reads + 2 health checks
  expect_equal(sum(seen %in% c("a1", "a2")), 2L + 2L)  # 2 audits + 2 health checks
  expect_error(morie_siu_audit_panel("x", readers = "dead", chat = fake), "healthy")
})

test_that("siu command line front end", {
  expect_output(st <- morie_siu_cli("version"), "siu")
  expect_equal(st, 0L)
  html <- tempfile(fileext = ".html")
  txt <- tempfile(fileext = ".txt")
  writeLines(siu_fixture(), html)
  writeLines("Subject Officials\nSO #1 Interviewed\n", txt)
  expect_output(morie_siu_cli(c("siu", "parse", html)), "Barrie Police Service")
  expect_output(st <- morie_siu_cli(c("resolve", txt)), "subject_officers=1")
  expect_equal(st, 0L)
  expect_message(st <- morie_siu_cli("bogus"), "usage")
  expect_equal(st, 2L)
})

test_that("SO, subject officer and subject official count the same people in both engines", {
  # reports before the SIU Act (2019) say "subject officer", later ones "subject official"
  for (txt in c("Subject Officer #1 declined. Subject Officer #2 was interviewed.",
                "Subject Official #1 declined. Subject Official #2 was interviewed.",
                "SO #1 declined. SO #2 was interviewed.")) {
    for (engine in c("native", "bricklayer")) {
      expect_identical(as.integer(morie_siu_resolve_so(txt, engine = engine)$count), 2L,
                       label = paste(engine, txt))
    }
  }
})
