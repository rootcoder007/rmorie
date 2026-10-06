# The SIU core is vendored from rmoriebricklayer, where 25 KB of whitespace
# through the regex passes overflowed the C stack and killed R (its 0.5.7
# review). The passes over a whole document are loops now and every line
# the extractors see is capped; this is the same guard the canonical
# package runs, so the copy here cannot drift back.

test_that("hostile text through every SIU core binding returns", {
  for (s in list(strrep(" ", 3e5), strrep("a", 3e5), paste0("<", strrep("a", 3e5)),
                 paste0("&#", strrep("9", 1e5), ";"), strrep("\n", 3e5), strrep("<p>x", 1e5),
                 paste0("<script>", strrep("x", 3e5)), strrep(" ", 2e5))) {
    expect_type(.siu_core_html_to_text(s), "character")
    expect_type(.siu_core_parse_html(s), "character")
    expect_type(.siu_core_resolve_so(s)$reason, "character")
    expect_type(.siu_core_strip_boilerplate(s), "character")
  }
  expect_error(.siu_core_to_iso_date(strrep("x", 5000)), "longer than 4096 bytes")
  expect_error(.siu_core_html_to_text(strrep("a", 3e6)), "larger than 2 MiB")
  # lines are capped before any extractor's regex runs
  out <- .siu_core_html_to_text(paste0("<p>", paste(rep("word", 1500), collapse = " "), "</p>"))
  expect_true(all(nchar(strsplit(out, "\n", fixed = TRUE)[[1]]) <= 4000L))
})

test_that("an absurd tag number is noise, not a crash (std::stoi past INT_MAX aborted the process)", {
  txt <- paste0("Subject Officials\nSO\n#", strrep("4", 30), "\nCivilian Witnesses\nCW #2\n")
  f <- .siu_core_parse_html(paste0("<pre>", txt, "</pre>"))
  expect_identical(unname(f["number_of_subject_officials"]), "1")
  expect_identical(unname(f["number_of_civilian_witnesses"]), "2")
  expect_type(.siu_core_resolve_so(txt)$reason, "character")
})

test_that("the 0.5.8 reviews' shapes return: form feeds, vertical tabs, boilerplate and glossary runs", {
  for (s in list(paste0("SO #1", strrep("\f", 25000), "x"), paste0("SO #1", strrep("\v", 25000), "x"),
                 paste0("this information may include", strrep("a b\n", 15000), "affected person."),
                 paste0("who, in the opinion of the SIU Director, is not a subject officer ", strrep("a b\n", 12500)))) {
    expect_type(suppressWarnings(.siu_core_parse_html(paste0("<pre>", s, "</pre>"))), "character")
    expect_type(suppressWarnings(.siu_core_resolve_so(s))$reason, "character")
  }
  expect_type(suppressWarnings(.siu_core_parse_html(paste0("<html><body><p>this information may include</p>",
                                                           strrep("<li>a b</li>", 12000), "</body></html>"))), "character")
  expect_lt(system.time(.siu_core_html_to_text(strrep("<script>", 40000)))[["elapsed"]], 5)
  expect_error(.siu_core_to_iso_date(strrep("x", 5000)), "longer than 4096 bytes")
  expect_warning(.siu_core_html_to_text(paste0("<p>", strrep("word ", 1000), "</p>")), "longer than 2000")
})
