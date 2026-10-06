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
  expect_identical(.siu_core_to_iso_date(strrep("x", 5000)), "")
  expect_error(.siu_core_html_to_text(strrep("a", 3e6)), "larger than 2 MiB")
  # lines are capped before any extractor's regex runs
  out <- .siu_core_html_to_text(paste0("<p>", paste(rep("word", 1500), collapse = " "), "</p>"))
  expect_true(all(nchar(strsplit(out, "\n", fixed = TRUE)[[1]]) <= 4000L))
})
