# R/siu.R, the self-serve lines PR #77 added that no other test reached: a scraper cache that does not
# know the case (extraction), and a report page that cannot be fetched (anomaly check). The case is
# resolved through the bundled drid manifest; the page fetch is a recorder, never siu.on.ca.

.su <- function(name) get(name, envir = environment(morie_siu_llm_extract))

.su_cache <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  utils::write.csv(data.frame(case_number = "99-TST-999", drid = 1L, nrid = 5L, stringsAsFactors = FALSE),
                   file.path(dir, "SIU.csv"), row.names = FALSE)
  dir
}

test_that("a scraper cache without the case still gives the case's row from the fetched page", {
  man <- .su(".siu_load_manifest")()
  skip_if(is.null(man), "no bundled SIU drid manifest")
  case <- "17-OVI-201"
  drid <- man$drid[man$case_number == case & man$X_language == "en"][1L]
  expect_identical(.su(".siu_resolve_drid")(case), drid)
  dir <- .su_cache()
  urls <- character()
  testthat::local_mocked_bindings(
    .siu_fetch_http_get = function(url, timeout_s = 60L) {
      urls <<- c(urls, url)
      "<html><p>The   Subject Officer</p></html>"
    },
    .package = "rmorie")
  out <- morie_siu_llm_extract(case, cache_dir = dir, mock_response_text = "{\"police_service\": \"OPP\"}")
  expect_identical(urls, paste0("https://www.siu.on.ca/en/directors_report_details.php?drid=", drid))
  expect_identical(names(out), .su(".siu_field_list")())
  expect_identical(out$case_number, case)
  expect_identical(out$drid, as.character(drid))
  expect_identical(out$nrid, "")   # the cache row of another case lends no nrid
  expect_identical(out$police_service, "OPP")
  expect_identical(out$source_url_report, urls)
})

test_that("an anomaly check whose report page cannot be fetched says there is no HTML", {
  man <- .su(".siu_load_manifest")()
  skip_if(is.null(man), "no bundled SIU drid manifest")
  dir <- .su_cache()
  tried <- 0L
  testthat::local_mocked_bindings(
    .siu_fetch_http_get = function(url, timeout_s = 60L) {
      tried <<- tried + 1L
      stop("could not be reached")
    },
    .package = "rmorie")
  expect_error(morie_siu_anomaly_check("17-OVI-201", cache_dir = dir, mock_response_text = "[]"),
               "No HTML available for '17-OVI-201': not in the bundled manifest and no cache under cache_dir.",
               fixed = TRUE)
  expect_identical(tried, 1L)
})
