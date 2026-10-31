# A CKAN datastore answers at most its page cap a request (1,000 rows on
# open.canada.ca), whatever `limit` asks; every reader must page with
# `offset` to the reported total, not stop after one request.
fake_datastore <- function(total = 2500L, cap = 1000L, seen = NULL) {
  function(url, query = NULL, ...) {
    off <- as.integer(query$offset %||% 0L)
    lim <- min(as.integer(query$limit %||% 100L), cap)
    if (!is.null(seen)) seen$offsets <- c(seen$offsets, off)
    n <- max(0L, min(lim, total - off))
    recs <- if (n) data.frame(a = seq.int(off + 1L, length.out = n)) else data.frame()
    list(success = TRUE, result = list(records = recs, total = total, limit = cap))
  }
}

test_that("the shared pager reads past a server page cap to the total", {
  seen <- new.env()
  local_mocked_bindings(.morie_dataset_http_json = fake_datastore(seen = seen))
  df <- .morie_ckan_datastore_all("https://x.example/datastore_search", list(resource_id = "r"), limit = Inf)
  expect_equal(nrow(df), 2500L)
  expect_equal(df$a, seq_len(2500L))
  expect_equal(seen$offsets, c(0L, 1000L, 2000L))
})

test_that("the shared pager stops at limit and on a refused query", {
  local_mocked_bindings(.morie_dataset_http_json = fake_datastore())
  expect_equal(nrow(.morie_ckan_datastore_all("u", list(), limit = 1500L)), 1500L)
  local_mocked_bindings(.morie_dataset_http_json = function(...) list(success = FALSE))
  expect_error(.morie_ckan_datastore_all("u", list(), what = "Toronto"), "Toronto CKAN datastore_search failed")
})

test_that("without a total, a page shorter than the server's cap ends the read", {
  local_mocked_bindings(.morie_dataset_http_json = function(url, query = NULL, ...) {
    off <- as.integer(query$offset)
    n <- max(0L, min(1000L, 1500L - off))
    list(success = TRUE, result = list(records = if (n) data.frame(a = seq_len(n)) else data.frame(), limit = 1000L))
  })
  expect_equal(nrow(.morie_ckan_datastore_all("u", list(), limit = 1e6)), 1500L)
})

test_that("Toronto, Montreal and CPADS readers return every row past the cap", {
  local_mocked_bindings(.morie_dataset_http_json = fake_datastore())
  expect_equal(nrow(morie_datasets_toronto_open_ckan_resource("rid", limit = 2500L)), 2500L)
  expect_equal(nrow(morie_datasets_montreal_ckan_resource("rid", limit = 2500L)), 2500L)
  expect_equal(nrow(.morie_to_ckan_dump_csv("rid")), 2500L)
  raw <- .morie_ckan_datastore_all(
    "https://open.canada.ca/data/api/3/action/datastore_search",
    query = list(resource_id = "d2639429-c304-45a6-90b3-770562f4d46d"), limit = 2500L
  )
  expect_equal(nrow(raw), 2500L)
})

test_that("the ARSAU sidecar fetch pages to the total", {
  local_mocked_bindings(.morie_dataset_http_text = function(url, ...) {
    off <- if (grepl("&offset=", url, fixed = TRUE)) as.integer(sub(".*&offset=", "", url)) else 0L
    n <- max(0L, min(1000L, 2500L - off))
    recs <- lapply(seq_len(n), function(i) list(a = off + i))
    .morie_to_json(list(result = list(fields = list(list(id = "a")), records = recs, total = 2500L)),
                   auto_unbox = TRUE)
  })
  out <- morie_arsau_fetch_sidecar(ARSAU_KINDS()[1], ARSAU_YEARS()[1], limit = 5000L)
  expect_length(out$records, 2500L)
  expect_equal(out$records[[2500L]]$a, 2500L)
})
