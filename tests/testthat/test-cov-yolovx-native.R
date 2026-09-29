# YOLOX (Ge et al. 2021): decoupled-head parameter counts, anchor-free
# ltrb encoding, IoU, center sampling and SimOTA dynamic top-k.

test_that("decoupled head counts the 1x1 reduction and two 3x3 branches", {
  h <- morie_yolovx_decoupled_head(512, 128, 20)
  expect_identical(h$reduce_params, 512L * 128L)
  expect_identical(h$cls_params, 128L * 128L * 9L + 128L * 20L)
  expect_identical(h$reg_params, 128L * 128L * 9L + 128L * 5L)
  expect_identical(h$total, h$reduce_params + h$cls_params + h$reg_params)
  expect_identical(h$coupled_total, 512L * 25L * 9L)
  expect_error(morie_yolovx_decoupled_head(0), "positive")
})

test_that("encode/decode are ltrb distances in stride units and invert", {
  box <- c(10, 20, 70, 50)
  e <- morie_yolovx_encode_box(box, 3, 2, stride = 8)
  px <- 3.5 * 8
  py <- 2.5 * 8
  expect_equal(e$ltrb, c(px - 10, py - 20, 70 - px, 50 - py) / 8, tolerance = 1e-12)
  expect_equal(morie_yolovx_decode_box(e$ltrb, 3, 2, stride = 8), box, tolerance = 1e-12)
  expect_error(morie_yolovx_encode_box(box, 0, 0, stride = 8), "outside the box")
  expect_error(morie_yolovx_encode_box(box, 3, 2, stride = 0), "stride")
  expect_error(morie_yolovx_decode_box(c(-1, 0, 1, 1), 0, 0), "negative")
})

test_that("box IoU is intersection over union", {
  a <- c(0, 0, 4, 3)
  b <- c(2, 1, 6, 5)
  expect_equal(morie_yolovx_box_iou(a, b), (2 * 2) / (12 + 16 - 4), tolerance = 1e-12)
  expect_identical(morie_yolovx_box_iou(a, c(10, 10, 11, 11)), 0)
  expect_identical(morie_yolovx_box_iou(c(1, 1, 1, 1), c(1, 1, 1, 1)), 0)
})

test_that("center sampling unions in-box and center-radius locations", {
  box <- c(2, 1, 9, 4)
  r <- morie_yolovx_center_sampling(box, 10, 6, stride = 1, radius = 1.5)
  g <- expand.grid(i = 0:9, j = 0:5)
  px <- g$i + 0.5
  py <- g$j + 0.5
  inb <- px >= 2 & px <= 9 & py >= 1 & py <= 4
  ctr <- abs(px - 5.5) <= 1.5 & abs(py - 2.5) <= 1.5
  expect_identical(length(r$in_box), sum(inb))
  expect_identical(length(r$in_center), sum(ctr))
  keep <- g[inb | ctr, ]
  keep <- keep[order(keep$i, keep$j), ]
  expect_identical(r$n_candidates, nrow(keep))
  expect_equal(do.call(rbind, r$candidates), unname(as.matrix(keep)))
})

test_that("SimOTA takes k = round(sum of top-q IoUs) cheapest predictions", {
  C <- rbind(c(1.0, 0.2, 3.0, 0.5, 2.0),
             c(0.3, 0.1, 0.4, 5.0, 0.2))
  I <- rbind(c(0.1, 0.8, 0.0, 0.6, 0.3),
             c(0.7, 0.9, 0.5, 0.0, 0.2))
  r <- morie_yolovx_simota_assign(C, I, top_q = 3)
  k <- vapply(1:2, function(g) max(1L, as.integer(round(sum(sort(I[g, ], TRUE)[1:3])))),
              integer(1))
  expect_identical(r$dynamic_k, k)
  claims <- lapply(1:2, function(g) sort(order(C[g, ])[seq_len(k[g])]))
  owner <- vapply(1:5, function(p) {
    who <- which(vapply(claims, function(cl) p %in% cl, logical(1)))
    if (length(who) == 0) NA_integer_ else who[which.min(C[who, p])]
  }, integer(1))
  for (g in 1:2) {
    expect_identical(r$assignment[[as.character(g - 1)]], which(owner == g) - 1L)
  }
  expect_identical(r$n_positives, sum(!is.na(owner)))
  expect_identical(r$contested, sum(table(unlist(claims)) > 1))
  expect_identical(morie_yolovx_simota_assign(C, I, top_q = 3, max_k = 1)$dynamic_k, c(1L, 1L))
  expect_identical(morie_yolovx(C, I, top_q = 3)$assignment, r$assignment)
  expect_error(morie_yolovx_simota_assign(C, I[, 1:4]), "differ in shape")
  expect_match(morie_yolovx_cheatsheet(), "SIMOTA", fixed = TRUE)
})
