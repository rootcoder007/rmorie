# morie.fn -- function file (rootcoder007/morie)
# GroupLens: predict from the people who agreed with you before.
#
# The premise is stated as an architecture, not an algorithm: people who
# agreed in the past will probably agree again, so a system can predict
# how much someone will like an item from the ratings of users who
# correlate with them -- **without any content analysis at all**. That
# is what makes it work on Usenet news, where the items are text nobody
# has modelled.
#
# **The correlation is over co-rated items only.** Two users are
# compared on the items they have *both* rated; everything else is
# silent, not zero. Treating unrated as zero is the standard mistake,
# and ``pearson`` refuses a pair with too little overlap instead of
# returning a confident number computed from two points.
#
# **Prediction is a weighted sum of DEVIATIONS, not of ratings.**
#
# .. math:: \hat r_{ai} = \bar r_a + \frac{\sum_u w_{au}
#           (r_{ui} - \bar r_u)}{\sum_u |w_{au}|},
#
# because users differ in how they use the scale -- one person's 3 is
# another's 5. Averaging raw ratings would import the neighbour's
# generosity along with their opinion, and the anchor constructs exactly
# that case.
#
# **The denominator uses absolute weights**, so a strongly *negative*
# correlate contributes without cancelling the normalisation -- a
# neighbour who reliably disagrees is information.
#
# **Significance weighting is the honest patch.** A correlation of 1.0
# from two co-rated items is not evidence; scaling the weight by
# :math:`\min(n/50, 1)` is the standard remedy, offered here rather than
# left as a footnote.
#
# References
# ----------
# Resnick, P., Iacovou, N., Suchak, M., Bergstrom, P. & Riedl, J.
# (1994) "GroupLens: An Open Architecture for Collaborative Filtering of
# Netnews", *Proceedings of the 1994 ACM Conference on Computer
# Supported Cooperative Work (CSCW '94)*, 175-186,
# doi:10.1145/192844.192905. [PDF supplied by Vee.] The premise that
# people who agreed in the past are likely to agree again and that
# predictions can therefore be made from correlated users' ratings with
# no content analysis; the Pearson correlation computed over co-rated
# items as the neighbour weight; and the prediction as the user's own
# mean plus a weighted average of the neighbours' deviations from their
# means, normalised by the sum of the absolute weights.
#
# Herlocker, J. L., Konstan, J. A., Borchers, A. & Riedl, J. (1999)
# "An Algorithmic Framework for Performing Collaborative Filtering",
# *SIGIR '99*, 230-237, doi:10.1145/312624.312682. Significance
# weighting for small overlaps.
#
# Sarwar, B., Karypis, G., Konstan, J. & Riedl, J. (2001)
# "Item-based Collaborative Filtering Recommendation Algorithms",
# *WWW '01*, 285-295, doi:10.1145/371920.372071. The item-based
# transpose of this.

.ucfR_EPS <- 1e-12

.ucfR_co_rated <- function(ratings_a, ratings_b) {
  A <- as.list(ratings_a)
  B <- as.list(ratings_b)
  common <- sort(intersect(names(A), names(B)))
  a_vals <- as.numeric(A[common])
  b_vals <- as.numeric(B[common])
  list(
    items = common,
    n = length(common),
    a = a_vals,
    b = b_vals,
    note = "an unrated item carries no information; scoring it as 0 would invent a strong opinion"
  )
}

.ucfR_significance_weight <- function(n_common, threshold = 50) {
  n <- as.integer(n_common)
  t <- as.integer(threshold)
  if (t < 1) stop("ucfR: the threshold must be positive")
  min(n / as.numeric(t), 1.0)
}

.ucfR_pearson <- function(ratings_a, ratings_b, min_common = 2,
                          significance = FALSE, threshold = 50) {
  c <- .ucfR_co_rated(ratings_a, ratings_b)
  n <- c$n
  if (n < as.integer(min_common)) {
    stop(sprintf("ucfR: only %d co-rated items, below the minimum of %d -- a correlation here would be noise",
                 n, as.integer(min_common)))
  }
  ma <- sum(c$a) / n
  mb <- sum(c$b) / n
  num <- sum((c$a - ma) * (c$b - mb))
  da <- sqrt(sum((c$a - ma)^2))
  db <- sqrt(sum((c$b - mb)^2))
  if (da <= .ucfR_EPS || db <= .ucfR_EPS) {
    return(list(w = 0.0, n_common = n, degenerate = TRUE,
                note = "one user gave identical ratings throughout, so no correlation is defined"))
  }
  w <- num / (da * db)
  sig <- as.logical(significance)
  if (sig) {
    w <- w * .ucfR_significance_weight(n, threshold)
  }
  list(w = w, n_common = n, degenerate = FALSE,
       significance_applied = sig)
}

.ucfR_neighbours <- function(target, others, min_common = 2,
                             top_k = NULL, significance = FALSE) {
  out <- list()
  if (length(others) > 0) {
    for (uid in names(others)) {
      r <- others[[uid]]
      p <- tryCatch(.ucfR_pearson(target, r, min_common, significance),
                    error = function(e) NULL)
      if (is.null(p)) next
      if (isTRUE(p$degenerate)) next
      out[[length(out) + 1]] <- list(user = uid, w = p$w,
                                     n_common = p$n_common)
    }
  }
  if (length(out) > 0) {
    abs_w <- vapply(out, function(d) abs(d$w), numeric(1))
    out <- out[order(-abs_w)]
  }
  if (!is.null(top_k)) {
    k <- as.integer(top_k)
    if (k > 0 && length(out) > 0) {
      k <- min(k, length(out))
      out <- out[seq_len(k)]
    }
  }
  list(neighbours = out, n = length(out),
       note = "ranked by |w|: a reliable DISAGREER is information too")
}

.ucfR_predict_rating <- function(target, others, item, min_common = 2,
                                 top_k = NULL, significance = FALSE) {
  item <- as.character(item)
  A <- as.list(target)
  if (length(A) == 0) {
    stop("ucfR: the target user has rated nothing, so there is no mean to anchor on")
  }
  mean_a <- sum(as.numeric(unlist(A))) / length(A)
  nb <- .ucfR_neighbours(target, others, min_common, top_k,
                         significance)$neighbours
  num <- 0.0
  den <- 0.0
  used <- 0L
  naive_num <- 0.0
  naive_den <- 0.0
  for (d in nb) {
    uid <- d$user
    R <- as.list(others[[uid]])
    if (!(item %in% names(R))) next
    mu <- sum(as.numeric(unlist(R))) / length(R)
    num <- num + d$w * (as.numeric(R[[item]]) - mu)
    den <- den + abs(d$w)
    naive_num <- naive_num + d$w * as.numeric(R[[item]])
    naive_den <- naive_den + d$w
    used <- used + 1L
  }
  if (used == 0L || den <= .ucfR_EPS) {
    return(list(
      estimate = mean_a,
      prediction = mean_a,
      n_neighbours = 0L,
      fell_back = TRUE,
      method = "user-based collaborative filtering; Resnick et al. (1994)",
      note = "nobody comparable rated this item, so the user's own mean is the honest answer"
    ))
  }
  est <- mean_a + num / den
  naive_mean <- if (abs(naive_den) > .ucfR_EPS) naive_num / naive_den else NULL
  list(
    estimate = est,
    prediction = est,
    naive_weighted_mean = naive_mean,
    user_mean = mean_a,
    n_neighbours = used,
    fell_back = FALSE,
    method = "user-based collaborative filtering; Resnick et al. (1994)",
    note = "deviations from each neighbour's own mean, normalised by the sum of ABSOLUTE weights"
  )
}

.ucfR_cheatsheet <- function() {
  "ucfR: people who agreed before will probably agree again -- so predict from correlated users, with NO content analysis, which is why it worked on Usenet news. Correlate over CO-RATED items only; unrated is silent, not zero. Predict the user's own mean plus a weighted average of neighbours' DEVIATIONS from their means, since one person's 3 is another's 5 -- averaging raw ratings imports the neighbour's generosity. Normalise by the sum of ABSOLUTE weights, so a reliable disagreer still counts. A correlation of 1.0 from two co-rated items is not evidence: scale by min(n/50, 1)."
}

# Compact alias per ledger/NAMING.md
morie_ucfR <- list(
  co_rated = .ucfR_co_rated,
  significance_weight = .ucfR_significance_weight,
  pearson = .ucfR_pearson,
  neighbours = .ucfR_neighbours,
  predict_rating = .ucfR_predict_rating,
  cheatsheet = .ucfR_cheatsheet,
  user_based_cf = .ucfR_predict_rating,
  user_cf = .ucfR_predict_rating,
  usercf = .ucfR_predict_rating
)
