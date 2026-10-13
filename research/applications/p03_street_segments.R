# P3, interference and displacement on the street-segment network the ledger names: Toronto's
# Centreline (road segments, joined where they share an intersection) inside Toronto Police
# Service Division 14, with the division's 2024 break-and-enters snapped to the nearest segment.
# The hot set is the repeat segments (two or more incidents); its conductance says how much of a hot-spot
# treatment's contiguity spillover stays on hot streets and how much lands next door
# (Research.P3.cheeger_easy checked), and the exposure probabilities under a completely randomised
# allocation of as many treated segments are the design condition a Horvitz-Thompson spillover
# estimator needs (Research.P3).
source(file.path(Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications")), "00_common.R"))

cl <- utils::read.csv(need_file(dpath("OT", "centreline-4326.csv")), stringsAsFactors = FALSE)[
  c("CENTRELINE_ID", "FEATURE_CODE_DESC", "FROM_INTERSECTION_ID", "TO_INTERSECTION_ID", "geometry")]
roads <- c("Local", "Collector", "Minor Arterial", "Major Arterial")
cl <- cl[cl$FEATURE_CODE_DESC %in% roads, ]
# the segment's midpoint: the mean of its vertices
mid <- t(vapply(cl$geometry, function(g) {
  v <- as.numeric(regmatches(g, gregexpr("-?[0-9]+\\.[0-9]+", g))[[1]])
  c(mean(v[c(TRUE, FALSE)]), mean(v[c(FALSE, TRUE)]))
}, numeric(2), USE.NAMES = FALSE))
cl$lon <- mid[, 1]
cl$lat <- mid[, 2]

be <- utils::read.csv(need_file(dpath("TPS", "tps_break_enter_d14_2024.csv")), stringsAsFactors = FALSE)
be <- be[is.finite(be$lon) & is.finite(be$lat), ]
pad <- 0.002
inside <- cl$lon > min(be$lon) - pad & cl$lon < max(be$lon) + pad & cl$lat > min(be$lat) - pad &
  cl$lat < max(be$lat) + pad
seg <- cl[inside, ]
# kilometres on a local plane
k_lon <- 111.32 * cos(mean(seg$lat) * pi / 180)
k_lat <- 110.57
nearest <- vapply(seq_len(nrow(be)), function(i) {
  d2 <- ((seg$lon - be$lon[i]) * k_lon)^2 + ((seg$lat - be$lat[i]) * k_lat)^2
  w <- which.min(d2)
  if (sqrt(d2[w]) <= 0.15) w else NA_integer_
}, integer(1))
seg$incidents <- tabulate(nearest[!is.na(nearest)], nrow(seg))

# adjacency: segments sharing an intersection
ends <- rbind(data.frame(seg = seq_len(nrow(seg)), node = seg$FROM_INTERSECTION_ID),
              data.frame(seg = seq_len(nrow(seg)), node = seg$TO_INTERSECTION_ID))
ends <- ends[!is.na(ends$node) & ends$node > 0, ]
pairs <- do.call(rbind, lapply(split(ends$seg, ends$node), function(s) {
  s <- unique(s)
  if (length(s) < 2L) NULL else t(utils::combn(s, 2L))
}))
pairs <- unique(t(apply(pairs, 1L, sort)))
n0 <- nrow(seg)
# the largest connected piece (a breadth-first search over the pairs)
nbr <- split(c(pairs[, 2], pairs[, 1]), c(pairs[, 1], pairs[, 2]))
comp <- integer(n0)
cid <- 0L
for (s0 in seq_len(n0)) {
  if (comp[s0]) next
  cid <- cid + 1L
  queue <- s0
  comp[s0] <- cid
  while (length(queue)) {
    v <- queue[1L]
    queue <- queue[-1L]
    nb <- nbr[[as.character(v)]]
    nb <- nb[comp[nb] == 0L]
    comp[nb] <- cid
    queue <- c(queue, nb)
  }
}
main <- which(comp == which.max(tabulate(comp)))
keep <- match(seq_len(n0), main)
seg <- seg[main, ]
pairs <- pairs[!is.na(keep[pairs[, 1]]) & !is.na(keep[pairs[, 2]]), ]
pairs <- cbind(keep[pairs[, 1]], keep[pairs[, 2]])
n <- nrow(seg)
W <- matrix(0, n, n)
W[pairs] <- 1
W[pairs[, 2:1]] <- 1
deg <- rowSums(W)

# repeat locations: a segment with two or more break-and-enters in the year (with 513 incidents
# over 3,000 segments a "top 5%" cut falls at one incident, and its ties would take in every
# segment that had any)
S <- seg$incidents >= 2
cb <- morie_cheeger_bound(W, S)
colnames(pairs) <- c("i", "j")
ex <- morie_spillover_exposure_probs(n = n, edges = pairs, n_treated = sum(S), n_draws = 2000L)
ep <- if (is.list(ex)) ex$marginal else ex
hot_nbr_share <- mean(rowSums(W[S, !S, drop = FALSE]) / deg[S])
snapped <- sum(!is.na(nearest))

lines <- c(
  "## P3 interference on the street-segment network: Toronto Police Service Division 14", "",
  sprintf(paste("Data: City of Toronto Centreline (road segments: %s), joined where two segments share an",
                "intersection, inside the extent of Division 14's 2024 break-and-enters; the largest connected",
                "piece has %s segments and %s adjacencies (degree %d to %d, mean %s). Of %d located",
                "incidents, %d lie within 150 m of a segment midpoint and are counted on the nearest."),
          paste(roads, collapse = ", "), format(n, big.mark = ","), format(nrow(pairs), big.mark = ","),
          min(deg), max(deg), fmt(mean(deg), 3), nrow(be), snapped), "",
  sprintf(paste("The hot set S is the repeat segments, those with two or more incidents in the year: %d of",
                "the %s segments (%.1f%%), holding %d of the %d snapped incidents (%.0f%%); %d segments had",
                "exactly one."), sum(S), format(n, big.mark = ","), 100 * sum(S) / n, sum(seg$incidents[S]),
          snapped, 100 * sum(seg$incidents[S]) / snapped, sum(seg$incidents == 1)), "",
  md_table(data.frame(quantity = c("edges cut", "volume of S", "volume of the complement", "conductance h(S)",
                                   "Rayleigh quotient of the test vector", "lambda_2 of the normalised Laplacian",
                                   "lambda_2 <= Rayleigh <= 2 h(S) holds"),
                      value = c(fmt(cb$cut), fmt(cb$vol_S), fmt(cb$vol_complement), fmt(cb$conductance, 4),
                                fmt(cb$rayleigh_test, 4), fmt(cb$lambda2, 4), as.character(cb$bound_holds)),
                      stringsAsFactors = FALSE)), "",
  sprintf(paste("Reading: the conductance %s is the share of the hot segments' adjacencies that lead off the",
                "hot set; on average %.0f%% of a hot segment's neighbouring segments are not hot. A patrol or",
                "crackdown on the hot segments therefore puts most of any displacement or diffusion on streets",
                "that were not treated, and a comparison of treated hot segments with their neighbours compares",
                "treated streets with spillover-exposed ones. lambda_2 = %s is the spectral gap of the",
                "division's street graph (small: long chains of local streets), checked against the proved",
                "easy direction lambda_2 <= 2 h(S)."),
          fmt(cb$conductance, 3), 100 * hot_nbr_share, fmt(cb$lambda2, 4)), "",
  sprintf("### Exposure probabilities, %d treated segments allocated at random (2,000 draws)", sum(S)), "",
  md_table(data.frame(class = c("untreated, no treated neighbour", "untreated, a treated neighbour", "treated"),
                      smallest_probability = c(min(ep[, "0"]), min(ep[, "1"]), min(ep[, "2"])),
                      mean_probability = c(mean(ep[, "0"]), mean(ep[, "1"]), mean(ep[, "2"])),
                      largest_probability = c(max(ep[, "0"]), max(ep[, "1"]), max(ep[, "2"])),
                      stringsAsFactors = FALSE), 4), "",
  sprintf(paste("Every segment needs a positive probability of every class for the Horvitz-Thompson spillover",
                "estimator to exist; %d segments drew no 'untreated with a treated neighbour' allocation in",
                "2,000 draws (the smallest probabilities set the variance). Snapping to the nearest midpoint",
                "is an approximation for long segments; the hot set was not randomised, so no effect is",
                "estimated here."), sum(ep[, "1"] == 0)), "",
  sprintf("Theorems: %s.", paste(unique(c(cb$theorems %||% "Research.P3.cheeger_easy")), collapse = ", ")))
write_result("p03_street_segments_d14", lines)
