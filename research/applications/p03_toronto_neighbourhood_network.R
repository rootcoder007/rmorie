# P3, interference and displacement: Toronto's neighbourhood network as the design object.
#
# The ledger asks for a street-segment application; the public files hold the 158 neighbourhoods
# and their polygons, not the segment network, so the neighbourhood contiguity graph stands in and
# the result is labelled as such. Two things are computed on it. (1) The Cheeger quantities of the
# cut that separates the Neighbourhood Improvement Areas (NIAs) from the rest: the conductance
# h(S), the Rayleigh quotient of the test vector and lambda_2, with the proved easy direction
# lambda_2 <= 2 h(S) checked (`morie_cheeger_bound()`, Research.P3.cheeger_easy). (2) The exposure
# probabilities a completely randomised allocation of |S| treated neighbourhoods would give every
# neighbourhood under the one-ring exposure map (`morie_spillover_exposure_probs()`,
# Research.P3 Horvitz-Thompson theorems): the design condition every Horvitz-Thompson spillover
# estimator needs, before any outcome is looked at.
source(file.path(Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications")), "00_common.R"))
if (!requireNamespace("sf", quietly = TRUE)) stop("the sf package is needed to read the neighbourhood polygons")

f <- need_file(dpath("OT", "Neighbourhoods - 4326.geojson"))
nb <- sf::st_read(f, quiet = TRUE)
nb <- nb[order(as.integer(nb$AREA_SHORT_CODE)), ]
polys <- lapply(sf::st_geometry(nb), function(g) {
  xy <- sf::st_coordinates(g)
  unname(xy[, c("X", "Y"), drop = FALSE])
})
W <- PolygonContiguity(polys, queen = TRUE, snap = 1e-7)
dimnames(W) <- list(nb$AREA_SHORT_CODE, nb$AREA_SHORT_CODE)
deg <- rowSums(W)
S <- grepl("Improvement Area", nb$CLASSIFICATION, fixed = TRUE)

cb <- morie_cheeger_bound(W, S)
edges <- which(W == 1 & upper.tri(W), arr.ind = TRUE)
colnames(edges) <- c("i", "j")
ex <- morie_spillover_exposure_probs(n = nrow(W), edges = edges, n_treated = sum(S), n_draws = 5000L)
ep <- if (is.list(ex)) ex$marginal else ex
ep_df <- data.frame(neighbourhood = nb$AREA_SHORT_CODE, degree = deg, nia = S,
                    p_untreated_no_treated_neighbour = ep[, "0"], p_untreated_treated_neighbour = ep[, "1"],
                    p_treated = ep[, "2"], stringsAsFactors = FALSE)
ep_df$min_class_prob <- pmin(ep_df[[4L]], ep_df[[5L]], ep_df[[6L]])
rarest <- ep_df[order(ep_df$min_class_prob), ][1:10, ]

lines <- c(
  "# P3: the Toronto neighbourhood network as the interference design",
  "",
  sprintf("Source: City of Toronto Neighbourhoods (158 polygons, WGS84) and Neighbourhood Improvement Areas; queen contiguity (%d edges, degrees %d to %d, mean %s).",
          nrow(edges), min(deg), max(deg), fmt(mean(deg), 2)),
  "The ledger names street segments; the public files carry neighbourhoods, so this is the neighbourhood-level",
  "version of the design computation and is labelled as such.",
  "",
  sprintf("## The NIA cut (%d of %d neighbourhoods)", sum(S), nrow(W)),
  "",
  md_table(data.frame(quantity = c("edges cut", "volume of S", "volume of the complement", "conductance h(S)",
                                   "Rayleigh quotient of the test vector", "lambda_2 of the normalised Laplacian",
                                   "lambda_2 <= Rayleigh <= 2 h(S) holds"),
                      value = c(cb$cut, cb$vol_S, cb$vol_complement, cb$conductance, cb$rayleigh_test, cb$lambda2,
                                as.character(cb$bound_holds)), stringsAsFactors = FALSE), 4),
  "",
  sprintf(paste("The conductance is the share of the NIAs' edge volume that leaves the set: %s. %s",
                "lambda_2 = %s is the spectral gap of the whole city graph; the proved chain",
                "lambda_2 <= E(f)/D(f) <= 2 h(S) is checked above, and the gap between %s and %s is the room",
                "the Cheeger hard direction (lambda_2 >= h^2/2, still open in Lean) would close."),
          fmt(cb$conductance, 3),
          if (cb$conductance < 0.25) {
            "That is a cluster: a treatment confined to the NIAs keeps most of its contiguity spillover inside the set."
          } else {
            paste("That is a scatter, not a cluster: about half of every NIA's neighbours are outside the set, so a",
                  "treatment confined to the NIAs puts most of its contiguity spillover on neighbourhoods that are",
                  "not treated, and a comparison of NIAs with their neighbours is a comparison of treated places with",
                  "the places that received the spillover.")
          },
          fmt(cb$lambda2, 4), fmt(cb$lambda2, 4), fmt(2 * cb$conductance, 3)),
  "",
  sprintf("## Exposure probabilities under a completely randomised allocation of %d treated neighbourhoods (5,000 draws)", sum(S)),
  "",
  md_table(data.frame(class = c("untreated, no treated neighbour", "untreated, a treated neighbour", "treated"),
                      smallest_probability = c(min(ep[, "0"]), min(ep[, "1"]), min(ep[, "2"])),
                      largest_probability = c(max(ep[, "0"]), max(ep[, "1"]), max(ep[, "2"])), stringsAsFactors = FALSE), 4),
  "",
  "The ten neighbourhoods with the rarest exposure class (the ones that set the Horvitz-Thompson variance):",
  "",
  md_table(rarest[, c("neighbourhood", "degree", "nia", "p_untreated_no_treated_neighbour", "p_untreated_treated_neighbour", "p_treated")], 4),
  "",
  paste("Every neighbourhood must have a positive probability of each exposure class for the Horvitz-Thompson",
        "estimator of the spillover effect to exist (Research.P3's design condition); the table shows the classes",
        "and the smallest probabilities, which set the variance. The outcomes (break and enter by neighbourhood)",
        "are not used here: the NIAs were not randomised, so an HT estimate would not be one."),
  ""
)
write_result("p03_toronto_neighbourhood_network", lines)
