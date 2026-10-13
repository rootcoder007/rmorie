# P12 ecological inference, applied to Toronto's 158 neighbourhoods (2021 Census profiles): what
# neighbourhood shares alone say about a group's rate (the Duncan-Davis bounds, Research.P12), checked
# where the census also publishes the answer, then applied where it does not.
source(file.path(Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications")), "00_common.R"))

x <- utils::read.csv(need_file(dpath("OT", "nbhd_2021_census_profile_full_158model.csv")), check.names = FALSE,
                     stringsAsFactors = FALSE)
rowv <- function(r) as.numeric(unlist(x[x$row == r, -(1:2)]))
nb <- names(x)[-(1:2)]
tot <- rowv(168)                       # population in private households, LIM universe
low <- rowv(173)                       # in low income (LIM-AT)
q <- low / tot                         # each neighbourhood's low-income share
check_group <- function(count_row, low_row, label) {
  g <- rowv(count_row)
  p <- g / tot
  b <- morie_ecological_bounds(p, q, weights = tot)
  nbh <- b$neighbourhoods
  truth <- rowv(low_row) / g
  # census counts are randomly rounded to multiples of 5: a truth within that rounding of a bound counts as inside
  slack <- 5 / pmax(g, 1)
  inside <- truth >= nbh$lower - slack & truth <= nbh$upper + slack
  agg_truth <- sum(rowv(low_row)) / sum(g)
  data.frame(group = label, group_share = sum(g) / sum(tot), mean_width = mean(nbh$upper - nbh$lower),
             inside = sum(inside), neighbourhoods = length(inside), city_lower = b$aggregate[["lower"]],
             city_upper = b$aggregate[["upper"]], city_truth = agg_truth, stringsAsFactors = FALSE)
}
val <- rbind(check_group(169, 174, "children 0-17"), check_group(172, 177, "seniors 65+"),
             check_group(171, 176, "adults 18-64"))

vm <- rowv(1642) / rowv(1641)
bv <- morie_ecological_bounds(vm, q, weights = tot)
vn <- bv$neighbourhoods
vtab <- data.frame(neighbourhood = nb, visible_minority_share = vm, low_income_share = q, lower = vn$lower,
                   upper = vn$upper, width = vn$upper - vn$lower, stringsAsFactors = FALSE)
eco_cor <- stats::cor(vm, q)
narrow <- utils::head(vtab[order(vtab$width), ], 8)

lines <- c(
  "## P12 ecological inference: Toronto's neighbourhoods, 2021 Census", "",
  paste("Data: City of Toronto Neighbourhood Profiles, 2021 Census, 158-neighbourhood model (25% sample,",
        "population in private households, low income by the after-tax low-income measure, LIM-AT)."), "",
  paste("Knowing only each neighbourhood's group share p and low-income share q, a group's low-income rate",
        "there lies in [max(0, (p + q - 1)/p), min(1, q/p)], both ends attainable (Research.P12.dd_bounds,",
        "Cells.ends_attained), and the city-wide rate's bounds are the population-weighted means",
        "(dd_aggregate_bounds)."), "",
  "### Checked against the answer the census publishes", "",
  paste("The profiles also publish low income by age, so the bounds computed from the age share and the",
        "overall share alone can be checked against the true rate in every neighbourhood:"), "",
  md_table(val), "",
  sprintf(paste("The true rate lies inside the bounds in %d of %d neighbourhood-group checks (allowing the",
                "census's rounding of counts to multiples of five), as the theorem requires, and the city-wide",
                "truths (%s) lie inside the aggregate intervals. The intervals are wide where the group is",
                "small: children are %.0f%% of residents, so their bounds average %.2f wide."),
          sum(val$inside), sum(val$neighbourhoods), paste(sprintf("%.3f", val$city_truth), collapse = ", "),
          100 * val$group_share[1], val$mean_width[1]), "",
  "### Applied where the census publishes no cross-tabulation: visible minority", "",
  sprintf(paste("Visible minority residents are %.0f%% of the city's private-household population. Their",
                "city-wide low-income rate lies in [%.3f, %.3f] from neighbourhood shares alone; the",
                "neighbourhood-level correlation of visible-minority share and low-income share is %.2f,",
                "which by itself says nothing about whether visible-minority residents are more often low",
                "income than others (Robinson 1950; Research.P12.ecological_ge: the ecological correlation",
                "overstates whenever the within-neighbourhood part is orthogonal). The narrowest",
                "neighbourhood intervals are where the group is nearly everyone:"),
          100 * sum(rowv(1642)) / sum(rowv(1641)), bv$aggregate[["lower"]], bv$aggregate[["upper"]], eco_cor), "",
  md_table(narrow), "",
  "Theorems: Research.P12.pq_ge, Research.P12.pq_le_p, Research.P12.pq_le_q, Research.P12.dd_bounds, Research.P12.Cells.ends_attained, Research.P12.dd_aggregate_bounds, Research.P12.ecological_ge.")
write_result("p12_toronto_ecological", lines)
