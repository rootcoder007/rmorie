# P10, near-repeat contagion: what the break-and-enter series commits you to.
#
# Data: Toronto Police Service break-and-enter occurrences (Public Safety Data Portal, one row per
# occurrence with the neighbourhood and the occurrence date). Two things are computed. (1) The
# second moments of weekly neighbourhood counts: the dispersion index var/mean per neighbourhood,
# which a self-exciting (contagion) process and a heterogeneous Poisson mixture both produce
# (Research.P10.Moments: second moments do not separate them; `morie_contagion_moment_witness()`).
# (2) A Hawkes fit of the city-wide daily series (`morie_tps_hawkes_temporal_fit()`), whose branching
# ratio is what the temporal order, not the moments, identifies.
source(file.path(Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications")), "00_common.R"))

f <- need_file(list.files(dpath("TPS", "BreakandEnter", "CSV"), pattern = "\\.csv$", full.names = TRUE)[1L])
bne <- utils::read.csv(f, stringsAsFactors = FALSE)
bne <- bne[!is.na(bne$OCC_YEAR) & bne$OCC_YEAR >= 2014 & bne$OCC_YEAR <= 2024 & bne$HOOD_158 != "NSA", ]
bne$occ <- as.POSIXct(bne$OCC_DATE, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/Toronto")
bne <- bne[!is.na(bne$occ), ]
t0 <- as.POSIXct("2014-01-01", tz = "America/Toronto")
bne$week <- floor(as.numeric(difftime(bne$occ, t0, units = "days")) / 7)
n_weeks <- max(bne$week) + 1L

hoods <- sort(unique(bne$HOOD_158))
weekly <- table(factor(bne$HOOD_158, levels = hoods), factor(bne$week, levels = 0:(n_weeks - 1L)))
m <- rowMeans(weekly)
v <- apply(weekly, 1L, stats::var)
disp <- v / m
nb_size <- ifelse(v > m, m^2 / (v - m), Inf)   # the mixed-Poisson (gamma) alternative with the same two moments

witness <- morie_contagion_moment_witness()
# one Hawkes fit per neighbourhood, on the five with the most occurrences and their full
# 2014-2024 series (no subsampling: the near-repeat signal lives in the short gaps)
top <- names(sort(table(bne$HOOD_158), decreasing = TRUE))[1:5]
hp <- function(h, nm) {
  v <- h[[nm]]
  if (is.null(v)) NA_real_ else as.numeric(v)[1L]
}
hawkes_tab <- do.call(rbind, lapply(top, function(hd) {
  d <- bne[bne$HOOD_158 == hd, ]
  h <- morie_tps_hawkes_temporal_fit(d, ds_name = paste("B&E, neighbourhood", hd), max_n = 20000L)
  kappa <- hp(h, "kappa")
  if (is.na(kappa)) kappa <- hp(h, "branching_ratio")
  data.frame(neighbourhood = hd, occurrences = nrow(d), dispersion = disp[[hd]], mu_per_day = hp(h, "mu"),
             kappa = kappa, omega_per_day = hp(h, "omega"), half_life_days = log(2) / hp(h, "omega"),
             stringsAsFactors = FALSE)
}))

summ <- data.frame(
  quantity = c("neighbourhoods", "weeks", "occurrences", "mean weekly count (median across neighbourhoods)",
               "dispersion var/mean (median)", "share of neighbourhoods with dispersion > 1",
               "share with dispersion > 1.5", "gamma-mixture size with the same moments (median)"),
  value = c(length(hoods), n_weeks, nrow(bne), stats::median(m), stats::median(disp), mean(disp > 1),
            mean(disp > 1.5), stats::median(nb_size[is.finite(nb_size)])),
  stringsAsFactors = FALSE)

lines <- c(
  "# P10: near-repeat contagion in Toronto break and enter, 2014-2024",
  "",
  "Source: Toronto Police Service Public Safety Data Portal, Break and Enter occurrences (neighbourhood and",
  "occurrence date; rows with no neighbourhood or an occurrence year outside 2014-2024 dropped).",
  "",
  "## Second moments of the weekly neighbourhood counts",
  "",
  md_table(summ, 4),
  "",
  "Every neighbourhood's weekly series is over-dispersed. By `Research.P10.Moments.second_moments_do_not_identify`",
  "that over-dispersion is reproduced exactly by a Poisson process whose rate varies across weeks (a gamma mixture",
  "with the size above) and by a self-exciting process with no heterogeneity at all; the witness laws of",
  sprintf("`morie_contagion_moment_witness()` (dispersion %s and %s) are the finite version of the same fact.",
          fmt(witness$dispersion[[1L]], 3), fmt(witness$dispersion[[2L]], 3)),
  "The second moments therefore commit the analyst to over-dispersion and to nothing about its source.",
  "",
  "## What the temporal order adds: Hawkes fits of the five busiest neighbourhoods, 2014-2024",
  "",
  md_table(hawkes_tab, 4),
  "",
  paste("The branching ratio is identified by the conditional intensity, the dependence of today's rate on the",
        "timing of past events, which is a third-order (and higher) property. Reading kappa as 'the share of",
        "break-ins caused by earlier break-ins' assumes the background is constant; a background that moves",
        "with the seasons produces clustering the Hawkes fit attributes to self-excitation. The constant-versus-KDE",
        "background comparison (library scan A17) is the honest next step and is not done here."),
  ""
)
write_result("p10_near_repeat_bne", lines)
