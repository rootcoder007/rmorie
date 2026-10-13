# P16 court backlog, applied to the Ontario Court of Justice's criminal court statistics:
# what the published mean time to disposition says next to the cases pending, read through
# Little's law (Research.P16), and how far the disposed-cases mean understates the time a case
# spends in court (Research.P16Censoring).
source(file.path(Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications")), "00_common.R"))

clr <- utils::read.csv(need_file(dpath("OCJ", "ocj_clearance_rate_criminal.csv")), check.names = FALSE)
dsp <- utils::read.csv(need_file(dpath("OCJ", "ocj_disposition_rate_criminal.csv")), check.names = FALSE)
off <- utils::read.csv(need_file(dpath("OCJ", "ocj_offence_based_criminal.csv")), check.names = FALSE)

# each workbook covers two years, so a month appears in two editions: keep the later one
latest <- function(d) {
  d <- d[d$row_type == "month" & nzchar(d$month), ]
  d <- d[order(d$location, d$month, -d$year), ]
  d[!duplicated(d[c("location", "month")]), ]
}
clr <- latest(clr)
dsp <- latest(dsp)
names(clr)[names(clr) == "Total Cases Pending (Net of Bench Warrants)"] <- "pending"
names(clr)[names(clr) == "Cases Received"] <- "received"
names(clr)[names(clr) == "Cases Disposed"] <- "disposed"
band_cols <- grep("% Disposed", names(dsp), value = TRUE)
avg_col <- grep("For All Cases$", names(dsp), value = TRUE)
m <- merge(clr[c("location", "month", "received", "disposed", "pending")],
           dsp[c("location", "month", band_cols, avg_col)], by = c("location", "month"))
names(m)[names(m) == avg_col] <- "avg_days"
m <- m[stats::complete.cases(m), ]
first <- as.Date(paste0(m$month, "-01"))
m$days <- as.numeric(as.Date(format(first + 32, "%Y-%m-01")) - first)
floors <- c(0, 304, 456, 548)  # under 10 months, 10-15, 15-18, over 18 (30.4-day months)

# a window of months: filings per day, time-average pending (the mean of month-end counts),
# the published mean and bands weighted by the cases disposed in each month
window <- function(d) {
  w <- d$disposed / sum(d$disposed)
  data.frame(months = nrow(d), received = sum(d$received), days = sum(d$days), pending = mean(d$pending),
             mean_days = sum(w * d$avg_days), t(colSums(w * as.matrix(d[band_cols]))), check.names = FALSE)
}
read_window <- function(d) {
  b <- morie_backlog_published(d$received, d$days, d$pending, d$mean_days,
                               band_shares = as.matrix(d[band_cols]), band_floors = floors)
  cbind(d[setdiff(names(d), band_cols)], b[c("filing_rate", "implied_mean_days", "ratio", "band_floor_bound")],
        over_18 = d[[band_cols[4]]])
}

prov <- m[m$location == "Ontario Overview", ]
prov$year <- substr(prov$month, 1, 4)
full <- names(which(table(prov$year) == 12))
py <- do.call(rbind, lapply(full, function(y) cbind(year = y, window(prov[prov$year == y, ]))))
py <- read_window(py)
py$pending <- round(py$pending)

last12 <- tail(sort(unique(prov$month)), 12)
courts <- m[m$location != "Ontario Overview" & m$month %in% last12, ]
cn <- table(courts$location)
courts <- courts[courts$location %in% names(cn[cn == 12]), ]
cw <- do.call(rbind, lapply(split(courts, courts$location), function(d) cbind(court = d$location[1], window(d))))
cw <- read_window(cw)
cw$pending <- round(cw$pending)
cw <- cw[order(-cw$ratio), ]
regions <- cw[grepl("Overview", cw$court), ]
regions$court <- sub(" Overview$", "", regions$court)
cw <- cw[!grepl("Overview", cw$court), ]

# offence groups, latest annual workbook (provincial overview, group totals)
oy <- max(off$year)
og <- off[off$year == oy & off$location == "Provincial Overview" & off[["OFFENCE TYPE"]] == "TOTAL", ]
ob <- morie_backlog_published(og[["Cases Received"]], 365, og[["Cases Pending"]],
                              og[["Avg Days to Disposition (for cases without bench warrants)"]])
otab <- data.frame(offence_group = og[["OFFENCE GROUP"]], received = og[["Cases Received"]],
                   pending = og[["Cases Pending"]], published_mean_days = ob$published_mean_days,
                   implied_mean_days = ob$implied_mean_days, ratio = ob$ratio)
otab <- otab[order(-otab$ratio), ]

lines <- c(
  "## P16 court backlog: Ontario Court of Justice criminal statistics", "",
  sprintf(paste("Data: OCJ criminal court statistics, clearance-rate and disposition-rate workbooks (%s to %s,",
                "%d courts, each month from its latest edition), and the offence-based workbook for %d."),
          min(m$month), max(m$month), length(unique(m$location)) - 1L, oy), "",
  paste("Little's law on a docket (Research.P16.little) makes the time-average number pending equal to",
        "the filing rate times the mean time a case spends in court, so the pending count and the",
        "filings fix that mean (little_target): implied days = pending / filings per day. The published",
        "mean counts only cases disposed in the window and excludes bench-warrant time, so it is the",
        "truncated mean of Research.P16Censoring; the ratio of implied to published is how much longer",
        "cases stay than the published figure says. The disposed-case bands give a second, model-free",
        "lower bound on the published mean (band_floor_le_mean), and the open 'over 18 months' band is",
        "why no upper bound exists (no_upper_bound)."), "",
  "### Province, by calendar year", "",
  md_table(py[c("year", "received", "pending", "mean_days", "implied_mean_days", "ratio", "band_floor_bound",
                "over_18")]), "",
  sprintf(paste("In %d of %d full years the backlog implies a longer stay than the published mean",
                "(ratio above 1), most in 2020-2022, when the pandemic closures grew the docket; the",
                "published mean is above its band floor bound in %d of %d."),
          sum(py$ratio >= 1), nrow(py), sum(py$mean_days >= py$band_floor_bound), nrow(py)), "",
  sprintf("### Regions, the 12 months %s to %s", min(last12), max(last12)), "",
  md_table(regions[c("court", "received", "pending", "mean_days", "implied_mean_days", "ratio")]), "",
  sprintf("### Courts, the same 12 months (%d courts, highest ratio first)", nrow(cw)), "",
  md_table(cw[c("court", "received", "pending", "mean_days", "implied_mean_days", "ratio")]), "",
  sprintf("### Offence groups, %d workbook (annual, provincial)", oy), "",
  md_table(otab), "",
  paste("Reading: the published mean time to disposition is a statement about the cases that finished.",
        "Where the implied mean is well above it, the cases still pending are older than the ones",
        "disposed, which is the condition under which Research.P16Censoring proves the published mean",
        "understates the truth. Little's law here is an identity over a window; month-end counts and",
        "a docket that is not in steady state make the implied figure an approximation, which is why",
        "it is read as a ratio over full years and over twelve-month windows, not month by month."),
  "", sprintf("Theorems: %s.", paste(attr(morie_backlog_published(1, 1, 1, 1, band_shares = rbind(c(1, 0, 0, 0)),
                                                                  band_floors = floors), "theorems"),
                                     collapse = ", ")))
write_result("p16_ocj_backlog", lines)
