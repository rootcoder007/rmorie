# P2 selection in police-recorded data, applied with the veil-of-darkness design to Nashville's
# traffic stops (Stanford Open Policing Project): in the inter-twilight window the same clock time
# is light on some dates and dark on others, so the drivers on the road are the same people under
# both conditions, while officers can see a driver's race only in daylight. The ratio of daylight
# to darkness odds of a group's share of stops identifies the relative visible-race factor
# (Research.P2.Veil.odds_ratio_identifies_bias); its sign is the sign of the share difference.
source(file.path(Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications")), "00_common.R"))

d <- utils::read.csv(need_file(dpath("OPP", "tn_nashville_2020_04_01.csv")), stringsAsFactors = FALSE)[
  c("date", "time", "lat", "lng", "subject_race", "type")]
n_all <- nrow(d)
d <- d[d$type == "vehicular" & d$subject_race %in% c("black", "white") & nzchar(d$time) & !is.na(d$date), ]

# sunset and the end of civil twilight (NOAA solar position), local time with daylight saving
solar_event <- function(date, lat, lon, zenith) {
  jd <- as.numeric(as.Date(date)) + 2440587.5
  t <- (jd + 0.5 - 2451545) / 36525
  l0 <- (280.46646 + t * (36000.76983 + 0.0003032 * t)) %% 360
  m <- 357.52911 + t * (35999.05029 - 0.0001537 * t)
  e <- 0.016708634 - t * (0.000042037 + 0.0000001267 * t)
  mr <- m * pi / 180
  c <- sin(mr) * (1.914602 - t * (0.004817 + 0.000014 * t)) + sin(2 * mr) * (0.019993 - 0.000101 * t) +
    sin(3 * mr) * 0.000289
  lam <- l0 + c - 0.00569 - 0.00478 * sin((125.04 - 1934.136 * t) * pi / 180)
  eps <- 23 + (26 + (21.448 - t * (46.815 + t * (0.00059 - t * 0.001813))) / 60) / 60 +
    0.00256 * cos((125.04 - 1934.136 * t) * pi / 180)
  decl <- asin(sin(eps * pi / 180) * sin(lam * pi / 180))
  y <- tan(eps * pi / 360)^2
  eqt <- 4 * 180 / pi * (y * sin(2 * l0 * pi / 180) - 2 * e * sin(mr) + 4 * e * y * sin(mr) * cos(2 * l0 * pi / 180) -
                           0.5 * y^2 * sin(4 * l0 * pi / 180) - 1.25 * e^2 * sin(2 * mr))
  ha <- acos(cos(zenith * pi / 180) / (cos(lat * pi / 180) * cos(decl)) - tan(lat * pi / 180) * tan(decl)) * 180 / pi
  utc_min <- 720 - 4 * (lon - ha) - eqt
  utc <- as.POSIXct(paste(date, "00:00:00"), tz = "UTC") + utc_min * 60
  loc <- as.POSIXlt(utc, tz = "America/Chicago")
  loc$hour * 60 + loc$min + loc$sec / 60
}
lat <- 36.1627
lon <- -86.7816
dates <- unique(d$date)
sunset <- stats::setNames(solar_event(dates, lat, lon, 90.833), dates)
dusk <- stats::setNames(solar_event(dates, lat, lon, 96), dates)
hm <- as.integer(substr(d$time, 1, 2)) * 60 + as.integer(substr(d$time, 4, 5))
d$min <- hm
d$sunset <- sunset[d$date]
d$dusk <- dusk[d$date]
# the inter-twilight window: clock times light on some dates and dark on others
lo <- min(sunset)
hi <- max(dusk)
w <- d[d$min >= lo & d$min <= hi, ]
w$light <- ifelse(w$min < w$sunset, "day", ifelse(w$min > w$dusk, "dark", NA))
w <- w[!is.na(w$light), ]
w$bin <- floor(w$min / 30) * 30
pooled_counts <- function(x) {
  day <- table(factor(x$subject_race[x$light == "day"], levels = c("white", "black")))
  dark <- table(factor(x$subject_race[x$light == "dark"], levels = c("white", "black")))
  list(day = c(white = day[["white"]], black = day[["black"]]), dark = c(white = dark[["white"]], black = dark[["black"]]))
}
pc <- pooled_counts(w)
pooled <- morie_veil_of_darkness(day = pc$day, night = pc$dark, reference = "white")
bins <- do.call(rbind, lapply(sort(unique(w$bin)), function(b) {
  x <- w[w$bin == b, ]
  cc <- pooled_counts(x)
  if (any(c(cc$day, cc$dark) == 0)) return(NULL)
  v <- morie_veil_of_darkness(day = cc$day, night = cc$dark, reference = "white")
  data.frame(clock = sprintf("%02d:%02d-%02d:%02d", b %/% 60, b %% 60, (b + 30) %/% 60, (b + 30) %% 60),
             day_stops = sum(cc$day), dark_stops = sum(cc$dark), black_share_day = cc$day[["black"]] / sum(cc$day),
             black_share_dark = cc$dark[["black"]] / sum(cc$dark), bias_ratio = v$bias_ratio[v$group == "black"],
             stringsAsFactors = FALSE)
}))
# Mantel-Haenszel combination over the clock bins (time of day held fixed)
mh_num <- mh_den <- 0
for (b in sort(unique(w$bin))) {
  x <- w[w$bin == b, ]
  a <- sum(x$light == "day" & x$subject_race == "black")
  bb <- sum(x$light == "day" & x$subject_race == "white")
  cc <- sum(x$light == "dark" & x$subject_race == "black")
  dd <- sum(x$light == "dark" & x$subject_race == "white")
  n <- a + bb + cc + dd
  if (n > 0) {
    mh_num <- mh_num + a * dd / n
    mh_den <- mh_den + bb * cc / n
  }
}
mh <- mh_num / mh_den

fmt_t <- function(m) sprintf("%02d:%02d", as.integer(m %/% 60), as.integer(round(m %% 60)))
lines <- c(
  "## P2 selection: the veil of darkness in Nashville's traffic stops", "",
  sprintf(paste("Data: Stanford Open Policing Project, Nashville TN standardized stops (release 2020-04-01, %s",
                "rows; Pierson et al. 2020). Vehicular stops of Black and white drivers in the inter-twilight",
                "window %s to %s (the earliest sunset to the latest end of civil twilight, local time,",
                "computed for each date at 36.16 N, 86.78 W); stops between sunset and dusk are dropped:",
                "%s stops in daylight and %s in darkness."),
          format(n_all, big.mark = ","), fmt_t(lo), fmt_t(hi), format(sum(w$light == "day"), big.mark = ","),
          format(sum(w$light == "dark"), big.mark = ",")), "",
  paste("Within the window the drivers are the same people in light and dark, and only daylight lets an",
        "officer see race before the stop. The ratio of the Black-to-white odds among daylight stops to",
        "the same odds in darkness is the relative visible-race factor (Research.P2.Veil.",
        "odds_ratio_identifies_bias); above one means Black drivers are stopped more when they can be seen."), "",
  "### Pooled over the window", "",
  md_table(pooled), "",
  "### By half-hour of clock time", "",
  md_table(bins), "",
  sprintf(paste("Holding the clock time fixed (Mantel-Haenszel over the half-hour bins) the daylight-to-darkness",
                "odds ratio is %.3f. %s The design's assumption is the theorem's antecedent: who drives at a",
                "given clock time does not change with light by race (a differential shift is observationally",
                "the same as bias, exposure_shift_confounds), and the result is a ratio, so a common factor",
                "on both groups is invisible (bias_not_absolute)."), mh,
          if (mh > 1) "Black drivers are a larger share of daylight stops than of darkness stops at the same time of day, the sign the veil test reads as visibility-driven disparity." else
            "Black drivers are not a larger share of daylight stops than of darkness stops at the same time of day, so the veil test finds no visibility-driven disparity here."), "",
  "Theorems: Research.P2.Veil.odds_ratio_identifies_bias, Research.P2.Veil.share_sign, Research.P2.Veil.exposure_shift_confounds, Research.P2.Veil.bias_not_absolute.")
write_result("p02_veil_nashville", lines)
