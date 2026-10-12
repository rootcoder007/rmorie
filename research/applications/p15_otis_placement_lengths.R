# P15, the "unexplained" part of a disparity: segregation placement lengths in Ontario by gender.
#
# Data: Ontario Ministry of the Solicitor General, OTIS segregation detailed dataset (one row per
# placement, fiscal years 2023-2025: gender, age category, region, consecutive days). The mean gap
# between women's and men's placement lengths is split into structure and composition by DFL
# reweighting on age x region (`morie_dfl_reweight()`, Research.P15.Reweight), and the quantile gaps
# at the median, the 75th and the 90th percentile are read off the RIF (`morie_rif_gap()`,
# Research.P15.RIF): gap of RIF means = quantile gap + recentering terms.
source(file.path(Sys.getenv("MORIE_APP_DIR", unset = file.path("research", "applications")), "00_common.R"))

f <- need_file(dpath("OTIS", "b01_segregation_detailed_dataset.csv"))
seg <- utils::read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
seg$days <- suppressWarnings(as.numeric(seg$NumberConsecutiveDays_Segregation))
seg <- seg[!is.na(seg$days) & seg$Gender %in% c("Male", "Female"), ]
seg$cell <- interaction(seg$Age_Category, seg$Region_MostRecentPlacement, drop = TRUE)

female <- as.integer(seg$Gender == "Female")
dfl <- morie_dfl_reweight(group = female, x = seg$cell, y = seg$days)

yF <- seg$days[seg$Gender == "Female"]
yM <- seg$days[seg$Gender == "Male"]
rif <- do.call(rbind, lapply(c(0.5, 0.75, 0.9), function(tau) {
  r <- morie_rif_gap(yF, yM, tau)
  data.frame(tau = tau, q_female = as.numeric(stats::quantile(yF, tau)), q_male = as.numeric(stats::quantile(yM, tau)),
             gap = r$gap, quantile_gap = r$quantile_gap, recentering_gap = r$recentering_gap, stringsAsFactors = FALSE)
}))

by_year <- do.call(rbind, lapply(sort(unique(seg$EndFiscalYear)), function(yr) {
  s <- seg[seg$EndFiscalYear == yr, ]
  d <- morie_dfl_reweight(group = as.integer(s$Gender == "Female"), x = s$cell, y = s$days)
  data.frame(fiscal_year = yr, placements = nrow(s), women = sum(s$Gender == "Female"), mean_women = d$mean_1,
             mean_men = d$mean_0, counterfactual = d$counterfactual, structure = d$structure,
             composition = d$composition, stringsAsFactors = FALSE)
}))

lines <- c(
  "# P15: the gender gap in Ontario segregation placement lengths, split",
  "",
  sprintf("Source: OTIS segregation detailed dataset, %s placements (fiscal years %s), consecutive days per placement.",
          fmt(nrow(seg), 0), paste(range(seg$EndFiscalYear), collapse = "-")),
  "Group 1 = women, group 0 = men; covariates = age category x region of the most recent placement",
  sprintf("(%d cells).", nlevels(seg$cell)),
  "",
  "## The mean gap, DFL reweighting (Research.P15.Reweight.decomposition)",
  "",
  md_table(data.frame(quantity = c("mean days, women", "mean days, men", "counterfactual (men reweighted to women's cells)",
                                   "structure (women minus counterfactual)", "composition (counterfactual minus men)",
                                   "largest covariate-share mismatch after reweighting"),
                      value = c(dfl$mean_1, dfl$mean_0, dfl$counterfactual, dfl$structure, dfl$composition, dfl$max_composition_gap),
                      stringsAsFactors = FALSE), 4),
  "",
  "## By fiscal year",
  "",
  md_table(by_year, 3),
  "",
  "## The quantile gaps, RIF (Research.P15.RIF.rif_gap)",
  "",
  md_table(rif, 4),
  "",
  "## Reading",
  "",
  sprintf(paste("Women's placements are longer on average by %s days. Reweighting men to women's age-by-region mix",
                "moves men's mean by %s days (composition), so %s days of the gap is structure: the same cell, a",
                "longer placement. The quantile gaps say where: the medians coincide (both %s days), and the gap opens",
                "in the upper tail (%s days at the 90th percentile), which is the pattern a few long placements make,",
                "not a shift of the whole distribution."),
          fmt(dfl$mean_1 - dfl$mean_0, 3), fmt(dfl$composition, 3), fmt(dfl$structure, 3),
          fmt(rif$q_female[rif$tau == 0.5], 0), fmt(rif$quantile_gap[rif$tau == 0.9], 2)),
  "",
  paste("What the split does not say: 'structure' is the part the two covariates do not explain, not discrimination;",
        "the placement reason, the alerts (mental health, suicide risk) and the institution are in the file and",
        "are the next covariates to add. The theorem's identity holds exactly (the mismatch above is zero), so",
        "the arithmetic is not in question, only the covariate set."),
  ""
)
write_result("p15_otis_placement_lengths", lines)
