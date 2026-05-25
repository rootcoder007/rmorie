# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie/siuiap.R -- Structured Intervention Unit Implementation
# Advisory Panel (FEDERAL).
#
# Counterpart to the Python `morie.siuiap` module. Pure metadata:
# panel members, panel reports, CRIMSL UToronto research reports,
# Federal Court affidavits, and a lookup `morie_siuiap_cite()`.
#
# NOTE: this is the FEDERAL SIU Implementation Advisory Panel
# (Sapers / Doob / Sprott, 2021-2024). It is distinct from
# `morie.siu` / `morie/R/siu*.R`, which is the ONTARIO Special
# Investigations Unit (police oversight) automation. Both share
# the acronym SIU but refer to different agencies.

#' SIU IAP -- Public Safety Canada landing page URL.
#' @export
MORIE_SIUIAP_URL <- paste0(
  "https://www.publicsafety.gc.ca/cnt/cntrng-crm/crrctns/",
  "siuiap-ccuis-en.aspx"
)

#' SIU IAP panel mandate (long-form prose).
#' @export
MORIE_SIUIAP_PANEL_MANDATE <- paste(
  "The Structured Intervention Unit Implementation Advisory Panel",
  "(SIU IAP) was appointed in April 2021 to monitor and assess the",
  "implementation of Structured Intervention Units within Canada's",
  "federal correctional system. The panel's mandate ended on",
  "December 31, 2024. SIU IAP reports cover thematic areas including",
  "mental health, Indigenous peoples in federal corrections, and the",
  "implementation of the legislative framework that replaced",
  "administrative segregation.",
  "NOTE: An EARLIER, SHORT-LIVED Implementation Advisory Panel was",
  "appointed in 2019 and chaired by Anthony Doob; it ceased to exist",
  "in mid-2020 and was not re-established until April 2021 under",
  "Howard Sapers. See MORIE_SIUIAP_ORIGINAL_PANEL_2019_2020 for the",
  "earlier panel."
)

#' Earlier (Doob-chaired) panel, established 2019, dissolved mid-2020.
#' @export
MORIE_SIUIAP_ORIGINAL_PANEL_2019_2020 <- list(
  name = "SIU-Implementation Advisory Panel (original, Doob-chaired)",
  established = "2019",
  dissolved = "mid-2020",
  chair = "Anthony N. Doob",
  context = paste(
    "Established when SIUs opened in November 2019. Doob's Federal",
    "Court affidavit (T-539-20) was filed in 2020 after this panel",
    "had been dissolved. Sprott & Doob (Feb 2021) explicitly note",
    "that this panel 'died a natural death after the 1-year terms",
    "of its members expired in mid-2020' and was not re-established",
    "until April 2021 (under Sapers, with new membership)."
  ),
  source = paste(
    "Sprott & Doob, *Solitary Confinement, Torture, and Canada's",
    "SIUs* (Feb 2021), p.7 footnote 7"
  )
)

#' SIU IAP panel members (2021-2024 panel, Sapers-chaired).
#' @export
MORIE_SIUIAP_PANEL_MEMBERS <- list(
  list(name = "Howard Sapers", role = "Chair",
       context = "Former Correctional Investigator of Canada"),
  list(name = "Anthony N. Doob", role = "Member",
       context = paste(
         "Prof. Emeritus, Centre for Criminology & Sociolegal Studies,",
         "U. of Toronto"
       )),
  list(name = "Jane B. Sprott", role = "Member",
       context = paste(
         "Prof., Department of Criminology, Toronto Metropolitan",
         "University"
       ))
)

#' SIU IAP panel reports (Public Safety Canada, 2022-2024).
#' @export
MORIE_SIUIAP_REPORTS <- list(
  final_2024 = list(
    title = "SIU IAP Final Report",
    year = 2024,
    type = "Final Report",
    publisher = "Public Safety Canada",
    authors = c("SIU IAP")
  ),
  annual_2023_2024 = list(
    title = "2023-2024 Annual Report",
    year = 2024,
    type = "Annual Report",
    notes = "Includes government responses",
    publisher = "Public Safety Canada",
    authors = c("SIU IAP")
  ),
  preliminary_observations = list(
    title = "Preliminary Observations",
    year = 2022,
    type = "Preliminary",
    publisher = "Public Safety Canada",
    authors = c("SIU IAP")
  ),
  thematic_mental_health = list(
    title = "Thematic update -- mental health in SIUs",
    year = 2023,
    type = "Thematic update",
    publisher = "Public Safety Canada",
    authors = c("SIU IAP")
  ),
  thematic_indigenous = list(
    title = "Thematic update -- Indigenous peoples in SIUs",
    year = 2023,
    type = "Thematic update",
    publisher = "Public Safety Canada",
    authors = c("SIU IAP")
  )
)

#' CRIMSL UToronto Sprott / Doob / Iftene research reports (2020-2021).
#' @export
MORIE_SIUIAP_CRIMSL_REPORTS <- list(
  sprott_doob_csc_siu_operation_2020 = list(
    title = paste(
      "Understanding the Operation of Correctional Service Canada's",
      "Structured Intervention Units: Some Preliminary Findings"
    ),
    authors = c("Jane B. Sprott", "Anthony N. Doob"),
    year = 2020,
    month = "October",
    publisher = paste(
      "Centre for Criminology & Sociolegal Studies,",
      "University of Toronto"
    ),
    type = "Research Report",
    notes = paste(
      "First systematic outside analysis of CSC's published SIU data",
      "showing patterns of prolonged stays."
    )
  ),
  sprott_doob_covid_attribution_2020 = list(
    title = paste(
      "Is there Clear Evidence that COVID-19 Was the Cause of",
      "Problems with the Operation of CSC's Structured",
      "Intervention Units?"
    ),
    authors = c("Jane B. Sprott", "Anthony N. Doob"),
    year = 2020,
    month = "November",
    publisher = paste(
      "Centre for Criminology & Sociolegal Studies,",
      "University of Toronto"
    ),
    type = "Research Report",
    notes = paste(
      "Tests CSC's claim that COVID-19 caused SIU operational",
      "problems; finds the data did not support this attribution."
    )
  ),
  sprott_doob_torture_solitary_2021 = list(
    title = paste(
      "Solitary Confinement, Torture, and Canada's Structured",
      "Intervention Units"
    ),
    authors = c("Jane B. Sprott", "Anthony N. Doob"),
    year = 2021,
    month = "February",
    publisher = paste(
      "Centre for Criminology & Sociolegal Studies,",
      "University of Toronto"
    ),
    type = "Research Report",
    url = paste0(
      "https://www.crimsl.utoronto.ca/sites/www.crimsl.utoronto.ca/",
      "files/TortureSolitarySIUsSprottDoob23Feb2021_0.pdf"
    ),
    notes = paste(
      "Frames prolonged SIU stays in relation to the Mandela Rules /",
      "international torture-norm thresholds."
    )
  ),
  sprott_doob_iftene_external_decision_makers_2021 = list(
    title = paste0(
      "Do Independent External Decision Makers Ensure that ",
      "\"An Inmate's Confinement in a Structured Intervention Unit ",
      "Is to End as Soon as Possible\"? [Corrections and Conditional ",
      "Release Act, Section 33]"
    ),
    authors = c("Jane B. Sprott", "Anthony N. Doob", "Adelina Iftene"),
    affiliations = c(
      "Ryerson University",
      "University of Toronto",
      "Schulich School of Law, Dalhousie University"
    ),
    year = 2021,
    month = "May",
    date = "2021-05-09",
    publisher = paste(
      "Schulich School of Law, Dalhousie University (Schulich Law",
      "Scholars / Reports & Public Policy Documents) -- Faculty",
      "Scholarship; with companion release at Centre for Criminology",
      "& Sociolegal Studies, University of Toronto"
    ),
    url = paste0(
      "https://digitalcommons.schulichlaw.dal.ca/cgi/",
      "viewcontent.cgi?article=1052&context=reports"
    ),
    alternate_url = "https://works.bepress.com/adelina-iftene/34/",
    type = "Research Report",
    n_iedm_stays_reviewed = 265L,
    notes = paste(
      "Evaluates IEDM reviews under CCRA s.37.8. N=265 stays.",
      "Headline findings: 87 percent of IEDM 'stay-in' decisions",
      "result in the prisoner remaining; 30 percent of cases CSC",
      "moved prisoner BEFORE IEDM rendered decision; 12 IEDMs varied",
      "38-86 percent on 'should remain' rate; 105 cases >=76 days in",
      "SIU with NO IEDM record (5 cases >=120 days). Indigenous =",
      "40.4 percent of reviewed stays; Black = 15.8 percent",
      "(over-represented vs Canadian Black population)."
    )
  )
)

#' Federal Court affidavits / expert evidence indexed by `morie`.
#' @export
MORIE_SIUIAP_AFFIDAVITS <- list(
  doob_t_539_20_2020 = list(
    title = paste(
      "Affidavit of Anthony N. Doob -- Federal Court of Canada,",
      "T-539-20"
    ),
    authors = c("Anthony N. Doob"),
    year = 2020,
    court = "Federal Court of Canada",
    file_no = "T-539-20",
    case = paste(
      "Canadian Civil Liberties Association, Canadian Prison Law",
      "Association, HIV Legal Network, HALCO, & Sean Johnston v.",
      "Attorney General of Canada"
    ),
    volume = "Application Record Vol. 3 of 5",
    pages = "778-795 (Bates-stamped)",
    type = "Expert Affidavit",
    notes = paste(
      "Source for Tables 1-3 + Figures 1-4 replicated in",
      "morie.doob_trends. CCRSO 2018 + StatsCan CANSIM",
      "+ Pastore-Maguire (US)."
    )
  )
)


#' Build a citation string for an SIU IAP / CRIMSL / affidavit entry.
#'
#' Searches `MORIE_SIUIAP_REPORTS`, then `MORIE_SIUIAP_CRIMSL_REPORTS`,
#' then `MORIE_SIUIAP_AFFIDAVITS`, in order, and returns a one-line
#' citation in the form `<authors> (<year>). <title>. <publisher>.`.
#'
#' @param report_id Character scalar. One of the names of
#'   `MORIE_SIUIAP_REPORTS`, `MORIE_SIUIAP_CRIMSL_REPORTS`, or
#'   `MORIE_SIUIAP_AFFIDAVITS`. Defaults to `"final_2024"`.
#'
#' @return A character scalar citation. Errors on unknown `report_id`.
#'
#' @examples
#' morie_siuiap_cite("final_2024")
#' morie_siuiap_cite("sprott_doob_torture_solitary_2021")
#'
#' @export
morie_siuiap_cite <- function(report_id = "final_2024") {
  stopifnot(is.character(report_id), length(report_id) == 1L)
  registries <- list(
    MORIE_SIUIAP_REPORTS,
    MORIE_SIUIAP_CRIMSL_REPORTS,
    MORIE_SIUIAP_AFFIDAVITS
  )
  for (reg in registries) {
    if (report_id %in% names(reg)) {
      r <- reg[[report_id]]
      authors <- if (!is.null(r$authors)) {
        paste(r$authors, collapse = ", ")
      } else {
        "SIU IAP"
      }
      year <- r$year
      title <- r$title
      publisher <- r$publisher
      if (is.null(publisher)) publisher <- r$court
      if (is.null(publisher)) publisher <- "Public Safety Canada"
      return(sprintf("%s (%s). %s. %s.",
                     authors, year, title, publisher))
    }
  }
  stop(sprintf(
    paste0("unknown report_id %s; available REPORTS: %s; ",
           "CRIMSL_REPORTS: %s; AFFIDAVITS: %s"),
    sQuote(report_id),
    paste(sort(names(MORIE_SIUIAP_REPORTS)), collapse = ", "),
    paste(sort(names(MORIE_SIUIAP_CRIMSL_REPORTS)), collapse = ", "),
    paste(sort(names(MORIE_SIUIAP_AFFIDAVITS)), collapse = ", ")
  ), call. = FALSE)
}


#' Human-readable summary of the SIU IAP panel.
#'
#' @return A character scalar summarising chair, members, mandate
#'   dates, and the Public Safety Canada URL.
#'
#' @examples
#' cat(morie_siuiap_panel_summary())
#'
#' @export
morie_siuiap_panel_summary <- function() {
  members <- vapply(
    MORIE_SIUIAP_PANEL_MEMBERS,
    function(m) sprintf("%s (%s)", m$name, m$role),
    character(1)
  )
  sprintf(
    paste0(
      "Structured Intervention Unit Implementation Advisory Panel ",
      "(SIU IAP). Federal counterpart to Ontario's OTIS provincial ",
      "data. Chair: Howard Sapers (former Correctional Investigator). ",
      "Members: %s. Panel mandate: 2021-04 to 2024-12. Page: %s"
    ),
    paste(members, collapse = ", "),
    MORIE_SIUIAP_URL
  )
}
