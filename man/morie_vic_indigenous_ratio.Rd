% Generated from the package source by gen_vic_rd.py -- roxygenise is not run on this branch
% Please edit documentation in R/datasets_vic.R
\name{morie_vic_indigenous_ratio}
\alias{morie_vic_indigenous_ratio}
\title{Indigenous victimisation rate ratio from the Victorian tables}
\usage{
morie_vic_indigenous_ratio(data, by = "Offence Division", year = NULL)
}
\arguments{
\item{data}{A frame shaped like
  \code{morie_datasets_vic_table("indigenous_victim_reports", 1)},
  carrying \code{Indigenous Status} and \code{Victim Reports}.}

\item{by}{Column to group by (default \code{"Offence Division"}).}

\item{year}{Optional year; defaults to the latest present. The table
  spans several years, so aggregating all of them silently mixes
  reporting periods.}

}
\value{
A \code{data.frame} with \code{group}, \code{indigenous},
  \code{non_indigenous} and \code{count_ratio}.
}
\description{
Compares victim-report counts for Aboriginal and/or Torres Strait
Islander people against the non-Indigenous count, per offence
division. This reports a COUNT ratio, not a population-adjusted rate
ratio: the published table carries counts only, so a value above 1
does not by itself establish over-representation without the
denominator populations. The table's "Total People" rows are
excluded, and one year is used at a time.
}
\references{
Crime Statistics Agency (Victoria), CC BY 4.0.
}
\examples{
d <- data.frame(
  `Offence Division` = c("A", "A", "B", "B"),
  `Indigenous Status` = c("Aboriginal and/or Torres Strait Islander",
                          "Non-Indigenous",
                          "Aboriginal and/or Torres Strait Islander",
                          "Non-Indigenous"),
  `Victim Reports` = c(8, 179, 20, 100),
  check.names = FALSE
)
morie_vic_indigenous_ratio(d)
}
