% Generated from the package source by gen_vic_rd.py -- roxygenise is not run on this branch
% Please edit documentation in R/datasets_vic.R
\name{morie_vic_offence_trend}
\alias{morie_vic_offence_trend}
\title{Offence-division trend from the Victorian criminal incidents table}
\usage{
morie_vic_offence_trend(data)
}
\arguments{
\item{data}{A frame shaped like
  \code{morie_datasets_vic_table("criminal_incidents", 1)}, with
  \code{Year}, \code{Offence Division} and \code{Incidents Recorded}.}

}
\value{
A \code{data.frame} with one row per division:
  \code{division}, \code{first_year}, \code{last_year},
  \code{first_count}, \code{last_count}, \code{abs_change} and
  \code{pct_change}.
}
\description{
Aggregates recorded incidents by year and offence division and
reports the change over the observed window.
}
\references{
Crime Statistics Agency (Victoria), CC BY 4.0.
}
\examples{
d <- data.frame(
  Year = c(2024, 2025, 2024, 2025),
  `Offence Division` = c("A", "A", "B", "B"),
  `Incidents Recorded` = c(100, 150, 80, 60),
  check.names = FALSE
)
morie_vic_offence_trend(d)
}
