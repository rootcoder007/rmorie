% Generated from the package source by gen_vic_rd.py -- roxygenise is not run on this branch
% Please edit documentation in R/datasets_vic.R
\name{morie_vic_lga_rates}
\alias{morie_vic_lga_rates}
\title{Rank Victorian local government areas by offence rate}
\usage{
morie_vic_lga_rates(data, year = NULL, top = 10L)
}
\arguments{
\item{data}{A frame shaped like
  \code{morie_datasets_vic_table("lga_criminal_incidents", 1)}.}

\item{year}{Optional year to restrict to; defaults to the latest.}

\item{top}{Number of areas to return (default 10).}

}
\value{
A \code{data.frame} ordered by rate, with \code{lga},
  \code{year}, \code{incidents}, \code{rate} and \code{rank}.
}
\description{
Rank Victorian local government areas by offence rate
}
\references{
Crime Statistics Agency (Victoria), CC BY 4.0.
}
\examples{
d <- data.frame(
  Year = c(2025, 2025, 2025),
  `Local Government Area` = c("Alpha", "Beta", "Gamma"),
  `Incidents Recorded` = c(500, 300, 900),
  `Rate per 100,000 population` = c(1200, 800, 2500),
  check.names = FALSE
)
morie_vic_lga_rates(d, top = 2)
}
