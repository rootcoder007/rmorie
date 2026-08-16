% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_statcan_wds_table}
\alias{.morie_statcan_wds_table}
\title{Native StatCan Web Data Service client -- fetches the full-table}
\usage{
.morie_statcan_wds_table(table_id, language = "en")
}
\arguments{
\item{table_id}{Character; passed to \code{gsub}.}
\item{language}{Character; passed to \code{tolower}. Defaults to \code{"en"}.}
}
\description{
CSV for a table id ("NN-MM-XXXX" or "NN-MM-XXXX-NN" cansim style, or a bare 8-digit PID) via the getFullTableDownloadCSV endpoint. Base R only (download.file + unzip + read.csv); replaces cansim::get_cansim. Column set is StatCan\\'s raw CSV schema (REF_DATE, GEO, VALUE, ...), which is the subset of get_cansim() output the callers use.
}
\value{
The value of \code{utils::read.csv}.
}
\keyword{internal}
