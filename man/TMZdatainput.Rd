% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TMZtester.API.R
\name{TMZdatainput}
\alias{TMZdatainput}
\title{Read multiple files exported from Thermec Master-Z tester}
\usage{
TMZdatainput(makeidx = FALSE, ...)
}
\arguments{
\item{makeidx}{A boolean value to control the index column, inserted in the first column.
Default setting is FALSE.}

\item{...}{Arguments to be passed to \code{\link[TPMplt:API4TMZ]{API4TMZ}}.}
}
\value{
A summary data frame for all input files.
}
\description{
Read data from multiple files with structurized file names, then generate
a summary data frame. It will also be available for the files from other tester apparatus by
correct setting.
}
\examples{
variable1 <- c("factor11", "factor12", "factor13")
variable2 <- c("factor21", "factor22")
variable3 <- c("factor31", "factor32", "factor33", "factor34")
conditions <- list(variable1, variable2, variable3)

\dontrun{
SummaryTable <- TMZdatainput(Cdl=conditions, wd="/Your_Data_Directory/")
}
}
\keyword{APIfunction}
