% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parseNMR.R
\name{parseNMR}
\alias{parseNMR}
\title{reads all information from bruker folder structures}
\usage{
parseNMR(
  folder,
  options = list(what = c("spec"), projectName = "", cohortName = "", runID = "", method
    = "", sampleMatrixType = "", specOpts = list(uncalibrate = FALSE, fromTo = c(-0.1,
    10), length.out = 44079))
)
}
\arguments{
\item{folder}{\itemize{
\item the root folder to read from
}}

\item{options}{\itemize{
\item options (what, specOpts(uncalibrate, fromTo, length.out))
}}
}
\description{
reads all information from bruker folder structures
}
