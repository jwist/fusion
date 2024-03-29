% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getData.R
\name{getData}
\alias{getData}
\title{S4 method get data from dataElement}
\usage{
getData(da, type = c("sample"), replicates = "takeLast")
}
\arguments{
\item{da}{dataElement}

\item{type}{either sample, ltr, pqc, qc, standard, or blank or a combination of}

\item{replicates}{either last, rm or all}
}
\value{
a dataElement with selected types
}
\description{
S4 method get data from dataElement
}
