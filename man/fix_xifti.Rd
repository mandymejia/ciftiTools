% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix_xifti.R
\name{fix_xifti}
\alias{fix_xifti}
\title{Fix a \code{"xifti"}}
\usage{
fix_xifti(xifti, verbose = TRUE)
}
\arguments{
\item{xifti}{A \code{"xifti"} object.}

\item{verbose}{Report each adjustment? Default: \code{TRUE}}
}
\value{
The fixed \code{"xifti"}
}
\description{
Make adjustments to a putative \code{"xifti"} so that it is valid. Each
adjustment is reported.
}
\details{
Right now it: coerces the data to numeric matrices; adds the
"Other" level to the subcortex labels.
}
