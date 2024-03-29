% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NLS.R
\name{NLS}
\alias{NLS}
\title{NLS}
\usage{
NLS(specdat, withBC = TRUE)
}
\arguments{
\item{specdat}{A complex number vector of observed frequency domain data.}

\item{withBC}{A logical parameter that enables/disables baseline correction after baseline correction}
}
\value{
A numeric vector of phase corrected absorption spectrum
}
\description{
Non-linear shrinkage
}
\details{
This function is used to process phase error correction through non-linear shrinkage, followed by Polynomial baseline correction when necessary.
}
\examples{
data("fdat")
nls_phased1 <- NLS(fdat$frequency_domain)
}
\references{
Liland KH, Almøy T, Mevik B (2010), Optimal Choice of Baseline Correction for Multivariate Calibration of Spectra, Applied Spectroscopy 64, pp. 1007-1016.
}
\author{
Aixiang Jiang
}
\concept{phase correction}
