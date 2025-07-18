% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap_rwa.R
\name{extract_ci}
\alias{extract_ci}
\title{Extract confidence intervals from bootstrap object}
\usage{
extract_ci(
  boot_object,
  conf_level = 0.95,
  variable_names = NULL,
  ci_type = "raw"
)
}
\arguments{
\item{boot_object}{Boot object from boot::boot()}

\item{conf_level}{Confidence level (default 0.95)}

\item{variable_names}{Names of variables for labeling}

\item{ci_type}{Type of CI to extract ("raw", "rand_diff", "focal_diff")}
}
\value{
Data frame with confidence intervals
}
\description{
Extract confidence intervals from bootstrap object
}
\keyword{internal}
