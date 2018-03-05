\name{joint_probability}
\alias{joint_probability}
\title{Compute the probability of data.}
\description{Compute the probability of data under an EDP population model.}
\usage{
joint_probability(pm, target, log = FALSE)
}
\arguments{
  \item{pm}{an EDP population model, from \code{popmod(...)}}
  \item{target}{a data frame whose column names are all in \code{names(pm)} and
      whose types are compatible with \code{schema(pm)}}
  \item{log}{should log-probabilities be returned?}
}
\value{
  If \code{log = FALSE}, a numeric vector of probabilities (or densities) of
  the rows of \code{target}.  If \code{log = TRUE}, a numeric vector of log
  probabilities (or densities) of the rows of \code{target}.
}
