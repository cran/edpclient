\name{simulate.edp_population_model}
\alias{simulate.edp_population_model}
\title{Fetch columns from a population model}
\description{Fetch specified columns from a population model. This is the data
the population model is trained on.}
\usage{
\method{simulate}{edp_population_model}(object, nsim = 1, seed = NULL, ...,
         target = NULL, columns = NULL, given = NULL)
}
\arguments{
  \item{object}{an EDP population model, from \code{popmod(...)}}
  \item{nsim}{number of rows to simulate}
  \item{seed}{if set, an integer to pass to EDP as a random seed for this call}
  \item{...}{ignored, accepted for compatibility with \code{simulate}}
  \item{target}{a character vector of column names, defaults to
      \code{names(pm)}}
  \item{columns}{alias for \code{target} for compatibility with \code{simulate}}
  \item{given}{a list of values to condition on; \code{list(a = 3)}
      means to simulate from the conditional distribution given \code{a} is 3.}
}
\value{
  a data frame with the columns specified in \code{target}.
}
