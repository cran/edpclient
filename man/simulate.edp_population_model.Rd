\name{simulate.edp_population_model}
\alias{simulate.edp_population_model}
\title{Sample from a population model}
\description{Draw a sample from a population model.}
\usage{
\method{simulate}{edp_population_model}(object, nsim = 1, seed = NULL, ...,
         target = NULL, columns = NULL, given = NULL, by_model = FALSE)
}
\arguments{
  \item{object}{an EDP population model, from \code{popmod(...)}}
  \item{nsim}{number of rows to simulate}
  \item{seed}{if set, an integer to pass to EDP as a random seed for this call}
  \item{...}{ignored, accepted for compatibility with \code{simulate}}
  \item{target}{a character vector of column names, defaults to
      \code{names(pm)}}
  \item{columns}{alias for \code{target} for compatibility with \code{simulate}}
  \item{given}{a single-row data frame of values to condition on;
      \code{data.frame(a = 3)} means to simulate from the conditional
      distribution given \code{a} is 3.}
  \item{by_model}{If false (the default), draws a sample of size \code{nsim}.
      If true, draws a sample of size \code{nsim} from each sub-model in the
      underlying model ensemble.}
}
\value{
  A data frame with the columns specified in \code{target}. If \code{by_model}
  is true, the result contains an additional column, "model," specifying which
  sub-model that row came from.
}
