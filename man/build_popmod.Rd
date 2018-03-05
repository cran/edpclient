\name{build_popmod}
\alias{build_popmod}
\title{Create a new population model}
\description{Ask the job server to start working on a model for a population.}
\usage{
build_popmod(pop, name, models = 16, builder = "lazy", iterations = 10)
}
\arguments{
  \item{pop}{a population from \code{\link{population}} or
      \code{\link{create_population}}}
  \item{name}{a name for the new model}
  \item{models}{ensemble size}
  \item{builder}{builder, rarely changed from the default}
  \item{iterations}{number of Gibbs sweeps to perform}
}
\value{
  A new EDP population model id. Eventually, this will return a population
  model object, but those cannot currently refer to un-built models. You can
  see whether the model is built yet with \code{\link{popmods}}.
}
\seealso{
  \code{\link{create_population}},
  \code{\link{wait_for}}
}
