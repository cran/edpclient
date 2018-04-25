\name{build_popmod}
\alias{build_popmod}
\title{Create a new population model}
\description{Creates a model for a population.}
\usage{
build_popmod(pop, models = 16, seed = NULL)
}
\arguments{
  \item{pop}{a population from \code{\link{population}} or
      \code{\link{create_population}}}
  \item{models}{ensemble size}
  \item{seed}{an optional integer random seed, for determinism}
}
\value{
  An EDP population model, similar to what is returned by \code{\link{popmod}}.
}
\seealso{
  \code{\link{create_population}}
}
