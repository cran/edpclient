\name{create_population}
\alias{create_population}
\title{Create a new population}
\description{Create a new population by uploading data and a schema.}
\usage{
create_population(sess, data, name)
}
\arguments{
  \item{sess}{an \code{edp_session} from \code{edp_session(...)}}
  \item{data}{a data frame containing the data to build models from}
  \item{name}{a length-one character naming the population}
}
\value{
  A new EDP population object. If you print it or cast it to character, you can
  get an id that will let you get the object back later with
  \code{\link{population}}.
}
\details{
  This function derives a population schema from \code{data}, uploads the data
  and schema, and creates a new remote population object. You can optionally
  use functions like \code{\link{stat_type<-}} and \code{\link{display_name<-}}
  on columns of the data frame before calling \code{create_population} if you
  want to give it hints about what the schema should be.
}
\seealso{
  \code{\link{edp_session}}, \code{\link{population}},
  \code{\link{stat_type}}, \code{\link{display_name}},
  \code{\link{identifying}}
}
\examples{
  d <- data.frame(x = c(1, 2, 3, 4, 5))
  display_name(d$x) <- "X!"
  stat_type(d$x) <- "realMultiplicative"
  \dontrun{pop <- create_population(sess, d, "my population")}
}
