visibility <- function (x) {
  UseMethod("visibility", x)
}

# Turns a JSON response from .../visibility into a form suitable for R.
parse_visibility_json <- function(viz) {
  # flatten lists to vectors
  viz$reader_domains <- as.character(viz$reader_domains)
  viz$readers <- as.character(viz$readers)
  return(viz)
}

visibility.edp_population <- function(x) {
  op <- paste("rpc/population", utils::URLencode(x$pid), "visibility",
              sep = "/")
  viz <- httr::content(edp_get(x$sess, op))
  return(parse_visibility_json(viz))
}

visibility.edp_population_model <- function(x) {
  return(visibility(population(x$sess, x$parent_id)))
}

add_reader <- function (x, reader = NULL, reader_domain = NULL) {
  UseMethod("add_reader", x)
}

add_reader.edp_population <- function(x, reader = NULL, reader_domain = NULL) {
  if (is.null(reader) == is.null(reader_domain)) {
    stop("Exactly one of `reader` and `reader_domain` must be specified.")
  }
  if (!is.null(reader)) {
    stopifnot(is.character(reader) && length(reader) == 1)
    stopifnot(length(grep("@", reader)) == 1)
    req <- list(readers = list(reader))
  } else {
    stopifnot(is.character(reader_domain) && length(reader_domain) == 1)
    stopifnot(grep("@", reader_domain) == 0)
    req <- list(reader_domain = reader_domain)
  }
  op <- paste("rpc/population", utils::URLencode(x$pid), "visibility",
              sep = "/")
  resp <- httr::content(edp_post(x$sess, op, req, verb = "PATCH"))
  return(parse_visibility_json(resp))
}

add_reader.edp_population_model <- function(x, reader = NULL,
                                            reader_domain = NULL) {
  add_reader(population(x$sess, x$parent_id), reader, reader_domain)
}
