population <- function(sess, pid) {
  stopifnot(is.edp_session(sess))
  stopifnot(is.character(pid) && length(pid) == 1 && startsWith(pid, "p-"))

  op <- paste("rpc/population", URLencode(pid), sep = "/")
  resp <- httr::content(edp_get(sess, op))
  x <- list(sess = sess, name = resp$name, pid = pid, models = resp$models,
            schema = schema_json(sess, pid))
  class(x) <- "edp_population"

  return(x)
}

is.population <- function(x) {
  "edp_population" %in% class(x)
}

as.character.edp_population <- function(x, ...) {
  x$pid
}

print.edp_population <- function(x, ...) {
  cat(sprintf("population(id=%s, name=%s, #models=%d)\n", x$pid, x$name,
              length(x$models)))
}

names.edp_population <- function(x, ...) {
  vapply(x$schema$columns, function(c) c$name, "")
}

# Exported; see ?edpclient::popmods.
popmods <- function(pop) {
  stopifnot(is.population(pop))
  # `pop$models` is a list of #/definitions/population_model in edp.schema.json.
  plyr::ldply(pop$models, function(pm) data.frame(
      stringsAsFactors = FALSE,
      name = pm$name,
      id = pm$id,
      creation_time = format_utc_time(pm$creation_time),
      build_status = pm$build_progress$status))
}

latest_popmod <- function(pop) {
  stopifnot(is.population(pop))
  stopifnot(length(pop$models) > 0)
  latest <- order(vapply(pop$models, function(m) m$creation_time, NA_real_),
                  decreasing = TRUE)[1]
  popmod(pop$sess, pop$models[[latest]]$id)
}

delete_population <- function(pop) {
  stopifnot(is.population(pop))
  edp_delete(pop$sess, paste("rpc/population", URLencode(pop$pid), sep = "/"))
}

select <- function(x, target = NULL, where = NULL, rowids = NULL) {
  UseMethod("select", x)
}

select.edp_population <- function(x, target = NULL, where = NULL,
                                  rowids = NULL) {
  op <- paste("rpc/population", utils::URLencode(x$pid), "select", sep = "/")
  if (is.null(target)) {
    target <- Filter(function(n) !(n %in% names(where)), names(x))
  }
  if (!all(target %in% names(x))) {
    stop("Not all target columns are in population")
  }
  if (!all(names(where) %in% names(x))) {
    stop("Not all where columns are in population")
  }
  if (!is.null(rowids) && !is.numeric(rowids)) {
    stop("Invalid rowids.")
  }
  if (!is.null(rowids) && !is.null(where)) {
    stop("At most one of `rowids` and `where` may be set.")
  }
  # TODO(madeleine): Check that the where values are the right type?
  req <- list(target = as.list(target))
  if (!is.null(where)) {
    req[["where"]] <- where
  }
  if (!is.null(rowids)) {
    req[["rowids"]] <- rowids
  }
  resp <- httr::content(edp_post(x$sess, op, req))
  return(result_set_to_data_frame(resp, target, x$schema))
}
