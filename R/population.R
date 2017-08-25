population <- function(sess, pid) {
  stopifnot(is.edp_session(sess))
  stopifnot(is.character(pid) && length(pid) == 1 && startsWith(pid, "p-"))

  op <- paste("rpc/population", URLencode(pid), sep = "/")
  resp <- httr::content(edp_get(sess, op))
  x <- list(sess = sess, name = resp$name, pid = pid, models = resp$models)
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
  stopifnot(is.population(x))
  select(latest_popmod(x), target = target, where = where, rowids = rowids)
}
