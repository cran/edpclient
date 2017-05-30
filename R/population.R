population <- function(sess, pid) {
  stopifnot(is.edp_session(sess))
  stopifnot(is.character(pid) && length(pid) == 1 && startsWith(pid, "p-"))

  op <- paste("rpc/population", URLencode(pid), sep = "/")
  resp <- httr::content(edp_get(sess, op))
  pmids <- vapply(resp$models, function(x) x$id, NA_character_)

  x <- list(sess = sess, name = resp$name, pid = pid, pmids = pmids)
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
              length(x$pmids)))
}

popmods.edp_population <- function(x) {
  pms <- popmods(x$sess)
  pms[pms$id %in% x$pmids, ]
}

delete_population <- function(pop) {
  stopifnot(is.population(pop))
  edp_delete(pop$sess, paste("rpc/population", URLencode(pop$pid), sep = "/"))
}
