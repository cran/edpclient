# Switch to testthat. https://github.com/empiricalsys/edpclient.R/issues/26
if (Sys.getenv("NOT_CRAN") != "true") {
  message("on CRAN, skipping")
  q("no")
}

library("edpclient")

GSS3_PID <- "p-c24kcon3pl4xiph7"
GSS3_PMID <- "pm-ezstu6pmcppn4ghc"

test_populations <- function(sess) {
  pops <- populations(sess)
  p <- pops[pops$id == GSS3_PID, ]
  stopifnot(nrow(p) == 1)
  stopifnot(p$name == "gss3")
  stopifnot(p$nmodels >= 3)
}

test_population_object <- function(sess) {
  p <- population(sess, GSS3_PID)
  stopifnot(is.population(p))
  stopifnot(!is.population("not a population"))
  stopifnot(as.character(p) == GSS3_PID)
  stopifnot(p$name == "gss3")
}

test_population_model_object <- function(sess) {
  pm <- popmod(sess, GSS3_PMID)
  stopifnot(is.popmod(pm))
  stopifnot(!is.popmod("not a population model"))
  stopifnot(as.character(pm) == GSS3_PMID)
  stopifnot(pm$name == "gss3m")
  stopifnot(pm$id == GSS3_PMID)
  stopifnot(pm$parent_id == GSS3_PID)
  stopifnot(pm$build_status == "built")
}

test_population_popmods <- function(sess) {
  pms <- popmods(population(sess, GSS3_PID))
  stopifnot(nrow(pms) >= 3)
  stopifnot(all(pms$parent_id == GSS3_PID))
  stopifnot(all(startsWith(pms$id, "pm-")))
}

profile <- ifelse(Sys.getenv("TRAVIS") == "true", "default", "testing")
sess <- edp_session(profile = profile)
test_populations(sess)
test_population_object(sess)
test_population_model_object(sess)
test_population_popmods(sess)
