# Switch to testthat. https://github.com/empiricalsys/edpclient.R/issues/26
if (Sys.getenv("NOT_CRAN") != "true") {
  message("on CRAN, skipping")
  q("no")
}

library("edpclient")

YAXCATPEOPLE_PMID <- "pm-pdly225iffjtlzqq"

test_and_return_session <- function() {
  # Check that we can connect to EDP and list population models.
  profile <- ifelse(Sys.getenv("TRAVIS") == "true", "default", "testing")
  sess <- edp_session(profile = profile)
  stopifnot(is.edp_session(sess))
  print(sess)
  pms <- popmods(sess)
  print(pms)
  stopifnot(nrow(pms) > 1)
  sess
}

test_population_model <- function(sess) {
  # Check that we can create an edp_population_model for yaxcatpeople and fetch
  # its column names and schema.
  pm <- popmod(sess, YAXCATPEOPLE_PMID)
  stopifnot(is.popmod(pm))
  stopifnot(as.character(pm) == YAXCATPEOPLE_PMID)
  stopifnot("FUELHEAT" %in% names(pm))
  s <- schema(pm)
  h <- s[s$name == "FUELHEAT", ]
  stopifnot(h$display_name == "Home heating fuel")
  stopifnot(h$stat_type == "categorical")
  stopifnot(h$nvalues == 6)
}

test_select <- function(sess) {
  pm <- popmod(sess, YAXCATPEOPLE_PMID)
  # Check that we can select from a population model.
  d <- select(pm, target = c("FUELHEAT"))
  stopifnot(is.factor(d$FUELHEAT))
  stopifnot(display_name(d$FUELHEAT) == "Home heating fuel")
  stopifnot(stat_type(d$FUELHEAT) == "categorical")
  # Check that we can select a subset of rows.
  d <- select(pm, "FUELHEAT", where = list(CARPOOL = "1"))
  stopifnot(all(dim(d) == c(359, 1)))
  # Check that we detect bad column names in `target` and `where`.
  bad_target <- try(select(pm, "does not exist"), silent = TRUE)
  stopifnot(class(bad_target) == "try-error")
  bad_where <- try(select(pm, "FUELHEAT", where = list(`does not exist` = 0)),
                   silent = TRUE)
  stopifnot(class(bad_where) == "try-error")
  # Check that we can select all 271 columns, including unmodeled ones.
  d <- select(pm)
  stopifnot(all(dim(d) == c(1000, 271)))
  # Check that an unmodeled column is stored as a character vector.
  stopifnot(class(d$NSUBFAM) == "character")
  # Check that identifying columns are set on selected data frame.
  stopifnot(isTRUE(all.equal(identifying_columns(d), c("SERIAL", "CNTRY"))))
}

test_simulate <- function(sess) {
  pm <- popmod(sess, YAXCATPEOPLE_PMID)
  # Check that we can simulate from a population model.
  d <- simulate(pm, target = "FUELHEAT", nsim = 10)
  stopifnot(names(d) == "FUELHEAT")
  stopifnot(is.factor(d$FUELHEAT))
  stopifnot(length(d$FUELHEAT) == 10)
  # Check `columns` alias for `target`.
  d <- simulate(pm, columns = "FUELHEAT", nsim = 10)
  stopifnot(names(d) == "FUELHEAT")
  # Check that we can simulate all 246 modeled columns.
  d <- simulate(pm, nsim = 1)
  stopifnot(all(dim(d) == c(1, 246)))
  # Check that we can simulate conditionally.
  d <- simulate(pm, target = "FUELHEAT", given = list(CARPOOL = "1"), nsim = 10)
  stopifnot(nrow(d) == 10)
}

test_column_association <- function(sess) {
  pm <- popmod(sess, YAXCATPEOPLE_PMID)
  # Check that we can compute R-squared.
  r2 <- col_assoc(pm, c("VETSTAT", "VETSTATD"), statistic = "R squared")
  # Check that two related columns have high R-squared.
  r2v <- r2["VETSTAT", "VETSTATD"]
  stopifnot(0.3 < r2v && r2v < 1.0)
  # Check that the R-squared matrix is symmetric.
  stopifnot(isTRUE(all.equal(r2, t(r2))))
}

test_conditional_r2 <- function(sess) {
  pm <- popmod(sess, YAXCATPEOPLE_PMID)
  # Check that an indicator for race being white is informative about an
  # indicator for race being black, but after conditioning on RACE=1 (white),
  # it is much less so.
  r2_uncond <- col_assoc(pm, c("RACWHT", "RACBLK"), statistic = "R squared")
  r2_cond <- col_assoc(pm, c("RACWHT", "RACBLK"), given = list(RACE = "1"),
                       statistic = "R squared")
  stopifnot(r2_uncond["RACWHT", "RACBLK"] > 2 * r2_cond["RACWHT", "RACBLK"])
}

test_predict <- function(sess) {
  pm <- popmod(sess, YAXCATPEOPLE_PMID)
  inferred <- predict(pm, target = c("VETSTAT", "VETSTATD"),
                      rowids = seq(1, 20))
  selected <- select(pm, target = c("VETSTAT", "VETSTATD"), rowids = seq(1, 20))
  stopifnot(dim(inferred) == c(20, 2))
  # Make sure we inferred a missing value.
  stopifnot(is.na(selected["3", "VETSTAT"]))
  stopifnot(is.na(selected["3", "VETSTATD"]))
  stopifnot(inferred["3", "VETSTAT"] == 1)
  stopifnot(inferred["3", "VETSTATD"] == 11)
  # Make sure all values are inferred, and the inferred values match the
  # observed ones when they exist.
  stopifnot(all(!is.na(inferred)))
  stopifnot(all(is.na(selected) | (selected == inferred)))

  # Now try inferring present values.
  inferred <- predict(pm, target = c("VETSTAT", "VETSTATD"),
                      rowids = seq(1, 20), infer_present = TRUE)
  stopifnot(dim(inferred) == c(20, 2))
  # Check that we inferred values like we did without `infer_present`.
  stopifnot(inferred["3", "VETSTAT"] == 1)
  stopifnot(inferred["3", "VETSTATD"] == 11)
  # Check that not all inferred values match the observed values.
  stopifnot(all(!is.na(inferred)))
  stopifnot(!all(selected == inferred))

  # Test infer all
  inferred <- predict(pm, target = c("VETSTAT", "VETSTATD"))
  stopifnot(dim(inferred) == c(1000, 2))
}

test_create_population <- function(sess) {
  # Create a population with one column and three rows.
  d <- data.frame(x = c(-1, 2, 17))
  name <- sprintf("test_session_%d", round(runif(1, 0, 1000000)))
  pop <- create_population(sess, d, name)
  # Check that the population and be fetched by id.
  pop2 <- population(sess, as.character(pop))
  stopifnot(pop2$name == name)
  # Kick off the builds for two population models.
  pm_name <- paste(name, "_model", sep = "")
  pm1 <- build_popmod(pop, pm_name, iterations = 2)
  pm2 <- build_popmod(pop, pm_name, iterations = 2)
  stopifnot(pm1$parent_id == as.character(pop))
  # The build may or may not have started when we queried for its status.
  stopifnot(pm1$build_status %in% c("unbuilt", "in_progress", "built"))
  # Wait for the first population model, smoke-testing the progress bar.
  pm1 <- wait_for(pm1)
  stopifnot(pm1$build_status == "built")
  # Wait for the second population model quietly and confirm we can simulate
  # from it.
  pm2 <- wait_for(pm2, quiet = TRUE)
  stopifnot(length(simulate(pm2, target = "x", nsim = 10)$x) == 10)
  stopifnot(pm2$build_status == "built")
}

sess <- test_and_return_session()
test_population_model(sess)
test_select(sess)
test_simulate(sess)
test_column_association(sess)
test_conditional_r2(sess)
test_predict(sess)
test_create_population(sess)
