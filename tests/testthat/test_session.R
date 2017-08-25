test_that("basic edp_session operations work", {
  skip_on_cran()
  expect_true(is.edp_session(SESS))
  print(SESS)
})

test_that("we can work with population models", {
  skip_on_cran()
  # Check that we can create an edp_population_model for yaxcatpeople and fetch
  # its column names and schema.
  pm <- popmod(SESS, YAXCATPEOPLE_PMID)
  expect_true(is.popmod(pm))
  expect_equal(as.character(pm), YAXCATPEOPLE_PMID)
  expect_true("FUELHEAT" %in% names(pm))
  s <- schema(pm)
  h <- s[s$name == "FUELHEAT", ]
  expect_equal(h$display_name, "Home heating fuel")
  expect_equal(h$stat_type, "categorical")
  expect_equal(h$nvalues, 6)
})

test_that("we can select from yaxcatpeople", {
  skip_on_cran()
  pm <- popmod(SESS, YAXCATPEOPLE_PMID)
  # Check that we can select from a population model.
  d <- select(pm, target = c("FUELHEAT"))
  expect_s3_class(d$FUELHEAT, "factor")
  expect_equal(display_name(d$FUELHEAT), "Home heating fuel")
  expect_equal(stat_type(d$FUELHEAT), "categorical")
  # Check that the result is the same if we select from the parent population.
  parent_pop <- population(SESS, pm$parent_id)
  d_pop <- select(parent_pop, target = c("FUELHEAT"))
  expect_equal(d, d_pop)
  # Check that we can select a subset of rows.
  d <- select(pm, "FUELHEAT", where = list(CARPOOL = "1"))
  expect_equal(dim(d), c(359, 1))
  # Check that we detect bad column names in `target` and `where`.
  expect_error(select(pm, "does not exist"),
               "Not all target columns are in population model")
  expect_error(select(pm, "FUELHEAT", where = list(`does not exist` = 0)),
               "Not all where columns are in population model")
  # Check that we can select all 271 columns, including unmodeled ones.
  d <- select(pm)
  expect_equal(dim(d), c(1000, 271))
  # Check that an unmodeled column is stored as a character vector.
  expect_is(d$NSUBFAM, "character")
  # Check that not all values are present.
  expect_true(anyNA(d$NSUBFAM))
  # Check that identifying columns are set on selected data frame.
  expect_equal(identifying_columns(d), c("SERIAL", "CNTRY"))
})

test_that("we can simulate from yaxcatpeople", {
  skip_on_cran()
  pm <- popmod(SESS, YAXCATPEOPLE_PMID)
  # Check that we can simulate from a population model.
  d <- simulate(pm, target = "FUELHEAT", nsim = 10)
  expect_equal(names(d), "FUELHEAT")
  expect_s3_class(d$FUELHEAT, "factor")
  expect_length(d$FUELHEAT, 10)
  # Check `columns` alias for `target`.
  d <- simulate(pm, columns = "FUELHEAT", nsim = 10)
  expect_equal(names(d), "FUELHEAT")
  # Check that we can simulate all 246 modeled columns.
  d <- simulate(pm, nsim = 1)
  expect_equal(dim(d), c(1, 246))
  # Check that we can simulate conditionally.
  d <- simulate(pm, target = "FUELHEAT", given = data.frame(CARPOOL = "1"),
                nsim = 10)
  expect_equal(nrow(d), 10)
})

test_that("we can compute column association on yaxcatpeople", {
  skip_on_cran()
  pm <- popmod(SESS, YAXCATPEOPLE_PMID)
  # Check that we can compute R-squared.
  r2 <- col_assoc(pm, c("VETSTAT", "VETSTATD"), statistic = "R squared")
  # Check that two related columns have high R-squared.
  r2v <- r2["VETSTAT", "VETSTATD"]
  expect_lt(0.3, r2v)
  expect_lt(r2v, 1.0)
  # Check that the R-squared matrix is symmetric.
  expect_equal(r2, t(r2))
})

test_that("conditional R^2 is reasonable", {
  skip_on_cran()
  pm <- popmod(SESS, YAXCATPEOPLE_PMID)
  # Check that an indicator for race being white is informative about an
  # indicator for race being black, but after conditioning on RACE=1 (white),
  # it is much less so.
  r2_uncond <- col_assoc(pm, c("RACWHT", "RACBLK"), statistic = "R squared")
  r2_cond <- col_assoc(pm, c("RACWHT", "RACBLK"), given = list(RACE = "1"),
                       statistic = "R squared")
  expect_gt(r2_uncond["RACWHT", "RACBLK"], 2 * r2_cond["RACWHT", "RACBLK"])
})

test_that("predictions work as expected", {
  skip_on_cran()
  pm <- popmod(SESS, YAXCATPEOPLE_PMID)
  inferred <- predict(pm, target = c("VETSTAT", "VETSTATD"),
                      rowids = seq(1, 20))
  selected <- select(pm, target = c("VETSTAT", "VETSTATD"), rowids = seq(1, 20))
  expect_equal(dim(inferred), c(20, 2))
  # Make sure we inferred a missing value.
  expect_true(is.na(selected["3", "VETSTAT"]))
  expect_true(is.na(selected["3", "VETSTATD"]))
  expect_equal(as.character(inferred["3", "VETSTAT"]), "1")
  expect_equal(as.character(inferred["3", "VETSTATD"]), "11")
  # Make sure all values are inferred, and the inferred values match the
  # observed ones when they exist.
  expect_true(all(!is.na(inferred)))
  expect_true(all(is.na(selected) | (selected == inferred)))

  # Now try inferring present values.
  inferred <- predict(pm, target = c("VETSTAT", "VETSTATD"),
                      rowids = seq(1, 20), infer_present = TRUE)
  expect_equal(dim(inferred), c(20, 2))
  # Check that we inferred values like we did without `infer_present`.
  expect_equal(as.character(inferred["3", "VETSTAT"]), "1")
  expect_equal(as.character(inferred["3", "VETSTATD"]), "11")
  # Check that not all inferred values match the observed values.
  expect_true(all(!is.na(inferred)))
  expect_false(all(selected == inferred))

  # Test infer all
  inferred <- predict(pm, target = c("VETSTAT", "VETSTATD"))
  expect_equal(dim(inferred), c(1000, 2))
})

(function() {

  pop <- NULL
  pm1 <- NULL
  pm2 <- NULL

  test_that("populations and models can be created", {
    skip_on_cran()
    # Create a population with one column and three rows.
    d <- data.frame(x = c(-1, 2, 17))
    name <- sprintf("test_session_%d", round(runif(1, 0, 1000000)))
    pop <<- create_population(SESS, d, name)
    # Check that the population can be fetched by id.
    pop2 <- population(SESS, as.character(pop))
    expect_equal(pop2$name, name)
    # Kick off the builds for two population models.
    pm_name <- paste(name, "_model", sep = "")
    pm1 <<- build_popmod(pop, pm_name, iterations = 2)
    pm2 <<- build_popmod(pop, pm_name, iterations = 2)
    expect_equal(pm1$parent_id, as.character(pop))
  })

  test_that("we can wait for populations to build", {
    skip_on_cran()
    if (is.null(pm1) || is.null(pm2)) {
      skip("population models not created, skipping")
    }
    # The build may or may not have started when we queried for its status.
    expect_true(pm1$build_status %in% c("unbuilt", "in_progress", "built"))
    # Wait for the first population model, smoke-testing the progress bar.
    pm1 <- wait_for(pm1)
    expect_equal(pm1$build_status, "built")
    # Wait for the second population model quietly and confirm we can simulate
    # from it.
    pm2 <- wait_for(pm2, quiet = TRUE)
    expect_length(simulate(pm2, target = "x", nsim = 10)$x, 10)
    expect_equal(pm2$build_status, "built")
  })

  test_that("visibility can be queried and changed", {
    skip_on_cran()
    # Test that the population has default visibility.
    locked_down_viz <- list(owner = SESS$username, public = FALSE,
                            reader_domains = character(0),
                            readers = character(0))
    expect_equal(locked_down_viz, visibility(pop))
    expect_equal(locked_down_viz, visibility(pm1))
    # Add readers to the population and a population model.
    add_reader(pop, "test1@empirical.com")
    add_reader(pm1, "test2@empirical.com")
    add_reader(pop, reader_domain = "test.empirical.com")
    # Check that the population is visible to everyone added, including by way
    # of the population model.
    visible_viz <- list(
        owner = SESS$username, public = FALSE,
        reader_domains = "test.empirical.com",
        readers = c("test1@empirical.com", "test2@empirical.com"))
    expect_equal(visible_viz, visibility(pop))
    expect_equal(visible_viz, visibility(pm1))
  })

  test_that("the population can be deleted", {
    skip_on_cran()
    delete_population(pop)
    expect_error(population(SESS, as.character(pop)), "HTTP status 404")
  })

})()
