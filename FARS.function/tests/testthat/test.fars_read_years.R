context("fars_read_years")

#testing fars_read_years()
t1<- test_that("return a list of FARS data for the vector of years", expect_is(fars_read_years(c(2013, 2014, 2015)), "list"))
