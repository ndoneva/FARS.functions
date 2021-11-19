context("make_filename")

#testing make_filename()
t1<- test_that("check the make_filename for 2013", expect_equal(make_filename(2013), "accident_2013.csv.bz2"))

t2<- test_that("check the make_filename for 2014", expect_equal(make_filename(2014), "accident_2014.csv.bz2"))

t3<- test_that("check the make_filename for 2015", expect_equal(make_filename(2015), "accident_2015.csv.bz2"))

t4<- test_that("check the make_filename for string", expect_equal(make_filename("abc"), "accident_NA.csv.bz2"))


