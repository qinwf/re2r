context("check extract")

test_that("check rewrite", {
    # from re2_test.cc
    expect_identical(re2_extract("boris@kremvax.ru", "(.*)@([^.]*)"),"boris@kremvax")
    expect_identical(re2_pextract("boris@kremvax.ru", "(.*)@([^.]*)"),"boris@kremvax")

    expect_identical(re2_extract("foo", ".*"),"foo")
    expect_identical(re2_pextract("foo", ".*"),"foo")
    expect_identical(re2_pextract(c("foo","foo"), ".*",grain_size = 1),c("foo","foo"))

    expect_true(is.na(re2_extract("baz", "bar")))
    expect_true(is.na(re2_pextract("baz", "bar")))

    expect_identical(re2_extract(c("baz", "bar",NA),c("bar")),c(NA, "bar", NA))
    expect_identical(re2_pextract(c("baz", "bar",NA),c("bar")),c(NA, "bar", NA))
    expect_identical(re2_pextract(c("baz", "bar",NA),c("bar"), grain_size = 1),c(NA, "bar", NA))

    expect_identical(re2_extract_all(c("baz", "barxbar_sbar bar",NA),c("bar")), list(NULL, c("bar", "bar", "bar", "bar"), NULL))
    expect_identical(re2_pextract_all(c("baz", "barxbar_sbar bar",NA),c("bar")), list(NULL, c("bar", "bar", "bar", "bar"), NULL))
    expect_identical(re2_pextract_all(c("baz", "barxbar_sbar bar",NA),c("bar"), grain_size = 1), list(NULL, c("bar", "bar", "bar", "bar"), NULL))

})
