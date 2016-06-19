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

test_that("vectorize extract",{
    extract_list = list(
        list(c("baz","bar"), "bar",c(NA,"bar")),
        list(c("baz","bar"), c("baz","bar"),c("baz","bar")),
        list(c("baz"), c("baz","bar"),c("baz",NA))
    )

    for (ind in extract_list){
        expect_identical(re2_extract(ind[[1]], ind[[2]]), ind[[3]])
        expect_identical(re2_pextract(ind[[1]], ind[[2]]), ind[[3]])
        expect_identical(re2_pextract(ind[[1]], ind[[2]],grain_size = 1), ind[[3]])
    }

    expect_warning(re2_extract(c("sd","ab","cd"), c("ab","af")))
    expect_warning(re2_pextract(c("sd","ab","cd"), c("ab","af")))
    expect_warning(re2_pextract(c("sd","ab","cd"), c("ab","af"), grain_size = 1))
})
