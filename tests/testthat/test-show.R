context("test show regex")

test_that("test show_regex",{
    show_regex(re2("(?P<testname>>this)( is)(?P<testname>>this)"))
    expect_error(show_regex(list()))
})
