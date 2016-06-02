context("operators")

test_that("test operators",{
    regexp %<~% "(12)"
    expect_true(re2_detect("12",regexp))
    expect_identical(get_pattern(regexp),"(12)")
    expect_true(!("12" %!~% regexp))
    expect_true(  "12" %=~% regexp)
})
