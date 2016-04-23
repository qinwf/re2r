context("Unicode")

test_that("unicode match",{
    x <- stringi::stri_conv("a\u6587bc", "UTF-8", "")
    expect_true(re2_match(x,"\u6587"))
})
