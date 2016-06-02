context("check quote")

test_that("quote", {
    # from re2_test.cc
    expect_identical(quote_meta("foo.bar"), "foo\\.bar")
    expect_identical(quote_meta("((a|b)c?d*e+[f-h]i)"),"\\(\\(a\\|b\\)c\\?d\\*e\\+\\[f\\-h\\]i\\)")
    expect_identical(quote_meta("foo.bar",parallel = T), "foo\\.bar")
    expect_identical(quote_meta("((a|b)c?d*e+[f-h]i)", parallel = T),"\\(\\(a\\|b\\)c\\?d\\*e\\+\\[f\\-h\\]i\\)")

    expect_identical(quote_meta(c("((a|b)c?d*e+[f-h]i)","((a|b)c?d*e+[f-h]i)"), parallel = T, grain_size = 1),c("\\(\\(a\\|b\\)c\\?d\\*e\\+\\[f\\-h\\]i\\)", "\\(\\(a\\|b\\)c\\?d\\*e\\+\\[f\\-h\\]i\\)"))
})

