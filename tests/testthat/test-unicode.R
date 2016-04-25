context("Unicode")

test_that("unicode match with native string",{
    skip_on_os("windows")
    # Windows cannot convert this string without Chinese locale setting.
    # warning: the Unicode codepoint \U00006587 cannot be converted to destination encoding
    x <- stringi::stri_conv("a\u6587bc", "UTF-8", "")
    expect_true(re2_match(x,"\u6587"))
})

test_that("unicode match",{
    expect_identical(re2_match(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"), c(TRUE,TRUE))
})
