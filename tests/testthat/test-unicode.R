context("Unicode")
library(stringi)

test_that("unicode match with native string",{
    letters <- stri_c(stri_enc_fromutf32(list(119L, 120L, 121L, 122L, 324L, 243L)), collapse = "")
    x <- enc2native(letters)
    expect_true(re2_detect(x,letters))
    expect_true(re2_pdetect(x,letters))
})

test_that("unicode match",{
    expect_identical(re2_detect(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"), c(TRUE,TRUE))
    expect_identical(re2_pdetect(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"), c(TRUE,TRUE))
})
