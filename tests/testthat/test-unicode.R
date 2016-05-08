context("Unicode")
library(stringi)

test_that("unicode match with native string",{
    # the Unicode codepoint cannot be converted to destination encoding
    skip_on_os("windows")
    letters <- stri_c(stri_enc_fromutf32(list(174L,	173L,182L,190L)), collapse = "")
    x <- stri_encode(letters,"UTF-8","")
    expect_true(re2_detect(x,letters))
    expect_true(re2_pdetect(x,letters))
})

test_that("unicode match",{

    expect_identical(re2_detect(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"), c(TRUE,TRUE))
    expect_identical(re2_pdetect(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"), c(TRUE,TRUE))
})
