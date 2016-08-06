context("Unicode")
library(stringi)

test_that("unicode match with native string",{
    # the Unicode codepoint cannot be converted to destination encoding
    skip_on_os("windows")
    letters <- stri_c(stri_enc_fromutf32(list(174L,	173L,182L,190L)), collapse = "")
    x <- stri_encode(letters,"UTF-8","")
    expect_true(re2_detect(x,letters))
    expect_true(re2_detect(x,letters, parallel = T, grain_size = 1))
})

test_that("unicode match",{

    expect_identical(re2_detect(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"), c(TRUE,TRUE))
    expect_identical(re2_detect(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*", parallel = T, grain_size = 1), c(TRUE,TRUE))
})

library(stringi)

test_that("Chinese",{
    expect_true(re2_detect("A", "\\p{L}"));
    expect_true(re2_detect("A", "\\p{Lu}"));
    expect_true(!re2_detect("A", "\\p{Ll}"));
    expect_true(!re2_detect("A", "\\P{L}"));
    expect_true(!re2_detect("A", "\\P{Lu}"));
    expect_true(re2_detect("A", "\\P{Ll}"));

    tan = stri_enc_fromutf32(35674)
    expect_true(re2_detect(tan , "\\p{L}"));
    expect_true(!re2_detect(tan , "\\p{Lu}"));
    expect_true(!re2_detect(tan , "\\p{Ll}"));
    expect_true(!re2_detect(tan , "\\P{L}"));
    expect_true(re2_detect(tan , "\\P{Lu}"));
    expect_true(re2_detect(tan , "\\P{Ll}"));

    tan = stri_enc_fromutf32(27704)
    expect_true(re2_detect(tan , "\\p{L}"));
    expect_true(!re2_detect(tan , "\\p{Lu}"));
    expect_true(!re2_detect(tan , "\\p{Ll}"));
    expect_true(!re2_detect(tan , "\\P{L}"));
    expect_true(re2_detect(tan , "\\P{Lu}"));
    expect_true(re2_detect(tan , "\\P{Ll}"));

    tan = stri_enc_fromutf32(37586)
    expect_true(re2_detect(tan , "\\p{L}"));
    expect_true(!re2_detect(tan , "\\p{Lu}"));
    expect_true(!re2_detect(tan , "\\p{Ll}"));
    expect_true(!re2_detect(tan , "\\P{L}"));
    expect_true(re2_detect(tan , "\\P{Lu}"));
    expect_true(re2_detect(tan , "\\P{Ll}"));

    tan = stri_enc_fromutf32(c(65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 35674L, 27704L, 37586L))

    expect_identical(structure(c("ABC","A", "B", "C"), .Dim = c(1L, 4L), .Dimnames = list(NULL, c(".match",".1", ".2", ".3"))),re2_match(tan,"(.).*?(.).*?(.)"))
    expect_identical(structure(c("ABC","A", "B", "C"), .Dim = c(1L, 4L), .Dimnames = list(NULL, c(".match",".1", ".2", ".3"))),re2_match(tan,"(.).*?([\\p{L}]).*?(.)"))
    expect_identical(structure(c(tan,stri_enc_fromutf32(list( 35674L, 27704L, 37586L))), .Dim = c(1L, 4L), .Dimnames = list( NULL, c(".match",".1", ".2", ".3"))),re2_match(tan,".*(.).*?([\\p{Lu}\\p{Lo}]).*?(.)"))
})
