context("check split")

test_that("split test",{
    expect_identical(structure(c("s", "", "d", "", "s", "", "d", "", "sds", ""), .Dim = c(2L, 5L)), re2_split_fixed(c("sdsdsds",NA),"",5))
    expect_identical(structure(c("s", "", "d", "", "s", "", "d", "", "sds", ""), .Dim = c(2L, 5L)), re2_psplit_fixed(c("sdsdsds",NA),"",5))
    expect_identical(structure(c("s", "", "d", "", "s", "", "d", "", "sds", ""), .Dim = c(2L, 5L)), re2_psplit_fixed(c("sdsdsds",NA),"",5, grain_size = 1))
    expect_identical(re2_psplit(c("sdsdsds",NA),""),list(c("s", "d", "s", "d", "s", "d", "s"), NA_character_))
    expect_identical(re2_psplit(c("sdsdsds",NA),"", grain_size = 1),list(c("s", "d", "s", "d", "s", "d", "s"), NA_character_))
    expect_identical(re2_split(c("sdsdsds",NA),""),list(c("s", "d", "s", "d", "s", "d", "s"), NA_character_))

    # split with char
    expect_identical(re2_psplit(c("sdsdsds",NA),"s", grain_size = 1), list(c("", "d", "d", "d", ""), NA_character_))
    expect_identical(re2_psplit_fixed(c("sdsdsds",NA),"s", grain_size = 1, part = 4), structure(c("", "", "d", "", "d", "", "ds", ""), .Dim = c(2L, 4L)))
    expect_identical(re2_split(c("sdsdsds",NA),"s"), list(c("", "d", "d", "d", ""), NA_character_))
    expect_identical(re2_split_fixed(c("sdsdsds",NA),"s", part = 4), structure(c("", "", "d", "", "d", "", "ds", ""), .Dim = c(2L, 4L)))
})

test_that("split error test",{
    expect_error(re2_split("sd",pattern = "s", part = numeric()),"need the number of pieces.")
})
