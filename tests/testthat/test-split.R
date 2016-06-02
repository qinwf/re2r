context("check split")

test_that("split test",{
    expect_identical(structure(c("s", "", "d", "", "s", "", "d", "", "sds", ""), .Dim = c(2L, 5L)), re2_split_fixed(c("sdsdsds",NA),"",5))
    expect_identical(structure(c("s", "", "d", "", "s", "", "d", "", "sds", ""), .Dim = c(2L, 5L)), re2_psplit_fixed(c("sdsdsds",NA),"",5))
    expect_identical(structure(c("s", "", "d", "", "s", "", "d", "", "sds", ""), .Dim = c(2L, 5L)), re2_psplit_fixed(c("sdsdsds",NA),"",5, grain_size = 1))
    expect_identical(re2_psplit(c("sdsdsds",NA),""),list(c("s", "d", "s", "d", "s", "d", "s"), NA_character_))
    expect_identical(re2_psplit(c("sdsdsds",NA),"", grain_size = 1),list(c("s", "d", "s", "d", "s", "d", "s"), NA_character_))
    expect_identical(re2_split(c("sdsdsds",NA),""),list(c("s", "d", "s", "d", "s", "d", "s"), NA_character_))
})
