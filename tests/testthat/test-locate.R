context("test locate")

test_that("re2_locate",{
    expect_identical( re2_plocate_all(c(NA,"sd"),"sd"),list(structure(c(NA_integer_, NA_integer_), .Dim = 1:2), structure(1:2, .Dim = 1:2)))
    expect_identical( re2_locate_all(c(NA,"sd"),"sd"),re2_plocate_all(c(NA,"sd"),"sd"))

    expect_identical( re2_locate(c(NA,"sd"),"sd"),structure(c(NA, 1L, NA, 2L), .Dim = c(2L, 2L)))
    expect_identical( re2_locate(c(NA,"sd"),"sd"),re2_plocate(c(NA,"sd"),"sd"))
})


