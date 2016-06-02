context("test locate")

test_that("re2_locate",{
    expect_identical( re2_plocate_all(c(NA,"sd"),"sd"),list(structure(c(NA_integer_, NA_integer_), .Dim = 1:2), structure(1:2, .Dim = 1:2)))
    expect_identical( re2_locate_all(c(NA,"sd"),"sd"),re2_plocate_all(c(NA,"sd"),"sd"))

    expect_identical( re2_locate(c(NA,"sd"),"sd"),structure(c(NA, 1L, NA, 2L), .Dim = c(2L, 2L)))
    expect_identical( re2_locate(c(NA,"sd"),"sd"),re2_plocate(c(NA,"sd"),"sd"))


    res_2 = re2_locate_all(c(NA,"sds"),"sd")

    expect_identical(sub_string("sds",res_2[[2]]),"sd")
    expect_identical(sub_string("sds",1,2),"sd")
})

test_that("test sub_string",{
    sds = "sds"
    sub_string(sds,1,2) <- "ab"

    sds = "sds"
    sub_string(sds, from = matrix(c(1,2), ncol = 2)) <- "ab"
    expect_identical(sds, "abs")
})


