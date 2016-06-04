context("test locate")

test_that("re2_locate",{
    expect_identical( re2_plocate_all(c(NA,"sd"),"sd"),list(structure(c(NA_integer_, NA_integer_), .Dim = 1:2), structure(1:2, .Dim = 1:2)))
    expect_identical( re2_locate_all(c(NA,"sd"),"sd"),re2_plocate_all(c(NA,"sd"),"sd"))
    expect_identical( re2_locate_all(c(NA,"sd"),"sd"),re2_plocate_all(c(NA,"sd"),"sd", grain_size = 1))

    expect_identical( re2_locate(c(NA,"sd"),"sd"),structure(c(NA, 1L, NA, 2L), .Dim = c(2L, 2L)))
    expect_identical( re2_locate(c(NA,"sd"),"sd"),re2_plocate(c(NA,"sd"),"sd"))
    expect_identical( re2_locate(c(NA,"sd"),"sd"),re2_plocate(c(NA,"sd"),"sd", grain_size = 1))


    res_2 = re2_locate_all(c(NA,"sds"),"sd")

    expect_identical(sub_string("sds",res_2[[2]]),"sd")
    expect_identical(sub_string("sds",1,2),"sd")
})

test_that("test locate NA",{

    # NULL
    expect_identical(re2_locate(NULL,pattern = "sd"),structure(integer(0), .Dim = c(0L, 2L)))

    # re2_locate_all

    expect_identical(re2_locate_all(c("as","as", NA),pattern = "sd"), list(structure(integer(0), .Dim = c(0L, 2L)), structure(integer(0), .Dim = c(0L, 2L)), structure(c(NA_integer_, NA_integer_), .Dim = 1:2)))

    expect_identical(
        re2_locate_all(c("as","as", NA),pattern = "sd"),
        re2_plocate_all(c("as","as", NA),pattern = "sd"))

    expect_identical(
        re2_locate_all(c("as","as", NA),pattern = "sd"),
        re2_plocate_all(c("as","as", NA),pattern = "sd", grain_size = 1))


    # re2_locate

    expect_identical(re2_locate(c("as","as", NA),pattern = "sd"),
                     structure(c(NA_integer_, NA_integer_,
                                 NA_integer_, NA_integer_,
                                 NA_integer_, NA_integer_),
                               .Dim = c(3L, 2L)))

    expect_identical(
        re2_locate(c("as","as", NA),pattern = "sd"),
        re2_plocate(c("as","as", NA),pattern = "sd"))

    expect_identical(
        re2_locate(c("as","as", NA),pattern = "sd"),
        re2_plocate(c("as","as", NA),pattern = "sd", grain_size = 1     ))

})


test_that("test sub_string",{
    sds = "sds"
    sub_string(sds,1,2) <- "ab"

    sds = "sds"
    sub_string(sds, from = matrix(c(1,2), ncol = 2)) <- "ab"
    expect_identical(sds, "abs")
})


