context("test locate")

test_that("re2_locate",{
    expect_identical( re2_plocate_all(c(NA,"sd"),"sd"),list(structure(c(NA_integer_, NA_integer_), .Dim = 1:2, .Dimnames = list(NULL, c("start", "end"))), structure(1:2, .Dim = 1:2, .Dimnames = list(NULL, c("start", "end")))))
    expect_identical( re2_locate_all(c(NA,"sd"),"sd"),re2_plocate_all(c(NA,"sd"),"sd"))
    expect_identical( re2_locate_all(c(NA,"sd"),"sd"),re2_plocate_all(c(NA,"sd"),"sd", grain_size = 1))

    expect_identical( re2_locate(c(NA,"sd"),"sd"),structure(c(NA, 1L, NA, 2L), .Dim = c(2L, 2L), .Dimnames = list(NULL, c("start", "end"))))
    expect_identical( re2_locate(c(NA,"sd"),"sd"),re2_plocate(c(NA,"sd"),"sd"))
    expect_identical( re2_locate(c(NA,"sd"),"sd"),re2_plocate(c(NA,"sd"),"sd", grain_size = 1))


    res_2 = re2_locate_all(c(NA,"sds"),"sd")

    expect_identical(sub_string("sds",res_2[[2]]),"sd")
    expect_identical(sub_string("sds",1,2),"sd")
})

test_that("test locate NA",{

    # NULL
    expect_identical(re2_locate(NULL,pattern = "sd"),structure(integer(0), .Dim = c(0L, 2L), .Dimnames = list(NULL, c("start", "end"))))

    # re2_locate_all

    expect_identical(re2_locate_all(c("as","as", NA),pattern = "sd"), list(
        structure(integer(0), .Dim = c(0L, 2L), .Dimnames = list(NULL, c("start", "end"))),
        structure(integer(0), .Dim = c(0L, 2L), .Dimnames = list(NULL, c("start", "end"))),
        structure(c(NA_integer_, NA_integer_), .Dim = 1:2, .Dimnames = list(NULL, c("start", "end")))
        )
        )

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
                               .Dim = c(3L, 2L), .Dimnames = list(NULL, c("start", "end"))))

    expect_identical(
        re2_locate(c("as","as", NA),pattern = "sd"),
        re2_plocate(c("as","as", NA),pattern = "sd"))

    expect_identical(
        re2_locate(c("as","as", NA),pattern = "sd"),
        re2_plocate(c("as","as", NA),pattern = "sd", grain_size = 1))

})

test_that("test locate empty string",{

    # NULL
    expect_identical(re2_locate(NULL,pattern = ""),structure(integer(0), .Dim = c(0L, 2L), .Dimnames = list(NULL, c("start", "end"))))

    # re2_locate_all

    expect_identical(re2_locate_all(c("as","as", NA),pattern = ""),
                     list(
                         structure(c(1L, 2L, 1L, 2L), .Dim = c(2L, 2L), .Dimnames = list( NULL, c("start", "end"))),
                         structure(c(1L, 2L, 1L, 2L), .Dim = c(2L, 2L), .Dimnames = list(NULL, c("start", "end"))),
                         structure(c(NA_integer_, NA_integer_), .Dim = 1:2, .Dimnames = list(NULL, c("start", "end" ))
                                   )
                         )
                     )

    expect_identical(
        re2_locate_all(c("as","as", NA),pattern = ""),
        re2_plocate_all(c("as","as", NA),pattern = ""))

    expect_identical(
        re2_locate_all(c("as","as", NA),pattern = ""),
        re2_plocate_all(c("as","as", NA),pattern = "", grain_size = 1))


    # re2_locate

    expect_identical(re2_locate(c("as","as", NA),pattern = ""),
                     structure(
                         c(1L, 1L, NA, 1L, 1L, NA), .Dim = c(3L, 2L),
                         .Dimnames = list(NULL, c("start", "end"))))

    expect_identical(
        re2_locate(c("as","as", NA),pattern = ""),
        re2_plocate(c("as","as", NA),pattern = ""))

    expect_identical(
        re2_locate(c("as","as", NA),pattern = ""),
        re2_plocate(c("as","as", NA),pattern = "", grain_size = 1))

})

test_that("test locate $",{

    # NULL
    expect_identical(re2_locate(NULL,pattern = "$"),structure(integer(0), .Dim = c(0L, 2L), .Dimnames = list(NULL, c("start", "end"))))

    # re2_locate_all

    expect_identical(re2_locate_all(c("as","as", NA),pattern = "$"),
                     list(
                         structure(c(2L, 1L), .Dim = 1:2, .Dimnames = list(NULL, c("start", "end"))),
                         structure(c(2L, 1L), .Dim = 1:2, .Dimnames = list(NULL, c("start", "end"))),
                         structure(c(NA_integer_, NA_integer_), .Dim = 1:2, .Dimnames = list(NULL, c("start", "end")))
                         )
    )

    expect_identical(
        re2_locate_all(c("as","as", NA),pattern = "$"),
        re2_plocate_all(c("as","as", NA),pattern = "$"))

    expect_identical(
        re2_locate_all(c("as","as", NA),pattern = "$"),
        re2_plocate_all(c("as","as", NA),pattern = "$", grain_size = 1))


    # re2_locate

    expect_identical(re2_locate(c("as","as", NA),pattern = "$"),
                     structure(c(3L, 3L, NA, 2L, 2L, NA), .Dim = c(3L, 2L),
                               .Dimnames = list(NULL, c("start", "end"))))

    expect_identical(
        re2_locate(c("as","as", NA),pattern = "$"),
        re2_plocate(c("as","as", NA),pattern = "$"))

    expect_identical(
        re2_locate(c("as","as", NA),pattern = "$"),
        re2_plocate(c("as","as", NA),pattern = "$", grain_size = 1))

    expect_identical(re2_locate(c("as","as", NA),pattern = "^"),structure(c(1L, 1L, NA, 0L, 0L, NA), .Dim = c(3L, 2L), .Dimnames = list(NULL, c("start", "end"))))

})


test_that("test sub_string",{
    sds = "sds"
    sub_string(sds,1,2) <- "ab"

    sds = "sds"
    sub_string(sds, from = matrix(c(1,2), ncol = 2)) <- "ab"
    expect_identical(sds, "abs")
})


