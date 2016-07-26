context("test locate")

test_that("re2_locate", {
    tta = function(string,pattern,res, rep_res = rep(res,1000), parallel_rep = FALSE){
        expect_identical(re2_locate_all(string, pattern),res)
        expect_identical(re2_locate_all(string, pattern, parallel = T), res)
        expect_identical(re2_locate_all(string, pattern, parallel = T, grain_size = 1), res)
        if(parallel_rep){
            expect_identical(re2_locate_all(rep(string,1000), pattern, parallel = T, grain_size = 1), rep_res)
        }
    }
    tt = function(string,pattern,res, rep_res = NULL, parallel_rep = FALSE){
        expect_identical(re2_locate(string, pattern),res)
        expect_identical(re2_locate(string, pattern, parallel = T), res)
        expect_identical(re2_locate(string, pattern, parallel = T, grain_size = 1), res)
        if(parallel_rep){
            expect_identical(re2_locate(rep(string,1000), pattern, parallel = T, grain_size = 1), rep_res)
        }
    }
    res = list(
        structure(
            c(NA_integer_, NA_integer_),
            .Dim = 1:2,
            .Dimnames = list(NULL, c("start", "end"))
        ),
        structure(1:2, .Dim = 1:2, .Dimnames = list(NULL, c("start", "end")))
    )
    tta(c(NA, "sd"), "sd", res)

    res = structure(
        c(NA, 1L, NA, 2L),
        .Dim = c(2L, 2L),
        .Dimnames = list(NULL, c("start", "end"))
    )
    tt(c(NA, "sd"), "sd", res)


    res_2 = re2_locate_all(c(NA, "sds"), "sd")

    expect_identical(sub_string("sds", res_2[[2]]), "sd")
    expect_identical(sub_string("sds", 1, 2), "sd")

    # NULL
    res = structure(
        integer(0),
        .Dim = c(0L, 2L),
        .Dimnames = list(NULL, c("start", "end"))
    )
    tt(NULL, pattern = "sd", res)

    # re2_locate_all
    res = list(
        structure(
            integer(0),
            .Dim = c(0L, 2L),
            .Dimnames = list(NULL, c("start", "end"))
        ),
        structure(
            integer(0),
            .Dim = c(0L, 2L),
            .Dimnames = list(NULL, c("start", "end"))
        ),
        structure(
            c(NA_integer_, NA_integer_),
            .Dim = 1:2,
            .Dimnames = list(NULL, c("start", "end"))
        )
    )
    tta(c("as", "as", NA), pattern = "sd", res)

    # re2_locate
    res =  structure(
        c(
            NA_integer_,
            NA_integer_,
            NA_integer_,
            NA_integer_,
            NA_integer_,
            NA_integer_
        ),
        .Dim = c(3L, 2L),
        .Dimnames = list(NULL, c("start", "end"))
    )
    tt(c("as", "as", NA), pattern = "sd", res)

    # test locate empty string
    # NULL
    res = structure(
        integer(0),
        .Dim = c(0L, 2L),
        .Dimnames = list(NULL, c("start", "end"))
    )
    tt(NULL, pattern = "", res)

    # re2_locate_all
    res = list(
        structure(
            c(0L, 1L,-1L, 0L),
            .Dim = c(2L, 2L),
            .Dimnames = list(NULL, c("start", "end"))
        ),
        structure(
            c(0L, 1L,-1L, 0L),
            .Dim = c(2L,
                     2L),
            .Dimnames = list(NULL, c("start", "end"))
        ),
        structure(
            c(NA_integer_,
              NA_integer_),
            .Dim = 1:2,
            .Dimnames = list(NULL, c("start", "end"))
        )
    )
    tta(c("as", "as", NA), pattern = "", res)


    # re2_locate
    res = structure(
        c(1L, 1L, NA, 0L, 0L, NA),
        .Dim = c(3L, 2L),
        .Dimnames = list(NULL, c("start", "end"))
    )
    tt(c("as", "as", NA), pattern = "", res)

    # "\\P{M}" word bound
    res = structure(
        c(1L, 1L, NA, 0L, 0L, NA),
        .Dim = c(3L, 2L),
        .Dimnames = list(NULL, c("start", "end"))
    )
    res = list(structure(c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L,
                           6L), .Dim = c(6L, 2L), .Dimnames = list(NULL, c("start", "end"
                           ))),
               structure(c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L),
                         .Dim = c(5L, 2L), .Dimnames = list(NULL, c("start", "end"))),
               structure(c(NA_integer_, NA_integer_),
                         .Dim = 1:2, .Dimnames = list(NULL, c("start", "end"))))
    tta(c("asasd%", "\\P{M}", NA), pattern = "\\P{M}", res)

    # test locate $
    # NULL
    res = structure(
        integer(0),
        .Dim = c(0L, 2L),
        .Dimnames = list(NULL, c("start", "end"))
    )
    tt(NULL, pattern = "$", res)

    # re2_locate_all
    res = list(
        structure(
            c(3L, 2L),
            .Dim = 1:2,
            .Dimnames = list(NULL, c("start", "end"))
        ),
        structure(
            c(3L, 2L),
            .Dim = 1:2,
            .Dimnames = list(NULL, c("start", "end"))
        ),
        structure(
            c(NA_integer_, NA_integer_),
            .Dim = 1:2,
            .Dimnames = list(NULL, c("start", "end"))
        )
    )
    tta(c("as", "as", NA), pattern = "$", res)

    # re2_locate
    res = structure(
        c(3L, 3L, NA, 2L, 2L, NA),
        .Dim = c(3L, 2L),
        .Dimnames = list(NULL, c("start", "end"))
    )
    tt(c("as", "as", NA), pattern = "$", res)

    res = structure(
        c(1L, 1L, NA, 0L, 0L, NA),
        .Dim = c(3L, 2L),
        .Dimnames = list(NULL, c("start", "end"))
    )
    tt(c("as", "as", NA), pattern = "^", res)

})


test_that("test sub_string", {
    sds = "sds"
    sub_string(sds, 1, 2) <- "ab"

    sds = "sds"
    sub_string(sds, from = matrix(c(1, 2), ncol = 2)) <- "ab"
    expect_identical(sds, "abs")
})

test_that("vectorize locate", {
    locate_list = list(list("bar", c("b", "a"), structure(
        c(1L, 2L, 1L, 2L),
        .Dim = c(2L, 2L),
        .Dimnames = list(NULL, c("start", "end"))
    )))
    for (ind in locate_list) {
        expect_identical(re2_locate(ind[[1]], ind[[2]]), ind[[3]])
    }
})
