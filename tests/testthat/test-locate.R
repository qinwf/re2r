context("test locate")

test_that("re2_locate", {
    tta = function(string,
                   pattern,
                   res,
                   rep_res = rep(res, 1000),
                   parallel_rep = FALSE) {
        expect_identical(re2_locate_all(string, pattern), res)
        expect_identical(re2_locate_all(string, pattern, parallel = T), res)
        expect_identical(re2_locate_all(
            string,
            pattern,
            parallel = T,
            grain_size = 1
        ),
        res)
        if (parallel_rep) {
            expect_identical(re2_locate_all(
                rep(string, 1000),
                pattern,
                parallel = T,
                grain_size = 1
            ),
            rep_res)
        }
    }
    tt = function(string,
                  pattern,
                  res,
                  rep_res = NULL,
                  parallel_rep = FALSE) {
        expect_identical(re2_locate(string, pattern), res)
        expect_identical(re2_locate(string, pattern, parallel = T), res)
        expect_identical(re2_locate(
            string,
            pattern,
            parallel = T,
            grain_size = 1
        ),
        res)
        if (parallel_rep) {
            expect_identical(re2_locate(
                rep(string, 1000),
                pattern,
                parallel = T,
                grain_size = 1
            ),
            rep_res)
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

    res = structure(c(NA, 1L, NA, 2L),
                    .Dim = c(2L, 2L),
                    .Dimnames = list(NULL, c("start", "end")))
    tt(c(NA, "sd"), "sd", res)


    res_2 = re2_locate_all(c(NA, "sds"), "sd")

    expect_identical(sub_string("sds", res_2[[2]]), "sd")
    expect_identical(sub_string("sds", 1, 2), "sd")

    # NULL
    res = structure(integer(0),
                    .Dim = c(0L, 2L),
                    .Dimnames = list(NULL, c("start", "end")))
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
    res = structure(integer(0),
                    .Dim = c(0L, 2L),
                    .Dimnames = list(NULL, c("start", "end")))
    tt(NULL, pattern = "", res)

    # re2_locate_all
    res = list(structure(c(1L, 2L, 3L, 0L, 1L, 2L), .Dim = c(3L, 2L), .Dimnames = list(
        NULL, c("start", "end"))), structure(c(1L, 2L, 3L, 0L, 1L,
                                               2L), .Dim = c(3L, 2L), .Dimnames = list(NULL, c("start", "end"
                                               ))), structure(c(NA_integer_, NA_integer_), .Dim = 1:2, .Dimnames = list(
                                                   NULL, c("start", "end"))))

    tta(c("as", "as", NA), pattern = "", res)


    # re2_locate
    res = structure(c(1L, 1L, NA, 0L, 0L, NA),
                    .Dim = c(3L, 2L),
                    .Dimnames = list(NULL, c("start", "end")))
    tt(c("as", "as", NA), pattern = "", res)

    # "\\P{M}" word bound
    res = structure(c(1L, 1L, NA, 0L, 0L, NA),
                    .Dim = c(3L, 2L),
                    .Dimnames = list(NULL, c("start", "end")))
    res = list(
        structure(
            c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 2L, 3L, 4L, 5L,
              6L),
            .Dim = c(6L, 2L),
            .Dimnames = list(NULL, c("start", "end"))
        ),
        structure(
            c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L),
            .Dim = c(5L, 2L),
            .Dimnames = list(NULL, c("start", "end"))
        ),
        structure(
            c(NA_integer_, NA_integer_),
            .Dim = 1:2,
            .Dimnames = list(NULL, c("start", "end"))
        )
    )
    tta(c("asasd%", "\\P{M}", NA), pattern = "\\P{M}", res)

    # test locate $
    # NULL
    res = structure(integer(0),
                    .Dim = c(0L, 2L),
                    .Dimnames = list(NULL, c("start", "end")))
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
    res = structure(c(3L, 3L, NA, 2L, 2L, NA),
                    .Dim = c(3L, 2L),
                    .Dimnames = list(NULL, c("start", "end")))
    tt(c("as", "as", NA), pattern = "$", res)

    res = structure(c(1L, 1L, NA, 0L, 0L, NA),
                    .Dim = c(3L, 2L),
                    .Dimnames = list(NULL, c("start", "end")))
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

test_that("stringi tests",{
    expect_is(re2_locate_all(character(0), "a"), "list")

    # not working
    # expect_equivalent(re2_locate_all(NA, character(0)), list())
    # expect_equivalent(re2_locate_all(LETTERS, character(0)), list())
    # suppressWarnings(expect_equivalent(re2_locate_all("abc", ""), list(matrix(c(NA_integer_,NA_integer_)))))
    # suppressWarnings(expect_equivalent(re2_locate_all("", ""), list(matrix(c(NA_integer_,NA_integer_)))))
    # suppressWarnings(expect_equivalent(re2_locate_all("", "abc"), list(matrix(c(NA_integer_,NA_integer_)))))

    expect_equivalent(re2_locate_all(character(0), NA), list())

    expect_equivalent(as.integer(re2_locate_all(NA, "[a-z]")[[1]]),
                      c(NA_integer_, NA_integer_))
    expect_equivalent(as.integer(re2_locate_all("?", "[a-z]")[[1]]),
                      integer(0))

    expect_equivalent(re2_locate_all("1a\u0105a", "\u0105"), list(matrix(c(3,3))))
    expect_equivalent(re2_locate_all("X\U00024B62\U00024B63\U00024B64X",
                                            c("\U00024B62", "\U00024B63", "\U00024B64")),
                      list(matrix(c(2L,2L)), matrix(c(3L,3L)), matrix(c(4L,4L))))
    expect_equivalent(re2_locate_all("aaa", "aa"), list(matrix(c(1,2))))
    expect_equivalent(re2_locate_all(c("", " "), "^.*$"), list(matrix(c(1,0)), matrix(c(1,1))))

    expect_equivalent(re2_locate_all("1a\u0105a", "a.a"), list(matrix(c(2,4))))
    expect_equivalent(re2_locate_all("ala ola ela ula", ".la"), list(matrix(c(1,5,9,13,3,7,11,15),ncol=2)))
    expect_equivalent(re2_locate_all("ala ola ela ula", "(a|u|z)la"), list(matrix(c(1,13,3,15),ncol=2)))

    expect_equivalent(re2_locate_all("aabaaaba", "(a+b)+"), list(matrix(c(1,7))))
    expect_equivalent(re2_locate_all("aabaacba", "(a+b)+"), list(matrix(c(1,3))))
    expect_equivalent(re2_locate_all("ababacba", "(a+b)+"), list(matrix(c(1,4))))

    expect_equivalent(re2_locate_all("aabdaaaba", "(a+b)+"), list(matrix(c(1,5,3,8),ncol=2)))
    expect_equivalent(re2_locate_all("aabdaacba", "(a+b)+"), list(matrix(c(1,3))))
    expect_equivalent(re2_locate_all("ababdacba", "(a+b)+"), list(matrix(c(1,4))))


    expect_equivalent(re2_locate_all(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"),
                      list(matrix(ncol=2, c(1,2,3,4,0,2,2,3)), matrix(ncol=2, c(1,2,3,0,1,2)))) # match of zero length


    # locate first

    expect_is(re2_locate(character(0), "a"), "matrix")

    # expect_equivalent(nrow(re2_locate(NA, character(0))), 0)
    expect_equivalent(nrow(re2_locate(character(0), NA)), 0)
    # expect_equivalent(nrow(re2_locate(LETTERS, character(0))), 0)
    #suppressWarnings(expect_equivalent(re2_locate("abc", ""), matrix(c(NA_integer_,NA_integer_))))
    suppressWarnings(expect_equivalent(re2_locate("", "abc"), matrix(c(NA_integer_,NA_integer_))))
    # suppressWarnings(expect_equivalent(re2_locate("", ""), matrix(c(NA_integer_,NA_integer_))))
    expect_equivalent(re2_locate(c("", " "), "^.*$"), matrix(c(1,0,1,1), byrow=TRUE, ncol=2))

    expect_equivalent(re2_locate("X\u0104\u0105\u106X", "\u0105"), matrix(c(3L,3L)))
    expect_equivalent(re2_locate("X\u9999\u9998\u9997X", "\u9998"), matrix(c(3L,3L)))
    expect_equivalent(re2_locate("X\U00024B62\U00024B63\U00024B64X", "\U00024B63"), matrix(c(3L,3L)))
    expect_equivalent(re2_locate("aaa", "aa"), matrix(c(1L,2L)))

    expect_equivalent(re2_locate("1a\u0105a", "a.a"), matrix(c(2,4)))
    expect_equivalent(re2_locate("ala ola ela ula", ".la"), matrix(c(1,3)))
    expect_equivalent(re2_locate("ala ola ela ula", "(e|u|z)la"), matrix(c(9,11)))

    expect_equivalent(re2_locate("aabaaaba", "(a+b)+"), matrix(c(1,7)))
    expect_equivalent(re2_locate("aabaacba", "(a+b)+"), matrix(c(1,3)))
    expect_equivalent(re2_locate("ababacba", "(a+b)+"), matrix(c(1,4)))

    expect_equivalent(re2_locate("aabdaaaba", "(a+b)+"), matrix(c(1,3)))
    expect_equivalent(re2_locate("aabdaacba", "(a+b)+"), matrix(c(1,3)))
    expect_equivalent(re2_locate("ababdacba", "(a+b)+"), matrix(c(1,4)))


    expect_equivalent(re2_locate(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"),
                      matrix(ncol=2, c(1,1,0,0))) # match of zero length
})
