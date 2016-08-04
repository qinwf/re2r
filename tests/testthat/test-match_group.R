context("check match group")

test_that("check match group 1", {
    # from re2_test.cc
    tt = function(string,
                  pattern,
                  match,
                  anchor = UNANCHORED,
                  pstring = string,
                  ppattern = pattern,
                  pmatch = match,
                  rep_match = rep(pmatch, 1000),
                  parallel_rep = TRUE) {
        expect_equal(re2_match_all(string, pattern, anchor = anchor), match)
        expect_equal(re2_match_all(
            pstring,
            ppattern,
            anchor = anchor,
            parallel = T
        ),
        pmatch)
        expect_equal(
            re2_match_all(
                pstring,
                ppattern,
                anchor = anchor,
                parallel = T,
                grain_size = 1
            ),
            pmatch
        )
        if (parallel_rep) {
            expect_equal(
                re2_match_all(
                    rep(pstring, 1000),
                    ppattern,
                    anchor = anchor,
                    parallel = T,
                    grain_size = 1
                ),
                rep_match
            )
        }
    }

    res = list(
        structure(
            c("   aaa", " b", "aaa", "b"),
            .Dim = c(2L, 2L),
            .Dimnames = list(NULL, c(".match", ".1"))
        ),
        structure(
            c("   aaa",
              " bb", " cccc", "aaa", "bb", "cccc"),
            .Dim = c(3L, 2L),
            .Dimnames = list(NULL, c(".match", ".1"))
        )
    )
    tt(c("   aaa b!@#$@#$cccc", "   aaa bb cccc"),
       "\\s*(\\w+)",
       res,
       anchor = 1)

    res = list(structure(
        c("   aaa", " b", "cccc", "aaa", "b", "cccc"),
        .Dim = c(3L, 2L),
        .Dimnames = list(NULL, c(".match", ".1"))
    ))
    tt("   aaa b!@#$@#$cccc", "\\s*(\\w+)", res)

    res = list(structure(
        c("   aaa", "aaa"),
        .Dim = 1:2,
        .Dimnames = list(NULL, c(".match", ".1"))
    ))
    tt("   aaa b!@#$@#$cccc", "^\\s*(\\w+)", res)

    res = list(structure(
        c("one", "two", "three", "4", "one", "two", "three",
          "4"),
        .Dim = c(4L, 2L),
        .Dimnames = list(NULL, c(".match", ".1"))
    ))
    tt(" one two three 4", "(\\w+)", res)

    # list return from re2_match_all
    str = c("this is test",
            "this is test, and this is not test",
            "they are tests",
            NA)

    res = list(
        structure(
            c("this is", "this", " is"),
            .Dim = c(1L, 3L),
            .Dimnames = list(NULL, c(".match", "testname", ".2"))
        ),
        structure(
            c("this is",
              "this is", "this", "this", " is", " is"),
            .Dim = 2:3,
            .Dimnames = list(NULL, c(".match", "testname", ".2"))
        ),
        structure(
            character(0),
            .Dim = c(0L,
                     3L),
            .Dimnames = list(NULL, c(".match", "testname", ".2"))
        ),
        structure(
            c(NA_character_, NA_character_, NA_character_),
            .Dim = c(1L,
                     3L),
            .Dimnames = list(NULL, c(".match", "testname", ".2"))
        )
    )
    tt(str, "(?P<testname>this)( is)", res)

    res = list(structure(c("", "\u0106", "", ""), .Dim = c(4L, 1L), .Dimnames = list(
        NULL, ".match")), structure(c("", "", ""), .Dim = c(3L, 1L
        ), .Dimnames = list(NULL, ".match")))
    tt(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*", res)

    res = list(
        structure(
            c("this is", "this", " is"),
            .Dim = c(1L, 3L),
            .Dimnames = list(NULL, c(".match", "testname", ".2"))
        ),
        structure(
            c("this is",
              "this", " is"),
            .Dim = c(1L, 3L),
            .Dimnames = list(NULL, c(".match",
                                     "testname", ".2"))
        ),
        structure(
            character(0),
            .Dim = c(0L, 3L),
            .Dimnames = list(NULL, c(".match", "testname", ".2"))
        ),
        structure(
            c(NA_character_,
              NA_character_, NA_character_),
            .Dim = c(1L, 3L),
            .Dimnames = list(NULL, c(".match", "testname", ".2"))
        )
    )
    tt(str, "(?P<testname>this)( is)", res, anchor = 1)

    res = list(
        structure(
            character(0),
            .Dim = c(0L, 3L),
            .Dimnames = list(NULL, c(".match", "testname", ".2"))
        ),
        structure(
            character(0),
            .Dim = c(0L,
                     3L),
            .Dimnames = list(NULL, c(".match", "testname", ".2"))
        ),
        structure(
            character(0),
            .Dim = c(0L, 3L),
            .Dimnames = list(NULL, c(".match", "testname", ".2"))
        ),
        structure(
            c(NA_character_,
              NA_character_, NA_character_),
            .Dim = c(1L, 3L),
            .Dimnames = list(NULL, c(".match", "testname", ".2"))
        )
    )

    tt(str, "(?P<testname>this)( is)", res, anchor = 2)

})

test_that("Test Match Number Peculiarity", {
    tt = function(string,
                  pattern,
                  match,
                  anchor = UNANCHORED,
                  pstring = string,
                  ppattern = pattern,
                  pmatch = match,
                  rep_match = rep(match, 1000),
                  parallel_rep = TRUE) {
        expect_equal(re2_match(string, pattern, anchor = anchor), match)
        expect_equal(re2_match(
            pstring,
            ppattern,
            anchor = anchor,
            parallel = T
        ),
        pmatch)
        expect_equal(
            re2_match(
                pstring,
                ppattern,
                anchor = anchor,
                parallel = T,
                grain_size = 1
            ),
            pmatch
        )
        if (parallel_rep) {
            expect_equal(
                re2_match(
                    rep(pstring, 1000),
                    ppattern,
                    anchor = anchor,
                    parallel = T,
                    grain_size = 1
                ),
                rep_match
            )
        }
    }
    p = re2("(foo)|(bar)|(baz)")

    res = matrix(
        c("foo", "foo", NA, NA),
        byrow = T,
        ncol = 4,
        dimnames = list(NULL, c(".match", ".1", ".2", ".3"))
    )
    resp = matrix(
        rep(c("foo", "foo",NA, NA), 1000),
        byrow = T,
        ncol = 4,
        dimnames = list(NULL, c(".match", ".1", ".2", ".3"))
    )

    tt("foo", p, res, rep_match = resp)

    res = matrix(
        c("baz", NA, NA, "baz"),
        byrow = T,
        ncol = 4,
        dimnames = list(NULL, c(".match", ".1", ".2", ".3"))
    )
    resp = matrix(
        rep(c( "baz", NA, NA, "baz"), 1000),
        byrow = T,
        ncol = 4,
        dimnames = list(NULL, c(".match", ".1", ".2", ".3"))
    )

    tt("baz", p, res, rep_match = resp)

    expect_identical(
        re2_match("hello", "(foo)|hello", anchor = 1),
        structure(
            c("hello",NA_character_),
            .Dim = c(1L, 2L),
            .Dimnames = list(NULL, c(".match",".1"))
        )
    )

})

test_that("simple match", {
    p = re2("((\\w+):([0-9]+))")
    expect_false(re2_detect("zyzzyva", p))

    expect_identical(re2_match("a chrisr:9000 here", p),
                     structure(
                         c("chrisr:9000","chrisr:9000", "chrisr", "9000"),
                         .Dim = c(1L, 4L),
                         .Dimnames = list(NULL, c(".match",".1", ".2", ".3"))
                     ))
})

test_that("no capture with value", {
    s = c("this is just one test", "the second test", NA)

    expect_identical(re2_match(s, "is"),
                     structure(
                         c("is", NA, NA),
                         .Dim = c(3L, 1L),
                         .Dimnames = list(NULL, ".match")
                     ))

    expect_identical(re2_match(s, "is", parallel = T), re2_match(s, "is"))
    expect_identical(re2_match(s, "is", parallel = T, grain_size = 1),
                     re2_match(s, "is"))

})


test_that("anchor start value not all", {
    tt = function(string,
                  pattern,
                  match,
                  anchor = UNANCHORED,
                  pstring = string,
                  ppattern = pattern,
                  pmatch = match,
                  rep_match = rep(match, 1000),
                  parallel_rep = TRUE) {
        expect_equal(re2_match(string, pattern, anchor = anchor), match)
        expect_equal(re2_match(
            pstring,
            ppattern,
            anchor = anchor,
            parallel = T
        ),
        pmatch)
        expect_equal(
            re2_match(
                pstring,
                ppattern,
                anchor = anchor,
                parallel = T,
                grain_size = 1
            ),
            pmatch
        )
        if (parallel_rep) {
            expect_equal(
                re2_match(
                    rep(pstring, 1000),
                    ppattern,
                    anchor = anchor,
                    parallel = T,
                    grain_size = 1
                ),
                rep_match
            )
        }
    }
    res = structure(c("ds","ds"),
                    .Dim = c(1L, 2L),
                    .Dimnames = list(NULL, c(".match",".1")))
    resp = structure(rep("ds", 2000),
                     .Dim = c(1000L, 2L),
                     .Dimnames = list(NULL, c(".match",".1")))

    tt("dsS",
       "(ds)",
       res,
       anchor = 1,
       rep_match = resp)

    res = structure(c("ds", NA, "ds", NA),
                    .Dim = c(2L, 2L),
                    .Dimnames = list(NULL, c(".match", ".1")))
    resp = structure(rep(c("ds", NA, "ds", NA), 1000),
                     .Dim = c(2000L, 2L),
                     .Dimnames = list(NULL, c(".match", ".1")))
    tt(c("dsS", NA),
       "(ds)",
       res,
       rep_match = resp,
       anchor = 1,
       parallel_rep = T)

    res = structure(c("ds", "ds"), .Dim = 1:2,
                    .Dimnames = list(NULL, c(".match", ".1")))
    tt("dsS", "(ds)", res, parallel_rep = F)


    res = structure(c(NA, "ds", NA, "ds"), .Dim = c(2L, 2L), .Dimnames = list(
        NULL, c(".match", ".1")))
    tt(c("dsS", "ds"),
       "(ds)",
       res,
       anchor = 2,
       parallel_rep = F)

    res = structure(c("ds", "ds", "ds", "ds"), .Dim = c(2L, 2L), .Dimnames = list(
        NULL, c(".match", ".1")))
    tt(c("dsS", "ds"),
       "(ds)",
       res,
       anchor = 0,
       parallel_rep = F)



})

test_that("big group", {
    big = paste0(rep("(a)", 1000L), collapse = "")
    bigchar = paste0(rep("a", 1000L), collapse = "")
    bigregex = re2(big)
    expect_true(re2_detect(bigchar, bigregex))
    bigres = re2_match(bigchar, bigregex)
    expect_warning(expect_true(unique(re2_match(bigchar, bigregex)[1,2:1000]) == "a"))
})

library(stringi)

test_that("match NA", {
    expect_identical(structure(
        c(NA, "sd"),
        .Dim = c(2L, 1L),
        .Dimnames = list(NULL, ".match")
    ),
    re2_match(c(NA, "sd"), "sd"))
    expect_identical(re2_match(c(NA, "sd"), "sd"),
                     re2_match(
                         c(NA, "sd"),
                         "sd",
                         parallel = T,
                         grain_size = 1
                     ))

    expect_identical(c(NA, TRUE), re2_detect(c(NA, "sd"), "sd"))
    expect_identical(c(NA, FALSE), re2_detect(c(NA, "sd"), "NA"))
    expect_identical(c(NA, NA), re2_detect(c(NA, "sd"), NA))

    expect_identical(re2_detect(c(NA, "sd"), "NA"),
                     re2_detect(
                         c(NA, "sd"),
                         "NA",
                         parallel = T,
                         grain_size = 1
                     ))
    expect_identical(re2_detect(c(NA, "sd"), NA),
                     re2_detect(
                         c(NA, "sd"),
                         NA,
                         parallel = T,
                         grain_size = 1
                     ))
    expect_identical(re2_detect(c(NA, "sd"), "sd"),
                     re2_detect(
                         c(NA, "sd"),
                         "sd",
                         parallel = T,
                         grain_size = 1
                     ))

})

test_that("Stringi test",{
    expect_equivalent(re2_match_all(NA, "test"), list(matrix(NA_character_,1,1)))
    expect_equivalent(re2_match_all("", "(test)(rest)"), list(matrix(NA_character_,0,3)))

    expect_equivalent(re2_match_all("abcd", "^(:)?([^:]*)(:)?$")[[1]],
                     matrix(c("abcd", NA, "abcd", NA) ,1,4))

    expect_equivalent(re2_match_all("abcd", "^(:)?([^:]*)(:)?$")[[1]],
                     matrix(c("abcd", NA, "abcd", NA) ,1,4))

    expect_equivalent(re2_match_all(":abcd", "^(:)?([^:]*)(:)?$")[[1]],
                     matrix(c(":abcd", ":", "abcd", NA) ,1,4))

    expect_equivalent(re2_match_all(c("", " "), "^.*$"), list(matrix(c("")), matrix(c(" "))))
    expect_equivalent(re2_match_all(c("", " "), "^(.*)$"), list(matrix(c("",""),ncol=2), matrix(c(" ", " "),ncol=2)))

    expect_equivalent(re2_match_all(NA, "(test)(rest)"), list(matrix(NA_character_,1,3)))
    expect_equivalent(re2_match_all("", "(test)(rest)"), list(matrix(NA_character_,0,3)))
    expect_equivalent(re2_match_all("test", NA), list(matrix(NA_character_,1,1)))
    # suppressWarnings(expect_identical(re2_match_all("test", ""), list(matrix(NA_character_,1,1))))

    expect_equivalent(re2_match_all(c("bacab", "bacaba\u0105a", "aa"), "a.a"),
                      list(structure("aca", .Dim = c(1L, 1L), .Dimnames = list(NULL, ".match")),
                           structure(c("aca", "aąa"), .Dim = c(2L, 1L), .Dimnames = list(NULL, ".match")),
                           structure(character(0), .Dim = 0:1, .Dimnames = list(NULL, ".match"))))
    res = list(
        structure(c("a=b", "c=d", "a", "c", "b", "d"), .Dim = 2:3, .Dimnames = list(NULL, c(".match", ".1", ".2"))),
        structure(character(0), .Dim = c(0L,3L), .Dimnames = list(NULL, c(".match", ".1", ".2"))),
        structure(c("e=f", "e", "f"), .Dim = c(1L, 3L), .Dimnames = list(NULL, c(".match", ".1", ".2"))))
    expect_equivalent(re2_match_all(c("a=b;c=d", "", "e=f"), "([a-z])=([a-z])"), res)

    expect_equivalent(re2_match_all(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"),
                      list(matrix(ncol=1, c("", "\u0106", "", "")), matrix(ncol=1, c("", "", "")))) # match of zero length


    # re2_match

    expect_equivalent(re2_match(NA, "test"), matrix(NA_character_,1,1))
    expect_equivalent(re2_match("", "(test)(rest)"), matrix(NA_character_,1,3))

    expect_equivalent(re2_match("abcd", "^(:)?([^:]*)(:)?$"),
                     matrix(c("abcd", NA, "abcd", NA) ,1,4))

    expect_equivalent(re2_match(c("", " "), "^.*$"), matrix(c("", " "),nrow=2))
    expect_equivalent(re2_match(c("", " "), "^(.*)$"), matrix(c("", " "),nrow=2,ncol=2))

    expect_equivalent(re2_match(":abcd", "^(:)?([^:]*)(:)?$"),
                     matrix(c(":abcd", ":", "abcd", NA) ,1,4))

    expect_equivalent(re2_match("test", NA), matrix(NA_character_,1,1))
    # suppressWarnings(expect_equivalent(re2_match("test", ""), matrix(NA_character_,1,1)))
    expect_equivalent(re2_match(c("bacab", "ba\u0105aacaba\u0105a", "aa"), "a.a"),
                      matrix(c("aca", "a\u0105a", NA_character_), 3, 1))
    expect_equivalent(re2_match(c("a=b;c=d", "", "e=f"), "([a-z])=([a-z])"),
                      matrix(c("a=b", NA, "e=f", "a", NA, "e", "b", NA, "f"), 3, 3))


    expect_equivalent(re2_match(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"),
                     matrix(ncol=1, c("", ""))) # match of zero length

})
