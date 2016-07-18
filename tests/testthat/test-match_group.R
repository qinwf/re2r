context("check match group")

test_that("check match group 1", {
    # from re2_test.cc
    tt = function(string, pattern, match, anchor = UNANCHORED, pstring = string, ppattern = pattern, pmatch = match, rep_match = rep(pmatch,1000), parallel_rep = TRUE){
        expect_equal(re2_match_all(string, pattern, anchor = anchor), match)
        expect_equal(re2_match_all(pstring, ppattern, anchor = anchor, parallel = T), pmatch)
        expect_equal(re2_match_all(pstring, ppattern, anchor = anchor, parallel = T, grain_size = 1), pmatch)
        if (parallel_rep){
            expect_equal(re2_match_all(rep(pstring,1000), ppattern, anchor = anchor, parallel = T, grain_size = 1), rep_match)
        }
    }

    res = list(structure(c("aaa", "b"), .Dim = c(2L, 1L), .Dimnames = list( NULL, ".1")), structure(c("aaa", "bb", "cccc"), .Dim = c(3L,1L), .Dimnames = list(NULL, ".1")))
    tt(c("   aaa b!@#$@#$cccc","   aaa bb cccc"), "\\s*(\\w+)", res, anchor = 1)

    res = list(structure(c("aaa", "b", "cccc"), .Dim = c(3L, 1L), .Dimnames = list(NULL, ".1")))
    tt("   aaa b!@#$@#$cccc", "\\s*(\\w+)",res)

    res = list(structure(c("one", "two", "three", "4"), .Dim = c(4L, 1L), .Dimnames = list(NULL, ".1")))
    tt(" one two three 4", "(\\w+)", res)
    # list return from re2_match_all
    str = c("this is test",
            "this is test, and this is not test",
            "they are tests",
            NA)

    res = list(structure(c("this", " is"), .Dim = 1:2, .Dimnames = list(NULL, c("testname", ".2"))),
               structure(c("this", "this", " is", " is"), .Dim = c(2L, 2L), .Dimnames = list(NULL, c("testname", ".2"))), NULL, NULL)

    tt(str, "(?P<testname>this)( is)", res)



    res =         list(
        structure(c("this", " is"), .Dim = 1:2, .Dimnames = list(
            NULL, c("testname", ".2"))),
        structure(c("this", " is"), .Dim = 1:2, .Dimnames = list(
            NULL, c("testname", ".2"))), NULL, NULL)
    tt(str, "(?P<testname>this)( is)", res, anchor = 1)

})

test_that("Test Match Number Peculiarity",{

    tt = function(string, pattern, match, anchor = UNANCHORED, pstring = string, ppattern = pattern, pmatch= match, rep_match = rep(match,1000), parallel_rep = TRUE){
        expect_equal(re2_match(string, pattern, anchor = anchor), match)
        expect_equal(re2_match(pstring, ppattern, anchor = anchor, parallel = T), pmatch)
        expect_equal(re2_match(pstring, ppattern, anchor = anchor, parallel = T, grain_size = 1), pmatch)
        if (parallel_rep){
            expect_equal(re2_match(rep(pstring,1000), ppattern, anchor = anchor, parallel = T, grain_size = 1), rep_match)
        }
    }
    p = re2("(foo)|(bar)|(baz)")

    res = matrix(c("foo", NA, NA), byrow = T, ncol = 3, dimnames = list(NULL,c(".1", ".2", ".3")))
    resp = matrix(rep(c("foo", NA, NA),1000), byrow = T, ncol = 3, dimnames = list(NULL,c(".1", ".2", ".3")))

    tt("foo", p, res, rep_match = resp)

    res = matrix(c(NA, NA, "baz"), byrow = T, ncol = 3, dimnames = list(NULL,c(".1", ".2", ".3")))
    resp = matrix(rep(c(NA, NA, "baz"),1000), byrow = T, ncol = 3, dimnames = list(NULL,c(".1", ".2", ".3")))

    tt("baz", p, res, rep_match = resp)

    expect_identical(
    re2_match("hello", "(foo)|hello", anchor = 1),
    structure(NA_character_, .Dim = c(1L, 1L), .Dimnames = list(NULL, ".1"))
    )

})

test_that("simple match",{
    p = re2("((\\w+):([0-9]+))")
    expect_false(re2_detect("zyzzyva",p))

    expect_identical(
    re2_match("a chrisr:9000 here", p),
    structure(c("chrisr:9000", "chrisr", "9000"), .Dim = c(1L, 3L
        ), .Dimnames = list(NULL, c(".1", ".2", ".3")))
    )
})

test_that("no capture with value", {
    s = c("this is just one test", "the second test", NA);
    expect_identical(re2_match(s, "is"),
                     structure(c("this is just one test", NA, NA), .Dim = c(3L, 1L), .Dimnames = list(NULL, ".match")))

    expect_identical(re2_match(s, "is", parallel = T), re2_match(s, "is"))
    expect_identical(re2_match(s, "is", parallel = T, grain_size = 1), re2_match(s, "is"))

})


test_that("anchor start value not all",{
    tt = function(string, pattern, match, anchor = UNANCHORED, pstring = string, ppattern = pattern, pmatch= match, rep_match = rep(match,1000), parallel_rep = TRUE){
        expect_equal(re2_match(string, pattern, anchor = anchor), match)
        expect_equal(re2_match(pstring, ppattern, anchor = anchor, parallel = T), pmatch)
        expect_equal(re2_match(pstring, ppattern, anchor = anchor, parallel = T, grain_size = 1), pmatch)
        if (parallel_rep){
            expect_equal(re2_match(rep(pstring,1000), ppattern, anchor = anchor, parallel = T, grain_size = 1), rep_match)
        }
    }
    res = structure("ds", .Dim = c(1L, 1L), .Dimnames = list(NULL, ".1"))
    resp = structure(rep("ds",1000), .Dim = c(1000L, 1L), .Dimnames = list(NULL, ".1"))

    tt("dsS","(ds)", res,anchor = 1, rep_match = resp)

    res = structure(c("ds", NA), .Dim = c(2L, 1L), .Dimnames = list(NULL, ".1"))
    tt(c("dsS", NA),"(ds)", res, anchor = 1, parallel_rep = F)


    res = structure("ds", .Dim = c(1L, 1L), .Dimnames = list(NULL, ".1"))
    tt("dsS","(ds)",res, parallel_rep = F)


    res = structure(c(NA, "ds"), .Dim = c(2L, 1L), .Dimnames = list(NULL, ".1"))
    tt(c("dsS","ds"),"(ds)", res, anchor = 2, parallel_rep = F)

    res = structure(c("ds", "ds"), .Dim = c(2L, 1L), .Dimnames = list(NULL,".1"))
    tt(c("dsS","ds"),"(ds)", res, anchor = 0, parallel_rep = F)



})

test_that("big group",{
    big = paste0(rep("(a)", 1000L), collapse = "")
    bigchar = paste0(rep("a", 1000L), collapse = "")
    bigregex = re2(
        big
    )
    expect_true(re2_detect(bigchar, bigregex))
    expect_true(unique(re2_match(bigchar, bigregex)[[1]]) == "a")
})

library(stringi)

test_that("match NA",{
    expect_identical(structure(c(NA, "sd"), .Dim = c(2L, 1L), .Dimnames = list(NULL, ".match")), re2_match(c(NA,"sd"),"sd"))
    expect_identical(re2_match(c(NA,"sd"),"sd"),re2_match(c(NA,"sd"),"sd", parallel = T, grain_size = 1))

    expect_identical(c(NA,TRUE), re2_detect(c(NA,"sd"),"sd"))
    expect_identical(c(NA, FALSE), re2_detect(c(NA,"sd"),"NA"))
    expect_identical(c(NA, NA), re2_detect(c(NA,"sd"),NA))

    expect_identical(re2_detect(c(NA,"sd"),"NA"),re2_detect(c(NA,"sd"),"NA", parallel = T, grain_size = 1))
    expect_identical(re2_detect(c(NA,"sd"),NA),re2_detect(c(NA,"sd"),NA, parallel = T, grain_size = 1))
    expect_identical(re2_detect(c(NA,"sd"),"sd"),re2_detect(c(NA,"sd"),"sd", parallel = T, grain_size = 1))

})
