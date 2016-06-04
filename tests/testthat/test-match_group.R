context("check match group")

test_that("check match group 1", {
    # from re2_test.cc

    expect_identical(
    re2_match_all(c("   aaa b!@#$@#$cccc","   aaa bb cccc"), "\\s*(\\w+)", anchor = 1),
    list(structure(c("aaa", "b"), .Dim = c(2L, 1L), .Dimnames = list( NULL, ".1")), structure(c("aaa", "bb", "cccc"), .Dim = c(3L,1L), .Dimnames = list(NULL, ".1")))

    )

    expect_identical(
    re2_match_all("   aaa b!@#$@#$cccc", "\\s*(\\w+)"),
    list(structure(c("aaa", "b", "cccc"), .Dim = c(3L, 1L), .Dimnames = list(NULL, ".1")))
    )


    expect_identical(
    re2_match_all(" one two three 4", "(\\w+)"),
    list(structure(c("one", "two", "three", "4"), .Dim = c(4L, 1L), .Dimnames = list(NULL, ".1")))
    )

})

test_that("Test Match Number Peculiarity",{
    p = re2("(foo)|(bar)|(baz)")

    expect_identical(
    re2_match("foo", p),
    structure(c("foo", NA, NA), .Dim = c(1L, 3L), .Dimnames = list(NULL, c(".1", ".2", ".3")))
    )

    expect_identical(
    re2_match("baz", p),
    structure(c(NA, NA, "baz"), .Dim = c(1L, 3L), .Dimnames = list(NULL, c(".1", ".2", ".3")))
    )

    expect_identical(
    re2_match("bar", p),
    structure(c(NA, "bar", NA), .Dim = c(1L, 3L), .Dimnames = list(NULL, c(".1", ".2", ".3")))
    )

    expect_identical(
    re2_match("f", p),
    structure(c(NA_character_, NA_character_, NA_character_), .Dim = c(1L, 3L), .Dimnames = list(NULL, c(".1", ".2", ".3")))
    )

    expect_identical(
    re2_match("hello", "(foo)|hello", anchor = 1),
    structure(NA_character_, .Dim = c(1L, 1L), .Dimnames = list(NULL, ".1"))
    )

})

test_that("simple match",{
    p = re2("((\\w+):([0-9]+))")

    expect_false("zyzzyva" %=~% p)

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

    expect_identical(re2_pmatch(s, "is"), re2_match(s, "is"))
    expect_identical(re2_pmatch(s, "is", grain_size = 1), re2_match(s, "is"))

})


test_that("anchor start value not all",{
    expect_identical(
        re2_match("dsS","(ds)", anchor = 1),
        structure("ds", .Dim = c(1L, 1L), .Dimnames = list(NULL, ".1"))
        )

    expect_identical(
        re2_pmatch("dsS","(ds)",anchor = 1),
        re2_match("dsS","(ds)", anchor = 1))

    expect_identical(
        re2_pmatch(re2_pmatch(c("dsS", NA),"(ds)",anchor = 1),"(ds)",anchor = 1),
        structure(c("ds", NA), .Dim = c(2L, 1L), .Dimnames = list(NULL, ".1")))

    expect_identical(
        re2_pmatch(re2_pmatch(c("dsS", NA),"(ds)",anchor = 1),"(ds)",anchor = 1),
        re2_pmatch(re2_pmatch(c("dsS", NA),"(ds)",anchor = 1),"(ds)",anchor = 1, grain_size = 1))


    expect_identical(
        re2_match("dsS","(ds)"),
        structure("ds", .Dim = c(1L, 1L), .Dimnames = list(NULL, ".1"))
    )

    expect_identical(
        re2_pmatch("dsS","(ds)"),
        re2_match("dsS","(ds)")
    )

    expect_identical(
        re2_match(c("dsS","ds"),"(ds)", anchor = 2),
        structure(c(NA, "ds"), .Dim = c(2L, 1L), .Dimnames = list(NULL, ".1"))
    )

    expect_identical(
        re2_pmatch(c("dsS","ds"),"(ds)", anchor = 2),
        re2_match(c("dsS","ds"),"(ds)", anchor = 2))

    expect_identical(
        re2_pmatch(c("dsS","ds"),"(ds)", anchor = 2, grain_size =  1),
        re2_match(c("dsS","ds"),"(ds)", anchor = 2))

    expect_identical(
        structure(c("ds", "ds"), .Dim = c(2L, 1L), .Dimnames = list(NULL,".1")),
        re2_match(c("dsS","ds"),"(ds)", anchor = 0))

    expect_identical(
        re2_pmatch(c("dsS","ds"),"(ds)", anchor = 0),
        re2_match(c("dsS","ds"),"(ds)", anchor = 0))

    expect_identical(
        re2_pmatch(c("dsS","ds"),"(ds)", anchor = 0, grain_size =  1),
        re2_match(c("dsS","ds"),"(ds)", anchor = 0))

})

test_that("tolist",{
    str = c("this is test",
            "this is test, and this is not test",
            "they are tests",
            NA)

    expect_identical(
        re2_match_all(str, "(?P<testname>this)( is)"),

        list(structure(c("this", " is"), .Dim = 1:2, .Dimnames = list(NULL, c("testname", ".2"))), structure(c("this", "this", " is", " is"), .Dim = c(2L, 2L), .Dimnames = list(NULL, c("testname", ".2"))), NULL, NULL))

    expect_identical(
        re2_match_all(str, "(?P<testname>this)( is)"),
        re2_pmatch_all(str,"(?P<testname>this)( is)")
        )
    expect_identical(
        re2_match_all(str, "(?P<testname>this)( is)"),
        re2_pmatch_all(str,"(?P<testname>this)( is)", grain_size =  1)
    )
})

test_that("big group",{
    big = paste0(rep("(a)", 1000L), collapse = "")
    bigchar = paste0(rep("a", 1000L), collapse = "")
    bigregex = re2(
        big
    )
    expect_true(re2_detect(bigchar, bigregex))
})

library(stringi)

test_that("match NA",{
    expect_identical(structure(c(NA, "sd"), .Dim = c(2L, 1L), .Dimnames = list(NULL, ".match")), re2_match(c(NA,"sd"),"sd"))
    expect_identical(re2_match(c(NA,"sd"),"sd"),re2_pmatch(c(NA,"sd"),"sd"))

    expect_identical(c(NA,TRUE), re2_detect(c(NA,"sd"),"sd"))
    expect_identical(c(NA, FALSE), re2_detect(c(NA,"sd"),"NA"))

    expect_identical(re2_detect(c(NA,"sd"),"NA"),re2_pdetect(c(NA,"sd"),"NA"))
    expect_identical(re2_detect(c(NA,"sd"),"sd"),re2_pdetect(c(NA,"sd"),"sd"))

    expect_identical(re2_detect(c(NA,"sd"),"sd"),re2_pdetect(c(NA,"sd"),"sd", grain_size = 1))
    expect_identical(re2_detect(c(NA,"sd"),"NA"),re2_pdetect(c(NA,"sd"),"NA", grain_size = 1))

})
