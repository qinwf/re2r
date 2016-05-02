context("check match group")

test_that("check match group 1", {
    # from re2_test.cc
    ree1 = re2("\\s*(\\w+)")
    res1 = re2_match("   aaa b!@#$@#$cccc", ree1, value = TRUE, anchor = 1, all = TRUE)
    exp1 = structure(c("1", "1", "aaa", "b"), .Dim = c(2L, 2L), .Dimnames = list(NULL, c("!n", "?1")))
    expect_identical(res1, exp1)

    res1_tolist = re2_match(c("   aaa b!@#$@#$cccc","   aaa bb cccc"), ree1, value = TRUE, anchor = 1, all = TRUE,tolist = T)

    exp1_tolist = list(structure(c("aaa", "b"), .Dim = c(2L, 1L), .Dimnames = list(
        NULL, "?1")), structure(c("aaa", "bb", "cccc"), .Dim = c(3L,
                                                                 1L), .Dimnames = list(NULL, "?1")))

    expect_identical(res1_tolist, exp1_tolist)

    res2 = re2_match("   aaa b!@#$@#$cccc", ree1, value = TRUE, anchor = 0, all = TRUE)
    exp2 = structure(c("1", "1", "1", "aaa", "b", "cccc"), .Dim = c(3L, 2L), .Dimnames = list(NULL, c("!n", "?1")))
    expect_identical(res2, exp2)

    res3 = re2_match(" one two three 4", "(\\w+)", value = TRUE, anchor = 0, all = TRUE)
    exp3 = structure(c("1", "1", "1", "1", "one", "two", "three", "4"), .Dim = c(4L, 2L), .Dimnames = list(NULL, c("!n", "?1")))
    expect_identical(res3, exp3)

})

test_that("Test Match Number Peculiarity",{
    ree1 = re2("(foo)|(bar)|(baz)")

    res1 = re2_match("foo", ree1, value = TRUE)
    exp1 = structure(c("foo", NA, NA), .Dim = c(1L, 3L), .Dimnames = list(NULL, c("?1", "?2", "?3")))
    expect_identical(res1, exp1)

    res2 = re2_match("baz", ree1, value = TRUE)
    exp2 = structure(c(NA, NA, "baz"), .Dim = c(1L, 3L), .Dimnames = list(NULL, c("?1", "?2", "?3")))
    expect_identical(res2, exp2)

    res3 = re2_match("bar", ree1, value = TRUE)
    exp3 = structure(c(NA, "bar", NA), .Dim = c(1L, 3L), .Dimnames = list(NULL, c("?1", "?2", "?3")))
    expect_identical(res3, exp3)

    res4 = re2_match("f", ree1, value = TRUE)
    exp4 = structure(c(NA_character_, NA_character_, NA_character_), .Dim = c(1L, 3L), .Dimnames = list(NULL, c("?1", "?2", "?3")))
    expect_identical(res4, exp4)

    res5 = re2_match("hello", "(foo)|hello", value = TRUE, anchor = 1)
    exp5 = structure(NA_character_, .Dim = c(1L, 1L), .Dimnames = list(NULL, "?1"))
    expect_identical(res5, exp5)

})

test_that("simple match",{
    ree1 = re2("((\\w+):([0-9]+))")
    expect_false("zyzzyva" %=~% ree1)
    res1 = re2_match("a chrisr:9000 here", ree1, value = TRUE)
    exp1 = structure(c("chrisr:9000", "chrisr", "9000"), .Dim = c(1L, 3L
        ), .Dimnames = list(NULL, c("?1", "?2", "?3")))
    expect_identical(res1, exp1)
})

test_that("no capture with value", {
    test_string = c("this is just one test", "the second test");
    res1 = re2_match(test_string, "is", value = TRUE)
    res1_par = re2_match(test_string, "is", value = TRUE, parallel = T)
    exp1 = structure(c("this is just one test", NA), .Dim = c(2L, 1L), .Dimnames = list(NULL, "?nocapture"))
    expect_identical(res1, exp1)
    expect_identical(res1_par, exp1)
})
