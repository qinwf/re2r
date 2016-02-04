context("check rewrite")

test_that("check rewrite", {
    # from re2_test.cc
    ree1 = re2("\\s*(\\w+)")
    res1 = re2_match(ree1, "   aaa b!@#$@#$cccc", value = TRUE, anchor = "start", all = TRUE)
    exp1 = structure(list(`!n` = c("1", "1"), `?1` = c("aaa", "b")),
                     .Names = c("!n","?1"), row.names = c("1", "2"),
                     class = "data.frame")
    expect_identical(res1, exp1)

    res2 = re2_match(ree1, "   aaa b!@#$@#$cccc", value = TRUE, anchor = "none", all = TRUE)
    exp2 = structure(list(`!n` = c("1", "1", "1"),
                          `?1` = c("aaa", "b",
                                   "cccc")),
                     .Names = c("!n", "?1"),
                     row.names = c("1", "2", "3"),
                     class = "data.frame")
    expect_identical(res2, exp2)

    res3 = re2_match("(\\w+)", " one two three 4", value = TRUE, anchor = "none", all = TRUE)
    exp3 = structure(list(`!n` = c("1", "1", "1", "1"),
                          `?1` = c("one", "two", "three", "4")),
                     .Names = c("!n", "?1"),
                     row.names = c("1", "2", "3", "4"),
                     class = "data.frame")
    expect_identical(res3, exp3)

})

test_that("Test Match Number Peculiarity",{
    ree1 = re2("(foo)|(bar)|(baz)")

    res1 = re2_match(ree1, "foo", value = TRUE)
    exp1 = structure(list(`?1` = "foo", `?2` = NA_character_,
                          `?3` = NA_character_),
                     .Names = c("?1","?2", "?3"),
                     row.names = "1", class = "data.frame")
    expect_identical(res1, exp1)

    res2 = re2_match(ree1, "baz", value = TRUE)
    exp2 = structure(list(`?1` = NA_character_, `?2` = NA_character_,
                          `?3` = "baz"),
                     .Names = c("?1", "?2", "?3"),
                     row.names = "1", class = "data.frame")
    expect_identical(res2, exp2)

    res3 = re2_match(ree1, "bar", value = TRUE)
    exp3 = structure(list(`?1` = NA_character_, `?2` = "bar",
                          `?3` = NA_character_),
                     .Names = c("?1", "?2", "?3"),
                     row.names = "1", class = "data.frame")
    expect_identical(res3, exp3)

    res4 = re2_match(ree1, "f", value = TRUE)
    exp4 = structure(list(`?1` = NA_character_, `?2` = NA_character_,
                          `?3` = NA_character_),
                     .Names = c("?1", "?2", "?3"),
                     row.names = "1", class = "data.frame")
    expect_identical(res4, exp4)

    res5 = re2_match("(foo)|hello", "hello", value = TRUE, anchor = "start")
    exp5 = structure(list(`?1` = NA_character_), .Names = "?1", row.names = "1", class = "data.frame")
    expect_identical(res5, exp5)

})

test_that("simple match",{
    ree1 = re2("((\\w+):([0-9]+))")
    expect_false("zyzzyva" %=~% ree1)
    res1 = re2_match(ree1, "a chrisr:9000 here", value = TRUE)
    exp1 = structure(list(`?1` = "chrisr:9000",
                          `?2` = "chrisr", `?3` = "9000"),
                     .Names = c("?1", "?2", "?3"),
                     row.names = "1", class = "data.frame")
    expect_identical(res1, exp1)
})
