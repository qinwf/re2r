context("RE2 Object Properties")

test_that("get_named_groups",{
    # no named group
    expect_identical(get_named_groups(re2("get_named_groups")), character(0))

    # one named group
    expect_identical(get_named_groups(re2("(?P<sd>sd)(abc)")), c("sd", ".2"))

    # two named group with the same name
    expect_identical(get_named_groups(re2("(?P<sd>sd)(?P<sd>abcd)(abc)")), c("sd", "sd", ".3"))

    # two named group with the same name matched result
    expect_identical(re2_match("sdabcdabc",re2("(?P<sd>sd)(?P<sd>abcd)(abc)")),structure(c("sd", "abcd", "abc"), .Dim = c(1L, 3L), .Dimnames = list(NULL, c("sd", "sd", ".3"))))

}
)

test_that("number of groups",{
    expect_identical(get_number_of_groups(re2("1")),0L)
    expect_identical(get_number_of_groups(re2("()")),1L)
    expect_identical(get_number_of_groups(re2("()()")),2L)
    expect_identical(
        get_number_of_groups(re2(
                                paste0(
                                    rep("(a)", 100000L),
                                    collapse = "")
                                )
                            )
        ,100000L
    )
})

test_that("other RE2 object tests",{
    get_expression_size(re2("abc"))
    get_program_fanout(re2("^(?:4[0-9]{12}(?:[0-9]{3})?|5[1-5][0-9]{14}|6011[0-9]{12}|622((12[6-9]|1[3-9][0-9])|([2-8][0-9][0-9])|(9(([0-1][0-9])|(2[0-5]))))[0-9]{10}|64[4-9][0-9]{13}|65[0-9]{14}|3(?:0[0-5]|[68][0-9])[0-9]{11}|3[47][0-9]{13})*$"))
    expect_output(print(re2("abc"), options = TRUE),"re2 pre-compiled regular expression.*pattern: abc.*number of capturing subpatterns: 0.*capturing names with indices:.*expression size:")
})

library(stringi)

test_that("posix syntax",{
    expect_true(re2_detect("abc" , re2("abc", posix_syntax = T)))
    expect_error(re2("(?P<name>re)", posix_syntax = T))
})

test_that("compile special cases",{
    expect_null(re2(NULL))
    expect_null(re2(NA))
    expect_true(is.list(re2(c("as","ab","cs"))))
})
