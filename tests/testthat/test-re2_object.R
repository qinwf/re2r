context("RE2 Object Properties")

test_that("get_named_groups",{
    # no named group
    expect_identical(get_named_groups(re2("get_named_groups")), ".match")

    # one named group
    expect_identical(get_named_groups(re2("(?P<sd>sd)(abc)")), c(".match","sd", ".2"))

    # two named group with the same name
    expect_identical(get_named_groups(re2("(?P<sd>sd)(?P<sd>abcd)(abc)")), c(".match","sd", "sd", ".3"))

    # two named group with the same name matched result
    eq_with_class(re2_match("sdabcdabc",re2("(?P<sd>sd)(?P<sd>abcd)(abc)")),structure(c("sdabcdabc","sd", "abcd", "abc"), .Dim = c(1L, 4L), .Dimnames = list(NULL, c(".match","sd", "sd", ".3"))))

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
    expect_true(is_re2c_na(re2(NA)))
    expect_true(is.list(re2(c("as","ab","cs"))))
    expect_true(is_re2c_na(re2(c("as","ab",NA))[[3]]))
})

test_that("mimics PCRE",{
    test_cases = list(
    list( "abc",       TRUE  ),
    list( "(a|b)c",    TRUE  ),
    list( "(a*|b)c",   TRUE  ),
    list( "(a|b*)c",   TRUE  ),
    list( "a(b|c)d",   TRUE  ),
    list( "a(()|())c", TRUE  ),
    list( "ab*c",      TRUE  ),
    list( "ab+c",      TRUE  ),
    list( "a(b*|c*)d", TRUE  ),
    list( "\\W",       TRUE  ),
    list( "\\W{1,2}",  TRUE  ),
    list( "\\d",       TRUE  ),

    # Check that repeated empty strings do not.
    list( "(a*)*",     FALSE ),
    list( "x(a*)*y",   FALSE ),
    list( "(a*)+",     FALSE ),
    list( "(a+)*",     TRUE  ),
    list( "(a+)+",     TRUE  ),
    list( "(a+)+",     TRUE  ),

    # \v is the only character class that should not.
    list( "\\b",       TRUE  ),
    list( "\\v",       FALSE ),
    list( "\\d",       TRUE  ),

    # The handling of ^ in multi-line mode is different, as is
    # the handling of $ in single-line mode.  (Both involve
    # boundary cases if the string ends with \n.)
    list( "\\A",       TRUE  ),
    list( "\\z",       TRUE  ),
    list( "(?m)^",     FALSE ),
    list( "(?m)$",     TRUE  ),
    list( "(?-m)^",    TRUE  ),
    list( "(?-m)$",    FALSE ),  # In PCRE, == \Z
    list( "(?m)\\A",   TRUE  ),
    list( "(?m)\\z",   TRUE  ),
    list( "(?-m)\\A",  TRUE  ),
    list( "(?-m)\\z",  TRUE  )
    )

    for (x in test_cases){
        expect_identical(re2r:::cpp_regex_mimicsPCRE(re2(x[[1]])),x[[2]])
    }
})
