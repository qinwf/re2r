context("RE2 Object Properties")

test_that("get_named_groups",{
    # no named group
    expect_identical(get_named_groups(re2("get_named_groups")),structure(integer(0), .Names = character(0)))

    # one named group
    expect_identical(get_named_groups(re2("(?P<sd>sd)(abc)")),structure(1L, .Names = "sd"))

    # two named group with the same name
    expect_identical(get_named_groups(re2("(?P<sd>sd)(?P<sd>abcd)(abc)")),structure(1L, .Names = "sd"))

    # two named group with the same name matched result
    expect_identical(re2_match("sdabcdabc",re2("(?P<sd>sd)(?P<sd>abcd)(abc)")),structure(c("sd", "abcd", "abc"), .Dim = c(1L, 3L), .Dimnames = list(NULL, c("sd", "sd", "?3"))))

}
)
