context("compile errors")

test_that("compile errors",{
    expect_error(re2("\\"), "trailing \\\\ at end of regexp: ")
    expect_error(re2("\\2"), "bad escape sequence: ")
    expect_error(re2("[[:prin:]]"), "bad character class range:")
    expect_error(re2("[a-4]"), "bad character class range")
    expect_error(re2("(sd"), "missing closing ")
    expect_error(re2("[[:print:]"),"missing closing")
    expect_error(re2("[[:print:]]++"),"bad repetition operator")
    expect_error(re2("[[:print:]]{3}{3}"), "bad repetition operator")
    expect_error(re2("(?P<.sd>sd"), "bad named capture group")
    expect_error(re2(stri_dup("a",1000000), "pattern too large "))

    })
