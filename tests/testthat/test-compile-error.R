context("compile errors")

test_that("compile errors",{
    expect_error(re2("\\",log_error = T), "trailing \\\\ at end of regexp: ")
    expect_error(re2("\\2",log_error = T), "bad escape sequence: ")
    expect_error(re2("[[:prin:]]",log_error = T), "bad character class range:")
    expect_error(re2("[a-4]",log_error = T), "bad character class range")
    expect_error(re2("(sd",log_error = T), "missing closing ")
    expect_error(re2("[[:print:]",log_error = T),"missing closing")
    expect_error(re2("[[:print:]]++",log_error = T),"bad repetition operator")
    expect_error(re2("[[:print:]]{3}{3}",log_error = T), "bad repetition operator")
    expect_error(re2("(?P<.sd>sd",log_error = T), "bad named capture group")
    expect_error(re2(stri_dup("a",1000000),log_error = T), "pattern too large ")

    })
