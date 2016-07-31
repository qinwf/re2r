context("compile errors")
library(stringi)

test_that("compile NA",{
    expect_na = function(x){expect_true(is.na(x))}

    na_string = re2(NA)
    expect_na(get_pattern(na_string))
    expect_na(get_number_of_groups(na_string))
    expect_na(get_simplify(na_string))
    expect_na(get_named_groups(na_string))
    expect_na(get_expression_size(na_string))
    # NA string pattern return a 0 lenght list
    expect_equal(length(get_options(na_string)),0)
    expect_equal(structure(list(index = NA, value = NA),
                           .Names = c("index", "value"),
                           row.names = c(NA, -1L), class = "data.frame"),
                get_program_fanout(na_string))
})

test_that("compile errors", {
    tt = function(pattern, output_string) {
        expect_output(expect_error(re2(pattern, log_error = T), output_string))
    }
    tt("\\" , "trailing \\\\ at end of regexp: ")
    tt("\\2", "bad escape sequence: ")
    tt("[[:prin:]]", "bad character class range:")
    tt("[a-4]", "bad character class range")
    tt("(sd", "missing closing")
    tt("[[:print:]", "missing closing")
    tt("[[:print:]]++", "bad repetition operator")
    tt("[[:print:]]{3}{3}", "bad repetition operator")
    tt("(?P<.sd>sd",  "bad named capture group")
    tt(stri_dup("a", 1000000), "pattern too large")

    tt("(((.{100}){100}){100}){100}", "bad repetition argument")
    tt("a\\", "trailing")
    tt("a[x", "missing closing")
    tt("a[z-a]", "bad character class range:")
    tt("a[[:foobar:]]", "bad character class range:")
    tt(stri_enc_fromutf32(c(97L, 98L, 92L, 945L, 99L, 100L)), "bad escape sequence:")
    tt(stri_enc_fromutf32(c(101L, 102L, 92L, 120L, 9786L, 48L, 49L)), "bad escape sequence")
    tt(stri_enc_fromutf32(c(103L, 104L, 92L, 120L, 49L, 9786L, 48L, 49L
    )), "bad escape sequence")
    tt("ij\\x1", "bad escape sequence")
    tt("kl\\x", "bad escape sequence")
    tt(stri_enc_fromutf32(c(117L, 118L, 92L, 120L, 123L, 48L, 48L, 48L, 48L, 9786L, 125L)), "bad escape sequence")
    tt("wx\\p{ABC", "bad character class range")
    tt("yz(?smiUX:abc)", "bad perl operator")
    # used to return (?s but the error is X
    tt(stri_enc_fromutf32(c(97L, 97L, 40L, 63L, 115L, 109L, 9786L, 105L)), "bad perl operator")
    tt("bb[abc", "missing closing")
    tt("mn\\x1\377", "")
    # no argument string returned for invalid UTF-8
    # c( "op\377qr", "" ),
    tt("st\\x{00000\377", "")
    tt("zz\\p{\377}", "")
    tt("zz\\x{00\377}", "")
    tt("zz(?P<name\377>abc)", "")
})
