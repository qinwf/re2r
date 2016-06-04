context("compile errors")
library(stringi)

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

    expect_error(re2("(((.{100}){100}){100}){100}"), "bad repetition argument")
    expect_error(re2("a\\"),"trailing")
    expect_error(re2("a[x"),"missing closing")
    expect_error(re2("a[z-a]"),"bad character class range:")
    expect_error(re2("a[[:foobar:]]"),"bad character class range:")

    error_cases = list(
        c( stri_enc_fromutf32(c(97L, 98L, 92L, 945L, 99L, 100L)), "bad escape sequence:" ),
        c( stri_enc_fromutf32(c(101L, 102L, 92L, 120L, 9786L, 48L, 49L)), "bad escape sequence" ),
        c( stri_enc_fromutf32(c(103L, 104L, 92L, 120L, 49L, 9786L, 48L, 49L)), "bad escape sequence" ),
        c( "ij\\x1", "bad escape sequence" ),
        c( "kl\\x", "bad escape sequence" ),
        c( stri_enc_fromutf32(c(117L, 118L, 92L, 120L, 123L, 48L, 48L, 48L, 48L, 9786L, 125L
        )), "bad escape sequence" ),
        c( "wx\\p{ABC", "bad character class range" ),
        c( "yz(?smiUX:abc)", "bad perl operator" ),  # used to return (?s but the error is X
        c( stri_enc_fromutf32(c(97L, 97L, 40L, 63L, 115L, 109L, 9786L, 105L)), "bad perl operator" ),
        c( "bb[abc", "missing closing" ),

        c( "mn\\x1\377", "" ),  # no argument string returned for invalid UTF-8
        # c( "op\377qr", "" ),
        c( "st\\x{00000\377", "" ),
        c( "zz\\p{\377}", "" ),
        c( "zz\\x{00\377}", "" ),
        c( "zz(?P<name\377>abc)", "" )
    )
    for ( x in error_cases ){
        print(x)
        expect_error(re2(x[1]), x[2])
    }
})

