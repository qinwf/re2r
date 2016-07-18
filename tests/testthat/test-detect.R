context("detect pattern")

test_that("detect pattern",{
    tt = function(x,y){
        expect_true(re2_detect(y,x))
        expect_true(re2_detect(y,x, parallel = T))
        expect_true(all(re2_detect(rep(y,10000),x, parallel = T, grain_size = 1)))
    }
    numbers = rawToChar(as.raw(c(0x61, 0x62, 0x63, 0x31, 0x32, 0x33, 0x32, 0x33, 0xc2, 0xbc, 0xc2, 0xbd, 0xc2, 0xbe, 0xe2, 0x82, 0x80)))

    tt( "a", "a" )
    tt( "a", "zyzzyva" )
    tt( "a+", "aa" )
    tt( "(a+|b)+", "ab" )
    tt( "ab|cd", "xabcdx" )
    tt( "h.*od?", "hello\ngoodbye\n" )
    tt( "h.*o", "hello\ngoodbye\n" )
    tt( "h.*o", "goodbye\nhello\n" )
    tt( "h.*o", "hello world" )
    tt( "h.*o", "othello, world" )
    tt( "[^\\s]", "aaaaaaa" )
    tt( "a", "aaaaaaa" )
    tt( "a*", "aaaaaaa" )
    tt( "a*", "" )
    tt( "ab|cd", "xabcdx" )
    tt( "a", "cab" )
    tt( "a*b", "cab" )
    tt( "((((((((((((((((((((x))))))))))))))))))))", "x" )
    tt( "[abcd]", "xxxabcdxxx" )
    tt( "[^x]", "xxxabcdxxx" )
    tt( "[abcd]+", "xxxabcdxxx" )
    tt( "[^x]+", "xxxabcdxxx" )
    tt( "(fo|foo)", "fo" )
    tt( "(foo|fo)", "foo" )


    # Make sure ^ and $ work.
    # The pathological cases didn't work
    # in the original grep code.
    tt( "foo|bar|[A-Z]", "foo" )
    tt( "^(foo|bar|[A-Z])", "foo" )
    tt( "(foo|bar|[A-Z])$", "foo" )
    tt( "^(foo|bar|[A-Z])$", "foo" )
    tt( "^(foo|bar|[A-Z])$", "bar" )
    tt( "^(foo|bar|[A-Z])$", "X" )
    # tt( "^(foo|bar|[A-Z])$", "XY" )
    tt( "^(fo|foo)$", "fo" )
    tt( "^(fo|foo)$", "foo" )
    tt( "^^(fo|foo)$", "fo" )
    tt( "^^(fo|foo)$", "foo" )
    tt( "^$", "" )
    tt( "^^$", "" )
    tt( "^$$", "" )
    tt( "^^$$", "" )
    tt( "^^^^^^^^$$$$$$$$", "" )
    tt( "^", "x" )
    tt( "$", "x" )

    # Word boundaries.
    tt( "\\bfoo\\b", "nofoo foo that" )
    tt( "a\\b", "faoa x" )
    tt( "\\bbar", "bar x" )
    tt( "bar\\b", "foobar" )
    tt( "bar\\b", "foobar\nxxx" )
    tt( "(foo|bar|[A-Z])\\b", "foo" )
    tt( "(foo|bar|[A-Z])\\b", "foo\n" )
    tt( "\\b", "x" )
    tt( "\\b(foo|bar|[A-Z])", "foo" )
    tt( "\\b(foo|bar|[A-Z])\\b", "X" )
    tt( "\\b(foo|bar|[A-Z])\\b", "ffoo bbar N x" )
    tt( "\\b(fo|foo)\\b", "fo" )
    tt( "\\b(fo|foo)\\b", "foo" )
    tt( "\\b\\b", "x" )
    tt( "\\b$", "x" )
    tt( "\\b$", "y x" )
    tt( "\\b.$", "x" )
    tt( "^\\b(fo|foo)\\b", "fo" )
    tt( "^\\b(fo|foo)\\b", "foo" )
    tt( "^\\b", "x" )
    tt( "^\\b\\b", "x" )
    tt( "^\\b.$", "x" )
    tt( "^\\b.\\b$", "x" )
    tt( "^^^^^^^^\\b.$$$$$$", "x" )

    # Non-word boundaries.
    tt( "\\Bfoo\\B", "n foo xfoox that" )
    tt( "a\\B", "faoa x" )
    tt( "(foo|bar|[A-Z])\\B", "foox" )
    tt( "\\B", "" )
    tt( "\\B(foo|bar|[A-Z])\\B", "xXy" )
    tt( "\\B(foo|bar|[A-Z])\\B", "XYZ" )
    tt( "\\B(foo|bar|[A-Z])\\B", "abara" )
    tt( "\\B(foo|bar|[A-Z])\\B", "xfoo_" )
    tt( "\\B(foo|bar|[A-Z])\\B", "foo bar vNx" )
    tt( "\\B(fo|foo)\\B", "xfoo" )
    tt( "\\B(foo|fo)\\B", "xfooo" )
    tt( "\\B\\B", "" )
    tt( "\\B$", "" )
    tt( "^\\B", "" )
    tt( "^\\B\\B", "" )
    tt( "^\\B$", "" )
    tt( "^^^^^^^^\\B$$$$$$$", "" )

    # PCRE uses only ASCII for \b computation.
    # All non-ASCII are *not* word characters.
    tt( "\\bx\\b", "x" )
    tt( "\\bx\\b", "x>" )
    tt( "\\bx\\b", "<x" )
    tt( "\\bx\\b", "<x>" )
    tt( "\\bx\\b", rawToChar(as.raw(c(0xc2, 0xab, 0x78))))
    tt( "\\bx\\b", rawToChar(as.raw(c(0x78, 0xc2, 0xbb))) )
    tt( "\\bx\\b", rawToChar(as.raw(c(0xc2, 0xab, 0x78, 0xc2, 0xbb))) )

    # Weird boundary cases.
    tt( "^$^$", "" )
    tt( "^$^", "" )
    tt( "$^$", "" )

    tt( "(foo\\$)", "foo$bar" )
    tt( "^...$", "abc" )

    # UTF-8
    tt( "^\xe6\x9c\xac$", "\xe6\x9c\xac" )
    tt( "...", "\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e" )
    tt( "...", ".\xe6\x9c\xac." )

    tt( "\\C\\C\\C", "\xe6\x9c\xac" )
    tt( "\\C", "\xe6\x9c\xac" )
    tt( "\\C\\C\\C", "\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e" )

    # Latin1
    tt( "...", "\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e" )
    tt( "...", ".\xe6\x9c\xac." )

    # Perl v Posix
    tt( "\\B(fo|foo)\\B", "xfooo" )
    tt( "(fo|foo)", "foo" )

    # Octal escapes.
    tt( "\\141", "a" )
    tt( "\\060", "0" )
    tt( "\\0600", "00" )
    tt( "\\608", "08" )
    tt( "\\01", "\01" )
    tt( "\\018", "\018" )

    # Hexadecimal escapes
    tt( "\\x{61}", "a" )
    tt( "\\x61", "a" )
    tt( "\\x{00000061}", "a" )

    # Unicode scripts.
    tt( "\\p{Greek}+", stringi::stri_enc_fromutf32(c(97L, 945L, 946L, 98L)) )
    tt( "\\P{Greek}+", stringi::stri_enc_fromutf32(c(97L, 945L, 946L, 98L))  )
    tt( "\\p{^Greek}+", stringi::stri_enc_fromutf32(c(97L, 945L, 946L, 98L))  )
    tt( "\\P{^Greek}+", stringi::stri_enc_fromutf32(c(97L, 945L, 946L, 98L))  )

    # Unicode properties.  Nd is decimal number.  N is any number.
    tt( "[^0-9]+",  "abc123" )
    tt( "\\p{Nd}+", numbers )
    tt( "\\p{^Nd}+", numbers )
    tt( "\\P{Nd}+", numbers )
    tt( "\\P{^Nd}+", numbers )
    tt( "\\pN+", numbers )
    tt( "\\p{N}+", numbers )
    tt( "\\p{^N}+", numbers )

    tt( "\\p{Any}+", "abc123" )

    # Character classes & case folding.
    tt( "(?i)[@-A]+", rawToChar(as.raw(c(0x40, 0x41, 0x61, 0x42))) )  # matches @Aa but not B
    tt( "(?i)[A-Z]+", "aAzZ" )
    tt( "(?i)[^\\\\]+", "Aa\\" )  # \\ is between A-Z and a-z -
    # splits the ranges in an interesting way.

    # would like to use, but PCRE mishandles in full-match, non-greedy mode
    # tt( "(?i)[\\\\]+", "Aa" )

    tt( "(?i)[acegikmoqsuwy]+", "acegikmoqsuwyACEGIKMOQSUWY" )

    # Character classes & case folding.
    tt( "[@-A]+", rawToChar(as.raw(c(0x40, 0x41, 0x61, 0x42))) )
    tt( "[A-Z]+", "aAzZ" )
    tt( "[^\\\\]+", "Aa\\" )
    tt( "[acegikmoqsuwy]+", "acegikmoqsuwyACEGIKMOQSUWY" )

    # Anchoring.  (^abc in aabcdef was a former bug)
    # The tester checks for a match in the text and
    # subpieces of the text with a byte removed on either side.
    tt( "^abc", "abcdef" )
    tt( "abc", "aabcdef" )
    tt( "^[ay]*[bx]+c", "abcdef" )
    tt( "^[ay]*[bx]+c", "aabcdef" )
    tt( "def$", "abcdef" )
    tt( "def", "abcdeff" )
    tt( "d[ex][fy]$", "abcdef" )
    tt( "d[ex][fy]", "abcdeff" )
    tt( "[dz][ex][fy]$", "abcdef" )
    tt( "[dz][ex][fy]", "abcdeff" )
    tt( "(?m)^abc", "abcdef" )
    tt( "(?m)^[ay]*[bx]+c", "abcdef" )
    tt( "(?m)def$", "abcdef" )
    tt( "(?m)def", "abcdeff" )
    tt( "(?m)d[ex][fy]$", "abcdef" )
    tt( "(?m)d[ex][fy]", "abcdeff" )
    tt( "(?m)[dz][ex][fy]$", "abcdef" )
    tt( "(?m)[dz][ex][fy]", "abcdeff" )
    tt( "^", "a" )
    tt( "^^", "a" )

    # Context.
    # The tester checks for a match in the text and
    # subpieces of the text with a byte removed on either side.
    tt( "a", "a" )
    tt( "ab*", "a" )
    tt( "a\\C*", "a" )
    tt( "a\\C?", "a" )
    tt( "a\\C*?", "a" )
    tt( "a\\C??", "a" )

    # Former bugs.
    tt( "a\\C*|ba\\C", "baba" )



    case_list = list(
        c( "aa", "aA" ),
        c( "a", "Aa" ),
        c( "a", "A" ),
        c( "ABC", "abc" ),
        c( "abc", "XABCY" ),
        c( "ABC", "xabcy" )
    )
    for (x in case_list){
        # print(x)
        expect_true(re2_detect(x[2],x[1],case_sensitive = F))
    }

    empty_charset = c( "[^\\S\\s]",
                       "[^\\S[:space:]]",
                       "[^\\D\\d]",
                       "[^\\D[:digit:]]")
    for ( x in empty_charset){
        expect_true(!re2_detect("abc", x))
    }

    bitstate_assumptions = c(
        "((((()))))[^\\S\\s]?",
        "((((()))))([^\\S\\s])?",
        "((((()))))([^\\S\\s]|[^\\S\\s])?",
        "((((()))))(([^\\S\\s]|[^\\S\\s])|)"
    )
    for ( x in bitstate_assumptions){
        expect_true(re2_detect("", x))
    }

})

test_that("bad regex",{

    strlong = paste0("x*", stri_dup("a",131072),"*x")
    expect_true( re2_detect(strlong ,"((?:\\s|xx.*\n|x[*](?:\n|.)*?[*]x)*)"))

    strlong_2 = paste0(stri_dup("c",515),'x');
    expect_true( re2_detect(strlong_2 , ".{512}x"))

})

test_that("never \n",{
    never_cases = list(
    c("(.*)", "abc\ndef\nghi\n", "abc" ),
    c("(?s)(abc.*def)", "abc\ndef\n", NA ),
    c("(abc(.|\n)*def)", "abc\ndef\n", NA ),
    c("(abc[^x]*def)", "abc\ndef\n", NA ),
    c("(abc[^x]*def)", "abczzzdef\ndef\n", "abczzzdef" ))

    for ( x in never_cases){
        if (is.na(x[3])){
            expect_true(!re2_detect(x[2], re2(x[1],never_nl = T)))
        }else{
            expect_identical(re2_match(x[2], re2(x[1],never_nl = T))[[1]], x[3])
        }
    }

})

test_that("dot nl",{
    expect_true(re2_detect("\n", re2(".", dot_nl = T)))
    expect_true(!re2_detect("\n", re2("(?-s).",dot_nl = T)))
    expect_true(!re2_detect("\n", re2(".",dot_nl = T, never_nl = T)))
})

test_that("never capture",{
    expect_identical(0L,get_number_of_groups(re2("(A)(v)",never_capture = T)))
})

test_that("longest match",{
    expect_identical(re2_match("aaabaaaa",re2("(a|aaa)",longest_match = T))[[1]], "aaa")
    expect_identical(re2_match("aaabaaaa",re2("(a|aaa)",longest_match = F))[[1]], "a")
})

test_that("utf_8 option",{
    expect_true(re2_detect(stringi::stri_enc_fromutf32(c(20013L, 25991L)),re2(stri_enc_fromutf32(c(20013L, 25991L)),utf_8 = F)))
})

test_that("literal option",{
    expect_identical(get_simplify(re2("[a-z]")),"[a-z]")
    expect_identical(get_simplify(re2("[a-z]", literal = T)),"\\[a\\-z\\]")
})

test_that("max mem",{
    expect_error(re2("asdddd*", max_mem = 3))
})

test_that("posix_syntax option",{

    expect_error(re2("\\b",posix_syntax = T))
    expect_error(re2("\\b"),NA)
    expect_error(re2("\\b",posix_syntax = T, word_boundary = T),NA)

    expect_error(re2("\\d"),NA)
    expect_error(re2("\\d",posix_syntax = T, perl_classes = T),NA)
    expect_error(re2("\\d",posix_syntax = T))

    expect_true(!re2_detect("s
sdsd",re2("^sdsd$")))
    expect_true(re2_detect("s
sdsd",re2("^sdsd$", posix_syntax = T)))
    expect_true(!re2_detect("s
sdsd",re2("^sdsd$", posix_syntax = T, one_line = T)))
})

test_that("vectorize detect",{
    detect_list  = list(
        list(
            c("a","b"),c("a","b"),c(TRUE,TRUE)
        )
    )
    for (ind in detect_list){
       expect_identical( re2_detect(ind[[1]], ind[[2]]), ind[[3]])
       expect_identical( re2_detect(ind[[1]], ind[[2]], parallel = TRUE), ind[[3]])
       expect_identical( re2_detect(ind[[1]], ind[[2]], parallel = TRUE, grain_size = 1), ind[[3]])
    }
})
