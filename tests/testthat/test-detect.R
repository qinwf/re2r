context("detect pattern")

test_that("detect pattern",{
    numbers = rawToChar(as.raw(c(0x61, 0x62, 0x63, 0x31, 0x32, 0x33, 0x32, 0x33, 0xc2, 0xbc, 0xc2, 0xbd, 0xc2, 0xbe, 0xe2, 0x82, 0x80)))
    pattern_list = list(
    c( "a", "a" ),
    c( "a", "zyzzyva" ),
    c( "a+", "aa" ),
    c( "(a+|b)+", "ab" ),
    c( "ab|cd", "xabcdx" ),
    c( "h.*od?", "hello\ngoodbye\n" ),
    c( "h.*o", "hello\ngoodbye\n" ),
    c( "h.*o", "goodbye\nhello\n" ),
    c( "h.*o", "hello world" ),
    c( "h.*o", "othello, world" ),
    c( "[^\\s]", "aaaaaaa" ),
    c( "a", "aaaaaaa" ),
    c( "a*", "aaaaaaa" ),
    c( "a*", "" ),
    c( "ab|cd", "xabcdx" ),
    c( "a", "cab" ),
    c( "a*b", "cab" ),
    c( "((((((((((((((((((((x))))))))))))))))))))", "x" ),
    c( "[abcd]", "xxxabcdxxx" ),
    c( "[^x]", "xxxabcdxxx" ),
    c( "[abcd]+", "xxxabcdxxx" ),
    c( "[^x]+", "xxxabcdxxx" ),
    c( "(fo|foo)", "fo" ),
    c( "(foo|fo)", "foo" ),


    # Make sure ^ and $ work.
    # The pathological cases didn't work
    # in the original grep code.
    c( "foo|bar|[A-Z]", "foo" ),
    c( "^(foo|bar|[A-Z])", "foo" ),
    c( "(foo|bar|[A-Z])$", "foo" ),
    c( "^(foo|bar|[A-Z])$", "foo" ),
    c( "^(foo|bar|[A-Z])$", "bar" ),
    c( "^(foo|bar|[A-Z])$", "X" ),
    # c( "^(foo|bar|[A-Z])$", "XY" ),
    c( "^(fo|foo)$", "fo" ),
    c( "^(fo|foo)$", "foo" ),
    c( "^^(fo|foo)$", "fo" ),
    c( "^^(fo|foo)$", "foo" ),
    c( "^$", "" ),
    c( "^^$", "" ),
    c( "^$$", "" ),
    c( "^^$$", "" ),
    c( "^^^^^^^^$$$$$$$$", "" ),
    c( "^", "x" ),
    c( "$", "x" ),

    # Word boundaries.
    c( "\\bfoo\\b", "nofoo foo that" ),
    c( "a\\b", "faoa x" ),
    c( "\\bbar", "bar x" ),
    c( "bar\\b", "foobar" ),
    c( "bar\\b", "foobar\nxxx" ),
    c( "(foo|bar|[A-Z])\\b", "foo" ),
    c( "(foo|bar|[A-Z])\\b", "foo\n" ),
    c( "\\b", "x" ),
    c( "\\b(foo|bar|[A-Z])", "foo" ),
    c( "\\b(foo|bar|[A-Z])\\b", "X" ),
    c( "\\b(foo|bar|[A-Z])\\b", "ffoo bbar N x" ),
    c( "\\b(fo|foo)\\b", "fo" ),
    c( "\\b(fo|foo)\\b", "foo" ),
    c( "\\b\\b", "x" ),
    c( "\\b$", "x" ),
    c( "\\b$", "y x" ),
    c( "\\b.$", "x" ),
    c( "^\\b(fo|foo)\\b", "fo" ),
    c( "^\\b(fo|foo)\\b", "foo" ),
    c( "^\\b", "x" ),
    c( "^\\b\\b", "x" ),
    c( "^\\b.$", "x" ),
    c( "^\\b.\\b$", "x" ),
    c( "^^^^^^^^\\b.$$$$$$", "x" ),

    # Non-word boundaries.
    c( "\\Bfoo\\B", "n foo xfoox that" ),
    c( "a\\B", "faoa x" ),
    c( "(foo|bar|[A-Z])\\B", "foox" ),
    c( "\\B", "" ),
    c( "\\B(foo|bar|[A-Z])\\B", "xXy" ),
    c( "\\B(foo|bar|[A-Z])\\B", "XYZ" ),
    c( "\\B(foo|bar|[A-Z])\\B", "abara" ),
    c( "\\B(foo|bar|[A-Z])\\B", "xfoo_" ),
    c( "\\B(foo|bar|[A-Z])\\B", "foo bar vNx" ),
    c( "\\B(fo|foo)\\B", "xfoo" ),
    c( "\\B(foo|fo)\\B", "xfooo" ),
    c( "\\B\\B", "" ),
    c( "\\B$", "" ),
    c( "^\\B", "" ),
    c( "^\\B\\B", "" ),
    c( "^\\B$", "" ),
    c( "^^^^^^^^\\B$$$$$$$", "" ),

    # PCRE uses only ASCII for \b computation.
    # All non-ASCII are *not* word characters.
    c( "\\bx\\b", "x" ),
    c( "\\bx\\b", "x>" ),
    c( "\\bx\\b", "<x" ),
    c( "\\bx\\b", "<x>" ),
    c( "\\bx\\b", rawToChar(as.raw(c(0xc2, 0xab, 0x78)))),
    c( "\\bx\\b", rawToChar(as.raw(c(0x78, 0xc2, 0xbb))) ),
    c( "\\bx\\b", rawToChar(as.raw(c(0xc2, 0xab, 0x78, 0xc2, 0xbb))) ),

    # Weird boundary cases.
    c( "^$^$", "" ),
    c( "^$^", "" ),
    c( "$^$", "" ),

    c( "(foo\\$)", "foo$bar" ),
    c( "^...$", "abc" ),

    # UTF-8
    c( "^\xe6\x9c\xac$", "\xe6\x9c\xac" ),
    c( "...", "\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e" ),
    c( "...", ".\xe6\x9c\xac." ),

    c( "\\C\\C\\C", "\xe6\x9c\xac" ),
    c( "\\C", "\xe6\x9c\xac" ),
    c( "\\C\\C\\C", "\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e" ),

    # Latin1
    c( "...", "\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e" ),
    c( "...", ".\xe6\x9c\xac." ),

    # Perl v Posix
    c( "\\B(fo|foo)\\B", "xfooo" ),
    c( "(fo|foo)", "foo" ),

    # Octal escapes.
    c( "\\141", "a" ),
    c( "\\060", "0" ),
    c( "\\0600", "00" ),
    c( "\\608", "08" ),
    c( "\\01", "\01" ),
    c( "\\018", "\018" ),

    # Hexadecimal escapes
    c( "\\x{61}", "a" ),
    c( "\\x61", "a" ),
    c( "\\x{00000061}", "a" ),

    # Unicode scripts.
    c( "\\p{Greek}+", stringi::stri_enc_fromutf32(c(97L, 945L, 946L, 98L)) ),
    c( "\\P{Greek}+", stringi::stri_enc_fromutf32(c(97L, 945L, 946L, 98L))  ),
    c( "\\p{^Greek}+", stringi::stri_enc_fromutf32(c(97L, 945L, 946L, 98L))  ),
    c( "\\P{^Greek}+", stringi::stri_enc_fromutf32(c(97L, 945L, 946L, 98L))  ),

    # Unicode properties.  Nd is decimal number.  N is any number.
    c( "[^0-9]+",  "abc123" ),
    c( "\\p{Nd}+", numbers ),
    c( "\\p{^Nd}+", numbers ),
    c( "\\P{Nd}+", numbers ),
    c( "\\P{^Nd}+", numbers ),
    c( "\\pN+", numbers ),
    c( "\\p{N}+", numbers ),
    c( "\\p{^N}+", numbers ),

    c( "\\p{Any}+", "abc123" ),

    # Character classes & case folding.
    c( "(?i)[@-A]+", rawToChar(as.raw(c(0x40, 0x41, 0x61, 0x42))) ),  # matches @Aa but not B
    c( "(?i)[A-Z]+", "aAzZ" ),
    c( "(?i)[^\\\\]+", "Aa\\" ),  # \\ is between A-Z and a-z -
    # splits the ranges in an interesting way.

    # would like to use, but PCRE mishandles in full-match, non-greedy mode
    # c( "(?i)[\\\\]+", "Aa" ),

    c( "(?i)[acegikmoqsuwy]+", "acegikmoqsuwyACEGIKMOQSUWY" ),

    # Character classes & case folding.
    c( "[@-A]+", rawToChar(as.raw(c(0x40, 0x41, 0x61, 0x42))) ),
    c( "[A-Z]+", "aAzZ" ),
    c( "[^\\\\]+", "Aa\\" ),
    c( "[acegikmoqsuwy]+", "acegikmoqsuwyACEGIKMOQSUWY" ),

    # Anchoring.  (^abc in aabcdef was a former bug)
    # The tester checks for a match in the text and
    # subpieces of the text with a byte removed on either side.
    c( "^abc", "abcdef" ),
    c( "abc", "aabcdef" ),
    c( "^[ay]*[bx]+c", "abcdef" ),
    c( "^[ay]*[bx]+c", "aabcdef" ),
    c( "def$", "abcdef" ),
    c( "def", "abcdeff" ),
    c( "d[ex][fy]$", "abcdef" ),
    c( "d[ex][fy]", "abcdeff" ),
    c( "[dz][ex][fy]$", "abcdef" ),
    c( "[dz][ex][fy]", "abcdeff" ),
    c( "(?m)^abc", "abcdef" ),
    c( "(?m)^[ay]*[bx]+c", "abcdef" ),
    c( "(?m)def$", "abcdef" ),
    c( "(?m)def", "abcdeff" ),
    c( "(?m)d[ex][fy]$", "abcdef" ),
    c( "(?m)d[ex][fy]", "abcdeff" ),
    c( "(?m)[dz][ex][fy]$", "abcdef" ),
    c( "(?m)[dz][ex][fy]", "abcdeff" ),
    c( "^", "a" ),
    c( "^^", "a" ),

    # Context.
    # The tester checks for a match in the text and
    # subpieces of the text with a byte removed on either side.
    c( "a", "a" ),
    c( "ab*", "a" ),
    c( "a\\C*", "a" ),
    c( "a\\C?", "a" ),
    c( "a\\C*?", "a" ),
    c( "a\\C??", "a" ),

    # Former bugs.
    c( "a\\C*|ba\\C", "baba" )
    )
    for (x in pattern_list){
        # print(x)
        expect_true(re2_detect(x[2],x[1]))
    }

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
    expect_true(re2_detect(stri_enc_fromutf32(c(20013L, 25991L)),re2(stri_enc_fromutf32(c(20013L, 25991L)),utf_8 = F)))
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
