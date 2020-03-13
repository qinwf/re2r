context("simplify regex")

test_that("get compile string",{
    re2r:::cpp_regex_to_string(re2("((www\\.|(http|https|ftp|news|file)+\\:\\/\\/)[_.a-z0-9-]+\\.[a-z0-9\\/_:@=.+?,##%&~-]*[^.|\\'|\\# |!|\\(|?|,| |>|<|;|\\)])"))
    re2r:::cpp_regex_to_string(re2("#?([a-f0-9]{6}|[a-f0-9]{3})"))
    re2r:::cpp_regex_to_string(re2("^(?:4[0-9]{12}(?:[0-9]{3})?|5[1-5][0-9]{14}|6011[0-9]{12}|622((12[6-9]|1[3-9][0-9])|([2-8][0-9][0-9])|(9(([0-1][0-9])|(2[0-5]))))[0-9]{10}|64[4-9][0-9]{13}|65[0-9]{14}|3(?:0[0-5]|[68][0-9])[0-9]{11}|3[47][0-9]{13})*$"))

    sim_list = list(
        # Already-simple constructs
        list( "a", "a" ),
        list( "ab", "ab" ),
        list( "a|b", "[a-b]" ),
        list( "ab|cd", "ab|cd" ),
        list( "(ab)*", "(ab)*" ),
        list( "(ab)+", "(ab)+" ),
        list( "(ab)?", "(ab)?" ),
        list( "[ac]", "[ac]" ),
        list( "[^ac]", "[^ac]" ),
        list( ".", "."),
        # Posix character classes
        list( "[[:alnum:]]", "[0-9A-Za-z]" ),
        list( "[[:alpha:]]", "[A-Za-z]" ),
        list( "[[:blank:]]", "[\\t ]" ),
        list( "[[:cntrl:]]", "[\\x00-\\x1f\\x7f]" ),
        list( "[[:digit:]]", "[0-9]" ),
        list( "[[:graph:]]", "[!-~]" ),
        list( "[[:lower:]]", "[a-z]" ),
        list( "[[:print:]]", "[ -~]" ),
        list( "[[:punct:]]", "[!-/:-@\\[-`{-~]" ),
        list( "[[:space:]]" , "[\\t-\\r ]" ),
        list( "[[:upper:]]", "[A-Z]" ),
        list( "[[:xdigit:]]", "[0-9A-Fa-f]" ),

        # Perl character classes
        list( "\\d", "[0-9]" ),
        list( "\\s", "[\\t-\\n\\f-\\r ]" ),
        list( "\\w", "[0-9A-Z_a-z]" ),
        list( "\\D", "[^0-9]" ),
        list( "\\S", "[^\\t-\\n\\f-\\r ]" ),
        list( "\\W", "[^0-9A-Z_a-z]" ),
        list( "[\\d]", "[0-9]" ),
        list( "[\\s]", "[\\t-\\n\\f-\\r ]" ),
        list( "[\\w]", "[0-9A-Z_a-z]" ),
        list( "[\\D]", "[^0-9]" ),
        list( "[\\S]", "[^\\t-\\n\\f-\\r ]" ),
        list( "[\\W]", "[^0-9A-Z_a-z]" ),

        # Posix repetitions
        list( "a{1}", "a" ),
        list( "a{2}", "aa" ),
        list( "a{5}", "aaaaa" ),
        list( "a{0,1}", "a?" ),
        # The next three are illegible because Simplify inserts (?:)
        # parens instead of () parens to avoid creating extra
        # captured subexpressions.  The comments show a version fewer parens.
        list( "(a){0,2}",                   "(?:(a)(a)?)?"     ),  #       (aa?)?
        list( "(a){0,4}",       "(?:(a)(?:(a)(?:(a)(a)?)?)?)?" ),  #   (a(a(aa?)?)?)?
        list( "(a){2,6}", "(a)(a)(?:(a)(?:(a)(?:(a)(a)?)?)?)?" ),  # aa(a(a(aa?)?)?)?
        list( "a{0,2}",           "(?:aa?)?"     ),  #       (aa?)?
        list( "a{0,4}",   "(?:a(?:a(?:aa?)?)?)?" ),  #   (a(a(aa?)?)?)?
        list( "a{2,6}", "aa(?:a(?:a(?:aa?)?)?)?" ),  # aa(a(a(aa?)?)?)?
        list( "a{0,}", "a*" ),
        list( "a{1,}", "a+" ),
        list( "a{2,}", "aa+" ),
        list( "a{5,}", "aaaaa+" ),

        # Test that operators simplify their arguments.
        # (Simplify used to not simplify arguments to a {} repeat.)
        list( "(?:a{1,}){1,}", "a+" ),
        list( "(a{1,}b{1,})", "(a+b+)" ),
        list( "a{1,}|b{1,}", "a+|b+" ),
        list( "(?:a{1,})*", "(?:a+)*" ),
        list( "(?:a{1,})+", "a+" ),
        list( "(?:a{1,})?", "(?:a+)?" ),
        list( "a{0}", "" ),

        # Character class simplification
        list( "[ab]", "[a-b]" ),
        list( "[a-za-za-z]", "[a-z]" ),
        list( "[A-Za-zA-Za-z]", "[A-Za-z]" ),
        list( "[ABCDEFGH]", "[A-H]" ),
        list( "[AB-CD-EF-GH]", "[A-H]" ),
        list( "[W-ZP-XE-R]", "[E-Z]" ),
        list( "[a-ee-gg-m]", "[a-m]" ),
        list( "[a-ea-ha-m]", "[a-m]" ),
        list( "[a-ma-ha-e]", "[a-m]" ),
        list( "[a-zA-Z0-9 -~]", "[ -~]" ),

        # Empty character classes
        list( "[^[:cntrl:][:^cntrl:]]", "[^\\x00-\\x{10ffff}]" ),

        # Full character classes
        list( "[[:cntrl:][:^cntrl:]]", "." ),

        # Unicode case folding.
        list( "(?i)A", "[Aa]" ),
        list( "(?i)a", "[Aa]" ),
        list( "(?i)K", "[Kk\\x{212a}]" ),
        list( "(?i)k", "[Kk\\x{212a}]" ),
        list( "(?i)\\x{212a}", "[Kk\\x{212a}]" ),
        list( "(?i)[a-z]", "[A-Za-z\\x{17f}\\x{212a}]" ),
        list( "(?i)[\\x00-\\x{FFFD}]", "[\\x00-\\x{fffd}]" ),
        list( "(?i)[\\x00-\\x{10ffff}]", "." ),

        # Empty string as a regular expression.
        # Empty string must be preserved inside parens in order
        # to make submatches work right, so these are less
        # interesting than they used to be.  ToString inserts
        # explicit (?:) in place of non-parenthesized empty strings,
        # to make them easier to spot for other parsers.
        list( "(a|b|)", "([a-b]|(?:))" ),
        list( "(|)", "((?:)|(?:))" ),
        list( "a()", "a()" ),
        list( "(()|())", "(()|())" ),
        list( "(a|)", "(a|(?:))" ),
        list( "ab()cd()", "ab()cd()" ),
        list( "()", "()" ),
        list( "()*", "()*" ),
        list( "()+", "()+" ),
        list( "()?" , "()?" ),
        list( "(){0}", "" ),
        list( "(){1}", "()" ),
        list( "(){1,}", "()+" ),
        list( "(){0,2}", "(?:()()?)?" ),

        # Test that coalescing occurs and that the resulting repeats are simplified.
        # Two-op combinations of *, +, ?, {n}, {n,} and {n,m} with a literal:
        list( "a*a*", "a*" ),
        list( "a*a+", "a+" ),
        list( "a*a?", "a*" ),
        list( "a*a{2}", "aa+" ),
        list( "a*a{2,}", "aa+" ),
        list( "a*a{2,3}", "aa+" ),
        list( "a+a*", "a+" ),
        list( "a+a+", "aa+" ),
        list( "a+a?", "a+" ),
        list( "a+a{2}", "aaa+" ),
        list( "a+a{2,}", "aaa+" ),
        list( "a+a{2,3}", "aaa+" ),
        list( "a?a*", "a*" ),
        list( "a?a+", "a+" ),
        list( "a?a?", "(?:aa?)?" ),
        list( "a?a{2}", "aaa?" ),
        list( "a?a{2,}", "aa+" ),
        list( "a?a{2,3}", "aa(?:aa?)?" ),
        list( "a{2}a*", "aa+" ),
        list( "a{2}a+", "aaa+" ),
        list( "a{2}a?", "aaa?" ),
        list( "a{2}a{2}", "aaaa" ),
        list( "a{2}a{2,}", "aaaa+" ),
        list( "a{2}a{2,3}", "aaaaa?" ),
        list( "a{2,}a*", "aa+" ),
        list( "a{2,}a+", "aaa+" ),
        list( "a{2,}a?", "aa+" ),
        list( "a{2,}a{2}", "aaaa+" ),
        list( "a{2,}a{2,}", "aaaa+" ),
        list( "a{2,}a{2,3}", "aaaa+" ),
        list( "a{2,3}a*", "aa+" ),
        list( "a{2,3}a+", "aaa+" ),
        list( "a{2,3}a?", "aa(?:aa?)?" ),
        list( "a{2,3}a{2}", "aaaaa?" ),
        list( "a{2,3}a{2,}", "aaaa+" ),
        list( "a{2,3}a{2,3}", "aaaa(?:aa?)?" ),
        # With a char class, any char and any byte:
        list( "\\d*\\d*", "[0-9]*" ),
        list( ".*.*", ".*" ),
        list( "\\C*\\C*", "\\C*" ),
        # FoldCase works, but must be consistent:
        list( "(?i)A*a*", "[Aa]*" ),
        list( "(?i)a+A+", "[Aa][Aa]+" ),
        list( "(?i)A*(?-i)a*", "[Aa]*a*" ),
        list( "(?i)a+(?-i)A+", "[Aa]+A+" ),
        # NonGreedy works, but must be consistent:
        list( "a*?a*?", "a*?" ),
        list( "a+?a+?", "aa+?" ),
        list( "a*?a*", "a*?a*" ),
        list( "a+a+?", "a+a+?" ),
        # The second element is the literal, char class, any char or any byte:
        list( "a*a", "a+" ),
        list( "\\d*\\d", "[0-9]+" ),
        list( ".*.", ".+" ),
        list( "\\C*\\C", "\\C+" ),
        # FoldCase works, but must be consistent:
        list( "(?i)A*a", "[Aa]+" ),
        list( "(?i)a+A", "[Aa][Aa]+" ),
        list( "(?i)A*(?-i)a", "[Aa]*a" ),
        list( "(?i)a+(?-i)A", "[Aa]+A" ),
        # The second element is a literal string that begins with the literal:
        list( "a*aa", "aa+" ),
        list( "a*aab", "aa+b" ),
        # FoldCase works, but must be consistent:
        list( "(?i)a*aa", "[Aa][Aa]+" ),
        list( "(?i)a*aab", "[Aa][Aa]+[Bb]" ),
        list( "(?i)a*(?-i)aa", "[Aa]*aa" ),
        list( "(?i)a*(?-i)aab", "[Aa]*aab" ),
        # Negative tests with mismatching ops:
        list( "a*b*", "a*b*" ),
        list( "\\d*\\D*", "[0-9]*[^0-9]*" ),
        list( "a+b", "a+b" ),
        list( "\\d+\\D", "[0-9]+[^0-9]" ),
        list( "a?bb", "a?bb" ),
        # Negative tests with capturing groups:
        list( "(a*)a*", "(a*)a*" ),
        list( "a+(a)", "a+(a)" ),
        list( "(a?)(aa)", "(a?)(aa)" ),
        # Just for fun:
        list( "aa*aa+aa?aa{2}aaa{2,}aaa{2,3}a", "aaaaaaaaaaaaaaaa+" ),

        # During coalescing, the child of the repeat changes, so we build a new
        # repeat. The new repeat must have the min and max of the old repeat.
        # Failure to copy them results in min=0 and max=0 -> empty match.
        list( "(?:a*aab){2}", "aa+baa+b" ),

        # During coalescing, the child of the capture changes, so we build a new
        # capture. The new capture must have the cap of the old capture.
        # Failure to copy it results in cap=0 -> ToString() logs a fatal error.
        list( "(a*aab)", "(aa+b)" )
    )

    for (x in sim_list){
        #print(x)
        expect_identical( get_simplify(re2(x[[1]], dot_nl  = T, perl_classes = T)),x[[2]])
    }

    expect_identical( get_simplify("ab"), "ab")

})
