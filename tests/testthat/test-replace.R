context("check replace")

test_that("re2_replace", {
    # from re2_test.cc
    tt = function(pattern, replacement,string,res, res_all, num){
        expect_identical(re2_replace(string, pattern, replacement), res)
        expect_identical(re2_replace(string, pattern, replacement ,parallel = T), res)
        expect_identical(re2_replace(string, pattern, replacement ,parallel = T, grain_size = 1), res)

        res = re2_replace_all(string, pattern,replacement)
        expect_identical(as.character(res), res_all)
        expect_identical(attr(res, "count"), as.numeric(num))

        pres = re2_replace_all(string, pattern, replacement, parallel= T, grain_size = 1)
        expect_identical(as.character(pres), res_all)
        expect_identical(attr(pres, "count"), as.numeric(num))
    }

        tt("(qu|[b-df-hj-np-tv-z]*)([a-z]+)", # regexp
           "\\2\\1ay", # rewrite
           "the quick brown fox jumps over the lazy dogs.", # origin
           "ethay quick brown fox jumps over the lazy dogs.", #result
           "ethay ickquay ownbray oxfay umpsjay overay ethay azylay ogsday.", #global
           9 # greplace_count
        )
        tt("\\w+",
          "\\0-NOSPAM",
          "abcd.efghi@google.com",
          "abcd-NOSPAM.efghi@google.com",
          "abcd-NOSPAM.efghi-NOSPAM@google-NOSPAM.com-NOSPAM",
          4 )
        tt("^",
          "(START)",
          "foo",
          "(START)foo",
          "(START)foo",
          1 )
        tt("^",
          "(START)",
          "",
          "(START)",
          "(START)",
          1 )
        tt( "$",
           "(END)",
           "",
           "(END)",
           "(END)",
           1 )
        tt( "b",
           "bb",
           "ababababab",
           "abbabababab",
           "abbabbabbabbabb",
           5 )
        tt( "b",
           "bb",
           "bbbbbb",
           "bbbbbbb",
           "bbbbbbbbbbbb",
           6 )
        tt( "b+",
           "bb",
           "bbbbbb",
           "bb",
           "bb",
           1 )
        tt( "b*",
           "bb",
           "bbbbbb",
           "bb",
           "bb",
           1 )
        tt( "b*",
           "bb",
           "aaaaa",
           "bbaaaaa",
           "bbabbabbabbabbabb",
           6 )
        # Check newline handling
        tt( "a.*a",
           "(\\0)",
           "aba\naba",
           "(aba)\naba",
           "(aba)\n(aba)",
           2 )

    expect_identical(re2_replace(string = c("a","aa", NA), pattern = "a", replacement  = "b", grain_size = 1), c("b","ba", NA))
})

test_that("replce vectorize",{
    tt = function(pattern, replacement,string,res, res_all, num, warns) {

        expect_identical(suppressWarnings(re2_replace(string, pattern, replacement)), res)
        expect_identical(suppressWarnings(re2_replace(string, pattern, replacement)), res, parallel = T)
        expect_true(identical(suppressWarnings(re2_preplace(string, pattern, replacement, parallel = T, grain_size = 1) ) , suppressWarnings(re2_replace(string, pattern, replacement)))
        )

        res = suppressWarnings(re2_replace_all(string, pattern, replacement))
        expect_identical(as.character(res), res_all)
        expect_identical(attr(res, "count"), num)
        expect_identical(res_all, suppressWarnings(re2_replace_all(string, pattern, replacement, parallel = T, grain_size = 1)))

        # vectorize warning
        if(ind[[7]] == TRUE){
            expect_warning(re2_replace_all(string, pattern, replacement))
        }

    }
    replace_list = list(
        list( c("a+", "b"),
              c("x", "y"),
              c("abacada", "aaa", "fdsueo"),
              c("xbacada", "aaa", "fdsueo"),
              c("xbxcxdx", "aaa", "fdsueo"),
              c(4,0,0) ,
              TRUE),
        list( c("a+", "bb"),
              c("x", "y"),
              c("abacada", "bb", "fasueo"),
              c("xbacada", "y", "fxsueo"),
              c("xbxcxdx", "y", "fxsueo"),
              c(4,1,1) ,
              TRUE),
        list( c("a+", "bb", "c"),
              c("x", "y"),
              c("abacada", "bb", "fcsuco"),
              c("xbacada", "y", "fxsuco"),
              c("xbxcxdx", "y", "fxsuxo"),
              c(4,1,2) ,
              TRUE)
        ,
        list( c("a+", "bb", "c"),
              c("x", "y", "zz"),
              c("abacada", "bb", "fcsuco"),
              c("xbacada", "y", "fzzsuco"),
              c("xbxcxdx", "y", "fzzsuzzo"),
              c(4,1,2) ,
              FALSE),
        list( rep(c("a+", "bb", "c"),1000),
              rep(c("x", "y", "zz"),1000),
              rep(c("abacada", "bb", "fcsuco"),1000),
              rep(c("xbacada", "y", "fzzsuco"),1000),
              rep(c("xbxcxdx", "y", "fzzsuzzo"),1000),
              rep(c(4,1,2),1000) ,
              FALSE)
    )
})
