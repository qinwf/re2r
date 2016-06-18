context("check replace")

test_that("re2_replace", {
    # from re2_test.cc
    replace_list = list(
        c( "(qu|[b-df-hj-np-tv-z]*)([a-z]+)", # regexp
           "\\2\\1ay", # rewrite
           "the quick brown fox jumps over the lazy dogs.", # origin
           "ethay quick brown fox jumps over the lazy dogs.", #result
           "ethay ickquay ownbray oxfay umpsjay overay ethay azylay ogsday.", #global
           9 # greplace_count
        ),
        c("\\w+",
          "\\0-NOSPAM",
          "abcd.efghi@google.com",
          "abcd-NOSPAM.efghi@google.com",
          "abcd-NOSPAM.efghi-NOSPAM@google-NOSPAM.com-NOSPAM",
          4 ),
        c("^",
          "(START)",
          "foo",
          "(START)foo",
          "(START)foo",
          1 ),
        c("^",
          "(START)",
          "",
          "(START)",
          "(START)",
          1 ),
        c( "$",
           "(END)",
           "",
           "(END)",
           "(END)",
           1 ),
        c( "b",
           "bb",
           "ababababab",
           "abbabababab",
           "abbabbabbabbabb",
           5 ),
        c( "b",
           "bb",
           "bbbbbb",
           "bbbbbbb",
           "bbbbbbbbbbbb",
           6 ),
        c( "b+",
           "bb",
           "bbbbbb",
           "bb",
           "bb",
           1 ),
        c( "b*",
           "bb",
           "bbbbbb",
           "bb",
           "bb",
           1 ),
        c( "b*",
           "bb",
           "aaaaa",
           "bbaaaaa",
           "bbabbabbabbabbabb",
           6 ),
        # Check newline handling
        c( "a.*a",
           "(\\0)",
           "aba\naba",
           "(aba)\naba",
           "(aba)\n(aba)",
           2 ))

    for (ind in replace_list) {

        expect_identical(re2_replace(ind[3], ind[1], ind[2]), ind[4])
        expect_identical(re2_preplace(ind[3], ind[1], ind[2]), ind[4])

        res = re2_replace_all(ind[3], ind[1],ind[2])
        expect_identical(as.character(res), ind[5])
        expect_identical(attr(res, "count"), as.numeric(ind[6]))

        pres = re2_preplace_all(ind[3], ind[1],ind[2])
        expect_identical(as.character(pres), ind[5])
        expect_identical(attr(pres, "count"), as.numeric(ind[6]))

    }

    expect_identical(re2_preplace(input = c("a","aa", NA), pattern = "a", rewrite = "b", grain_size = 1), c("b","ba", NA))
    expect_identical(re2_preplace_all(input = c("a","aa", NA), pattern = "a", rewrite = "b", grain_size = 1), structure(c("b", "bb", NA), count = c(1, 2, 0)))
})

test_that("replce vectorize",{
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

    for (ind in replace_list) {


        expect_identical(suppressWarnings(re2_replace(ind[[3]], ind[[1]], ind[[2]])), ind[[4]])

        res = suppressWarnings(re2_replace_all(ind[[3]], ind[[1]], ind[[2]]))
        expect_identical(as.character(res), ind[[5]])
        expect_identical(attr(res, "count"), ind[[6]])

        # vectorize warning
        if(ind[[7]] == TRUE){
            expect_warning(re2_replace_all(ind[[3]], ind[[1]], ind[[2]]))
        }

    }
})
