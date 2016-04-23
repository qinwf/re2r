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

        expect_identical(re2_replace(ind[3], ind[1], ind[2], all = FALSE), ind[4])

        res = re2_replace(ind[3], ind[1],ind[2], all = TRUE)
        expect_identical(as.character(res), ind[5])
        expect_identical(attr(res, "count"), as.numeric(ind[6]))

    }

})

