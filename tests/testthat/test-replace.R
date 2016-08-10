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

test_that("stringi replace tests",{
    expect_equivalent(re2_replace_all(character(0),"1","2"),character(0))
    #expect_equivalent(re2_replace_all(1,character(0),2),character(0))
    expect_equivalent(re2_replace_all(1,"2",character(0)),character(0))
    expect_equivalent(re2_replace_all("abab123 a","a",1),"1b1b123 1")
    expect_equivalent(re2_replace_all(NA,"A",1),NA_character_)
    expect_equivalent(re2_replace_all("ALA",NA,"1"),NA_character_)
    expect_equivalent(re2_replace_all("ALA","A",NA),NA_character_)
    expect_equivalent(re2_replace_all(NA,NA,"A"),NA_character_)
    expect_equivalent(re2_replace_all(NA,"A",NA),NA_character_)
    expect_equivalent(re2_replace_all("A",NA,NA),NA_character_)
    expect_equivalent(re2_replace_all(NA,NA,NA),NA_character_)
    expect_warning(re2_replace_all('fasgasgas',c(" ","o"),1:3))

    expect_equivalent(re2_replace_all(c("1", "NULL", "3"), "NULL", NA), c("1", NA, "3"))

    expect_equivalent(re2_replace_all("","^.*$","hey!"),"hey!")
    expect_equivalent(re2_replace_all("  ","^.*$","hey!"),"hey!")

    # difference "$1"
    expect_equivalent(re2_replace_all("abc!def!ghi","(\\p{L})\\p{L}{2}","\\1"),"a!d!g")
    expect_equivalent(re2_replace_all("abc!def!ghi","(\\p{L}{3})","@\\1@"),"@abc@!@def@!@ghi@")

    expect_equivalent(re2_replace_all(c('a', 'b', 'c', 'd'),
                                            c('[ac]', '[bd]'), '!'),
                     rep('!', 4))

    x1 <- rawToChar(as.raw(198))
    x2 <- rawToChar(as.raw(230))
    Encoding(x1) <- 'latin1'
    Encoding(x2) <- 'latin1'
    expect_equivalent(re2_replace_all(x1, x1, x2), '\u00e6')

    expect_equivalent(re2_replace_all("X\U00024B62X",
                                            c("\U00024B62", "\U00024B63", "X"), ""),
                     c("XX", "X\U00024B62X", "\U00024B62"))

    # re2_replace
    expect_identical(re2_replace("abc!def!ghi","(\\p{L})\\p{L}{2}","\\1"),"a!def!ghi")
    expect_identical(re2_replace("abc!def!ghi","(\\p{L}{3})","@\\1@"),"@abc@!def!ghi")
    expect_identical(re2_replace("123!345!456","(\\p{L}{3})","@\\1@"),"123!345!456")
    expect_identical(re2_replace("abc","a","2"),"2bc")
    expect_identical(re2_replace("abc","d","2"),"abc")

    expect_identical(re2_replace(NA,"A",1),NA_character_)
    expect_identical(re2_replace("ALA",NA,"1"),NA_character_)
    expect_identical(re2_replace("ALA","A",NA),NA_character_)
    expect_identical(re2_replace(NA,NA,"A"),NA_character_)
    expect_identical(re2_replace(NA,"A",NA),NA_character_)
    expect_identical(re2_replace("A",NA,NA),NA_character_)
    expect_identical(re2_replace(NA,NA,NA),NA_character_)



    expect_identical(re2_replace(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*", "\u0108"),
                     c("\u0108\u0105\u0106\u0107", "\u0108\u0105\u0107")) # match of zero length
    # expect_identical(re2_replace(c("\u0105\u0106\u0107", "\u0105\u0107"), "(?<=\u0106)", "\u0108"),
    #                 c("\u0105\u0106\u0108\u0107", "\u0105\u0107")) # match of zero length:
    expect_identical(re2_replace("","^.*$","hey!"),"hey!")
    expect_identical(re2_replace("  ","^.*$","hey!"),"hey!")

    expect_identical(re2_replace(c("1", "NULL", "3"), "NULL", NA), c("1", NA, "3"))
})
