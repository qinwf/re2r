context("check split")

test_that("split test",{
    tta = function(string,pattern,len,res, rep_res = rep(res,1000), parallel_rep = FALSE){
        expect_identical(re2_split_fixed(string, pattern,len),res)
        expect_identical(re2_split_fixed(string, pattern, len,parallel = T), res)
        expect_identical(re2_split_fixed(string, pattern, len,parallel = T, grain_size = 1), res)
        if(parallel_rep){
            expect_identical(re2_split_fixed(rep(string,1000), pattern, parallel = T, grain_size = 1), rep_res)
        }
    }
    tt = function(string,pattern,res, rep_res = NULL, parallel_rep = FALSE){
        expect_identical(re2_split(string, pattern),res)
        expect_identical(re2_split(string, pattern, parallel = T), res)
        expect_identical(re2_split(string, pattern, parallel = T, grain_size = 1), res)
        if(parallel_rep){
            expect_identical(re2_split(rep(string,1000), pattern, parallel = T, grain_size = 1), rep_res)
        }
    }
    tta(c("sdsdsds",NA),"",5,structure(c("s", "", "d", "", "s", "", "d", "", "sds", ""), .Dim = c(2L, 5L)))
    tt(c("sdsdsds",NA),"",list(c("s", "d", "s", "d", "s", "d", "s"), NA_character_))

    # split with char
    tt(c("sdsdsds",NA),"s" , list(c("", "d", "d", "d", ""), NA_character_))
    tta(c("sdsdsds",NA),"s",4, structure(c("", "", "d", "", "d", "", "ds", ""), .Dim = c(2L, 4L)))

    res = structure(c("", "", "d", "", "d", "", "d", "", "", "", "", "",
                      "", "", "", "", "", "", "", ""), .Dim = c(2L, 10L))
    tta(c("sdsdsds",NA),"s",10,res)

    # error
    expect_error(re2_split("sd",pattern = "s", part = numeric()),"need the number of pieces.")

    library(stringi)
    # stringi tests
    expect_identical(stri_split_regex(character(0)," "),re2_split(character(0)," "))

    # NA pattern
    expect_identical(stri_split_regex(NA,NA),re2_split(NA,NA))
    expect_identical(stri_split_regex(NA,"a"),re2_split(NA,"a"))
    expect_identical(stri_split_regex("NA",NA),re2_split("NA",NA))

    # NA
    expect_identical(stri_split_regex("NA",NA,NA),re2_split("NA",NA,NA))

    # tocheck  empty string
    expect_identical(stri_split_regex(" "," "),re2_split(" "," "))

    # differences
    # expect_identical(stri_split_regex("","Z"),re2_split("","Z"))
    # expect_identical(stri_split_regex("",".*"),re2_split("",".*"))

    # tocheck empty string
    expect_identical(stri_split_regex("aa","a"),list(rep("",3)))
    input_list = list(
        c("ala ma kota 1 a","[a-z] [a-z]"),
        c("ala ma kota 1 a","[a-z] [a-z]*"),
        c("ala ma kota 1 a","[a-z] [a-z]+"),
        c("ala ma kota 1 a","[a-z] [1-9]"),
        c("ala ma kota 1 a","[a-z] [1-9]+"),
        c(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"),
        c(c("\u0105\u0106\u0107", "\u0105\u0107"), "(?<=\u0106)")
        )
    for(x in input_list){
        # print(x)
        expect_identical(stri_split_regex(x[1],x[2]), re2_split(x[1],x[2]))
    }

    # n
    expect_identical(stri_split_regex(";123", ";", n=2), re2_split(";123", ";", 2))
    expect_identical(stri_split_regex("123;456", ";", n=2), re2_split("123;456", ";", 2))
    expect_identical(stri_split_regex("123;456;789", ";", n=2), re2_split("123;456;789", ";", 2))

    # expect_identical(stri_split_regex("123-456-789", "-", n=1:3), list(c("123-456-789"),c("123","456-789"),c("123","456","789")))

    expect_identical(stri_split_regex("123-456-789", "[1-8]-.", n=5), re2_split("123-456-789", "[1-8]-.", 5))

    # tokens_only
    expect_identical(stri_split_regex("a_b_c_d", "_"), re2_split("a_b_c_d", "_"))

    # empty string
    expect_identical(stri_split_regex("a_b_c__d", "_"), re2_split("a_b_c__d", "_"))

})

test_that("vectorize split",{
    split_test = list(
        list("a_b_c_d", c("","_"),list(c("a", "_", "b", "_", "c", "_", "d"), c("a", "b", "c", "d")))
    )
    for (ind in split_test) {
        expect_identical(re2_split(ind[[1]],ind[[2]]), ind[[3]])
        expect_identical(re2_split(ind[[1]],ind[[2]]), ind[[3]])
        expect_identical(re2_split(ind[[1]],ind[[2]],grain_size = 1), ind[[3]])

    }

})
