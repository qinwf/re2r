context("check split")

test_that("split test",{
    expect_identical(structure(c("s", "", "d", "", "s", "", "d", "", "sds", ""), .Dim = c(2L, 5L)), re2_split_fixed(c("sdsdsds",NA),"",5))
    expect_identical(structure(c("s", "", "d", "", "s", "", "d", "", "sds", ""), .Dim = c(2L, 5L)), re2_psplit_fixed(c("sdsdsds",NA),"",5))
    expect_identical(structure(c("s", "", "d", "", "s", "", "d", "", "sds", ""), .Dim = c(2L, 5L)), re2_psplit_fixed(c("sdsdsds",NA),"",5, grain_size = 1))
    expect_identical(re2_psplit(c("sdsdsds",NA),""),list(c("s", "d", "s", "d", "s", "d", "s"), NA_character_))
    expect_identical(re2_psplit(c("sdsdsds",NA),"", grain_size = 1),list(c("s", "d", "s", "d", "s", "d", "s"), NA_character_))
    expect_identical(re2_split(c("sdsdsds",NA),""),list(c("s", "d", "s", "d", "s", "d", "s"), NA_character_))

    # split with char
    expect_identical(re2_psplit(c("sdsdsds",NA),"s", grain_size = 1), list(c("", "d", "d", "d", ""), NA_character_))
    expect_identical(re2_psplit_fixed(c("sdsdsds",NA),"s", grain_size = 1, part = 4), structure(c("", "", "d", "", "d", "", "ds", ""), .Dim = c(2L, 4L)))
    expect_identical(re2_split(c("sdsdsds",NA),"s"), list(c("", "d", "d", "d", ""), NA_character_))
    expect_identical(re2_split_fixed(c("sdsdsds",NA),"s", part = 4), structure(c("", "", "d", "", "d", "", "ds", ""), .Dim = c(2L, 4L)))
    expect_identical(re2_split_fixed(c("sdsdsds",NA),"s", part = 10), structure(c("", "", "d", "", "d", "", "d", "", "", "", "", "", "", "", "", "", "", "", "", ""), .Dim = c(2L, 10L)))

    # more parts than the splited result
    expect_identical(re2_split_fixed(c("sdsdsds",NA),"s", part = 4), structure(c("", "", "d", "", "d", "", "ds", ""), .Dim = c(2L, 4L)))
    expect_identical(re2_split_fixed(c("sdsdsds",NA),"s", part = 10), re2_psplit_fixed(c("sdsdsds", NA), "s", part = 10, grain_size = 1))
    expect_identical(re2_split_fixed(c("sdsdsds",NA),"s", part = 10), re2_psplit_fixed(c("sdsdsds", NA), "s", part = 10))
})

test_that("split error test",{
    expect_error(re2_split("sd",pattern = "s", part = numeric()),"need the number of pieces.")
})

test_that("Stringi test cases",{
    expect_identical(stri_split_regex(character(0)," "),re2_split(character(0)," "))
    # todo NA pattern
    # expect_identical(stri_split_regex(NA,NA),re2_split(NA,NA))
    expect_identical(stri_split_regex(NA,"a"),re2_split(NA,"a"))
    # expect_identical(stri_split_regex("NA",NA),re2_split("NA",NA))

    # todo n = NA
    # expect_identical(stri_split_regex("NA","a",NA),re2_split("NA","a",NA))

    # tocheck  empty string
    expect_identical(stri_split_regex(" "," "),re2_split(" "," "))
    # expect_identical(stri_split_regex("","Z"),re2_split("","Z"))
    # expect_identical(stri_split_regex("",".*"),re2_split("",".*"))

    # expect_identical(stri_split_regex("","Z", omit_empty=TRUE),list(character(0)))

    # tocheck empty string
    # expect_identical(stri_split_regex("aa","a"),list(rep("",3)))
    input_list = list(
        # tocheck
        #c("ala ma kota 1 a","[a-z] [a-z]"),
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

    # tocheck empty string
    # expect_identical(stri_split_regex("a_b_c__d", "_"), re2_split("a_b_c__d", "_"))


})
