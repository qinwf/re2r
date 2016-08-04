context("check extract")

test_that("extract one", {
    # from re2_test.cc
    tt = function(string,pattern,match,stringp = string, patternp = pattern, matchp = match, parallel_rep = TRUE){
        expect_identical(re2_extract(string, pattern), match)
        expect_identical(re2_extract(string, pattern, parallel = T), match)
        expect_identical(re2_extract(stringp, patternp, parallel = T, grain_size = 1), matchp)
        if(parallel_rep){
            expect_identical(re2_extract(rep(stringp, 100), rep(patternp, 100), parallel = T, grain_size = 1), rep(matchp, 100))
        }
    }
    # one pattern
    tt("boris@kremvax.ru", "(.*)@([^.]*)","boris@kremvax",
       c("boris@kremvax.ru", "happy123@tress.com"),
       "(.*)@([^.]*)",
       c("boris@kremvax", "happy123@tress"))

    # one pattern with NA
    input_list = c("this is 911", "Ok, let's do it.", "Call me at 102-232-333")
    res_list = c("9", NA, "1")
    tt(input_list, "\\d", res_list,
       input_list, "\\d", res_list)

    # one string multiple pattern
    input_list = c("this is 911")
    pattern_list = c("[a-z]+", "\\d", "[a-z]{1,3}")
    res_list = c("this", "9", "thi")
    tt(input_list, pattern_list, res_list)

    tt("foo", ".*", "foo")

    tt("baz", "bar", NA_character_)

    tt(c("baz", "bar",NA),c("bar"),c(NA, "bar", NA))

    expect_warning(tt(c("sd","abc"), c("a","b","c"), c(NA,"b",NA), parallel_rep = FALSE))
})

test_that("extract all",{
    tt = function(string,pattern,match,stringp = string, patternp = pattern, matchp = match, parallel_rep = TRUE){
        expect_identical(re2_extract_all(string, pattern), match)
        expect_identical(re2_extract_all(string, pattern, parallel = T), match)
        expect_identical(re2_extract_all(stringp, patternp, parallel = T, grain_size = 1), matchp)
        if(parallel_rep){
            expect_identical(re2_extract_all(rep(stringp, 100), rep(patternp, 100), parallel = T, grain_size = 1), rep(matchp, 100))
        }
    }

    tt(c("baz", "barxbar_sbar bar",NA),c("bar"), list(character(), c("bar", "bar", "bar", "bar"), NA_character_))

    expect_warning(tt(c("baz", "barxbar_sbar bar",NA),c("bar","ba"), list(character(), c("ba", "ba", "ba", "ba"), NA_character_), parallel_rep = FALSE))

})

test_that("stringi tests",{
    expect_identical(re2_extract_all(character(0), "test"), list())
    # not working
    # expect_identical(re2_extract_all("test", character(0)), list())
    # expect_identical(re2_extract_all(character(0), character(0)), list())
    expect_identical(re2_extract_all(NA, "test"), list(NA_character_))
    expect_identical(re2_extract_all("test", NA), list(NA_character_))

    # differences
    suppressWarnings(expect_identical(re2_extract_all("test", ""), list(c("", "", "", "", ""))))
    expect_identical(re2_extract_all(c("bacab", "bacaba\u0105a", "aa"), "a.a"),
                     list("aca", c("aca", "a\u0105a"), character(0)))
    expect_identical(re2_extract_all("", " "), list(character(0)))

    expect_identical(re2_extract_all(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"),
                     list(c("", "\u0106", "", ""), c("", "", ""))) # match of zero length

    expect_identical(re2_extract_all("", "^.*$"), list(""))

    expect_identical(re2_extract_all("test", " "), list(character(0)))

    expect_identical(re2_extract_all(c("ab_c", "d_ef_g", "h", ""), "\\p{L}+"),
                     list(c("ab", "c"), c("d", "ef", "g"), "h", character(0)))


    ## extract
    expect_identical(re2_extract(character(0), "test"), character(0))

    # not working
    # expect_identical(re2_extract("test", character(0)), character(0))
    # expect_identical(re2_extract(character(0), character(0)), character(0))

    expect_identical(re2_extract(NA, "test"), NA_character_)
    expect_identical(re2_extract("test", NA), NA_character_)

    # diferences
    # suppressWarnings(expect_identical(re2_extract("test", ""), NA_character_))
    expect_identical(re2_extract("\U00f0ffffb\u0105deb!d", "b.d"), "b\u0105d")

    expect_identical(re2_extract("\U00f0ffffb\u0105deb!d", re2("B.D",case_sensitive = F)), "b\u0105d")

    expect_identical(re2_extract(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"),
                     c("", "")) # match of zero length

    expect_identical(re2_extract(c("ababab", NA, "ab", "ba"), "ab"),
                     c("ab", NA, "ab", NA))

    expect_identical(re2_extract(c("", " "), "^.*$"), c("", " "))
})
