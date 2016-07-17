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

    tt(c("baz", "barxbar_sbar bar",NA),c("bar"), list(character(), c("bar", "bar", "bar", "bar"), character()))

    expect_warning(tt(c("baz", "barxbar_sbar bar",NA),c("bar","ba"), list(character(), c("ba", "ba", "ba", "ba"), character()), parallel_rep = FALSE))

})
