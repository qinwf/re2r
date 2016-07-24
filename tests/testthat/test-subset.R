context("Subsetting")

test_that("basic subsetting for fixed patterns works", {
    expect_equal(re2_subset(c("i", "I"), "i"), "i")

    expect_equal(
        re2_subset(c("i", "I"), re2("i", case_sensitive = FALSE)),
        c("i", "I")
    )

})

library(stringi)

test_that("stringi test cases",{

    # NA string pattern
    expect_identical(stri_subset_regex(NA, NA), re2_subset(NA, NA ,omit_na = F))
    expect_identical(character(0), re2_subset(NA, NA))

    expect_identical(stri_subset_regex("","a"), re2_subset("","a" ,omit_na = F))

    expect_identical(stri_subset_regex(c(NA,"","ala","bkb"),"ala"), re2_subset(c(NA,"","ala","bkb"),"ala",omit_na = F))
    expect_identical(stri_subset_regex(c(NA,"","ala","bkb"),"ala", omit_na = T), re2_subset(c(NA,"","ala","bkb"),"ala"))

    expect_identical(stri_subset_regex(c("","ala","AlA"),"ala", opts_regex=stri_opts_regex(case_insensitive=TRUE)), re2_subset(c("","ala","AlA"), re2("ala", case_sensitive = F),omit_na = F))

    # vectorize
    expect_identical(re2_subset(c("","ala", "ala", "bbb"),c("ala", "bbb")), c("ala", "bbb"))
    expect_identical(re2_subset(c("ala","", "", "bbb"),c("ala", "bbb")), c("ala", "bbb"))

    # with NA input
    expect_identical(stri_subset_regex(c("a","b", NA, "aaa", ""),c("a")), re2_subset(c("a","b", NA, "aaa", ""),c("a"),omit_na = F))

    expect_identical(stri_subset_regex(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"), re2_subset(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*")) # match of zero length

    expect_identical(re2_subset(c("\u0105\u0106\u0107", "\u0105\u0107"), "(\u0106)"), "\u0105\u0106\u0107") # match of zero length

    expect_identical(stri_subset_regex(c('', ' ', '  '), '^.*$'), re2_subset(c('', ' ', '  '), '^.*$'))
    expect_identical(stri_subset_regex(c('', ' ', '  '), '^.+$'), re2_subset(c('', ' ', '  '), '^.+$'))

    expect_identical(re2_subset('a', c('a', 'b', 'c')), "a")

    expect_identical(stri_subset_regex(c('a', 'b', 'c'), 'a'), re2_subset(c('a', 'b', 'c'), 'a'))

    # vectorize
    suppressWarnings(expect_identical(re2_subset(LETTERS[1:2], LETTERS[1:3]), LETTERS[1:2]))
    suppressWarnings(expect_identical(re2_subset(LETTERS[1:3], LETTERS[1:5]), LETTERS[1:3]))
    suppressWarnings(expect_identical(re2_subset(LETTERS[1:2], LETTERS[1:5]), LETTERS[1:2]))
    suppressWarnings(expect_identical(re2_subset(LETTERS[1:4], LETTERS[1:5]), LETTERS[1:4]))

    s <- c("Lorem", "123", " ", " ", "kota", "4\t\u0105")
    p <- c("[[:alpha:]]+", "[[:blank:]]+")
    expect_identical(re2_subset(s, p), s[c(T, F, F, T, T, T)])
    expect_identical(re2_subset("Lo123\trem", c("[[:alpha:]]", "[4-9]+")), "Lo123\trem")

    expect_warning(stri_subset_regex(rep("asd", 5), rep("[A-z]", 2)))

    expect_identical(stri_subset_regex("aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"), re2_subset("aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"))
    expect_identical(stri_subset_regex("aaaaaaaaaaaaaaa",  "aaaaaaaaaaaaaaa"), re2_subset("aaaaaaaaaaaaaaa",  "aaaaaaaaaaaaaaa"))
    expect_identical(stri_subset_regex("aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaa"), re2_subset("aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaa"))

    expect_equivalent(stri_subset_regex("\u0105\u0105\u0105\u0105\u0105\u0105\u0105b","\u0105+b$"),re2_subset("\u0105\u0105\u0105\u0105\u0105\u0105\u0105b","\u0105+b$"))

    expect_equivalent(stri_subset_regex("aaaab", "ab"), re2_subset("aaaab", "ab"))
    expect_equivalent(stri_subset_regex("bababababaab", "aab"), re2_subset("bababababaab", "aab"))

    expect_equivalent(stri_subset_regex("caabaab", "(a+b)+"), re2_subset("caabaab", "(a+b)+"))
    expect_equivalent(stri_subset_regex("caacbaab", "(a+b)+"), re2_subset("caacbaab", "(a+b)+"))
    expect_equivalent(stri_subset_regex("caacbacab", "(a+b)+"), re2_subset("caacbacab", "(a+b)+"))
    expect_equivalent(stri_subset_regex("caacbacacb", "(a+b)+"), re2_subset("caacbacacb", "(a+b)+"))

    expect_equivalent(re2_subset("abc", c("a", "b", "d")), c("abc", "abc"))

    expect_identical(re2_subset(c("a","b", NA, "aaa", ""),c("a"), omit_na=TRUE), c("a", "aaa"))
    expect_identical(stri_subset_regex('a', c('a', 'b', 'c'), omit_na=TRUE), "a")
})

