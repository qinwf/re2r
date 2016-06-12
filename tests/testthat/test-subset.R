context("Subsetting")

test_that("basic subsetting for fixed patterns works", {
    expect_equal(re2_subset(c("i", "I"), "i"), "i")
    expect_equal(re2_subset(c("i", "I"), "i"), re2_psubset(c("i", "I"), "i", grain_size = 1))

    expect_equal(
        re2_subset(c("i", "I"), re2("i", case_sensitive = FALSE)),
        c("i", "I")
    )
    expect_equal(
        re2_subset(c("i", "I"), re2("i", case_sensitive = FALSE)),
        re2_psubset(c("i", "I"), re2("i", case_sensitive = FALSE), grain_size = 1))
})

library(stringi)

test_that("stringi test cases",{

    # todo NA string pattern
    # expect_identical(stri_subset_regex(NA, NA), re2_subset(NA, NA))
    # expect_identical(stri_subset_regex(character(0), character(0)), character(0))

    # empty in re2_subset?
    # suppressWarnings(expect_identical(stri_subset_regex("",""), re2_subset("","")))
    # suppressWarnings(expect_identical(stri_subset_regex("a",""), NA_character_))

    expect_identical(stri_subset_regex("","a"), re2_subset("","a"))

    expect_identical(stri_subset_regex(c(NA,"","ala","bkb"),"ala"), re2_subset(c(NA,"","ala","bkb"),"ala"))

    expect_identical(stri_subset_regex(c("","ala","AlA"),"ala", opts_regex=stri_opts_regex(case_insensitive=TRUE)), re2_subset(c("","ala","AlA"), re2("ala", case_sensitive = F)))

    # todo vectorize
    # expect_identical(stri_subset_regex(c("","ala", "ala", "bbb"),c("ala", "bbb")), c("ala", "bbb"))
    # expect_identical(stri_subset_regex(c("ala","", "", "bbb"),c("ala", "bbb")), c("ala", "bbb"))

    # with NA input
    expect_identical(stri_subset_regex(c("a","b", NA, "aaa", ""),c("a")), re2_subset(c("a","b", NA, "aaa", ""),c("a")))

    expect_identical(stri_subset_regex(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*"), re2_subset(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*")) # match of zero length

    # unsupported
    # expect_identical(stri_subset_regex(c("\u0105\u0106\u0107", "\u0105\u0107"), "(?<=\u0106)"), "\u0105\u0106\u0107") # match of zero length

    expect_identical(stri_subset_regex(c('', ' ', '  '), '^.*$'), re2_subset(c('', ' ', '  '), '^.*$'))
    expect_identical(stri_subset_regex(c('', ' ', '  '), '^.+$'), re2_subset(c('', ' ', '  '), '^.+$'))

    # expect_identical(stri_subset_regex('a', c('a', 'b', 'c')), "a")

    expect_identical(stri_subset_regex(c('a', 'b', 'c'), 'a'), re2_subset(c('a', 'b', 'c'), 'a'))

    # todo vectorize
    # suppressWarnings(expect_identical(stri_subset_regex(LETTERS[1:2], LETTERS[1:3]), LETTERS[1:2]))
    # suppressWarnings(expect_identical(stri_subset_regex(LETTERS[1:3], LETTERS[1:5]), LETTERS[1:3]))
    # suppressWarnings(expect_identical(stri_subset_regex(LETTERS[1:2], LETTERS[1:5]), LETTERS[1:2]))
    # suppressWarnings(expect_identical(stri_subset_regex(LETTERS[1:4], LETTERS[1:5]), LETTERS[1:4]))

    # s <- c("Lorem", "123", " ", " ", "kota", "4\t\u0105")
    # p <- c("[[:alpha:]]+", "[[:blank:]]+")
    # expect_identical(stri_subset_regex(s, p, omit_na = TRUE), s[c(T, F, F, T, T, T)])
    # expect_identical(stri_subset_regex(s, p, omit_na = FALSE), s[c(T, F, F, T, T, T)])
    # expect_identical(stri_subset_regex("Lo123\trem", c("[[:alpha:]]", "[4-9]+")), "Lo123\trem")

    # expect_warning(stri_subset_regex(rep("asd", 5), rep("[A-z]", 2)))

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

    # expect_equivalent(stri_subset_regex("abc", c("a", "b", "d")), c("abc", "abc")) # this is weird

    # expect_identical(stri_subset_regex(NA, NA, omit_na=TRUE), character(0))
    # suppressWarnings(expect_identical(stri_subset_regex("","", omit_na=TRUE), character(0)))
    # suppressWarnings(expect_identical(stri_subset_regex("a","", omit_na=TRUE), character(0)))
    # suppressWarnings(expect_identical(stri_subset_regex("","a", omit_na=TRUE), character(0)))
    # expect_identical(stri_subset_regex(c("a","b", NA, "aaa", ""),c("a"), omit_na=TRUE), c("a", "aaa"))
    # expect_identical(stri_subset_regex('a', c('a', 'b', 'c'), omit_na=TRUE), "a")


    x <- c("", NA, "1")
    re2_subset(x, "2") <- "e"
    expect_identical(x, c("", NA, "1"))

    # x <- c("2", NA, "2")
    # re2_subset(x, "2", negate=TRUE) <- "e"
    # expect_identical(x, c("2", NA, "2"))

    x <- c("stringi R", "123", "ID456", "", NA)
    expect_warning(re2_subset(x, "1") <- c(NA, "8"))
    expect_identical(x, c("stringi R", NA, "ID456", "", NA))

    x <- c("stringi R", "123", "ID456", "", NA)
    re2_subset(x, "1") <- c(NA)
    expect_identical(x, c("stringi R", NA, "ID456", "", NA))

    # x <- c(NA, "stringi R", "123", "", "ID456")
    # re2_subset(x, "1", negate=TRUE) <- c("A","B","C","D")
    # expect_identical(x, c(NA, "A", "123", "B", "C"))

    x <- c("stringi R", "123", "ID456", "", NA)
    re2_subset(x, "7") <- c("a", "b")
    expect_identical(x, c("stringi R", "123", "ID456", "", NA))

    x <- c("stringi R", "123", NA, "ID456", "")
    re2_subset(x, "7") <- c("a", "b")
    expect_identical(x, c("stringi R", "123", NA, "ID456", ""))

    x <- c("stringi R", NA, "173", "ID457", "7")
    expect_warning(re2_subset(x, "7") <- c("a", "b"))
    expect_identical(x, c("stringi R", NA, "a", "b", "a"))

    x <- c("stringi R", "173", "ID457", "7")
    expect_error(re2_subset(x, "7") <- character(0))
    # expect_warning(re2_subset(x, c("7","8")) <- NA)
    expect_error(re2_subset(x, character(0)) <- NA)

    x <- c("stringi R", "123", "ID456", "")
    re2_subset(x, re2("S",case_sensitive = F)) <- NA
    expect_identical(x, c(NA, "123", "ID456", ""))

    x <- c("stringi R", "123", "ID456", "")
    re2_subset(x, "[^0-9]+|^$") <- NA
    expect_identical(x, c(NA, "123", NA, NA))
})

