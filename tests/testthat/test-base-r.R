context("base R")
library(stringi)
source('helper.R')

test_that("base R examples",{
    "base R example"
    eq_with_class(re2_match(letters, "[a-z]"),
                  grep("[a-z]", letters, value = T))

    txt <- c("arm","foot","lefroo", "bafoobar")
    expect_equivalent(grepl("foo", txt), re2_detect(txt, "foo"))

    # differences
    gsub("([ab])", "\\1_\\1_", "abc and ABC")
    re2_replace_all("abc and ABC", pattern  = "([ab])",replacement =  "\\1_\\1")
    stri_replace_all_regex("abc and ABC", pattern  = "([ab])",replacement =  "$1_$1")

    txt <- c("The", "licenses", "for", "most", "software", "are",
             "designed", "to", "take", "away", "your", "freedom",
             "to", "share", "and", "change", "it.",
             "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
             "is", "intended", "to", "guarantee", "your", "freedom", "to",
             "share", "and", "change", "free", "software", "--",
             "to", "make", "sure", "the", "software", "is",
             "free", "for", "all", "its", "users")

    expect_equivalent(re2_detect(txt, "[gu]"), grepl("[gu]", txt))
    expect_equivalent( sub("[b-e]",".", txt) ,re2_replace(txt, "[b-e]", "."))
    expect_equivalent( gsub("[b-e]",".", txt), re2_replace_all(txt, "[b-e]", "."))

    expect_equivalent(
        gsub("g","#", txt, ignore.case = TRUE),
        re2_replace_all(txt, re2("g", case_sensitive = F), "#")
    )

    re2_match(txt, "en")
    re2_match_all(txt, "en")

    ## trim trailing white space
    str <- "Now is the time      "
    expect_equivalent(
        sub(" +$", "", str),  ## spaces only
        re2_replace(str, " +$", "")
    )

    ## what is considered 'white space' depends on the locale.
    sub("[[:space:]]+$", "", str) ## white space, POSIX-style
    expect_equivalent(
        re2_replace(str, "[[:space:]]+$", ""),
        ## what PCRE considered white space changed in version 8.34: see ?regex
        sub("\\s+$", "", str, perl = TRUE) ## PCRE-style white space
    )

    ## capitalizing
    # txt <- "a test of capitalizing"
    # gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", txt, perl=TRUE)
    # gsub("\\b(\\w)",    "\\U\\1",       txt, perl=TRUE)
    #
    # txt2 <- "useRs may fly into JFK or laGuardia"
    # gsub("(\\w)(\\w*)(\\w)", "\\U\\1\\E\\2\\U\\3", txt2, perl=TRUE)
    # sub("(\\w)(\\w*)(\\w)", "\\U\\1\\E\\2\\U\\3", txt2, perl=TRUE)
    ## named capture
    notables <- c("  Ben Franklin and Jefferson Davis",
                  "\tMillard Fillmore")
    # name groups 'first' and 'last'
    name.rex <- "(?P<first>[[:upper:]][[:lower:]]+) (?P<last>[[:upper:]][[:lower:]]+)"
    res = re2_match_all(notables, name.rex)
    tmp = list(
        structure(c("Ben Franklin", "Jefferson Davis", "Ben", "Jefferson", "Franklin", "Davis"),
        .Dim = 2:3,
        .Dimnames = list(NULL, c(".match", "first", "last")), class = "re2_matrix"),
        structure(c("Millard Fillmore", "Millard", "Fillmore"),
        .Dim = c(1L, 3L),
        .Dimnames = list(NULL, c(".match", "first", "last")), class = "re2_matrix"))
    expect_equivalent(res, tmp)

    pattern <- "([[:alpha:]]+)([[:digit:]]+)"
    s <- "Test: A1 BC23 DEF456"
    res = re2_match_all(s, pattern)
    tmp = list(structure(
        c("A1", "BC23", "DEF456", "A", "BC", "DEF", "1",
                           "23", "456"),
        .Dim = c(3L, 3L),
        .Dimnames = list(NULL, c(".match", ".1", ".2")),
        class = "re2_matrix"))
    expect_equivalent(res, tmp)

    x <- "http://stat.umn.edu:80/xyz"
    res = re2_match(x, "^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)")

    tmp = structure(c("http://stat.umn.edu:80/xyz", "http://", "http",
                "stat.umn.edu", ":80", "80", "/xyz"),
              .Dim = c(1L, 7L),
              .Dimnames = list(NULL,
                  c(".match", ".1", ".2", ".3", ".4", ".5", ".6")),
              class = "re2_matrix")
    expect_equivalent(res, tmp)
})
