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
