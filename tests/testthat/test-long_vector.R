context("Long String")

test_that("long string", {
    if (memory.limit() > 300) {
        r = stri_c(stringi::stri_dup("x", 2 ^ 28 - 1), "y")
        expect_true(re2_match(r, "y"))
    }
    if (memory.limit() > 4500) {
        r = stri_c(stringi::stri_dup("x", 2 ^ 31 - 3), "y")
        expect_true(re2_match(r, "y"))
    }
})

test_that("long vector", {
    if (memory.limit() > 300) {
        r = replicate(2 ^ 21, "x")
        expect_true(all(re2_match(r, "x")))
    }

    if (memory.limit() > 4500 &&
        Sys.getenv("RE2R_LONG_TEST") == "TRUE") {
        r = c(replicate(2 ^ 31-2, "x"),replicate(2 ^ 4, "x"))
        expect_true(all(re2_match(r, "x")))
    }
})
