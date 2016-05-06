context("Long String")

test_that("long string", {
    if (memory.limit() > 300) {
        r = stri_c(stringi::stri_dup("x", 2 ^ 28 - 1), "y")
        expect_true(re2_detect(r, "y"))
    }
    if (memory.limit() > 4500 &&
        Sys.getenv("RE2R_LONG_TEST") == "TRUE") {
        r = stri_c(stringi::stri_dup("x", 2 ^ 31 - 3), "y")
        expect_true(re2_detect(r, "y"))
    }
})

test_that("long vector", {
    if (memory.limit() > 300) {
        #for ( x in 1:100) { print(x);dd <<-re2_pdetect(r300, "x"); if(!all(dd)){break;} }
        r300 = replicate(2 ^ 21, "x")
        expect_true(all(re2_detect(r300, "x")))
        expect_true(all(re2_pdetect(r300, "x")))
        rm(r300)
    }

    if (memory.limit() > 4500 &&
        Sys.getenv("RE2R_LONG_TEST") == "TRUE") {
        r4500 = c(replicate(2 ^ 31 - 2, "x"),replicate(2 ^ 4, "x"))
        expect_true(all(re2_detect(r4500, "x")))
        expect_true(all(re2_pdetect(r4500, "x")))
        rm(r4500)
    }
})
