context("check rewrite")

test_that("check rewrite", {
    # from re2_test.cc
    ree1 = re2("\\s*(\\w+)")
    re2_match(ree1, "   aaa b!@#$@#$cccc", value = TRUE, anchor = "start", all = TRUE)
})

