context("check rewrite")

test_that("check rewrite", {
    # from re2_test.cc
    lists = list(
        list("abc", c("foo", TRUE),
             c("foo\\", FALSE), c("foo\\0bar", TRUE)),

        list("a(b)c", c("foo", TRUE), c("foo\\0bar", TRUE),
             c( "foo\\1bar", TRUE), c("foo\\2bar", FALSE),
             c("f\\\\2o\\1o", TRUE)),

        list("a(b)(c)", c("foo\\12", TRUE),
             c( "f\\2o\\1o", TRUE), c("f\\oo\\1", FALSE))
    )


    for (ind in lists) {
        regexp = re2(ind[[1]])
        for (exp in 2:length(ind)) {
            if (ind[[exp]][2] == "FALSE") {
                expect_error(re2_replace(ind[[1]], ind[[exp]][1], "a") ,
                             "rewrite string error")
            } else { # "TRUE"
                expect_equal(re2_replace(ind[[1]], ind[[exp]][1], "q"), "q")
            }
        }
    }

})

