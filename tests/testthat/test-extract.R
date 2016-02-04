context("check rewrite")

test_that("check rewrite", {
    # from re2_test.cc
    expect_identical(re2_extract("(.*)@([^.]*)", "boris@kremvax.ru","\\2!\\1"),"kremvax!boris")
    expect_identical(re2_extract(".*", "foo","'\\0'"),"'foo'")
    expect_identical(re2_extract("bar", "baz","'\\0'"),"")

})

