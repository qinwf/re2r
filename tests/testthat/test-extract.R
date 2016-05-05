context("check extract")

test_that("check rewrite", {
    # from re2_test.cc
    expect_identical(re2_extract("boris@kremvax.ru", "(.*)@([^.]*)","\\2!\\1"),"kremvax!boris")
    expect_identical(re2_pextract("boris@kremvax.ru", "(.*)@([^.]*)","\\2!\\1"),"kremvax!boris")

    expect_identical(re2_extract("foo", ".*", "'\\0'"),"'foo'")
    expect_identical(re2_pextract("foo", ".*", "'\\0'"),"'foo'")

    expect_true(is.na(re2_extract("baz", "bar", "'\\0'")))
    expect_true(is.na(re2_pextract("baz", "bar", "'\\0'")))

})
