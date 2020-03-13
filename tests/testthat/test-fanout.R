context("test fanout")

test_that("test fanout",{
    re1 = re2("(?:(?:(?:(?:(?:.)?){1})*)+)")
    re10 = re2("(?:(?:(?:(?:(?:.)?){10})*)+)")
    re100 = re2("(?:(?:(?:(?:(?:.)?){100})*)+)")
    re1000 = re2("(?:(?:(?:(?:(?:.)?){1000})*)+)")

    expect_identical(get_program_fanout(re1)[2,2],1)
    expect_identical(get_program_fanout(re1)[2,1],3)
    expect_identical(get_program_fanout(re10)[2,2],10)
    expect_identical(get_program_fanout(re10)[2,1],6)
    expect_identical(get_program_fanout(re100)[2,2],100)
    expect_identical(get_program_fanout(re100)[2,1],9)
    expect_identical(get_program_fanout(re1000)[2,2],1000)
    expect_identical(get_program_fanout(re1000)[2,1],13)
})
