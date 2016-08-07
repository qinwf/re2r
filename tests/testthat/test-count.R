context("test count")

test_that("test count",{
    tt = function(string, pattern,res, anchor = UNANCHORED,  rep_res = rep(res,1000), parallel_rep = FALSE){
        expect_equal(re2_count(string,pattern,anchor), res)
        expect_equal(re2_count(string,pattern,anchor,parallel = T), res)
        expect_equal(re2_count(string,pattern,anchor,parallel = T, grain_size = 1), res)
        if(parallel_rep){
            expect_equal(re2_count(rep(string, 1000),pattern,anchor, parallel=T, grain_size = 1), rep_res)
        }
    }
    # stringi tests
    tt(character(0)," ",integer(0))
    tt(NA,"a",NA_integer_)
    tt("NA",NA,NA_integer_)
    tt("   "," ",3L)
    tt("###",c("#","##","###"),c(3L,1L,1L))
    tt("a a","a",2L)
    tt("aba","abcdef",0L)
    tt("","", 1)
    tt("a","", 2)

    tt(c("", " ", "     "), "^.*$", c(1L, 1L, 1L))
    tt(c("", " ", "     "), "^.+$", c(0L, 1L, 1L))
    tt(c("", " ", "     "), ".*", c(1L, 2L, 2L))

    tt(c("\u0105\u0106\u0107", "\u0105\u0107"), "\u0106*", c(4L, 3L)) # match of zero length

    library(stringi)
    s <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit. Proin
    nibh augue, suscipit a, scelerisque sed, lacinia in, mi. Cras vel
    lorem. Etiam pellentesque aliquet tellus. Phasellus pharetra nulla ac
    diam. Quisque semper justo at risus. Donec venenatis, turpis vel
    hendrerit interdum, dui ligula ultricies purus, sed posuere libero dui
    id orci. Nam congue, pede vitae dapibus aliquet, elit magna vulputate
    arcu, vel tempus metus leo non est. Etiam sit amet lectus quis est
    congue mollis. Phasellus congue lacus eget neque. Phasellus ornare,
    ante vitae consectetuer consequat, purus sapien ultricies dolor, et
    mollis pede metus eget nisi. Praesent sodales velit quis augue. Cras
    suscipit, urna at aliquam rhoncus, urna quam viverra nisi, in interdum
    massa nibh nec erat."
    s <- stri_dup(s,1:3)
    expect_warning(tt(s,c("o","a"),c(25L, 100L, 75L)))
    tt("ALA","ala", 0L)
    tt("ALA",re2("ala", case_sensitive = F), 1L)
    # difference [[a-z]]
    tt(s,"m [a-z]", 1:3*7L)
    tt(s,"m, [a-z]", 1:3)
    tt(s,"[[:digit:]]", c(0L,0L,0L))
    tt(s," [a-z]*\\. Phasellus (ph|or|co)", 1:3*3L)
    s <- c("abababab babab abab bbaba","a")
    tt(s,"bab",c(5L,0L))
    tt(c("lalal","12l34l56","\u0105\u0f3l\u0142"),"l",3:1)
    tt("aaaab", "ab", 1L)
    tt("bababababaab", "aab", 1L)

    tt("X\U00024B62\U00024B63\U00024B64X",
        c("\U00024B62", "\U00024B63", "\U00024B64", "X"),
                     c(1L, 1L, 1L, 2L))

    tt(NA, "\\p{Z}", NA_integer_)
    tt(c(""), "\\p{L}", 0L)

    tt(c("a", "ab", "abc", "1234"), "\\p{L}", c(1L,2L,3L,0L))
    tt("a\u0105bc", c("\\p{L}", "\\p{Ll}", "\\p{Lu}"), c(4L,4L,0L))

})
