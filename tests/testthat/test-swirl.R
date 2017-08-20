context("swirl tests")
source('helper.R')

test_that("swirl cases",{
    tt = function(string, pattern, replacement = NULL,
                  detect = NULL, match = NULL, match_all = NULL,
                  replace = NULL, replace_all = NULL,
                  split = NULL, split_fixed = NULL, split_n = NULL,
                  extract = NULL, extract_all = NULL,
                  locate = NULL, locate_all = NULL,
                  count = NULL, ...){
        if(!inherits(pattern, "list") && !inherits(pattern, "re2c")){
            pattern = re2(pattern)
        }
        if(!is.null(detect)){
            expect_equal(re2_detect(string = string, pattern = pattern), detect)
        }
        if(!is.null(match)){
            eq_with_class(re2_match(string, pattern), match)
        }
        if(!is.null(match_all)){
            eq_with_class(re2_match_all(string,pattern),match_all)
        }
        if(!is.null(replace) && !is.null(replacement)){
            expect_equivalent(re2_replace(string,pattern,replacement),replace)
        }
        if(!is.null(replace_all) && !is.null(replacement)){
            expect_equivalent(re2_replace_all(string,pattern,replacement),replace_all)
        }
        if(!is.null(split)){
            expect_equivalent(re2_split(string,pattern),split)
        }
        if(!is.null(split_fixed) && !is.null(split_n)){
            expect_equal(re2_split_fixed(string,pattern,split_n),split_fixed)
        }
        if(!is.null(extract)){
            expect_equal(re2_extract(string,pattern),extract)
        }
        if(!is.null(extract_all)){
            expect_equivalent(re2_extract_all(string,pattern),extract_all)
        }
        if(!is.null(locate)){
            eq_with_class(re2_locate(string,pattern),locate)
        }
        if(!is.null(locate_all)){
            eq_with_class(re2_locate_all(string,pattern),locate_all)
        }
        if(!is.null(count)){
            expect_equivalent(re2_count(string,pattern),count)
        }
    }

    tt("sdsd\n","\n", replacement = "a",
       detect = TRUE, match = "\n", replace = "sdsda",
       replace_all = "sdsda",
       count = 1,split = list(c("sdsd","")),
       extract = "\n", extract_all = "\n")

    "Unicode properties"

    tt("sdsd\n","\\p{L}+", replacement = "a",
       detect = TRUE, match = "sdsd", replace = "a\n",
       replace_all = "a\n",
       count = 1,split = list(c("","\n")),
       extract = "sdsd", extract_all = "sdsd")

    tt("SDSD\n","\\p{Ll}+", replacement = "a",
       detect = FALSE, match = NA_character_, replace = "SDSD\n",
       replace_all = "SDSD\n",
       count = 0,split = list(c("SDSD\n")),
       extract = NA_character_, extract_all = list(character(0)))

    "number"
    tt("123123","^[0-9]*$",detect = T, match = "123123")
    tt("123s", "^[0-9]*$", detect = FALSE, match = NA_character_)
    tt("12s3", "^[0-9]*$", detect = FALSE, match = NA_character_)

    "n numbers"
    tt("123283","^\\d{6}$",detect = T, match = "123283")
    tt("123283","^\\d{7}$",detect = FALSE, match = NA_character_)

    "m-n numbers"
    tt("123232","^\\d{2,8}$",detect = T,match = "123232")
    tt("1","^\\d{2,8}$",detect = FALSE, match= NA_character_)

    "number with at most two digits"
    tt("123.2","^([1-9][0-9]*)+(\\.[0-9]{1,2})?$",detect = T,match = c("123.2","123",".2"))
    tt("123x2","^([1-9][0-9]*)+(\\.[0-9]{1,2})?$",detect = F,match = rep(NA_character_,3))
    tt("123.211","^([1-9][0-9]*)+(\\.[0-9]{1,2})?$",detect = F,match = rep(NA_character_,3))

    "dollar"
    tt("123 USD","\\b(\\d+)\\s?USD",replacement = "\\1 $", replace = "123 $")

    "alter"
    tt("this is the day.","th(e|is|at)", match_all =
           list(structure(c("this", "the", "is", "e"),
                          .Dim = c(2L, 2L),
                          .Dimnames = list(
                              NULL, c(".match", ".1")))))

    "html tag with show_regex to read"
    pattern = re2("(<script(\\s|\\S)*?<\\/script>)|(<style(\\s|\\S)*?<\\/style>)|(<!--(\\s|\\S)*?-->)|(<\\/?(\\s|\\S)*?>)")
    # show_regex(pattern)
    tt("<a>sdsd<\a>",pattern, match = c("<a>", NA, NA, NA, NA, NA, NA, "<a>", "a"))
    tt("<style>sasdsd</a>",pattern, match = c("<style>", NA, NA, NA, NA, NA, NA, "<style>", "e"))
    tt("<style>sasdsd</style>",pattern, match = c("<style>sasdsd</style>", NA, NA, "<style>sasdsd</style>", "d",
                                         NA, NA, NA, NA))

    "removing leading and trailing whitespace"
    tt("    foo bar ","^\\s+","",replace = "foo bar ", replace_all = "foo bar ")
    tt("    foo bar ","\\s+$","",replace = "    foo bar", replace_all = "    foo bar")
    tt("foo bar",c("^\\s+","\\s+$"), detect = c(F,F))

    "html hex"
    tt("#fff", "#([a-fA-F0-9]){3}(([a-fA-F0-9]){3})?$", detect = T)
    tt("#926231", "#([a-fA-F0-9]){3}(([a-fA-F0-9]){3})?$", detect = T)
    tt("#9a", "#([a-fA-F0-9]){3}(([a-fA-F0-9]){3})?$", detect = FALSE)

    "US SSN"
    tt("012-22-1212","\\d{3}-\\d{2}-\\d{4}",detect = T)
    tt("012-221-1212","\\d{3}-\\d{2}-\\d{4}",detect = FALSE)

    "$"
    tt(c("$330","$12","000.11","$1.0001","$.99"),"^\\$(\\d{1,3}(\\,\\d{3})*|\\d+)(\\.\\d{2})?$",
       detect = c(T,T,F,F,F))

    "date"
    tt("02/13/1998 21:15:38","\\d\\d\\/\\d\\d\\/\\d\\d\\d\\d \\d\\d:\\d\\d:\\d\\d",detect = T)
    tt("4/30/1998 20:25:30, 4/31/28","\\d\\d\\/\\d\\d\\/\\d\\d\\d\\d \\d\\d:\\d\\d:\\d\\d",detect = F)

    "ip" # never look back, write as a line
    pattern = "^(\\d|[01]?\\d\\d|2[0-4]\\d|25[0-5])\\.(\\d|[01]?\\d\\d|2[0-4]\\d|25[0-5])\\.(\\d|[01]?\\d\\d|2[0-4]\\d|25[0-5])\\.(\\d|[01]?\\d\\d|2[0-4]\\d|25[0-5])$"
    # show_regex(pattern)
    tt(c("127.0.0.1","999.999.999.999","127.0.1"), pattern,
       detect = c(T,F,F))

    "mac"
    pattern = "([0-9a-fA-F]{2}:){5}[0-9a-fA-F]{2}"
    tt(c("02:23:41:62:29:ab","as:ss:a"), pattern, detect = c(T,F))

    "email"
    pattern = "[0-9a-zA-Z]([-.\\w]*[0-9a-zA-Z_+])*@([0-9a-zA-Z][-\\w]* [0-9a-zA-Z]\\.)+[a-zA-Z]{2,9}"
    tt(c("tony@example.com",
         "ton@sa.com",
         "ton@mail.example.mu"), pattern, rep(T,3))
    tt(c(".@example.com", "ton@-.com",
         "tony@example.a"), pattern, rep(F,3))

    "HTML"
    pattern = "((https?):\\/\\/([0-9a-zA-Z][-\\w]*[0-9a-zA-Z]\\.)+[a-zA-Z]{2,9})(:\\d{1,4})?([-\\w\\/#~:.?+=&%@~]*)"
    tt(c("https://example.com",
         "http://foo.com:8080/bar.html"),pattern,
       detect = c(T,T))

    "3X"
    pattern = "^[0369]*(([147][0369]*|[258][0369]*[258][0369]*)([147][0369]*[258][0369]*)*([258][0369]*|[147][0369]*[147][0369]*)|[258][0369]*[147][0369]*)*$"
    expect_true(re2_detect(stri_dup("123",100),pattern))

    # dd = stri_dup("123",1000)
    # microbenchmark(re2_detect(dd,pattern),stri_detect_regex(dd,pattern))

    "(a+)+"
    expect_true(re2_detect("aaa","(a+)+"), TRUE)
    # dd = stri_dup("a",10000)
    # microbenchmark(re2_detect(dd,"(a+)+"),stri_detect_regex(dd,"(a+)+"))


})
