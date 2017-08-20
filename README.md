re2r
====

[![Build
Status](https://travis-ci.org/qinwf/re2r.svg?branch=master)](https://travis-ci.org/qinwf/re2r)
[![Build
status](https://ci.appveyor.com/api/projects/status/n34unrvurpv18si5/branch/master?svg=true)](https://ci.appveyor.com/project/qinwf/re2r/branch/master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/re2r)](http://cran.r-project.org/package=re2r)
[![codecov](https://codecov.io/gh/qinwf/re2r/branch/master/graph/badge.svg)](https://codecov.io/gh/qinwf/re2r)

RE2 is a primarily DFA based regexp engine from Google that is very fast
at matching large amounts of text.

Installation
------------

From GitHub:

    library(devtools)
    install_github("qinwf/re2r", build_vignettes = T)

To learn how to use, you can check out the
[vignettes](https://qinwenfeng.com/re2r_doc/).

Related Work
------------

[Google Summer of
Code](https://github.com/rstats-gsoc/gsoc2016/wiki/re2-regular-expressions)
- re2 regular expressions.

Brief Intro
-----------

### 1. Search a string for a pattern

`re2_detect(string, pattern)` searches the string expression for a
pattern and returns boolean result.

    test_string = "this is just one test";
    re2_detect(test_string, "(o.e)")

    ## [1] TRUE

Here is an example of email pattern.

    show_regex("\\b[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}\\b", width = 670, height = 280)

![email
pattern](https://raw.githubusercontent.com/qinwf/re2r/master/inst/img/email.png)

    re2_detect("test@gmail.com", "\\b[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}\\b")

    ## [1] TRUE

`re2_match(string, pattern)` will return the capture groups in `()`.

    (res = re2_match(test_string, "(o.e)"))

    ##      .match .1   
    ## [1,] "one"  "one"

The return result is a character matrix. `.1` is the first capture group
and it is unnamed group.

Create named capture group with `(?P<name>pattern)` syntax.

    (res = re2_match(test_string, "(?P<testname>this)( is)"))

    ##      .match    testname .2   
    ## [1,] "this is" "this"   " is"

If there is no capture group, the matched origin strings will be
returned.

    test_string = c("this is just one test", "the second test");
    (res = re2_match(test_string, "is"))

    ##      .match
    ## [1,] "is"  
    ## [2,] NA

`re2_match_all()` will return the all of patterns in a string instead of
just the first one.

    re2_match_all(c("this is test", 
                "this is test, and this is not test", 
                "they are tests"), 
              pattern = "(?P<testname>this)( is)")

    ## [[1]]
    ##      .match    testname .2   
    ## [1,] "this is" "this"   " is"
    ## 
    ## [[2]]
    ##      .match    testname .2   
    ## [1,] "this is" "this"   " is"
    ## [2,] "this is" "this"   " is"
    ## 
    ## [[3]]
    ##      .match testname .2

### 2. Replace a substring

    re2_replace(string, pattern, rewrite)

Searches the string "input string" for the occurence(s) of a substring
that matches 'pattern' and replaces the found substrings with "rewrite
text".

    input_string = "this is just one test";
    new_string = "my"
    re2_replace(new_string, "(o.e)", input_string)

    ## [1] "my"

### 3. Extract a substring

    re2_extract(string, pattern, replacement)

Extract matching patterns from a string.

    re2_extract("yabba dabba doo", "(.)")

    ## [1] "y"

    re2_extract("test@me.com", "(.*)@([^.]*)")

    ## [1] "test@me"

### 4. `Regular Expression Object` for better performance

We can create a regular expression object (RE2 object) from a string. It
will reduce the time to parse the syntax of the same pattern.

And this will also give us more option for the pattern. run `help(re2)`
to get more detials.

    regexp = re2("test",case_sensitive = FALSE)
    print(regexp)

    ## re2 pre-compiled regular expression
    ## 
    ## pattern: test
    ## number of capturing subpatterns: 0
    ## capturing names with indices: 
    ## .match
    ## expression size: 10

    regexp = re2("test",case_sensitive = FALSE)
    re2_match("TEST", regexp)

    ##      .match
    ## [1,] "TEST"

    re2_replace("TEST", regexp, "ops")

    ## [1] "ops"

### 5. Multithread

Use `parallel` option to enable multithread feature. It will improve
performance for large inputs with a multi core CPU.

    re2_match(string, pattern, parallel = T)
