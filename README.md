# re2r

[![Build Status](https://travis-ci.org/qinwf/re2r.svg?branch=master)](https://travis-ci.org/qinwf/re2r) [![Build status](https://ci.appveyor.com/api/projects/status/n34unrvurpv18si5/branch/master?svg=true)](https://ci.appveyor.com/project/qinwf/re2r/branch/master) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/re2r)](http://cran.r-project.org/package=re2r) 


RE2 is a primarily DFA based regexp engine from Google that is very fast at matching large amounts of text.

## Installation

To install from GitHub:

```r
library(devtools)
install_github("rstudio/rmarkdown")
install_github("yihui/knitr")
install_github("qinwf/re2r", build_vignettes = T, force = T)
```

To learn how to use, you can check out the [vignettes](vignettes/re2r-intro.Rmd).

## Related Work

[Google Summer of Code](https://github.com/rstats-gsoc/gsoc2016/wiki/re2-regular-expressions) - re2 regular expressions.

## Brief Intro

### 1. Search a string for a pattern

```r
## Sys.setlocale(locale = "English") ## for Windows users with non-UTF8 locale
## re2_match(pattern, string)

test_string = "this is just one test";
re2_match("(o.e)", test_string)
```

```r
## [1] TRUE
```

Searches the string expression for the occurence(s) of a substring that matches 'pattern' and returns boolean result.

With `value = TRUE` option, function will return the capture groups with `()`.

```r
(res = re2_match("(o.e)", test_string, value = TRUE))

##     ?1   
##[1,] "one"

str(res)

## chr [1, 1] "one"
## - attr(*, "dimnames")=List of 2
##  ..$ : NULL
##  ..$ : chr "?1"
```

The return result is a data.frame. `?1` is the first capture group and it is unnamed group.

We can create named capture group with `(?P<name>pattern)` syntax.

```r
(res = re2_match("(?P<testname>this)( is)", test_string, value = TRUE))

##     testname ?2   
##[1,] "this"   " is"

str(res)

##[1] "matrix"
```

With `all = TRUE` option, function will return the all of patterns in a string instead of just the first one.

```r
(res = re2_match("(is)", test_string, value = TRUE, all = TRUE))
```

```r
##     !n  ?1  
##[1,] "1" "is"
##[2,] "1" "is"
```

`!n` is the index of the input string. We can provide a character vector to a pattern.

```r
test_string = c("this is just one test", "the second test");
(res = re2_match("(is)", test_string, value = TRUE, all = TRUE))
```

```r
##     !n  ?1  
##[1,] "1" "is"
##[2,] "1" "is"
##[3,] "2" NA  
```

If there is no capture group, and `value = TRUE`, the matched origin strings will be returned.

```r
test_string = c("this is just one test", "the second test");
(res = re2_match("is", test_string, value = TRUE))
```

```r
##     ?nocapture             
##[1,] "this is just one test"
##[2,] NA    
```

### 2. Replace a substring

```r
## re2_replace(pattern, rewrite, input)
```

Searches the string "input string" for the occurence(s) of a substring that matches 'pattern' and replaces the found substrings with "rewrite text".

```r
input_string = "this is just one test";
new_string = "my"
re2_replace("(o.e)", new_string, input_string)
```

```r
## [1] "this is just my test"
```

### 3. Extract a substring

```r
## re2_extract(pattern, input, rewrite = optional)
```

Searches the string "input string" for the occurence(s) of a substring that matches 'pattern' and return the found substrings with "rewrite text".

```r
re2_extract("(.)","yabba dabba doo")
```

```r
## [1] "y"
```

```r
re2_extract("(.*)@([^.]*)","test@me.com","\\2!\\1")
```

```r
## [1] "me!test"
```

`\\1` and `\\2` are the first and second capture groups.

### 4. `Regular Expression Object` for better performance

We can create a regular expression object (RE2 object) from a string. It will reduce the time to parse the syntax of the same pattern. 

And this will also give us more option for the pattern. run `help(re2)` to get more detials.

```r
regexp = re2("test",case_sensitive = FALSE)
print(regexp)
## re2 pre-compiled regular expression
## 
## pattern: test
## number of capturing subpatterns: 0
## capturing names with indices: 
## named integer(0)
## expression size: 11

regexp %<~% "(?P<first>1*)"
regexp
## re2 pre-compiled regular expression
## 
## pattern: (?P<first>1*)
## number of capturing subpatterns: 1
## capturing names with indices: 
## first 
##     1 
## expression size: 8
```

`%<~%` is a operator to create a new RE2 object.

```r
regexp = re2("test",case_sensitive = FALSE)
re2_match(regexp, "TEST")
## [1] TRUE
re2_replace(regexp, "ops", "TEST")
## [1] "ops"
```

If you come from a `Perl` world, you may be insterested in `%=~%`  `%!~%`.

```r
"TEST" %=~% regexp
## [1] TRUE
"TEST" %!~% regexp
## [1] FALSE
```
