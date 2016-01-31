# Improvement

## 1 Remove Rcpp and exception

### pro: use plain C++ and R intergration may improve performance.

### con: more code is needed, the improvement may not be obvious.

We can try remove Rcpp and exception to compare the difference.

## 2 whether check encoding of input string. 

### pro: make sure the value used in c++ code is valid. 

### con: slow down the speed. 

This is needed only on Windows machines with a none-UTF-8 locale settings. 

For example, most Windows computers in China will used CP936 charset with GBK encoding instead of UTF-8 encoding, and most input strings are not valid UTF-8 input in R session. The strings need to be update by enc2utf8() function.

## 3 Parallel support?

### pro: RE2 pre-compiled pattern is thread safe. It may give better performance.

### con: cross platform.


### con: slow down the speed.