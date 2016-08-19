## This file is part of the 're2r' package for R.
## Copyright (C) 2016, Qin Wenfeng
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##
## 1. Redistributions of source code must retain the above copyright notice,
## this list of conditions and the following disclaimer.
##
## 2. Redistributions in binary form must reproduce the above copyright notice,
## this list of conditions and the following disclaimer in the documentation
## and/or other materials provided with the distribution.
##
## 3. Neither the name of the copyright holder nor the names of its
## contributors may be used to endorse or promote products derived from
## this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
## "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
## BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
## FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
## HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
## SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
## PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
## OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
## WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
## OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
## EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

cat_lines = function(des, res){
    cat(des)
    cat(res)
    cat("\n")
}

#' Print information about a pre-compiled regular expression
#' @param x a pre-compiled regular expression
#' @param options print options
#' @param ... further arguments passed to or from other methods.
#' @examples
#' re2("(.*)@([^.]*)")
#' re2("(?P<name>sd)")
#' print(re2("sd"), options = TRUE)
#' @export
print.re2c = function(x, options = FALSE, ...) {

    cat("re2 pre-compiled regular expression\n\n")
    na_status = is_re2c_na(x)
    if(na_status){
        cat("NA string pattern\n")
    }
    cat_lines("pattern: ", get_pattern(x))
    cat_lines("number of capturing subpatterns: ", get_number_of_groups(x))
    cat_lines("capturing names with indices: \n", paste(get_named_groups(x)))
    cat_lines("expression size: ", get_expression_size(x))

    if(options && !na_status){
        cat("\nOptions:\n\n")
        opt = get_options(x)
        cat_lines("UTF-8 pattern: ", opt$utf_8)
        cat_lines("sase sensitive: ", opt$case_sensitive)
        cat_lines("posix syntax: ", opt$posix_syntax)
        if(opt$posix_syntax){
            cat_lines("^$ only match beginning and end of text: ", opt$one_line)
            cat_lines("perl classes: ", opt$perl_classes)
            cat_lines("word boundary: ", opt$word_boundary)
        }
        cat_lines("dot match new line: ", opt$dot_nl)
        cat_lines("literal pattern string: ", opt$literal)
        cat_lines("longest match: ", opt$longest_match)
        cat_lines("never match \\n: ", opt$never_nl)
        cat_lines("never capture: ", opt$never_capture)
        cat_lines("max memory: ", opt$max_mem)
    }
    invisible(x)
}
