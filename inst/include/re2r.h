#ifndef RE2R_RE2R_H
#define RE2R_RE2R_H

#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

#include <re2/re2.h>
using re2::RE2;
using re2::StringPiece;

// exception

#define RCPP_EXCEPTION_CLASS(__CLASS__,__WHAT__)                               \
    class __CLASS__ : public std::exception{                                       \
    public:                                                                        \
        __CLASS__( const std::string& message ) throw() : message( __WHAT__ ){} ;  \
        virtual ~__CLASS__() throw(){} ;                                           \
        virtual const char* what() const throw() { return message.c_str() ; }      \
    private:                                                                       \
        std::string message ;                                                      \
    } ;

#define RCPP_SIMPLE_EXCEPTION_CLASS(__CLASS__,__MESSAGE__)                     \
    class __CLASS__ : public std::exception{                                       \
    public:                                                                        \
        __CLASS__() throw() {} ;                                                   \
        virtual ~__CLASS__() throw(){} ;                                           \
        virtual const char* what() const throw() { return __MESSAGE__ ; }          \
    } ;

RCPP_EXCEPTION_CLASS(ErrorInternal, std::string("unexpected error:") + message)

RCPP_EXCEPTION_CLASS(ErrorBadEscape, std::string("bad escape sequence: ") + message)

RCPP_EXCEPTION_CLASS(ErrorBadCharRange, std::string("bad character class range: ") + message)

RCPP_EXCEPTION_CLASS(ErrorBadCharClass, std::string("bad character class: ") + message)

RCPP_EXCEPTION_CLASS(ErrorMissingBracket, std::string("missing closing ]: ") + message)

RCPP_EXCEPTION_CLASS(ErrorMissingParen, std::string("missing closing ): ") + message)

RCPP_EXCEPTION_CLASS(ErrorTrailingBackslash, std::string("trailing \\ at end of regexp: ") + message)

RCPP_EXCEPTION_CLASS(ErrorRepeatArgument, std::string("repeat argument: ") + message)

RCPP_EXCEPTION_CLASS(ErrorRepeatSize, std::string("bad repetition argument: ") + message)

RCPP_EXCEPTION_CLASS(ErrorRepeatOp, std::string("bad repetition operator: ") + message)

RCPP_EXCEPTION_CLASS(ErrorBadPerlOp, std::string("bad perl operator: ") + message)

RCPP_EXCEPTION_CLASS(ErrorBadUTF8, std::string("invalid UTF-8 in regexp: ") + message)

RCPP_EXCEPTION_CLASS(ErrorBadNamedCapture, std::string("bad named capture group: ") + message)

RCPP_EXCEPTION_CLASS(ErrorPatternTooLarge, std::string("pattern too large (compile failed): ") + message)

RCPP_EXCEPTION_CLASS(ErrorRewriteString, std::string("rewrite string error: ") + message)

RCPP_EXCEPTION_CLASS(ErrorAnchorType, std::string("anchor type error: ") + message)

XPtr<RE2> cpp_re2_compile(const char* pattern,
                          bool log_errors_value,
                          bool utf_8_value,
                          bool posix_syntax_value,
                          bool case_sensitive_value,
                          bool dot_nl_value,
                          bool literal_value,
                          bool longest_match_value,
                          bool never_nl_value,
                          bool never_capture_value,
                          bool one_line_value,
                          bool perl_classes_value,
                          bool word_boundary_value,
                          int64_t max_mem_value);

#endif
