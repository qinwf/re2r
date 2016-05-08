// This file is part of the 're2r' package for R.
// Copyright (C) 2016, Qin Wenfeng
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// 3. Neither the name of the copyright holder nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
// BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
// OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
// OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


#include "../inst/include/re2r.h"

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;

inline size_t getUtf8CharSize(char ch) {
    return ((0xE5000000 >> ((ch >> 3) & 0x1E)) & 3) + 1;
}

struct SplitP : public Worker
{
    vector<string>& input;
    vector<vector<string>>& output;
    RE2& tt;
    RE2::Options& opt;
    size_t limit;

    SplitP(vector<string>&  input_, vector<vector<string>>& output_, RE2& tt_, RE2::Options& opt_, size_t limit_)
        : input(input_), output(output_), tt(tt_), opt(opt_),limit(limit_){}

    void operator()(std::size_t begin, std::size_t end) {
        RE2 pattern(tt.pattern(),opt);
        std::transform(input.begin() + begin,
                       input.begin() + end,
                       output.begin() + begin,
                       [this,&pattern](string& x) -> vector<string>{
                           StringPiece str(x);
                           auto str_size = x.length();
                           size_t lastIndex = 0;
                           StringPiece match;
                           vector<string> pieces;

                           while (lastIndex < str_size &&
                                  pattern.Match(str, lastIndex, str_size, RE2::UNANCHORED,
                                                 &match, 1)) {

                               if (pieces.size() >= limit-1) {
                                   break;
                               }
                               if (match.size()) {
                                   if (match.data() == str.data() || match.data() - str.data() > lastIndex) {
                                       pieces.push_back(StringPiece(str.data() + lastIndex, match.data() - str.data() - lastIndex).as_string());
                                   }
                                   lastIndex = match.data() - str.data() + match.size();
                               } else {
                                   size_t sym_size = getUtf8CharSize(str.data()[lastIndex]);
                                   pieces.push_back( StringPiece(str.data() + lastIndex, sym_size).as_string());
                                   lastIndex += sym_size;
                               }

                           }
                           if ( pieces.size() < limit && (lastIndex < str_size || (lastIndex == str_size && match.size()))) {
                               pieces.push_back(StringPiece(str.data() + lastIndex, str_size - lastIndex).as_string());
                           }
                           return pieces;
                       });
    }
};

struct SplitFixP : public Worker
{
    vector<string>& input;
    vector<vector<string>>& output;
    RE2& tt;
    RE2::Options& opt;
    size_t limit;

    SplitFixP(vector<string>&  input_, vector<vector<string>>& output_, RE2& tt_, RE2::Options& opt_,size_t limit_)
        : input(input_), output(output_), tt(tt_), opt(opt_),limit(limit_){}

    void operator()(std::size_t begin, std::size_t end) {
        RE2 pattern(tt.pattern(),opt);
        std::transform(input.begin() + begin,
                       input.begin() + end,
                       output.begin() + begin,
                       [this,&pattern](string& x) -> vector<string>{
                           StringPiece str(x);
                           auto str_size = x.length();
                           size_t lastIndex = 0;
                           StringPiece match;
                           vector<string> pieces;

                           size_t split_n = 0;
                           while (lastIndex < str_size &&
                                  pattern.Match(str, lastIndex, str_size, RE2::UNANCHORED,
                                                 &match, 1)) {

                               if (split_n >= limit-1) {
                                   break;
                               }
                               if (match.size()) {
                                   if (match.data() == str.data() || match.data() - str.data() > lastIndex) {
                                       pieces.push_back(StringPiece(str.data() + lastIndex, match.data() - str.data() - lastIndex).as_string());

                                       split_n++;
                                   }
                                   lastIndex = match.data() - str.data() + match.size();
                               } else {
                                   size_t sym_size = getUtf8CharSize(str.data()[lastIndex]);
                                   pieces.push_back(StringPiece(str.data() + lastIndex, sym_size).as_string());
                                   lastIndex += sym_size;
                                   split_n++;
                               }

                           }
                           if ( split_n < limit && (lastIndex < str_size || (lastIndex == str_size && match.size()))) {
                               pieces.push_back( StringPiece(str.data() + lastIndex, str_size - lastIndex).as_string());
                               split_n++;
                           }
                           while (split_n < limit){
                               pieces.push_back("");
                               split_n++;
                           }

                           return pieces;
                       });
    }
};


SEXP cpp_split_not_fixed(CharacterVector& input,RE2* pattern,size_t limit){
    SEXP inputx = input;
    R_xlen_t index = 0;
    Shield<SEXP> ress(Rf_allocVector(VECSXP,input.size()));
    SEXP res = ress;

    for(auto it = 0; it != input.size(); it++){
        StringPiece str(R_CHAR(STRING_ELT(inputx, it)));
        auto str_size = strlen(R_CHAR(STRING_ELT(inputx, it)));
        size_t lastIndex = 0;
        StringPiece match;
        vector<string> pieces;

        while (lastIndex < str_size &&
               pattern->Match(str, lastIndex, str_size, RE2::UNANCHORED,
                              &match, 1)) {

            if (pieces.size() >= limit-1) {
                break;
            }
            if (match.size()) {
                if (match.data() == str.data() || match.data() - str.data() > lastIndex) {
                    pieces.push_back(StringPiece(str.data() + lastIndex, match.data() - str.data() - lastIndex).as_string());
                }
                lastIndex = match.data() - str.data() + match.size();
            } else {
                size_t sym_size = getUtf8CharSize(str.data()[lastIndex]);
                pieces.push_back( StringPiece(str.data() + lastIndex, sym_size).as_string());
                lastIndex += sym_size;
            }

        }
        if ( pieces.size() < limit && (lastIndex < str_size || (lastIndex == str_size && match.size()))) {
            pieces.push_back(StringPiece(str.data() + lastIndex, str_size - lastIndex).as_string());
        }
        SET_VECTOR_ELT(res, index, Shield<SEXP>(toprotect_vec_string_sexp(pieces)));
        index++;
    }

    return res;
}

SEXP cpp_split_fixed(CharacterVector& input,RE2* pattern,size_t limit){
    SEXP inputx = input;
    Shield<SEXP> ress( Rf_allocMatrix(STRSXP, (R_xlen_t) input.size(), (R_xlen_t) limit));
    SEXP res = ress;
    auto empstring = Rf_mkCharLenCE( "",  strlen("") , CE_UTF8);
    for(auto it = 0; it != input.size(); it++){
        StringPiece str(R_CHAR(STRING_ELT(inputx, it)));
        auto str_size = strlen(R_CHAR(STRING_ELT(inputx, it)));
        size_t lastIndex = 0;
        StringPiece match;
        size_t split_n = 0;
        while (lastIndex < str_size &&
               pattern->Match(str, lastIndex, str_size, RE2::UNANCHORED,
                              &match, 1)) {

            if (split_n >= limit-1) {
                break;
            }
            if (match.size()) {
                if (match.data() == str.data() || match.data() - str.data() > lastIndex) {
                    string tmpstring = StringPiece(str.data() + lastIndex, match.data() - str.data() - lastIndex).as_string();
                    SET_STRING_ELT(res, it + split_n * input.size(),Rf_mkCharLenCE(tmpstring.c_str(),  strlen(tmpstring.c_str()) , CE_UTF8));
                    split_n++;
                }
                lastIndex = match.data() - str.data() + match.size();
            } else {
                size_t sym_size = getUtf8CharSize(str.data()[lastIndex]);
                string tmpstring = StringPiece(str.data() + lastIndex, sym_size).as_string();
                SET_STRING_ELT(res, it + split_n * input.size(),Rf_mkCharLenCE(tmpstring.c_str(),  strlen(tmpstring.c_str()) , CE_UTF8));
                lastIndex += sym_size;
                split_n++;
            }

        }
        if ( split_n < limit && (lastIndex < str_size || (lastIndex == str_size && match.size()))) {
            string tmpstring = StringPiece(str.data() + lastIndex, str_size - lastIndex).as_string();
            SET_STRING_ELT(res, it + split_n * input.size(),Rf_mkCharLenCE(tmpstring.c_str(),  strlen(tmpstring.c_str()) , CE_UTF8));
            split_n++;
        }
        while (split_n < limit){
            SET_STRING_ELT(res, it + split_n * input.size(),empstring);
            split_n++;
        }
    }

    return res;


}

// [[Rcpp::export]]
SEXP cpp_split(CharacterVector input, XPtr<RE2Obj>& ptr, NumericVector part, bool fixed, bool parallel){
    RE2* pattern = &(ptr->regexp);
    SEXP inputx = input;

    if (part.size() == 0){
        stop("need the number of pieces.");
    }
    size_t limit = numeric_limits<R_xlen_t>::max();

    if (R_finite(part[0])){
        limit = as<size_t>(part);

    }
    if (!parallel){
        if (!fixed){
            return cpp_split_not_fixed(input, pattern, limit);
        } else {
            return cpp_split_fixed(input, pattern, limit);
       }

    }
    else{
        vector<string> inputv = as<vector<string>>(input);
        vector<vector<string>> res(input.size());

        if (!fixed){


            SplitP pobj(inputv, res, *pattern, *(ptr->options), limit);
            parallelFor(0, input.size(), pobj, 150000);

            Shield<SEXP>  xs(Rf_allocVector(VECSXP, input.size()));
            SEXP x = xs;

            R_xlen_t index = 0;

            for (auto resi : res){
                if (resi.empty()) {
                    SET_VECTOR_ELT(x, index, R_NilValue);
                }else{
                    SET_VECTOR_ELT(x, index, Shield<SEXP>(toprotect_vec_string_sexp(resi)));
                }
                index ++;
            }
            return x;
        } else {

            SplitFixP pobj(inputv, res, *pattern, *(ptr->options), limit);
            parallelFor(0, input.size(), pobj,250000);
            Shield<SEXP>  xs(Rf_allocMatrix(STRSXP, input.size(),limit));
            SEXP x = xs;

            R_xlen_t rowi = 0;
            R_xlen_t coli = 0;

            for (auto resi : res){
                for(string& cell : resi){
                    SET_STRING_ELT(x, rowi+ coli*input.size(), Rf_mkCharLenCE(cell.c_str(),  strlen(cell.c_str()) , CE_UTF8));
                    coli++;
                }

                rowi ++;
                coli = 0;
            }
            return x;
        }

    }
}
