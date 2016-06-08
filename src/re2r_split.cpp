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
    optstring& input;
    vector<tr2::optional<vector<string>>>& output;
    RE2& tt;
    RE2::Options& opt;
    size_t limit;

    SplitP(optstring&  input_, vector<tr2::optional<vector<string>>>& output_, RE2& tt_, RE2::Options& opt_, size_t limit_)
        : input(input_), output(output_), tt(tt_), opt(opt_),limit(limit_){}

    void operator()(std::size_t begin, std::size_t end) {
        RE2 pattern(tt.pattern(),opt);
        std::transform(input.begin() + begin,
                       input.begin() + end,
                       output.begin() + begin,
                       [this,&pattern](tr2::optional<string>& x) -> tr2::optional<vector<string>>{
                           if(!bool(x)){
                               return tr2::nullopt;
                           }
                           StringPiece str(x.value());
                           auto str_size = x.value().length();
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
                           return tr2::make_optional(pieces);
                       });
    }
};

struct SplitFixP : public Worker
{
    optstring& input;
    vector<tr2::optional<vector<string>>>& output;
    RE2& tt;
    RE2::Options& opt;
    size_t limit;

    SplitFixP(optstring&  input_, vector<tr2::optional<vector<string>>>& output_,
              RE2& tt_, RE2::Options& opt_,size_t limit_)
        : input(input_), output(output_), tt(tt_), opt(opt_),limit(limit_){}

    void operator()(std::size_t begin, std::size_t end) {
        RE2 pattern(tt.pattern(),opt);
        std::transform(input.begin() + begin,
                       input.begin() + end,
                       output.begin() + begin,
                       [this,&pattern](tr2::optional<string>& x) -> tr2::optional<vector<string>>{
                           if(!bool(x)){
                               return tr2::nullopt;
                           }
                           StringPiece str(x.value());
                           auto str_size = x.value().length();
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

                           return tr2::make_optional(pieces);
                       });
    }
};


SEXP cpp_split_not_fixed(CharacterVector& input,RE2* pattern,size_t limit){
    SEXP inputx = input;
    R_xlen_t index = 0;
    Shield<SEXP> ress(Rf_allocVector(VECSXP,input.size()));
    SEXP res = ress;

    for(auto it = 0; it != input.size(); it++){
        auto rstr = STRING_ELT(inputx, it);
        if (rstr == NA_STRING){
            Shield<SEXP> na_string(Rf_allocVector(STRSXP,1));
            SET_STRING_ELT(na_string , 0 ,NA_STRING);
            SET_VECTOR_ELT(res, it,  na_string);
            continue;
        }
        StringPiece str(R_CHAR(rstr));
        auto str_size = strlen(R_CHAR(rstr));
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

SEXP cpp_split_fixed(CharacterVector input,RE2* pattern,size_t limit){
    SEXP inputx = input;
    Shield<SEXP> ress( Rf_allocMatrix(STRSXP, (R_xlen_t) input.size(), (R_xlen_t) limit));
    SEXP res = ress;
    auto empstring = Rf_mkCharLenCE( "",  strlen("") , CE_UTF8);
    for(auto it = 0; it != input.size(); it++){
        auto rstr = STRING_ELT(inputx, it);
        if (rstr == NA_STRING){
            size_t coli = 0;
            while(coli < limit){
                SET_STRING_ELT(res, it + coli* input.size(), empstring);
                coli++;
            }
            continue;
        }
        StringPiece str(R_CHAR(rstr));
        auto str_size = strlen(R_CHAR(rstr));
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
SEXP cpp_split(CharacterVector input, XPtr<RE2Obj>& ptr, NumericVector part, bool fixed, bool parallel, size_t grain_size){
    RE2* pattern = &(ptr->regexp);

    if (part.size() == 0){
        stop("need the number of pieces.");
    }
    size_t limit = numeric_limits<R_xlen_t>::max();

    if (R_finite(part[0])){
        limit = as<size_t>(part);

    }
    if (!parallel || input.size() < grain_size){
        if (!fixed){
            return cpp_split_not_fixed(input, pattern, limit);
        } else {
            return cpp_split_fixed(input, pattern, limit);
       }

    }
    else{
        auto inputv = as_vec_opt_string(input);
        vector<tr2::optional<vector<string>>> res(input.size());

        if (!fixed){
            SplitP pobj(inputv, res, *pattern, *(ptr->options), limit);
            parallelFor(0, input.size(), pobj, grain_size);

            Shield<SEXP>  xs(Rf_allocVector(VECSXP, input.size()));
            SEXP x = xs;

            R_xlen_t index = 0;

            for (tr2::optional<vector<string>>& resi : res){
                if ( !bool(resi)) {
                    Shield<SEXP> na_string(Rf_allocVector(STRSXP,1));
                    SET_STRING_ELT(na_string , 0 ,NA_STRING);
                    SET_VECTOR_ELT(x, index,  na_string);
                }
                // else if(resi.value().empty()){
                //     SET_VECTOR_ELT(x, index, Shield<SEXP>(Rf_allocVector(STRSXP,0)));
                // }
                else{
                    SET_VECTOR_ELT(x, index, Shield<SEXP>(toprotect_vec_string_sexp(resi.value())));
                }
                index ++;
            }
            return x;
        } else {

            SplitFixP pobj(inputv, res, *pattern, *(ptr->options), limit);
            parallelFor(0, input.size(), pobj, grain_size);
            Shield<SEXP>  xs(Rf_allocMatrix(STRSXP, input.size(),limit));
            SEXP x = xs;

            R_xlen_t rowi = 0;
            R_xlen_t coli = 0;
            auto empstring = Rf_mkCharLenCE( "",  strlen("") , CE_UTF8);
            for (tr2::optional<vector<string>>& resi : res){
                if ( !bool(resi) || resi.value().empty()) {
                    while(coli < limit){
                        SET_STRING_ELT(x, rowi+ coli*input.size(), empstring);
                        coli++;
                    }
                    continue;
                }

                for(string& cell : resi.value()){
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
