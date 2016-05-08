#include "../inst/include/re2r.h"

using namespace std;

SEXP toprotect_vec_string_sexp(const vector<string>& input){
    SEXP x;
    PROTECT(x = Rf_allocVector(STRSXP, input.size()));
    R_xlen_t index = 0;
    for(const string& dd : input){
        SET_STRING_ELT(x, index, Rf_mkCharLenCE(dd.c_str(),  strlen(dd.c_str()) , CE_UTF8));
        index ++;
    }
    UNPROTECT(1);
    return x;
}

vector<tr2::optional<string>> as_vec_opt_string(CharacterVector& input){
    SEXP inputx = input;
    vector<tr2::optional<string>> res;
    res.reserve(input.size());

    for(auto it = 0; it != input.size(); it++){
        auto rstr = STRING_ELT(inputx, it);
        if (rstr == NA_STRING){
            res.push_back(tr2::nullopt);
            continue;
        }else{
            res.push_back(tr2::make_optional(string(R_CHAR(rstr))));
        }
    }
    return res;
}

SEXP toprotect_optstring_sexp(const optstring& input){
    SEXP x;
    PROTECT(x = Rf_allocVector(STRSXP, input.size()));
    string tmpres;
    R_xlen_t index = 0;

    for(auto dd : input){
        if (bool(dd)) {
            SET_STRING_ELT(x, index, Rf_mkCharLenCE(dd.value().c_str(),  strlen(dd.value().c_str()) , CE_UTF8));
        } else{
            SET_STRING_ELT(x, index, NA_STRING);
        }
        index++;
    }
    UNPROTECT(1);
    return x;
}
