#include "../inst/include/re2r.h"

using namespace std;

SEXP toprotect_vec_string_sexp(const vector<string> &input) {
  SEXP x;
  PROTECT(x = Rf_allocVector(STRSXP, input.size()));
  R_xlen_t index = 0;
  for (const string &dd : input) {
    SET_STRING_ELT(x, index,
                   Rf_mkCharLenCE(dd.c_str(), strlen(dd.c_str()), CE_UTF8));
    index++;
  }
  UNPROTECT(1);
  return x;
}

vector<tr2::optional<string>> as_vec_opt_string(CharacterVector &input) {
  SEXP inputx = input;
  vector<tr2::optional<string>> res;
  res.reserve(input.size());

  for (auto it = 0; it != input.size(); it++) {
    auto rstr = STRING_ELT(inputx, it);
    if (rstr == NA_STRING) {
      res.push_back(tr2::nullopt);
      continue;
    } else {
      res.push_back(tr2::make_optional(string(R_CHAR(rstr))));
    }
  }
  return res;
}

SEXP toprotect_optstring_sexp(const optstring &input) {
  SEXP x;
  PROTECT(x = Rf_allocVector(STRSXP, input.size()));
  string tmpres;
  R_xlen_t index = 0;

  for (auto dd : input) {
    if (bool(dd)) {
      SET_STRING_ELT(x, index,
                     Rf_mkCharLenCE(dd.value().c_str(),
                                    strlen(dd.value().c_str()), CE_UTF8));
    } else {
      SET_STRING_ELT(x, index, NA_STRING);
    }
    index++;
  }
  UNPROTECT(1);
  return x;
}

void clone_vec_regex(const vector<OptRE2 *>& input, vector<unique_ptr<OptRE2>>& res ){
    res.reserve(input.size());
    for(vector<OptRE2 *>::const_iterator x = input.begin(); x != input.end(); x++){
        if(!bool(**x)){
            res.emplace_back(unique_ptr<OptRE2>(new OptRE2(tr2::nullopt)));
        }else{
            auto ptr = (**x).value().get();
            res.emplace_back(unique_ptr<OptRE2>(new OptRE2(tr2::in_place, new RE2(ptr->pattern(), ptr->options()))));
        }
    }
}

SEXP toprotect_vec_optstring_to_charmat(const vector<optstring> &res,
                                        int cap_nums) {
    auto rows = res.size();
    Shield<SEXP> resv(Rf_allocMatrix(STRSXP, rows, cap_nums));
    SEXP x = resv;

    auto rowi = 0;
    auto coli = 0;
    for (const optstring &ind : res) {
        for (const tr2::optional<string> &dd : ind) {
            if (bool(dd)) {
                SET_STRING_ELT(x, rowi + coli * rows,
                               Rf_mkCharLenCE(dd.value().c_str(),
                                              strlen(dd.value().c_str()), CE_UTF8));
            } else {
                SET_STRING_ELT(x, rowi + coli * rows, NA_STRING);
            }
            coli += 1;
        }
        rowi += 1;
        coli = 0;
    }
    return resv;
}



SEXP toprotect_optstring_to_charmat(const optstring &res) {

    Shield<SEXP> resv(Rf_allocMatrix(STRSXP, res.size(), 1));
    SEXP dims = Rf_getAttrib(resv, R_DimSymbol);
    SEXP new_dimnames = Shield<SEXP>((Rf_allocVector(VECSXP, Rf_length(dims))));
    SET_VECTOR_ELT(new_dimnames, 1, CharacterVector::create(".match"));
    Rf_setAttrib(resv, R_DimNamesSymbol, new_dimnames);

    SEXP x = resv;

    R_xlen_t index = 0;

    for (auto dd : res) {
        if (bool(dd)) {
            SET_STRING_ELT(x, index,
                           Rf_mkCharLenCE(dd.value().c_str(),
                                          strlen(dd.value().c_str()), CE_UTF8));
        } else {
            SET_STRING_ELT(x, index, NA_STRING);
        }
        index++;
    }

    return resv;
}

SEXP toprotect_na_charmat(SEXP groups_name, size_t cols) {
    Shield<SEXP> res(Rf_allocMatrix(STRSXP, 1, cols));
    SEXP resx = res;
    for(auto i=0; i!=cols; i++){
        SET_STRING_ELT(resx, i, NA_STRING);
    }
    Rf_setAttrib(resx, R_DimNamesSymbol, groups_name);
    return resx;
}

SEXP toprotect_optstring_to_list_charmat(const optstring &optinner, size_t cols,
                                         SEXP groups_name) {

    auto rows = optinner.size() / cols;
    Shield<SEXP> res(Rf_allocMatrix(STRSXP, rows, cols));

    if(optinner.size() == 0){
        Rf_setAttrib(res, R_DimNamesSymbol, groups_name);
        return res;
    }

    SEXP x = res;

    size_t rowi = 0;
    size_t coli = 0;
    for (auto dd : optinner) {
        if (bool(dd)) {
            SET_STRING_ELT(x, rowi + coli * rows,
                           Rf_mkCharLenCE(dd.value().c_str(),
                                          strlen(dd.value().c_str()), CE_UTF8));
        } else {
            SET_STRING_ELT(x, rowi + coli * rows, NA_STRING);
        }
        bump_count(coli, rowi, cols);
    }

    Rf_setAttrib(res, R_DimNamesSymbol, groups_name);

    return res;
}

void build_regex_vector(SEXP regexp, vector<OptRE2 *> &ptrv) {
  if (TYPEOF(regexp) == EXTPTRSXP) {
    auto ptr = R_ExternalPtrAddr(regexp);
    if (ptr == nullptr)
      stop(INVALID_ERROR_STRING);
    ptrv.push_back((OptRE2 *)ptr);
  } else if (TYPEOF(regexp) == VECSXP) {

    auto len = Rf_xlength(regexp);
    ptrv.reserve(len);
    for (auto it = 0; it != len; it++) {
        Shield<SEXP> ptri(VECTOR_ELT(regexp, it));
        if (TYPEOF(ptri) == EXTPTRSXP){
            auto ptr = R_ExternalPtrAddr(ptri);
            if (ptr == nullptr)
                stop(INVALID_ERROR_STRING);
            ptrv.push_back((OptRE2 *)ptr);
        }
        else {
            stop("expecting a pre-compiled RE2 object for pattern %d.", it+1);
        }
    }
  } else {
    stop("expecting a pre-compiled RE2 object.");
  }
}

// see stri__recycling_rule
// https://github.com/gagolews/stringi/blob/0d49dca84fbe703afa596e5ba0e6f223720dfa87/src/stri_common.cpp

#define MSG__WARN_RECYCLING_RULE                                               \
  "longer object length is not a multiple of shorter object length"

R_xlen_t re2r_recycling_rule(bool enableWarning, int n, ...) {
  R_xlen_t nsm = 0;
  va_list arguments;

  va_start(arguments, n);
  for (R_len_t i = 0; i < n; ++i) {
    R_len_t curlen = va_arg(arguments, R_len_t);
    if (curlen <= 0)
      return 0;
    if (curlen > nsm)
      nsm = curlen;
  }
  va_end(arguments);

  if (enableWarning) {
    va_start(arguments, n);
    for (R_len_t i = 0; i < n; ++i) {
      R_len_t curlen = va_arg(arguments, R_len_t);
      if (nsm % curlen != 0) {
        Rf_warning(MSG__WARN_RECYCLING_RULE);
        break;
      }
    }
    va_end(arguments);
  }

  return nsm;
}

R_xlen_t vectorize_next(R_xlen_t i, R_xlen_t nrecycle, R_xlen_t n) {
  if (i == nrecycle - 1 - (nrecycle % n))
    return nrecycle; // this is the end
  i = i + n;
  if (i >= nrecycle)
    return (i % n) + 1;
  else
    return i;
}
