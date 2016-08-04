// This file is part of the 're2r' package for R.
// Copyright (C) 2016, Qin Wenfeng
// All rights reserved.

// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:

// 1. Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.

// 2. Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.

// 3. Neither the name of the copyright holder nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.

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

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;

#include "../inst/include/re2r.h"

#include <cstddef>

#include <memory>
#include <sstream>

#define RE2R_STATIC_SIZE 10

template <typename T> inline string NumberToString(T Number) {
  ostringstream ss;
  ss << ".";
  ss << Number;
  return ss.str();
}

vector<string> get_groups_name(RE2 *pattern, int cap_nums) {
  auto groups_name = pattern->CapturingGroupNames();

  vector<int> alls;
  alls.reserve(cap_nums - 1);
  int cnt = 1;
  while (cnt <= cap_nums - 1) {
    alls.push_back(cnt);
    cnt += 1;
  }

  vector<int> nums;
  nums.reserve(cap_nums - 1);

  vector<string> cap_names;
  cap_names.reserve(cap_nums);

  for (auto it = groups_name.begin(); it != groups_name.end(); ++it) {
    nums.push_back(it->first);
  }

  vector<int> diff_nums(alls.size() + nums.size());

  auto diff_res = set_difference(alls.begin(), alls.end(), nums.begin(),
                                 nums.end(), diff_nums.begin());
  diff_nums.resize(diff_res - diff_nums.begin());

  for (auto ind : diff_nums) {
    groups_name.insert(make_pair(ind, NumberToString(ind)));
  }

  vector<string> res;
  res.reserve(res.size());
  res.push_back(".match");
  for (auto it = groups_name.begin(); it != groups_name.end(); it++) {
    res.push_back(it->second);
  }

  return res;
}

// begin real work

SEXP cpp_detect(CharacterVector &input, vector<OptRE2 *> &ptrv,
                RE2::Anchor anchor_type, size_t nrecycle) {

  SEXP inputx = input;
  LogicalVector res(nrecycle);
  auto resi = res.begin();
  for (auto it = 0; it != nrecycle; it++, resi++) {
    auto rstr = STRING_ELT(inputx, it % input.size());
    auto optpattern = ptrv[it % ptrv.size()];
    if (rstr == NA_STRING || !bool(*optpattern)) {
      *resi = NA_LOGICAL;
      continue;
    }
    auto pattern = optpattern->value().get();

    auto r_char = R_CHAR(rstr);
    auto piece = StringPiece(r_char);
    *resi = pattern->Match(piece, 0, piece.size(), anchor_type, nullptr, 0);
  }
  return wrap(res);
}

struct BoolP : public Worker {
  vector<tr2::optional<string>> &input;
  RVector<int> output;
  vector<OptRE2 *> &tt;
  const RE2::Anchor anchor_type;

  BoolP(vector<tr2::optional<string>> &input_, RVector<int> output_,
        vector<OptRE2 *> &tt_, const RE2::Anchor &anchor_type_)
      : input(input_), output(output_), tt(tt_), anchor_type(anchor_type_) {}

  void operator()(std::size_t begin, std::size_t end) {
    size_t index = begin;
    for (auto x = output.begin() + begin; x != output.begin() + end; x++) {
      auto inputi = input[index % input.size()];
      auto optptr = tt[index % tt.size()];
      index++;

      if (!bool(inputi) || !bool(*optptr)) {
        *x = NA_LOGICAL;
        continue;
      }
      auto ptr = optptr->value().get();
      *x = ptr->Match(inputi.value(), 0, (int)inputi.value().length(),
                      anchor_type, nullptr, 0);
    }
  }
};

SEXP cpp_detect_parallel(CharacterVector &input, vector<OptRE2 *> &pattern,
                         RE2::Anchor anchor_type, size_t grain_size,
                         size_t nrecycle) {
  LogicalVector reso(nrecycle);
  RVector<int> res(reso);
  auto inputv = as_vec_opt_string(input);
  BoolP pobj(inputv, res, pattern, anchor_type);
  parallelFor(0, nrecycle, pobj, grain_size);
  return wrap(reso);
}

inline void fill_match_not_all(int cap_nums, StringPiece *piece, SEXP x,
                               size_t &rowi, size_t &coli, size_t rows,
                               size_t cols, bool matched) {

  if (matched) {
    for (auto it = 0; it != cap_nums; ++it) {
      if ((piece[it]).data() != NULL) {
        SET_STRING_ELT(x, rowi + coli * rows,
                       Rf_mkCharLenCE(piece[it].as_string().c_str(),
                                      strlen(piece[it].as_string().c_str()),
                                      CE_UTF8));
      } else {
        SET_STRING_ELT(x, rowi + coli * rows, NA_STRING);
      }

      bump_count(coli, rowi, cols);
    }
  } else {
    for (size_t it = 0; it != cols; ++it) {
      SET_STRING_ELT(x, rowi + coli * rows, NA_STRING);
      bump_count(coli, rowi, cols);
    }
  }
}

SEXP cpp_match_not_all(CharacterVector &input, RE2 *pattern,
                       RE2::Anchor anchor_type, StringPiece *piece_ptr,
                       vector<string> &groups_name, int cap_nums) {
  const auto cols = groups_name.size();
  const auto rows = input.size();
  size_t rowi = 0;
  size_t coli = 0;

  SEXP inputx = input;
  Shield<SEXP> ress(
      Rf_allocMatrix(STRSXP, input.size(),
                     groups_name.size())); // will be constructed as Matrix
  SEXP res = ress;

  for (auto it = 0; it != input.size(); it++) {
    auto rstr = STRING_ELT(inputx, it);
    if (rstr == NA_STRING) {
      fill_match_not_all(cap_nums, piece_ptr, res, rowi, coli, rows, cols,
                           false);
      continue;
    }
    auto r_char = R_CHAR(rstr);

    for (int pn = 0; pn != cap_nums; pn++)
      piece_ptr[pn].clear();

    fill_match_not_all(cap_nums, piece_ptr, res, rowi, coli, rows, cols,
                       pattern->Match(r_char, 0, strlen(r_char), anchor_type,
                                      piece_ptr, cap_nums));
  }

  // generate CharacterMatrix
  SEXP dims = Rf_getAttrib(res, R_DimSymbol);
  Shield<SEXP> new_dimnames((Rf_allocVector(VECSXP, Rf_length(dims))));
  SET_VECTOR_ELT(new_dimnames, 1,
                 Shield<SEXP>(toprotect_vec_string_sexp(groups_name)));
  Rf_setAttrib(res, R_DimNamesSymbol, new_dimnames);
  return res;
}

optstring fill_not_all_parallel(int cap_nums, StringPiece *piece,
                                bool matched) {
  optstring res(cap_nums);
  if (matched) {
    auto it = res.begin();
    for (auto i = 0; i != cap_nums; ++i) {
      if ((piece[i]).data() != NULL) {
        *it = tr2::make_optional(piece[i].as_string());
      } else {
        *it = tr2::nullopt;
      }
      it++;
    }
  } else {
    for (auto it = res.begin(); it != res.end(); ++it) {
      *it = tr2::nullopt;
    }
  }
  return res;
}

struct NotAllValue : public Worker {
  vector<tr2::optional<string>> &input;
  vector<optstring> &output;
  RE2 &tt;
  const RE2::Anchor &anchor_type;

  NotAllValue(vector<tr2::optional<string>> &input_, vector<optstring> &output_,
              RE2 &tt_, const RE2::Anchor &anchor_type_)
      : input(input_), output(output_), tt(tt_), anchor_type(anchor_type_) {}

  void operator()(std::size_t begin, std::size_t end) {
    auto cap_nums = tt.NumberOfCapturingGroups() + 1;
    auto piece = unique_ptr<StringPiece[]>(new StringPiece[cap_nums]);
    auto piece_ptr = piece.get();
    auto y = output.begin() + begin;
    for (auto x = input.begin() + begin; x != input.begin() + end; x++, y++) {
      if (!bool(*x)) {
        *y = fill_not_all_parallel(cap_nums, piece_ptr, false);
          continue;
      }

      for (int pn = 0; pn != cap_nums; pn++)
        piece_ptr[pn].clear();
      string &r_char = x->value();
      *y = fill_not_all_parallel(
          cap_nums, piece_ptr,
          tt.Match(r_char, 0, r_char.size(), anchor_type, piece_ptr, cap_nums));
    }
  }
}

;

SEXP cpp_match_not_all_parallel(CharacterVector &input, RE2 *pattern,
                                RE2::Anchor anchor_type,
                                vector<string> &groups_name, int cap_nums,
                                size_t grain_size) {
  vector<optstring> output(input.size());
  auto inputv = as_vec_opt_string(input);

  NotAllValue pobj(inputv, output, *pattern, anchor_type);
  parallelFor(0, input.size(), pobj, grain_size);
  Shield<SEXP> res(toprotect_vec_optstring_to_charmat(output, cap_nums));

  // generate CharacterMatrix
  SEXP dims = Rf_getAttrib(res, R_DimSymbol);
  Shield<SEXP> new_dimnames((Rf_allocVector(VECSXP, Rf_length(dims))));
  SET_VECTOR_ELT(new_dimnames, 1,
                 Shield<SEXP>(toprotect_vec_string_sexp(groups_name)));
  Rf_setAttrib(res, R_DimNamesSymbol, new_dimnames);
  return res;
}

void fill_match_all(int cap_nums, StringPiece *piece, optstring &res//,
//                    size_t cnt
){
  // auto all_na = true;

  // // don't get all na
  // if (cnt > 1) {
  //   for (auto it = 0; it != cap_nums; ++it) {
  //     if ((piece[it]).data() != NULL) {
  //       all_na = false;
  //       break;
  //     }
  //   }
  //   if (all_na)
  //     return;
  // }

  for (auto it = 0; it != cap_nums; ++it) {
    if ((piece[it]).data() != NULL) {
      res.push_back(tr2::make_optional(piece[it].as_string()));
    } else {
      res.push_back(tr2::nullopt);
    }
  }
}

inline void bump_listi(List::iterator &listi,
                       const optstring &optinner, size_t cols,
                       SEXP groups_name, bool is_na) {
  if (is_na) { // no one match, all NA return
    *listi = Shield<SEXP>(toprotect_na_charmat(groups_name, cols));
  }else { // generate CharacterMatrix
    *listi = Shield<SEXP>(
        toprotect_optstring_to_list_charmat(optinner, cols, groups_name));
  }
  listi += 1; // bump times_n !n
}

SEXP cpp_match_all(CharacterVector &input, RE2 *pattern,
                   RE2::Anchor anchor_type, StringPiece *piece_ptr,
                   vector<string> &groups_name, int cap_nums) {
  List listres(input.size());
  SEXP inputx = input;
  auto listi = listres.begin();
  // for each input string, get a !n label.
  Shield<SEXP> new_dimnames((Rf_allocVector(VECSXP, 2)));
  SET_VECTOR_ELT(new_dimnames, 1,
                 Shield<SEXP>(toprotect_vec_string_sexp(groups_name)));

  for (auto it = 0; it != input.size(); it++) {

    optstring optinner;
    auto rstr = STRING_ELT(inputx, it);
    if (rstr == NA_STRING) {
      bump_listi(listi, optinner, groups_name.size(), new_dimnames, true);
      continue;
    }
    auto r_char = R_CHAR(rstr);
    StringPiece todo_str(r_char);
    for (int pn = 0; pn != cap_nums; pn++)
      piece_ptr[pn].clear();
    size_t lastIndex = 0;
    auto str_size = strlen(r_char);

    while (pattern->Match(todo_str, lastIndex, str_size, anchor_type, piece_ptr,
                          cap_nums)) {
      fill_match_all(cap_nums, piece_ptr, optinner);

      if (!piece_ptr[0].size()) {
        size_t sym_size = getUtf8CharSize(todo_str.data()[lastIndex]);
        lastIndex += sym_size;
      }else{
          auto piece_ptr_data = piece_ptr[0];
          lastIndex =
              piece_ptr_data.data() - todo_str.data() + piece_ptr_data.size();
      }

      for (int pn = 0; pn != cap_nums; pn++)
        piece_ptr[pn].clear();

      // try next place
    } // while
    bump_listi(listi, optinner, groups_name.size(), new_dimnames, false);
  }
  return listres;
}

struct MatValue : public Worker {
  vector<tr2::optional<string>> &input;
  vector<tr2::optional<optstring>> &output;
  RE2 &tt;
  const RE2::Anchor &anchor_type;

  MatValue(vector<tr2::optional<string>> &input_,
           vector<tr2::optional<optstring>> &output_, RE2 &tt_,
           const RE2::Anchor &anchor_type_)
      : input(input_), output(output_), tt(tt_), anchor_type(anchor_type_) {}

  void operator()(std::size_t begin, std::size_t end) {
    auto cap_nums = tt.NumberOfCapturingGroups() + 1;
    auto piece = unique_ptr<StringPiece[]>(new StringPiece[cap_nums]);
    auto piece_ptr = piece.get();
    auto resi = output.begin() + begin;
    for (auto ind = input.begin() + begin; ind != input.begin() + end;
         ind++, resi++) {

      if (!bool(*ind)) {
        *resi = tr2::nullopt; // NA return
        continue;
      }

      StringPiece todo_str(ind->value());
      for (int pn = 0; pn != cap_nums; pn++)
        piece_ptr[pn].clear();
      size_t cnt = 0;
      optstring optinner;
      size_t lastIndex = 0;
      auto str_size = todo_str.size();

      while (tt.Match(todo_str, lastIndex, str_size, anchor_type, piece_ptr,
                      cap_nums)) {
        fill_match_all(cap_nums, piece_ptr, optinner);

        if (!piece_ptr[0].size()) {
          size_t sym_size = getUtf8CharSize(todo_str.data()[lastIndex]);
          lastIndex += sym_size;
        }else{
          auto piece_ptr_data = piece_ptr[0];
          lastIndex = piece_ptr_data.data() - todo_str.data() + piece_ptr_data.size();
        }

        for (int pn = 0; pn != cap_nums; pn++)
          piece_ptr[pn].clear();

        // advanced try next place
      } // else while

        *resi = tr2::make_optional(move(optinner));

    }
  }
};

SEXP cpp_match_all_parallel(CharacterVector &input, RE2 *pattern,
                            RE2::Anchor anchor_type,
                            vector<string> &groups_name, int cap_nums,
                            size_t grain_size) {

  List listres(input.size());

  vector<tr2::optional<optstring>> res(input.size());
  auto inputv = as_vec_opt_string(input);

  MatValue pobj(inputv, res, *pattern, anchor_type);
  parallelFor(0, input.size(), pobj, grain_size);

  Shield<SEXP> new_dimnames((Rf_allocVector(VECSXP, 2)));
  SET_VECTOR_ELT(new_dimnames, 1,
                 Shield<SEXP>(toprotect_vec_string_sexp(groups_name)));

  // fill in result
  auto resi = res.begin();
  for (auto it = listres.begin(); it != listres.end(); it++, resi++) {
    if (!bool(*resi)) { // NA string
      *it = Shield<SEXP>(toprotect_na_charmat(new_dimnames, groups_name.size()));
    } else {
      *it = Shield<SEXP>(toprotect_optstring_to_list_charmat(
          resi->value(), groups_name.size(), new_dimnames));
    }
  }
  return listres;
}

// [[Rcpp::export]]
SEXP cpp_match(CharacterVector input, SEXP regexp, bool value, size_t anchor,
               bool all, bool parallel, size_t grain_size) {
  RE2::Anchor anchor_type = get_anchor_type(anchor);

  if (value == false) {

    vector<OptRE2 *> ptrv;
    build_regex_vector(regexp, ptrv);
    auto nrecycle = re2r_recycling_rule(true, 2, input.size(), ptrv.size());

    if (!parallel || input.size() < grain_size) {
      return cpp_detect(input, ptrv, anchor_type, nrecycle);
    } else {
      return cpp_detect_parallel(input, ptrv, anchor_type, grain_size,
                                 nrecycle);
    }

  } else {
    OptRE2 *optpattern;
    if (TYPEOF(regexp) == EXTPTRSXP) {
      auto ptr = R_ExternalPtrAddr(regexp);
      if (ptr == nullptr)
        stop(INVALID_ERROR_STRING);
      optpattern = (OptRE2 *)ptr;
    } else {
      if (TYPEOF(regexp) == VECSXP && Rf_xlength(regexp) > 0) {
        if (Rf_xlength(regexp) != 1) {
          warning(
              "only the first pattern is used in re2_match() re2_match_all()");
        }

        if (TYPEOF(VECTOR_ELT(regexp, 0)) == EXTPTRSXP){
            auto ptr = R_ExternalPtrAddr(VECTOR_ELT(regexp, 0));
            if (ptr == nullptr)
                stop(INVALID_ERROR_STRING);
            optpattern = (OptRE2 *)ptr;
        }
        else {
            stop("expecting a pre-compiled RE2 object for the first pattern.");
        }

      } else {
        stop("expecting a pre-compiled RE2 object.");
      }
    }

    if (!bool(*optpattern)) {

      if (all) {
          List res(input.size());
          CharacterMatrix resi(1,1);
          resi(0,0) = NA_STRING;
          std::fill(res.begin(), res.end(), resi);
        return res;
      } else {
        CharacterMatrix res(input.size(), 1);
        colnames(res) = CharacterVector::create(".match");
        std::fill(res.begin(), res.end(), NA_STRING);
        return (res);
      }
    }
    RE2 *pattern = optpattern->value().get();
    auto cap_nums = pattern->NumberOfCapturingGroups();

    cap_nums += 1;

    // at least one capture group, return a matrix
    // set up the args and stringpiece
    vector<string> groups_name = get_groups_name(pattern, cap_nums);

    if (parallel && input.size() > grain_size) {
      if (all) {
        return cpp_match_all_parallel(input, pattern, anchor_type, groups_name,
                                      cap_nums, grain_size);
      } else {
        return cpp_match_not_all_parallel(input, pattern, anchor_type,
                                          groups_name, cap_nums, grain_size);
      }

    } else {
      // not parallel, value, all
      // static when the number of capture group is smaller than 10
      StringPiece match_static[RE2R_STATIC_SIZE];
      StringPiece *match;
      // dynamic when the number of capture group is bigger than 10
      // We use exception, it will be better to use unique_ptr instead of raw
      // ptr.
      unique_ptr<StringPiece[]> matchp;

      // when we have a small number of capture groups,
      // we do not need to used heap
      if (cap_nums <= RE2R_STATIC_SIZE) { // RE2R_STATIC_SIZE = 10
        match = match_static;
      } else {
        matchp = unique_ptr<StringPiece[]>(new StringPiece[cap_nums]);
        match = matchp.get();
      };

      // do the work

      if (all == false) {
        return cpp_match_not_all(input, pattern, anchor_type, match,
                                 groups_name, cap_nums);
      } else { // all == true
        return cpp_match_all(input, pattern, anchor_type, match, groups_name,
                             cap_nums);
      } // all == true
    }
    // unique_ptr go out of scrope
  }

  throw ErrorInternal("unreachable cpp_match");
}
