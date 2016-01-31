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

#include "../inst/include/re2r.h"

#include <cstddef>

#include <sstream>
#include <memory>

template <typename T>
inline string NumberToString ( T Number )
{
    ostringstream ss;
    ss << "?";
    ss << Number;
    return ss.str();
}

template <typename T>
inline string numbertostring ( T Number )
{
    ostringstream ss;
    ss << Number;
    return ss.str();
}

map<int,string> get_groups_name(XPtr<RE2>& pattern, int cap_nums){
    auto groups_name = pattern->CapturingGroupNames();

    vector<int> alls;
    alls.reserve(cap_nums);
    int cnt = 1;
    while(cnt <= cap_nums){
        alls.push_back(cnt);
        cnt+=1;
    }

    vector<int> nums;
    nums.reserve(cap_nums);

    vector<string> cap_names;
    cap_names.reserve(cap_nums);

    for(auto it = groups_name.begin(); it != groups_name.end(); ++it) {
        nums.push_back(it->first);
    }

    vector<int> diff_nums(alls.size()+nums.size());

    auto diff_res = set_difference(alls.begin(),
                                   alls.end(),
                                   nums.begin(),
                                   nums.end(),
                                   diff_nums.begin());
    diff_nums.resize(diff_res-diff_nums.begin());

    for(auto ind : diff_nums) {
        groups_name.insert(make_pair(ind, NumberToString(ind)));
    }
    return groups_name;
}

void fill_all_res(string& times_n,
                  int cap_nums,
                  const vector<int>& groups_number,
                  StringPiece* piece,
                  map<int,CharacterVector>& res, bool matched){
    auto gn_iter = groups_number.begin();
    res[*gn_iter].push_back(times_n);
    gn_iter++;

    if(matched){
        for(auto it = 0; it != cap_nums; ++it) {
            if((piece[it]).data() != NULL){
                res[*gn_iter].push_back(piece[it].as_string());
            } else{
                res[*gn_iter].push_back(NA_STRING);
            }
            gn_iter++;
        }
    }else{
        for(auto it = 0; it != cap_nums; ++it) {
            res[*gn_iter].push_back(NA_STRING);
            gn_iter++;
        }
    }
}


void fill_res(int cap_nums,
              const vector<int>& groups_number,
              StringPiece* piece,
              map<int,CharacterVector>& res, bool matched){
    auto gn_iter = groups_number.begin();
    if(matched){
        for(auto it = 0; it != cap_nums; ++it) {
            if((piece[it]).data() != NULL){
                res[*gn_iter].push_back(piece[it].as_string());
            } else{
                res[*gn_iter].push_back(NA_STRING);
            }
            gn_iter++;
        }
    }else{
        for(auto it = 0; it != cap_nums; ++it) {
            res[*gn_iter].push_back(NA_STRING);
            gn_iter++;
        }
    }
}

RE2::Anchor get_anchor_type(const string& anchor){
    if (anchor == "none") {
        return RE2::UNANCHORED;
    } else if (anchor == "start") {
        return RE2::ANCHOR_START;
    } else if (anchor == "both") {
        return RE2::ANCHOR_BOTH;
    }
    throw ErrorAnchorType(anchor);
}

// [[Rcpp::export]]
SEXP cpp_match(XPtr<RE2>&     pattern,
                  vector<string>& input,
                  bool value,
                  string& anchor,
                  bool all){
    RE2::Anchor anchor_type = get_anchor_type(anchor);

    if (value == false){
        vector<bool> res;
        res.reserve(input.size());
        for(const string& ind : input){
            res.push_back(pattern->Match(ind,0,(int) ind.length(),
                                         anchor_type, nullptr, 0));
        }
        return wrap(res);
        // bool return, the fastest one
    } else{
        auto cap_nums = pattern->NumberOfCapturingGroups();

        // no capture group, return CharacterVector, like grep(value = T)

        if ( cap_nums == 0){
            vector<string> res;
            res.reserve(input.size());
            for(const string& ind : input){
                if(pattern->Match(ind,0,(int) ind.length(),
                                    anchor_type, nullptr, 0)){
                    res.push_back(ind);
                }
            }
            return wrap(res);
            // no capture group return
        }

        // at least one capture group, return a data.frame

        // set up the args and stringpiece
        map<int,string> g_numbers_names = get_groups_name(pattern, cap_nums);
        vector<string> groups_name;
        vector<int> groups_number;

        // We use exception, it will be better to use unique_ptr instead of raw ptr.
        unique_ptr<RE2::Arg[]> argv =  unique_ptr<RE2::Arg[]>(new RE2::Arg[cap_nums]);
        unique_ptr<RE2::Arg*[]> args =  unique_ptr<RE2::Arg*[]>(new RE2::Arg*[cap_nums]);
        unique_ptr<StringPiece[]> piece = unique_ptr<StringPiece[]>(new StringPiece[cap_nums]);

        // It should be safe, as long as we do not manually delete this.
        auto piece_ptr = piece.get();

        for (int nn = 0; nn != cap_nums; nn++){
            args[nn] = &argv[nn];
            argv[nn] = &piece_ptr[nn];
        }
        // each string get a group of result
        if (all == false) {

            groups_name.reserve(g_numbers_names.size());
            groups_number.reserve(g_numbers_names.size());
            for(auto it = g_numbers_names.begin(); it!= g_numbers_names.end(); it++) {
                groups_number.push_back(it->first);
                groups_name.push_back(it->second);
            }
            map<int,CharacterVector> res; // data.frame

            // init res
            for(auto it: groups_number) {
                res[it] = CharacterVector();
            }

            for(const string& ind : input){
                for(int pn = 0; pn!=cap_nums; pn++) piece[pn].clear();

                switch(anchor_type){
                case RE2::UNANCHORED:
                fill_res(cap_nums, groups_number,
                         piece_ptr, res,
                         RE2::PartialMatchN(ind, *pattern, args.get(), cap_nums));
                    break;
                default:
                fill_res(cap_nums, groups_number,
                         piece_ptr, res,
                         RE2::FullMatchN(ind, *pattern, args.get(), cap_nums));
                    break;
                }
                //res.push_back(pattern->Match(ind,0,(int) ind.length(),
                                             //anchor_type, nullptr, 0));
            }

            // generate data.frame

            List res2 = wrap(res);
            vector<string> row_names;
            row_names.reserve(input.size());
            for (unsigned int i = 1; i <= input.size(); i++) {
                row_names.emplace_back(numbertostring(i));
            }

            res2.attr("row.names") = row_names;
            res2.attr("class") = "data.frame";
            res2.attr("names") = groups_name;

            return wrap(res2);

        } else { // all == true

            // each string get at least one group of result
            groups_name.reserve(g_numbers_names.size()+1);
            groups_number.reserve(g_numbers_names.size()+1);
            groups_name.push_back("!n");
            groups_number.push_back(0);

            for(auto it = g_numbers_names.begin(); it!= g_numbers_names.end(); it++) {
                groups_number.push_back(it->first);
                groups_name.push_back(it->second);
            }

            map<int,CharacterVector> res; // data.frame

            // init res
            for(auto it: groups_number) {
                res[it] = CharacterVector();
            }

            // for each input string, get a !n label.
            size_t times_n = 1;
            for(const string& ind : input){
                StringPiece todo_str(ind);    // Wrap a StringPiece around it
                StringPiece tmp_piece = StringPiece(todo_str.data(), todo_str.length());
                for(int pn = 0; pn!=cap_nums; pn++) piece[pn].clear();
                size_t cnt = 0;
                if (anchor_type == RE2::UNANCHORED){
                    while (RE2::FindAndConsumeN(&todo_str, *pattern, args.get(), cap_nums)) {
                        cnt+=1;
                        string numstring = numbertostring(times_n);
                        fill_all_res(numstring, cap_nums, groups_number, piece_ptr, res, true);

                        for(int pn = 0; pn!=cap_nums; pn++) piece[pn].clear();

                        // Note that if the
                        // regular expression matches an empty string, input will advance
                        // by 0 bytes.  If the regular expression being used might match
                        // an empty string, the loop body must check for this case and either
                        // advance the string or break out of the loop.
                        //
                        if(todo_str.length() == 0) break; // end of search for this string

                        if((todo_str.data() == tmp_piece.data()) &&
                           (todo_str.length() == tmp_piece.length()) &&
                           (todo_str.length() !=0) ){
                            todo_str.remove_prefix(1);
                        }

                        // update tmp_piece
                        StringPiece tmp_piece = StringPiece(todo_str.data(), todo_str.length());

                        // try next place
                    }   // while
                }else{
                    while (RE2::ConsumeN(&todo_str, *pattern, args.get(), cap_nums)) {
                        for(int pn = 0; pn!=cap_nums; pn++) piece[pn].clear();
                        cnt+=1;
                        string numstring = numbertostring(times_n);
                        fill_all_res(numstring, cap_nums, groups_number, piece_ptr, res, true);

                        for(int pn = 0; pn!=cap_nums; pn++) piece[pn].clear();

                        // Note that if the
                        // regular expression matches an empty string, input will advance
                        // by 0 bytes.  If the regular expression being used might match
                        // an empty string, the loop body must check for this case and either
                        // advance the string or break out of the loop.
                        //
                        if(todo_str.length() == 0) break; // end of search for this string

                        if((todo_str.data() == tmp_piece.data()) &&
                           (todo_str.length() == tmp_piece.length()) &&
                           (todo_str.length() !=0) ){
                            todo_str.remove_prefix(1);
                        }

                        // update tmp_piece
                        StringPiece tmp_piece = StringPiece(todo_str.data(), todo_str.length());

                        // advanced try next place
                    }   // else while
                } // else

                if(cnt == 0){ // no one match, all NA return
                    string numstring = numbertostring(times_n);
                    fill_all_res(numstring, cap_nums, groups_number, piece_ptr, res, false);
                }
                times_n+=1; //bump times_n !n

                // try next string
            }

            // generate data.frame
            List res2 = wrap(res);
            auto res_begin = res.begin();
            auto res_size = res_begin->second.size();
            vector<string> row_names;
            row_names.reserve(res_size);
            for (unsigned int i = 1; i <= res_size ; i++) {
                row_names.emplace_back(numbertostring(i));
            }

            res2.attr("row.names") = row_names;
            res2.attr("class") = "data.frame";
            res2.attr("names") = groups_name;

            return wrap(res2);

        }

        // unique_ptr go out of scrope
    }
    throw ErrorInternal("unreachable cpp_match");
}

