#include "../inst/include/re2r.h"

#include <cstddef>

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
SEXP cpp_match(XPtr<RE2>     pattern,
                  vector<string> input,
                  bool value,
                  string anchor,
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
    } else{
        // return value
        return wrap(1);
    }
}
