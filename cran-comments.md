Test environments

1. local OS X install, release and devel

2. Ubuntu 14.04, release and devel

3. win-builder, release and devel

4. Ubuntu with Valgrind, R 3.4.1

5. Ubuntu with UBSAN, R 3.4.1

R CMD check results:

0 errors | 0 warnings | 2 notes

* checking for GNU extensions in Makefiles
  GNU make is a SystemRequirements.

* checking CRAN incoming feasibility
  Maintainer: ‘Qin Wenfeng <mail@qinwenfeng.com>’
  License components with restrictions and base license 
  permitting such:
  BSD_3_clause + file LICENSE
  File 'LICENSE':
  YEAR: 2016-2017
  COPYRIGHT HOLDER: Qin Wenfeng
  
  Possibly mis-spelled words in DESCRIPTION:
     RE2 (3:8, 13:14)


Ubuntu with Valgrind Result:

==92== Use of uninitialised value of size 8
==92==    at 0x11297419: re2::SparseArray<re2::NFA::Thread*>::has_index(int) const [clone .part.56] (sparse_array.h:454)
==92==    by 0x112976D7: has_index (sparse_array.h:450)
==92==    by 0x112976D7: re2::NFA::AddToThreadq(re2::SparseArray<re2::NFA::Thread*>*, int, int, int, char const*, re2::NFA::Thread*) (nfa.cc:240)
==92==    by 0x112990AC: re2::NFA::Search(re2::StringPiece const&, re2::StringPiece const&, bool, bool, re2::StringPiece*, int) (nfa.cc:618)
==92==    by 0x1129A667: re2::Prog::SearchNFA(re2::StringPiece const&, re2::StringPiece const&, re2::Prog::Anchor, re2::Prog::MatchKind, re2::StringPiece*, int) (nfa.cc:725)
==92==    by 0x112AEC55: re2::RE2::Match(re2::StringPiece const&, unsigned long, unsigned long, re2::RE2::Anchor, re2::StringPiece*, int) const (re2.cc:777)
==92==    by 0x112C08E4: cpp_detect(Rcpp::Vector<16, Rcpp::PreserveStorage>&, std::vector<std::experimental::optional<std::unique_ptr<re2::RE2, std::default_delete<re2::RE2> > >*, std::allocator<std::experimental::optional<std::unique_ptr<re2::RE2, std::default_delete<re2::RE2> > >*> >&, re2::RE2::Anchor, unsigned long) (re2r_match.cpp:107)
==92==    by 0x112C3D90: cpp_match(Rcpp::Vector<16, Rcpp::PreserveStorage>, SEXPREC*, bool, unsigned long, bool, bool, unsigned long) (re2r_match.cpp:475)
==92==    by 0x1127D070: _re2r_cpp_match (RcppExports.cpp:205)
==92==    by 0x4F0FC49: ??? (in /usr/lib/R/lib/libR.so)
==92==    by 0x4F101D6: ??? (in /usr/lib/R/lib/libR.so)
==92==    by 0x4F4E0DC: Rf_eval (in /usr/lib/R/lib/libR.so)
==92==    by 0x4F50B68: ??? (in /usr/lib/R/lib/libR.so)
==92==

This result is expected. RE2 library can use a macro to disable this warning. See  https://github.com/google/re2/blob/b6e2ddf6c70156078099e5ef6b5c9c941a324b36/util/sparse_array.h#L313-L325


Ubuntu with UBSAN Result:

   dfa.cc:1006:38: runtime error: index 1 out of bounds for type 'atomic [0x10000000000000000]'
   dfa.cc:1079:26: runtime error: index 1 out of bounds for type 'atomic [0x10000000000000000]'
   dfa.cc:1385:36: runtime error: index 1 out of bounds for type 'atomic [0x10000000000000000]'
   dfa.cc:1471:41: runtime error: index 2 out of bounds for type 'atomic [0x10000000000000000]'

This is expected. More discussion on  https://github.com/google/re2/issues/102#issuecomment-268752821
