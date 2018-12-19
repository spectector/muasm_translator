%%   Copyright 2018 The muasm_translator authors
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.

:- module(_, [], [dcg]).

:- export(empty/2).
empty([],[]).

:- export(ignore_rest/2).
ignore_rest(_, []).

:- export(digit/3).
digit(X) --> [X], {X>=0'0, X=<0'9}.

:- export(digit16/3).
digit16(X) --> [X], {X>=0'0, X=<0'9}, !.
digit16(X) --> [X], {X>=0'a, X=<0'f}, !.
digit16(X) --> [X], {X>=0'A, X=<0'F}.

:- export(alpha/3).
alpha(X) --> [X], { X>=0'a, X=<0'z -> true ; X>=0'A,X=<0'Z -> true ; fail }. 

:- export(blank/2).
blank --> [X], { X=<32 }.

:- export(blanks1/2).
blanks1 --> blank, blanks.

:- export(blanks/2).
blanks --> blank, !, blanks.
blanks --> [].

:- export(num/3).
num(X) --> "-", numcodes(Cs, Base), !, { number_codes(X1, Base, Cs), X is -X1 }.
num(X) --> numcodes(Cs, Base), !, { number_codes(X, Base, Cs) }.

numcodes([X|Cs], 16) --> "0x", digit16(X), !, numcodes_(Cs).
numcodes([X|Cs], 10) --> digit(X), !, numcodes_(Cs).

numcodes_([X|Cs]) --> digit(X), !, numcodes_(Cs).
numcodes_("") --> "".

numcodes16_([X|Cs]) --> digit16(X), !, numcodes16_(Cs).
numcodes16_("") --> "".

:- export(idcodes/3).
idcodes("_"||Cs) --> "_", !, idcodes_(Cs).
idcodes("."||Cs) --> ".", !, idcodes_(Cs).
idcodes([X|Cs]) --> alpha(X), !, idcodes_(Cs).

idcodes_("."||Cs) --> ".", !, idcodes_(Cs).
idcodes_("_"||Cs) --> "_", !, idcodes_(Cs).
idcodes_([X|Cs]) --> digit(X), !, idcodes_(Cs).
idcodes_([X|Cs]) --> alpha(X), !, idcodes_(Cs).
idcodes_("") --> "".

:- export(num_or_id/3).
num_or_id(Off) --> num(Off), !.
num_or_id(Off) --> idcodes(Off0), !, { atom_codes(Off, Off0) }.

:- export(insname/3).
insname([X|Cs]) --> alpha(X), !, insname(Cs).
insname("") --> "".

:- export(offset/3).
offset(A+B) --> num_or_id(A), "+", !, num_or_id(B).
offset(A-B) --> num_or_id(A), "-", !, num_or_id(B).
offset(A) --> num_or_id(A), !.

