% Copyright 2018 The Spectector authors
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
% ===========================================================================

:- module(_, [], [assertions, dcg]).

% X86 instruction table (for parsing and muAsm translation)

% TODO: add format, generate mnemonics from variants (it is easy)
% TODO: incomplete
% ins(X86InsName, FormatMasm, SemanticsMasm)
:- export(ins/3).
ins(lfence, o, spbarr).
ins(leave, o, leave).
ins(clflush, o, clflush).
ins(cmp, o, uflags(compare)).
ins(comiss, o, uflags(compare)). % TODO: Well done?
ins(ucomiss, o, uflags(compare)). % TODO: Well done?
ins(test, o, uflags(test)).
ins(idiv, o, assign_exp1(div)). % TODO: Because the definition it's only applied to 1 operand, the source will be in rax register
ins(div, o, assign_exp1(/)).
ins(mul, o, assign_exp1(mul)).
ins(imul, o, exp2(*)).
ins(lea, o, lea).
ins(add, o, assign_exp2(+)).
ins(sub, o, assign_exp2(-)).
ins(neg, o, assign_exp1(neg)).
ins(not, o, assign_exp1(not)).
ins(sbb, o, subb). % TODO: add borrow from every operation
ins(pxor, o,  assign_exp2(pxor)).
ins(xor, o,  assign_exp2(#)).
ins(and, o, assign_exp2(/\)).
ins(andps, o, assign_exp2(/\)). % TODO: and of packed single-precision floating-point values
ins(or, o, assign_exp2(\/)).
ins(dec, o, assign_exp1(dec)).
ins(inc, o, assign_exp1(inc)).
ins(cmovs, o, condmov(<)).
ins(cmovns, a, condmov(>=)).
ins(cmova, o, condmov(ug)).
ins(cmovae, o, condmov(uge)).
ins(cmovb, o, condmov(ul)).
ins(cmovbe, o, condmov(ule)).
ins(cmove, o, condmov(=)).
ins(cmovne, o, condmov(\=)).
ins(cmovl, o, condmov(<)).
ins(cmovle, o, condmov(=<)).
ins(cmovg, o, condmov(>)).
ins(cmovge, o, condmov(>=)).
ins(mov, o, <-).
ins(movabs, o, <-).
ins(movups, o, <-). % TODO: Move Unaligned Packed Single-Precision Floating- Point Values
ins(movz, o, <-). % TODO: zero extension
ins(movzx, o, <-). % TODO: zero extension
ins(movs, o, <-). % TODO: Sign extension
ins(movsxd, o, <-). % TODO: sign-extension
ins(stmxcsr, o, st_flags). % TODO: like (<-) with eflags
ins(ldmxcsr, o, st_flags). % TODO: like (<-) with eflags
% TODO: jp
ins(js, a, branch(<)).
ins(jns, a, branch(>=)).
ins(jl, a, branch(<)).
ins(jle, a, branch(=<)).
ins(je, a, branch(=)).
ins(jne, a, branch(\=)).
ins(ja, a, branch(ug)).
ins(jae, a, branch(uge)).
ins(jnb, a, branch(uge)).
ins(jb, a, branch(ul)).
ins(jbe, a, branch(ule)).
ins(jg, a, branch(>)).
ins(jge, a, branch(>=)).
ins(setl, a, condset(<)).
ins(seta, a, condset(ug)).
ins(setae, a, condset(uge)).
ins(setg, a, condset(>)).
ins(setge, a, condset(>=)).
ins(setle, a, condset(=<)).
ins(setb, a, condset(ul)).
ins(setbe, a, condset(ule)).
ins(sete, a, condset(=)).
ins(setne, a, condset(\=)).
ins(jmp, a, jmp).
% TODO: ins(ror, o, assign_exp2(ror)).
ins(shr, o, assign_exp2(>>)).
ins(shl, o, assign_exp2(<<)).
ins(sar, o, assign_exp2(ashr)).
ins(sal, o, assign_exp2(<<)). % TODO: fix?
ins(cltd, o, clt). % TODO: Introduce functionality
ins(cwtl, o, clt).
ins(cltq, o, clt).
ins(cdqe, o, clt).
ins(cqto, o, clt).
ins(push, o, push).
ins(pop, o, pop).
ins(call, a, call).
ins(ret, o, ret).
ins(ud2, o, exception). % TODO: Include in the semantics
ins(rdtsc, o, skip).
ins(nop, o, skip).
ins(npad, o, skip). % TODO: Introduce several skips

% TODO: I am not sure about this, change parsing mode for operands instead? (see grammar docs)
:- export(fixins/3).
fixins(a, [addr(X,0,0,0),Y], [X,Y]) :- !.
fixins(a, [addr(X,0,0,0)], [X]) :- !.
fixins(_, Xs, Xs).
