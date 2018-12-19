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
ins(cmpl, o, uflags(compare)).
ins(cmpq, o, uflags(compare)).
ins(cmpb, o, uflags(compare)).
ins(cmp, o, uflags(compare)).
ins(test, o, uflags(test)).
ins(testb, o, uflags(test)).
ins(testl, o, uflags(test)).
ins(testq, o, uflags(test)).
ins(idivl, o, exp2(div)).
ins(divl, o, exp2(/)).
ins(imull, o, exp2(*)).
ins(imul, o, exp2(*)).
ins(leaq, o, lea).
ins(leal, o, lea).
ins(lea, o, lea).
ins(addl, o, assign_exp2(+)).
ins(addq, o, assign_exp2(+)).
ins(addb, o, assign_exp2(+)).
ins(add, o, assign_exp2(+)).
ins(sub, o, assign_exp2(-)).
ins(subl, o, assign_exp2(-)).
ins(subb, o, assign_exp2(-)).
ins(subq, o, assign_exp2(-)).
ins(neg, o, assign_exp1(neg)).
ins(negl, o, assign_exp1(neg)).
ins(sbb, o, assign_exp2(-)). % TODO: add borrow
ins(xorl, o,  assign_exp2(#)).
ins(xorq, o,  assign_exp2(#)).
ins(xor, o,  assign_exp2(#)).
ins(andl, o, assign_exp2(/\)).
ins(andb, o, assign_exp2(/\)).
ins(andq, o, assign_exp2(/\)).
ins(and, o, assign_exp2(/\)).
ins(or, o, assign_exp2(\/)).
ins(orb, o, assign_exp2(\/)).
ins(orl, o, assign_exp2(\/)).
ins(orq, o, assign_exp2(\/)).
ins(dec, o, assign_exp1(dec)).
ins(decl, o, assign_exp1(dec)).
ins(decq, o, assign_exp1(dec)).
ins(inc, o, assign_exp1(inc)).
ins(incq, o, assign_exp1(inc)).
ins(cmovs, o, condmov(<)).
ins(cmovsq, o, condmov(<)).
ins(cmovns, a, condmov(>=)).
ins(cmovnsq, a, condmov(>=)).
ins(cmova, o, condmov(ug)).
ins(cmovaq, o, condmov(ug)).
ins(cmovaeq, o, condmov(uge)).
ins(cmovae, o, condmov(uge)).
ins(cmovbq, o, condmov(ul)).
ins(cmovb, o, condmov(ul)).
ins(cmovbeq, o, condmov(ule)).
ins(cmoveq, o, condmov(=)).
ins(cmovneq, o, condmov(\=)).
ins(cmovlq, o, condmov(<)).
ins(cmovll, o, condmov(<)).
ins(cmovleq, o, condmov(=<)).
ins(cmovgq, o, condmov(>)).
ins(cmovgeq, o, condmov(>=)).
ins(movabsq, o, <-).
ins(mov, o, <-).
ins(movb, o, <-).
ins(movl, o, <-).
ins(movq, o, <-).
ins(movsbl, o, <-).
ins(movslq, o, <-).
ins(movzbl, o, <-).
ins(movzx, o, <-).
ins(movsxd, o, <-).
ins(js, a, branch(<)).
ins(jns, a, branch(>=)).
ins(jl, a, branch(<)).
ins(jle, a, branch(=<)).
ins(je, a, branch(=)).
ins(jne, a, branch(\=)).
ins(ja, a, branch(ug)).
ins(jae, a, branch(uge)).
ins(jb, a, branch(ul)).
ins(jbe, a, branch(ule)).
ins(jg, a, branch(>)).
ins(jge, a, branch(>=)).
ins(seta, a, condset(ug)).
ins(setae, a, condset(uge)).
ins(setb, a, condset(ul)).
ins(sete, a, condset(=)).
ins(jmp, a, jmp).
ins(shrq, o, assign_exp2(>>)).
ins(shrl, o, assign_exp2(>>)).
ins(shr, o, assign_exp2(>>)).
ins(shll, o, assign_exp2(<<)).
ins(shl, o, assign_exp2(<<)).
ins(shlq, o, assign_exp2(<<)).
ins(sarq, o, assign_exp2(ashr)).
ins(cltd, o, clt).
ins(cltq, o, clt).
ins(cdqe, o, clt).
ins(pushq, o, push).
ins(push, o, push).
ins(pushl, o, push).
ins(popq, o, pop).
ins(pop, o, pop).
ins(callq, a, call).
ins(call, a, call).
ins(retq, o, ret).
ins(ret, o, ret).
ins(rdtsc, o, skip).
ins(nop, o, skip).
ins(npad, o, skip). % TODO: The operator matters?

% TODO: I am not sure about this, change parsing mode for operands instead? (see grammar docs)
:- export(fixins/3).
fixins(a, [addr(X,0,0,0),Y], [X,Y]) :- !.
fixins(a, [addr(X,0,0,0)], [X]) :- !.
fixins(_, Xs, Xs).
