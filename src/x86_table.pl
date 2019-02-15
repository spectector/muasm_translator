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
% TODO: add the instructions that last
% ins(X86InsName, FormatMasm, SemanticsMasm)
:- export(ins/4).
ins(lfence, o, 0, spbarr).
ins(leave, o, 0, leave).
ins(clflush, o, 0, clflush).
% TODO: ins(bt, o, ???). % Set the carry flag to the select bit (2 operands)
ins(cmp, o, 2, uflags(compare)).
ins(comis, o, 2, uflags(compare)). % TODO: Well done?
ins(ucomis, o, 2, uflags(compare)). % TODO: Well done?
ins(test, o, 2, uflags(test)).
ins(idiv, o, 1, assign_exp1(div)). % TODO: Because the definition it's only applied to 1 operand, the source will be in rax register
ins(div, o, 1, assign_exp1(/)).
ins(div, o, 1, exp1(/)).
ins(mul, o, 1, assign_exp1(mul)).
ins(mulsd, o, 2, assign_exp2(*)).
ins(imul, o, 1, assign_exp1(*)).
ins(imul, o, 2, assign_exp2(*)).
ins(imul, o, 3, exp2(*)).
ins(lea, o, 2, lea).
ins(add, o, 2, assign_exp2(+)).
ins(padd, o, 2, assign_exp2(+)).
ins(sub, o, 2, assign_exp2(-)).
ins(neg, o, 1, assign_exp1(neg)).
ins(not, o, 1, assign_exp1(not)).
ins(sbb, o, 2, subb). % TODO: add borrow from every operation
ins(pxor, o, 2, assign_exp2(pxor)).
ins(xor, o, 2, assign_exp2(#)).
ins(xorpd, o, 2, assign_exp2(#)).
ins(xorps, o, 2, assign_exp2(#)).
ins(and, o, 2, assign_exp2(/\)).
ins(pand, o, 2, assign_exp2(/\)).
ins(andps, o, 2, assign_exp2(/\)). % TODO: and of packed single-precision floating-point values
ins(andpd, o, 2, assign_exp2(/\)). % TODO: and of packed single-precision floating-point values
ins(or, o, 2, assign_exp2(\/)).
ins(dec, o, 1, assign_exp1(dec)).
ins(inc, o, 1, assign_exp1(inc)).
ins(cmovs, o, 2, condmov(<)).
ins(cmovns, a, 2, condmov(>=)).
ins(cmova, o, 2, condmov(ug)).
ins(cmovae, o, 2, condmov(uge)).
ins(cmovb, o, 2, condmov(ul)).
ins(cmovbe, o, 2, condmov(ule)).
ins(cmove, o, 2, condmov(=)).
ins(cmovne, o, 2, condmov(\=)).
ins(cmovl, o, 2, condmov(<)).
ins(cmovle, o, 2, condmov(=<)).
ins(cmovg, o, 2, condmov(>)).
ins(cmovge, o, 2, condmov(>=)).
ins(mov, o, 2, <-).
ins(movhlps, o, 2, <-).
ins(movlhps, o, 2, <-).
ins(movaps, o, 2, <-).
ins(movapd, o, 2, <-).
ins(movabs, o, 2, <-).
ins(movdqu, o, 2, <-).
ins(movdqa, o, 2, <-).
ins(movups, o, 2, <-). % TODO: Move Unaligned Packed Single-Precision Floating- Point Values
ins(movupd, o, 2, <-). % TODO: Move Unaligned Packed Double-Precision Floating- Point Values
ins(movz, o, 2, <-). % TODO: zero extension
ins(movzx, o, 2, <-). % TODO: zero extension
ins(movs, o, 2, <-). % TODO: Sign extension
ins(movsxd, o, 2, <-). % TODO: sign-extension
ins(cvtdq2pd, o, 2, <-). % TODO: sign-extension
ins(cvtsi2ss, o, 2, <-). % TODO: sign-extension
ins(cvtsd2ss, o, 2, <-). % TODO: sign-extension
ins(cvtsi2sd, o, 2, <-). % TODO: sign-extension
ins(cvtss2sd, o, 2, <-). % TODO: sign-extension
ins(cvttss2si, o, 2, <-). % TODO: sign-extension
ins(cvttsd2si, o, 2, <-). % TODO: sign-extension
ins(stmxcsr, o, 1, st_flags). % TODO: like (1, <-) with eflags
ins(ldmxcsr, o, 1, ld_flags). % TODO: like (1, <-) with eflags
ins(jp, a, 1, branch(parity)).
ins(js, a, 1, branch(<)).
ins(jns, a, 1, branch(>=)).
ins(jl, a, 1, branch(<)).
ins(jle, a, 1, branch(=<)).
ins(je, a, 1, branch(=)).
ins(jne, a, 1, branch(\=)).
ins(ja, a, 1, branch(ug)).
ins(jae, a, 1, branch(uge)).
ins(jnb, a, 1, branch(uge)).
ins(jb, a, 1, branch(ul)).
ins(jbe, a, 1, branch(ule)).
ins(jg, a, 1, branch(>)).
ins(jge, a, 1, branch(>=)).
ins(setl, a, 1, condset(<)).
ins(seta, a, 1, condset(ug)).
ins(setae, a, 1, condset(uge)).
ins(setg, a, 1, condset(>)).
ins(setge, a, 1, condset(>=)).
ins(setle, a, 1, condset(=<)).
ins(setb, a, 1, condset(ul)).
ins(setnb, a, 1, condset(uge)).
ins(setbe, a, 1, condset(ule)).
ins(sete, a, 1, condset(=)).
ins(setne, a, 1, condset(\=)).
ins(jmp, a, 1, jmp).
% TODO: ins(bswap) -> Using an additional register
% TODO: ins(maxsd) -> Maybe a condmov with greter as condition?
% TODO: ins(ror, o, 2, assign_exp2(ror)).
ins(shr, o, 2, assign_exp2(>>)).
ins(shr, o, 1, assign_exp1(>>)).
ins(shl, o, 2, assign_exp2(<<)).
ins(pslld, o, 2, assign_exp2(<<)).
ins(sar, o, 2, assign_exp2(ashr)).
ins(sar, o, 1, assign_exp1(ashr)).
ins(psrldq, o, 2, assign_exp2(>>)). % TODO: Good?
ins(psrad, o, 2, assign_exp2(ashr)). % TODO: Good?
ins(sal, o, 2, assign_exp2(<<)). % TODO: fix?
ins(cltd, o, 0, clt). % TODO: Introduce functionality
ins(cwtl, o, 0, clt).
ins(cltq, o, 0, clt).
ins(cdqe, o, 0, clt).
ins(cqto, o, 0, clt).
ins(cbtw, o, 0, clt).
ins(push, o, 1, push).
ins(pop, o, 1, pop).
ins(call, a, 1, call).
ins(ret, o, 0, ret).
ins(ret, o, 1, ret). % TODO: Pop the number of bytes (argument) from stack!!
ins('rep mov', o, _, rep_mov). % TODO: modifying the rcx register -> Load (E)CX bytes from DS:[(E)SI] to AL.
ins('rep', o, _, rep). % TODO: modifying the rcx register -> Load (E)CX bytes from DS:[(E)SI] to AL.
ins(ud2, o, _, exception). % TODO: Include in the semantics
ins(rdtsc, o, _, skip).
ins(nop, o, _, skip).
ins(npad, o, _, skip). % TODO: Introduce several skips

% TODO: I am not sure about this, change parsing mode for operands instead? (see grammar docs)
:- export(fixins/3).
fixins(a, [addr(X,0,0,0),Y], [X,Y]) :- !.
fixins(a, [addr(X,0,0,0)], [X]) :- !.
fixins(_, Xs, Xs).
