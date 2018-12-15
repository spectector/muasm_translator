:- module(_, [parse_file_gas/2], [assertions, dcg]).

:- doc(title, "GAS assembly parser").

% This is a parser for the AT&T style assembly files, for X86 instructions.
% See https://csiflabs.cs.ucdavis.edu/~ssdavis/50/att-syntax.htm

:- use_module(engine(messages_basic)).
:- use_module(library(port_reify)). 
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(stream_utils), [get_line/2]).

:- use_module(.(parser_aux)).
:- use_module(.(x86_table), [fixins/3, ins/3]).

% % (for testing)
% :- export(main/1).
% main([F]) :-
% 	parse_file_gas(F, R),
% 	( member(I, R),
% 	    writeq(I), nl,
% 	    fail
% 	; true
% 	).

% Parse program from file F into R
parse_file_gas(F, R) :-
	catch(parse_file_(F, R), E, handle_err(E)).

parse_file_(F, Insns) :-
	open(F, read, S),
	once_port_reify(parse_stream(S, Insns), Port),
	close(S),
	port_call(Port).

handle_err(syntax_error(Msg)) :- !,
	message(error, ['Could not parse:\n', $$(Msg)]), fail.
handle_err(E) :- throw(E).

parse_stream(S, Insns):-
	get_line(S, Cs),
	( Cs = end_of_file ->
	    Insns = []
	; parse_line(Cs, Ins),
	  ( Ins = '#' ->
	      Insns = Insns0
	  ; Insns = [Ins|Insns0]
	  ),
	  parse_stream(S, Insns0)
	).

parse_line(Cs,X):- sent(X,Cs,[]), !.
parse_line(Cs,_):- throw(syntax_error(Cs)).

% ---------------------------------------------------------------------------

sent('#') --> empty, !.
sent('#') --> comment, !.
sent('#') --> directive(I), { skip_directive(I) }, !. % TODO: all directives ignored
sent(label(Id)) --> label(Id), !.
sent(Ins) --> instruction(Ins).

comment --> blanks, "#", !, ignore_rest.

label(Label) --> idcodes(Cs), ":", { atom_codes(Label, Cs) }, ( comment -> [] ; [] ).

directive(X) --> blanks, idcodes(Cs), { Cs = "."||_, atom_codes(X, Cs) }, ignore_rest.

instruction(Ins) -->
	blanks, 
	insname(InsName1), { atom_codes(InsName,InsName1) },
	{ ins(InsName,Fmt,_) }, 
	( blanks1,
	  oplist(Operands) -> []
	; { Operands = [] }
	), blanks,
	( ";" -> ignore_rest
	; "#" -> ignore_rest
	; []
	),
	{ fixins(Fmt, Operands, Operands2) -> true ; Operands2 = Operands },
	{ Ins =.. [InsName|Operands2] }.

oplist(Ops)-->
	operand(Op),
	{ Ops = [Op|Ops0] },
	blanks,
	( "," ->
	    blanks,
	    oplist(Ops0)
	; { Ops0 = [] }
	).

reg(Reg) --> "%", idcodes(Cs), { atom_codes(Reg, [0'%|Cs]) }. % TODO: register!

% TODO: segment-override memory addressing is not implemented. E.g.,
%   GAS "es:100", NASM "[es:100]"
%   GAS "%ds:-10(%ebp)", NASM "[ds:ebp-10]"

% Address operand (see memory addressing syntax)
% GAS address operand has the form "Offset(Base,Index,Scale)",
% where:
%
%  - Offset is a signed offset (or symbolic value)
%  - Base is a register (or 0 if omitted)
%  - Index is a register (or 0 if omitted)
%  - Scale is a number (or 1 if omitted)
%
% It is equivalent to the memory address at "Base+Index*Scale+Offset"
% address (or "[Base+Index*Scale+Offset]" in NASM syntax).
%

operand(addr(SignedOffset,Base,Index,Scale)) -->
	( offset(SignedOffset) -> []
	; { SignedOffset = 0 }
	),
	"(",
	( reg(Base) -> []
	; { Base = 0 }
	),
	( ",", reg(Index) ->
	    ( ",", num(Scale) ->
	        []
	    ; { Scale = 1 }
	    )
	; { Index = 0, Scale = 0 } 
	),
	")",
	!.
operand(addr(SignedOffset,0,0,0)) --> % TODO: not mem in "call printf"?
	offset(SignedOffset),
	!.
operand(X) --> "$", num_or_id(X), !.
operand(X) --> reg(X), !.

% ---------------------------------------------------------------------------
% Directives

skip_directive('.XMM').
skip_directive('.686P').
skip_directive('.file').
skip_directive('.ident').
skip_directive('.section').
skip_directive('.space').
skip_directive('.comm').
skip_directive('.data').
skip_directive('.text').
skip_directive('.globl').
skip_directive('.type').
skip_directive('.size').
skip_directive('.asciz').
skip_directive('.ascii').
skip_directive('.align').
skip_directive('.cfi_restore').
skip_directive('.quad').
skip_directive('.long').
skip_directive('.byte').
skip_directive('.zero').
skip_directive('.bss').
skip_directive('.local').
skip_directive('.p2align').
skip_directive('.cfi_offset').
skip_directive('.cfi_startproc').
skip_directive('.cfi_endproc').
skip_directive('.cfi_def_cfa').
skip_directive('.cfi_def_cfa_offset').
skip_directive('.cfi_def_cfa_register').
skip_directive('.model').


