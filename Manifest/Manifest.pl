:- bundle(muasm_translator).
version('1.0').
depends([
    core-[version>='1.18']
]).
alias_paths([
    muasm_translator = 'src'
]).
lib('src').

