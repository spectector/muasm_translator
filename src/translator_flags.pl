% Copyright 2019 The Spectector authors
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

:- module(_, [], [assertions, datafacts]).

:- doc(title, "Flags").

:- data ignore_unknown_instructions/0.
:- export(ignore_unknown_instructions/0).
:- export(init_ignore_unknown_instructions/0).
init_ignore_unknown_instructions :- set_fact(ignore_unknown_instructions).
