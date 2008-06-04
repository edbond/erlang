%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id: gregexp.erl,v 1.1 2003/11/03 17:31:19 mikl Exp $

%%
%% Parser and some interpretator code stolen from 
%% gregexp.erl (C) March 2001 pascal.brisset@cellicium.com .
%%
%% RE optimizator and it's interpretator written by Gaspar Chilingarov
%% (c) December 2006
%% 

-module(re).
-author("Gaspar Chilingarov <nm@web.am>").
-vsn(1.0).

-export([parse/1]).
-export([first_match/2, first_match_g/2]).
-export([matches/2, matches_g/2]).

-export([m/2, mg/2, mgg/2]).
-export([mall/2, mallg/2, mallgg/2]).


-import(lists, [reverse/1]).

%-compile(export_all).


%%% --------------------------------------------------------------------------------
%%% --------------------------------------------------------------------------------
%%%
%%% Public exported functions
%%%

first_match(String, RE) when is_list(RE) -> % {{{
	first_match(String, parse(RE));

first_match(String, {re, RE}) ->
	case re_apply(String, RE) of
	nomatch -> nomatch;
	{match, Start, End, _Groups} -> {match, {Start, End-Start}}
	end. % }}}

first_match_g(String, RE) when is_list(RE) -> % {{{
	first_match_g(String, parse(RE));

first_match_g(String, {re, RE}) ->
	case re_apply(String, RE) of
	nomatch -> nomatch;
	{match, Start, End, Groups} -> 
%		erlang:display({match, Start, End, Groups}),
		{match, {Start, End-Start}, expand_groups(Groups)}
	end. % }}}

%% -type matches(String, RegExp) -> {match,[{Start,Length}]} | {error,E}.
%%  Return the all the non-overlapping matches of RegExp in String.
matches(S, RegExp) when list(RegExp) -> % {{{
    case parse(RegExp) of
	{re, RE} -> matches(S, {re, RE});
	{error, E} -> {error, E}
    end;
matches(S, {re, RE}) ->
    {match, reverse(matches(S, RE, [], 0))}.

matches([], _, Result, _) ->
	Result;

matches(String, RE, Result, Position) ->
	case re_apply(String, RE) of
	nomatch -> Result;
	{match, Start, End, _Groups} -> 
		matches(lists:nthtail(End-1, String), RE, [ {Position+Start, End-Start} | Result ], Position+End-1)
	end. % }}}

matches_g(S, RegExp) when list(RegExp) -> % {{{
    case parse(RegExp) of
	{re, RE} -> matches_g(S, {re, RE});
	{error, E} -> {error, E}
    end;
matches_g(S, {re, RE}) ->
    {match, reverse(matches_g(S, RE, [], 0))}.

matches_g([], _, Result, _) ->
	Result;

matches_g(String, RE, Result, Position) ->
	case re_apply(String, RE) of
	nomatch -> Result;
	{match, Start, End, Groups} -> 
%		erlang:display({chopping, End-1, off, length(String)}),
		matches_g(lists:nthtail(End-1, String), RE, [ {Position+Start, End-Start, expand_groups(Groups, Position) } | Result ], Position+End-1)
	end. % }}}

%%%
%%% extract match substrings
%%%
m(String, {re, RE}) -> % {{{
	case re_apply(String, RE) of
	nomatch -> nomatch;
	{match, Start, End, _Groups} -> { lists:sublist(String, Start, End-Start) }
	end. % }}}

%%% XXX: does not support deep nested matches - i,e,  /(\\+([0-9]+))/ to match whole phone number and only numbers ...
mg(String, {re, RE}) -> % {{{
	case re_apply(String, RE) of
	nomatch -> nomatch;
	{match, Start, End, Groups} ->
		SubString = lists:sublist(String, Start, End-Start),
		{ SubString, extract_submatches(Groups, SubString, Start, []) }
%		{ , [ lists:sublist(String, X, X1-X) || {X, X1, _} <- Groups ] }
	end; 
mg(String, RE) when is_list(RE) ->
	mg(String, parse(RE)). % }}}

mgg(String, {re, RE}) -> % {{{
	case re_apply(String, RE) of
	nomatch -> nomatch;
	{match, Start, End, Groups} ->
		SubString = lists:sublist(String, Start, End-Start),
		list_to_tuple(extract_submatches(Groups, SubString, Start, []))
	end; 
mgg(String, RE) when is_list(RE) ->
	mgg(String, parse(RE)). % }}}

%%%
%%% match globally and return matches substrings
%%%
mall(String, {re, RE}) -> % {{{
	case matches(String, {re, RE}) of
	{match, List} -> 
		extract_matches(List, String, 1, [])
	end; 
mall(String, RE) when is_list(RE) ->
	mall(String, parse(RE)). % }}}

mallg(String, {re, RE}) -> % {{{
	case matches_g(String, {re, RE}) of
	{match, List} -> 
		extract_matches(List, String, 1, [])
	end; 
	
mallg(String, RE) when is_list(RE) ->
	mallg(String, parse(RE)). % }}}

mallgg(String, {re, RE}) -> % {{{
	case matches_g(String, {re, RE}) of
	{match, List} -> 
		extract_onlysubmatches(List, String, 1, [])
	end; 
	
mallgg(String, RE) when is_list(RE) ->
	mallgg(String, parse(RE)). % }}}

%%%  utility functions / private /

%%% convert deep lists of {start, stop, submatches} records to {start, len, submatches} | {start, len}
expand_groups(Groups) -> % {{{
	expand_groups(Groups, 0, []).

expand_groups(Groups, Pos) -> 
	expand_groups(Groups, Pos, []).

expand_groups([], _Pos, Result) -> reverse(Result);
expand_groups([{Start, End, []}|Tail], Pos, Result) -> 
	expand_groups(Tail, Pos, [ {Pos+Start, End-Start} | Result ]);
expand_groups([{Start, End, List}|Tail], Pos, Result) -> 
	expand_groups(Tail, Pos, [ {Pos+Start, End-Start, expand_groups(List, Pos, []) } | Result ]). % }}}

extract_submatches([], _, _, Result) -> reverse(Result); % {{{
extract_submatches([{Start, Stop}|Tail], String, Position, Result) ->
	extract_submatches(Tail, lists:nthtail(Stop-Position, String), Stop, [ lists:sublist(String, Start-Position+1, Stop-Start) | Result ]);
extract_submatches([{Start, Stop, []}|Tail], String, Position, Result) ->
	extract_submatches(Tail, lists:nthtail(Stop-Position, String), Stop, [ lists:sublist(String, Start-Position+1, Stop-Start) | Result ]); 
extract_submatches([{Start, Stop, G}|Tail], String, Position, Result) ->
	extract_submatches(Tail, lists:nthtail(Stop-Position, String), Stop, [ {lists:sublist(String, Start-Position+1, Stop-Start),
		extract_submatches(G, lists:sublist(String, Start-Position+1, Stop-Start), Position, [])
	} | Result ]). % }}}

extract_submatcheslen([], _, _, Result) -> reverse(Result); % {{{
extract_submatcheslen([{Start, Len}|Tail], String, Position, Result) ->
	extract_submatcheslen(Tail, lists:nthtail(Start+Len-Position, String), Start+Len, [ lists:sublist(String, Start-Position+1, Len) | Result ]);
extract_submatcheslen([{Start, Len, []}|Tail], String, Position, Result) ->
	extract_submatcheslen(Tail, lists:nthtail(Start+Len-Position, String), Start+Len, [ lists:sublist(String, Start-Position+1, Len) | Result ]). % }}}

extract_matches([], _, _, Result) -> reverse(Result); % {{{
extract_matches([{Start, Len}|Tail], String, Position, Result) ->
	extract_matches(Tail, lists:nthtail(Start+Len-Position, String), Start+Len, [ lists:sublist(String, Start-Position+1, Len) | Result ]);
extract_matches([{Start, Len, SubMatches}|Tail], String, Position, Result) ->
	SubString = lists:sublist(String, Start-Position+1, Len),
	extract_matches(Tail, lists:nthtail(Start+Len-Position, String), Start+Len, 
		[ {SubString, extract_submatcheslen(SubMatches, SubString, Start, [])} | Result ]). % }}}

extract_onlysubmatches([], _, _, Result) -> reverse(Result); % {{{
extract_onlysubmatches([{Start, Len}|Tail], String, Position, Result) ->
	extract_onlysubmatches(Tail, lists:nthtail(Start+Len-Position, String), Start+Len, [ lists:sublist(String, Start-Position+1, Len) | Result ]);
extract_onlysubmatches([{Start, Len, SubMatches}|Tail], String, Position, Result) ->
	SubString = lists:sublist(String, Start-Position+1, Len),
	extract_onlysubmatches(Tail, lists:nthtail(Start+Len-Position, String), Start+Len, 
		[ list_to_tuple(extract_submatcheslen(SubMatches, SubString, Start, [])) | Result ]). % }}}

%%%
%%% parse regular expression
%%%
parse(Txt) -> % {{{
	RegExp = reg(Txt),
%	erlang:display(RegExp),
	Optim = convert(RegExp),
%	erlang:display(Optim),
	Optim1 = to_anchors(element(1, Optim)),
%	erlang:display(Optim1),
	{re, Optim1}.
%	compile(Optim). % }}}


%%% --------------------------------------------------------------------------------
%%% --------------------------------------------------------------------------------
%%%
%%% Internal module functions
%%%

%%% --------------------------------------------------------------------------------
%%% Parser
%%%
%% This is the regular expression grammar used. It is equivalent to the % {{{
%% one used in AWK, except that we allow ^ $ to be used anywhere and fail
%% in the matching.
%%
%% reg -> reg1 : '$1'.
%% reg1 -> reg1 "|" reg2 : {'or','$1','$2'}.
%% reg1 -> reg2 : '$1'.
%% reg2 -> reg2 reg3 : {concat,'$1','$2'}.
%% reg2 -> reg3 : '$1'.
%% reg3 -> reg3 "*" : {kclosure,'$1'}.
%% reg3 -> reg3 "+" : {pclosure,'$1'}.
%% reg3 -> reg4 : '$1'.
%% reg4 -> "(" reg ")" : '$2'.
%% reg4 -> "\\(" reg "\\)" : '$2'.
%% reg4 -> "\\" char : '$2'.
%% reg4 -> "^" : bos.
%% reg4 -> "$" : eos.
%% reg4 -> "." : char.
%% reg4 -> "[" class "]" : {char_class,char_class('$2')}
%% reg4 -> "[" "^" class "]" : {comp_class,char_class('$3')}
%% reg4 -> "\"" chars "\"" : char_string('$2')
%% reg4 -> char : '$1'.
%% reg4 -> empty : epsilon.
%%  The grammar of the current regular expressions. The actual parser
%%  is a recursive descent implementation of the grammar. % }}}

reg(S) ->  % {{{
	reg1(S).

%% reg1 -> reg2 reg1'
%% reg1' -> "|" reg2
%% reg1' -> empty

reg1(S0) ->
    {L, S1} = reg2(S0),
    reg1p(S1, L).

reg1p([$||S0], L) ->
    {R, S1} = reg2(S0),
    reg1p(S1, {'or', L, R});
reg1p(S, L) -> {L, S}. % }}}

%% reg2 -> reg3 reg2' % {{{
%% reg2' -> reg3
%% reg2' -> empty

reg2(S0) ->
    {L, S1} = reg3(S0),
    reg2p(S1, L).

%reg2p([$\\,$)|_]=S, L) -> {L, S};
reg2p([C|S0], L) when C /= $|, C /= $) ->
    {R, S1} = reg3([C|S0]),
    reg2p(S1, {concat, L, R});
reg2p(S, L) -> {L, S}. % }}}

%% reg3 -> reg4 reg3' % {{{
%% reg3' -> "*" reg3'
%% reg3' -> "+" reg3'
%% reg3' -> "?" reg3'
%% reg3' -> empty

reg3(S0) ->
    {L, S1} = reg4(S0),
    reg3p(S1, L).

reg3p([$*,$?|S], L) -> reg3p(S, {kclosure_ng, L});	% non-greedy
reg3p([$+,$?|S], L) -> reg3p(S, {pclosure_ng, L});	% non-greedy
reg3p([$?,$?|S], L) -> reg3p(S, {optional_ng, L});	% parsed, but not processed
reg3p([$*|S], L) -> reg3p(S, {kclosure, L});
reg3p([$+|S], L) -> reg3p(S, {pclosure, L});
reg3p([$?|S], L) -> reg3p(S, {closure, {0,1}, L});
reg3p([${|S], L) -> {Bounds, Tail} = quantifier(S),
					case Bounds of	
					{Low, High} when Low > High -> throw({error, {illegal_bounds, Bounds}});
					_ -> ok
					end,
					reg3p(Tail, {closure, Bounds, L});
reg3p(S, L) -> {L, S}. % }}}

reg4([$(,$?,$:|S0]) ->	% group subpatterns without matching % {{{
    case reg(S0) of
	{R,[$)|S1]} -> {R, S1};
	{_R, _S} -> throw({error,{unterminated,"("}})
    end; 
reg4([$(|S0]) ->
    case reg(S0) of
	{R,[$)|S1]} -> {{match, R}, S1};
	{_R, _S} -> throw({error,{unterminated,"("}})
    end; 
%reg4([$\\,$(|S0]) ->
%    case reg(S0) of
%	{R,[$\\,$)|S1]} -> {{group, R}, S1};
%	{R, S} -> throw({error,{unterminated,"\\("}})
%    end;
reg4([$\\, O1, O2, O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    {(O1*8 + O2)*8 + O3 - 73*$0, S};
reg4([$\\, C|S]) -> {escape_char(C), S};
reg4([$\\]) -> throw({error,{unterminated,"\\"}});  % "
reg4([$^|S]) -> {bos, S};
reg4([$$|S]) -> {eos, S};
%reg4([$.|S]) -> {{comp_class,"\n"}, S};
reg4([$.|S]) -> {any_symbol, S};
reg4("[^" ++ S0) ->
    case char_class(S0) of
	{Cc,[$]|S1]} -> {{comp_class, Cc}, S1};
	{_Cc, _S} -> throw({error,{unterminated,"["}})
    end;
reg4([$[|S0]) ->
    case char_class(S0) of
	{Cc,[$]|S1]} -> {{char_class, Cc}, S1};
	{_Cc, _S1} -> throw({error,{unterminated,"["}})
    end;
reg4([C|S]) when C /= $*, C /= $+, C /= $?, C /= $] -> {C, S};
reg4([C|_S]) -> throw({error,{illegal,[C]}});
reg4([]) -> {epsilon,[]}. % }}}

escape_char($n) -> $\n;				%\n = LF % {{{
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $\s;				%\s = SPACE
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C. % }}}

char_class([$]|S]) -> char_class(S, [$]]); % {{{
char_class(S) -> char_class(S, []). % }}}

char($\\, [O1, O2, O3|S]) when % {{{
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    {(O1*8 + O2)*8 + O3 - 73*$0, S};
char($\\, [C|S]) -> {escape_char(C), S};
char(C, S) -> {C, S}. % }}}

char_class([C1|S0], Cc) when C1 /= $] -> % {{{
    case char(C1, S0) of
	{Cf,[$-, C2|S1]} when C2 /= $] ->
	    case char(C2, S1) of
		{Cl, S2} when Cf < Cl -> char_class(S2, [{Cf, Cl}|Cc]); 
		{Cl, _S2} -> throw({error,{char_class,[Cf,$-, Cl]}})
	    end;
	{C, S1} -> char_class(S1, [C|Cc])
    end;
char_class(S, Cc) -> {Cc, S}. % }}}

quantifier(S) -> % {{{
	quantifier(S, {[], none}).

quantifier([ C | Tail ], {Low, none}) when (C >= $0) and (C =< $9) ->
	quantifier(Tail, { [C|Low], none });
quantifier([ $, | Tail ], {[], none}) -> % {, means 0 or more matches
	quantifier(Tail, {"0", []});
quantifier([ $, | Tail ], {Low, none}) ->
	quantifier(Tail, {Low, []});
quantifier([ C | Tail ], {Low, High}) when (C >= $0) and (C =< $9) ->
	quantifier(Tail, {Low, [C|High]});
quantifier([ $} | Tail ], {Low, none}) ->
	Len = list_to_integer(reverse(Low)),
	{{Len, Len}, Tail};
quantifier([ $} | Tail ], {Low, []}) ->
	{{list_to_integer(reverse(Low)), inf}, Tail};
quantifier([ $} | Tail ], {Low, High}) ->
	{{list_to_integer(reverse(Low)), list_to_integer(reverse(High))}, Tail};
quantifier(_, _) ->
	throw({error,{unterminated,"}"}}). % }}}

%%% --------------------------------------------------------------------------------
%%% RE optimizer
%%%

convert({pclosure, K}) -> K1 = convert(K), {concat, K1, {kclosure, K1}}; % {{{
convert({pclosure_ng, K}) -> K1 = convert(K), {concat, K1, {kclosure_ng, K1}};

convert({closure, {0, inf}, K}) -> 	
	{kclosure, convert(K) };
convert({closure, {N, inf}, K}) -> 
	K1 = convert(K),
	lists:foldl(
		fun(_, AccIn) -> {concat, K1, AccIn} end, 
		{kclosure, K1},
		lists:seq(1, N)
	);
convert({closure, {0, 0}, _K}) -> epsilon;
convert({closure, {N, M}, K}) when N > 0 -> 
	K1 = convert(K),
	lists:foldl(
		fun(_, AccIn) -> {concat, K1, AccIn} end, 
		convert({closure, {0, M-N}, K1}),
		lists:seq(1, N)
	);

convert({concat, C, C1}) when is_integer(C) and is_integer(C1) -> {const, [ C, C1 ]};
convert({'or', {char_class, L1}, {char_class, L2}}) -> {char_class, L1 ++ L2};

convert({char_class, L}) ->
	{char_class, char_class_optimize(L)};

convert({comp_class, L}) ->
	comp_class_optimize(L);

convert({concat, L, R}) -> 
	case {convert(L), convert(R)} of
		{C, {const, Lst}} when is_integer(C) -> {const, [ C | Lst ]};
		{{const, Lst}, C} when is_integer(C) -> {const, Lst ++ [ C ]};
		{{const, Lst}, {const, Lst1}} -> {const, Lst ++ Lst1};
		{{concat, X, C}, C1} when is_integer(C) and is_integer(C1) -> {concat, X, {const, [C, C1]}};
		{{concat, X, {const, Lst}}, C} when is_integer(C) -> {concat, X, {const, Lst ++ [C]}};
		{{concat, X, C}, {const, Lst}} when is_integer(C) -> {concat, X, {const, [C|Lst]}};
		{{concat, X, {const, Lst1}}, {const, Lst2}}  -> {concat, X, {const, Lst1 ++ Lst2 }};
		{C, {concat, C1, X}} when is_integer(C) and is_integer(C1) -> {concat, {const, [C, C1]}, X};
		{C, {concat, {const, Lst}, X}} when is_integer(C) -> {concat, {const, [C|Lst]}, X};
		{{const, Lst0}, {concat, C, X}} when is_integer(C) -> {concat, {const, Lst0 ++ [C]}, X};
		{{const, Lst0}, {concat, {const, Lst}, X}} -> {concat, {const, Lst0 ++ Lst}, X};
		{X, Y} -> {concat, X, Y}
	end;
convert({const, _} = X) -> X;
convert({L, R}) -> {convert(L), convert(R)};
convert({X, L, R}) -> {X, convert(L), convert(R)};
convert(X) -> X. % }}}

%%%
%%% XXX: detect unicode charactes, please
%%%
char_class_to_list(L) -> char_class_to_list(L, []). % {{{

char_class_to_list([], Result) -> char_class_sequential(lists:sort(Result));
char_class_to_list([{C1, C2}|Tail], Result) -> 
	char_class_to_list(Tail, lists:seq(C1, C2) ++ Result);
char_class_to_list([C1|Tail], Result) -> 
	char_class_to_list(Tail, [C1|Result]). % }}}


char_class_sequential(List) -> % {{{
	case char_class_sequential(List, seq) of
	{seq, Last} -> {hd(List), Last};
	_ -> List
	end.

char_class_sequential([H, H1], seq) when H1 == (H+1) -> 
	{seq, H1};
char_class_sequential([H, H1 | Tail], seq) when H1 == (H+1) -> 
	char_class_sequential([H1|Tail], seq);
char_class_sequential(_, _) -> notseq. % }}}


%%%
%%% XXX: because of limited size of tuple we will have problems with unicode chars ( > 255)
%%% 
char_class_optimize({_, _}=X) -> X; % {{{
%char_class_optimize([{_, _}]=X) -> {char_class, X}; 
char_class_optimize(CList) ->
	case char_class_to_list(CList) of
	[L] -> L;	% one character class
	{_, _} = T -> T; % single range
	L when length(L) > 30 ->
		lists:foldl(fun(X, Acc) -> setelement(X+1, Acc, true) end,
					erlang:make_tuple(256, false),
					L);
	L -> L
	end. % }}}

comp_class_optimize({_, _}=X) -> {comp_class, X}; % {{{
comp_class_optimize([{_, _}]=X) -> {comp_class, X}; 
comp_class_optimize(CList) -> 
	case char_class_to_list(CList) of
	[L] -> {comp_class, L};	% one character class
	{_, _} = T -> T; % single range
	L when length(L) > 30 ->
		{char_class, 	% convert to char_class
		lists:foldl(fun(X, Acc) -> setelement(X+1, Acc, false) end,
					erlang:make_tuple(256, true),
					L)
		};
	L -> {comp_class, L}	% stay comp_class
	end. % }}}

%%% 
%%% flatten presentation from deeply nested tree to list 
%%%
to_anchors(X) ->  % {{{
	R = to_anchors(X, []),
	R1 = to_anchors2( R ),
	R1.

to_anchors({concat, X, Y}, List) ->
	 to_anchors(X, []) ++ to_anchors(Y, []) ++ List;
to_anchors({'or', X, Y}, List) ->
	 [{'or', to_anchors(X, []), to_anchors(Y, [])}] ++ List;
%to_anchors({match, R}, List) ->
%	[ {match, to_anchors(R)} | List ];
to_anchors({T, R}, List) when (T == match) or (T == kclosure) or (T == kclosure_ng)  ->
	[ {T, to_anchors(R)} | List ];
to_anchors({T, X, R}, List) when (T == closure) ->
	[ {T, X, to_anchors(R)} | List ];
to_anchors(N, List) when is_integer(N) ->
	[ {const, [N]} | List ];
to_anchors(X, List) ->
	[ X | List ].
	
to_anchors2([]) -> [];
to_anchors2([ epsilon | Tail ]) ->
	to_anchors2(Tail);
to_anchors2([ {const, Txt}, {const, Txt1} | Tail ]) ->
	to_anchors2([ {const, Txt ++ Txt1} | Tail ]);
to_anchors2([ {const, Txt}, C | Tail]) when is_integer(C) ->
	to_anchors2([ {const, Txt ++ [C]} | Tail ]);
to_anchors2([ C, {const, Txt} | Tail]) when is_integer(C) ->
	to_anchors2([ {const, [C] ++ Txt} | Tail ]);
to_anchors2([ C | Tail]) when is_integer(C) ->
	to_anchors2([ {const, [C]} | Tail ]);
to_anchors2([ {Type, _} = X | Tail ]) when 
	(Type == const) or (Type == char_class) or (Type == comp_class) ->
	[ X | to_anchors2(Tail) ];
to_anchors2([ {X, R} | Tail ]) ->
	[ {X, to_anchors2(R)} | to_anchors2(Tail) ];
%to_anchors2([ {X, R, R1} | Tail ]) ->
%	[ {X, to_anchors2(R), to_anchors2(R1)} | to_anchors2(Tail) ];
to_anchors2([ X | Tail ]) ->
	[ X | to_anchors2(Tail) ]. % }}}

%%%% stage 3 optimize not used yet ...
%to_anchors3([]) -> []; % {{{
%to_anchors3([ {const, Txt} | Tail ]) ->
%	[ {hard, length(Txt), length(Txt), {const, Txt}}  | to_anchors3(Tail) ];
%
%to_anchors3([ X | Tail ]) when (X == bos) or (X == eos) ->
%	[ {hard, 0, 0, X} | to_anchors3(Tail) ];
%to_anchors3([ {char_class, _} = X | Tail ]) ->
%	[ {hard, 1, 1, X} | to_anchors3(Tail) ];
%to_anchors3([ {comp_class, _} = X | Tail ]) ->
%	[ {hard, 1, 1, X} | to_anchors3(Tail) ];
%to_anchors3([ {X, R} | Tail ]) when (X == kclosure) ->
%	Trans = to_anchors3(R),
%	[ {soft, 0, inf, {X, Trans}} | to_anchors3(Tail) ];
%to_anchors3([ {closure, {RMin, RMax}, R} | Tail ]) ->
%	Trans = to_anchors3(R),
%	{Min, Max} = calc_len(Trans),
%	[ {soft, RMin*Min, Max*RMax, {closure, {RMin, RMax}, Trans}} | to_anchors3(Tail) ];
%to_anchors3([ {match, Trans} | Tail ]) ->
%	{Min, Max} = calc_len(Trans),
%	[ {soft, Min, Max, {match, Trans}} | to_anchors3(Tail) ];
%
%to_anchors3([ X | Tail ]) ->
%	[ X | to_anchors3(Tail) ];
%
%to_anchors3(T) when is_tuple(T) -> to_anchors3([T]). % }}}
%calc_len(L) -> calc_len(L, {0,0}). % {{{
%
%calc_len([], X) -> X;
%calc_len([ {_, Mi, Ma, _} | Tail ], {Min, Max}) when (Ma == inf) or (Max == inf) -> 
%	calc_len(Tail, {Min+Mi, inf});
%calc_len([ {_, Mi, Ma, _} | Tail ], {Min, Max}) -> 
%	calc_len(Tail, {Min+Mi, Max+Ma}). % }}}
%have_hard_anchors(RE) -> % {{{
%	have_hard_anchors(RE, false).
%
%have_hard_anchors([ {const, _} | _ ], _) -> true;
%have_hard_anchors({const, _}, _) -> true;
%have_hard_anchors({char_class, _}, _) -> true;
%have_hard_anchors([ {char_class, _} | _ ], _) -> true;
%have_hard_anchors([ {kclosure, _} | Tail ], _) -> have_hard_anchors(Tail);
%have_hard_anchors([ {_, Tokens} | Tail ], _) -> 
%	have_hard_anchors(Tokens) orelse have_hard_anchors(Tail);
%have_hard_anchors([ {'or', Tokens, Tokens1} | Tail ], _) -> 
%	(have_hard_anchors(Tokens) and have_hard_anchors(Tokens1)) orelse have_hard_anchors(Tail);
%have_hard_anchors([ {_, Tokens, Tokens1} | Tail ], _) -> 
%	(have_hard_anchors(Tokens) or have_hard_anchors(Tokens1)) orelse have_hard_anchors(Tail);
%have_hard_anchors(_, _) -> false. % }}}


%%% --------------------------------------------------------------------------------
%%% RE Executor
%%%

%%% re_apply(String, RE) -> match() | nomatch
%%% match() -> {match, Position, RemainingString, Group}
%%%

% dbg:tracer(). dbg:p(all, c). dbg:tpl(re, re_apply, [{'_',[], [{return_trace}]}]).
re_apply(String, RE) ->
	re_deep_matches(re_apply_list1(RE, String, 1, direct)).

re_deep_matches(nomatch) -> nomatch;
re_deep_matches({_, _, _, []} = M) -> M;
re_deep_matches({match, Start, Stop, Group}) ->
	{match, Start, Stop, re_deep_matches(tl(Group), [hd(Group)])}.

re_deep_matches([], Result) -> reverse(Result);
re_deep_matches([ H | Tail ], [ H1 | _ ] = Result) 
	when element(1, H) > element(1, H1) ->
	re_deep_matches(Tail, [ H | Result ]);

re_deep_matches(List, Result) ->
	re_deep_matches_back(List, Result).


re_deep_matches_back([{Start, Stop, Group} | Tail], [ H | Result ]) 
	when (element(1, H) >= Start) and (element(2, H) =< Stop) ->
	re_deep_matches_back([ {Start, Stop, [H|Group] } | Tail ], Result);
re_deep_matches_back([ H | Tail ], Result) ->
	re_deep_matches(Tail, [ H | Result ]).

	

%%% do lookup of string beginning, if pattern is not 
%%% anchored to begin ^
%%% passes execution to re_apply_list()
%re_apply_list1([ {hard, _, _, {const, Text}} | T ], String, 1, direct) -> % {{{
%	re_apply_list1( [ {const, Text} | T ], String, 1, direct);

re_apply_list1([ {const, Text} | T ] = All, String, Position, direct) ->
	case string:str(String, Text) of
	0 -> nomatch;
	N -> 
%		erlang:display({found, const, Text, pos, N}),
		TextLen = length(Text),
		% do forward search if we failed at first match
		SubStr = lists:nthtail(N, String),
		case re_apply_list(T, lists:nthtail(TextLen-1, SubStr), Position+N+TextLen-1, direct) of
		nomatch -> re_apply_list1(All, SubStr, Position+N-1, direct);
		Match -> match_start(Position+N-1, Match)
		end
	end;

%re_apply_list1([ {match, RE}| More ] = All, String, Position, Direction) ->
%	% use re_apply_list1 for fast searching of const if const is a first element
%	% of RE
%	case re_apply_list1(RE, String, Position, Direction) of
%	nomatch -> nomatch;
%	{match, StartPos, EndPos, Group} ->
%		_MatchLen = EndPos - StartPos, 
%%		erlang:display({match, Position, StartPos, len, EndPos-StartPos, EndPos, String}),
%		StringRemainder = lists:nthtail(EndPos-Position, String),
%%		erlang:display({remained, StringRemainder, More}),
%		case re_apply_list(More, StringRemainder, EndPos, Direction) of
%		nomatch -> 
%			% we found non complete first match, go on
%			% XXX: may be it's more reliable to go to StartPos + 1 instead of EndPos + 1?
%			re_apply_list1(All, lists:nthtail(StartPos-Position+1, String), StartPos+1, Direction);
%		{match, _Sta, End, Grp} ->
%%			erlang:display({match, Position, StartPos, EndPos-StartPos, Group, RE, String}),
%%			erlang:display({match, _Sta, End, Grp}),
%%			erlang:halt(),
%			{match, StartPos, End, [ {StartPos, EndPos, Group} | Grp ]}
%		end
%	end;

re_apply_list1([ {match, RE}| _ ] = All, String, Position, Direction) ->
	% use re_apply_list1 for fast searching of const if const is a first element
	% of RE
	% XXX: are there any other solution - without applying RE for test???
	case re_apply_list1(RE, String, Position, Direction) of
	nomatch -> nomatch;
	{match, StartPos, _, _} ->
		StringRemainder = lists:nthtail(StartPos-Position, String),
		case re_apply_list(All, StringRemainder, StartPos, Direction) of
		nomatch -> 
			% we found non complete first match, go on
			% XXX: may be it's more reliable to go to StartPos + 1 instead of EndPos + 1?
			re_apply_list1(All, tl(StringRemainder), StartPos+1, Direction);
		{match, _, _, _} = M -> M
		end
	end;
	

re_apply_list1([ {char_class, Class} | T ] = All, String, Position, direct) ->
	case lookup_char_class(Class, String, Position) of
	nomatch -> nomatch;
	{Tail, NewPosition} -> 
		case re_apply_list(T, Tail, NewPosition, direct) of
		nomatch -> re_apply_list1(All, Tail, NewPosition, direct);
		Match -> match_start(NewPosition-1, Match) % char_class match is only 1 symbol wide
		end
	end;

re_apply_list1([ {comp_class, Class} | T ] = All, String, Position, direct) ->
	case lookup_comp_class(Class, String, Position) of
	nomatch -> nomatch;
	{Tail, NewPosition} -> 
		case re_apply_list(T, Tail, NewPosition, direct) of
		nomatch -> re_apply_list1(All, Tail, NewPosition+1, direct);
		Match -> match_start(NewPosition-1, Match) % char_class match is only 1 symbol wide
		end
	end;

% XXX: optimization place - we can find fist hard anchor and
% then move back to find matching pattern
re_apply_list1([ {closure, _, _} | _ ] = All, String, Position, direct) ->
	case re_apply_list(All, String, Position, direct) of
	nomatch -> re_apply_list1(All, tl(String), Position+1, direct);
	Match -> match_start(Position, Match)
	end;
% we've reached end of string, no more search
re_apply_list1([ {Type, _} | _ ], [], _, direct)
	when (Type == kclosure) or (Type == kclosure_ng) ->
	nomatch;	
re_apply_list1([ {Type, _} | _ ] = All, String, Position, direct)
	when (Type == kclosure) or (Type == kclosure_ng) ->
	case re_apply_list(All, String, Position, direct) of
	nomatch -> re_apply_list1(All, tl(String), Position+1, direct);
	Match -> match_start(Position, Match)
	end;

re_apply_list1(L, String, Pos, Direction) ->
	re_apply_list(L, String, Pos, Direction). % }}}

%%% 
%%% Take care of splitting list of RE to head/tail
%%% 
%%% report match if we reached end of RE
re_apply_list([], _String, Pos, _Direction) -> {match, Pos, Pos, []}; % {{{
re_apply_list([[H]|T], String, Pos, Direction) -> % remove illegal wrap
	re_apply(H, T, String, Pos, Direction);
re_apply_list([H|T], String, Pos, Direction) ->
	re_apply(H, T, String, Pos, Direction). % }}}


%%%
%%% re_apply - recursively apply RE to String
%%% 
%%% -type re_apply(CurrentRegExp, RemainingRegExpsList, String, Position, Direction) -> match() | nomatch
%%%

% --------------------------------------------------------------------------------
% XXX: put EOS processing here (before empty string processing)
% kclosure also processed here - 0 match is also valid match for us

%%%
%%% shortcut path for . symbol (which match anything except \n)
%%%
re_apply(any_symbol, More, [C|String], Position, Direction) when C/= $\n -> 
	re_apply_list(More, String, Position+1, Direction);

% we reached EOS, but still have commands, 
% which can be zero width (kclosure || closure || eos)
% so strip them, and report match
re_apply({Type, _}, [], [], Pos, _Direction) when (Type == kclosure_ng) or (Type == kclosure) -> 
	{match, Pos, Pos, []};

re_apply({Type, _}, More, [], Pos, Direction) when (Type == kclosure_ng) or (Type == kclosure) -> 
	re_apply_list(More, [], Pos, Direction);

re_apply({closure, _, _}, [], [], Pos, _Direction) ->
	{match, Pos, Pos, []};

%%% we can have group finishing exactly at the string end
re_apply({endgroup, Pos}, More, [], Position, Direction) ->
	case re_apply_list(More, [], Position, Direction) of
	nomatch -> nomatch;
	{match, _StartPos, EndPos, Group} ->
		{match, Pos, EndPos, [ {Pos, Position, []} | Group ]}
	end;

re_apply({closure, _, _}, More, [], Pos, Direction) ->
	re_apply_list(More, [], Pos, Direction);

% we reached EOS, but still have commands, 
% which may not evaluate to zero width, report nomatch
re_apply(_RE, _More, [], _Pos, _Direction) -> nomatch;	

%%%
%%% symbol match clauses
re_apply({char_class, L}, More, [C|String], Position, direct) -> 
	case in_char_class(C, L) of
	true -> match_start(Position, re_apply_list(More, String, Position+1, direct));
	false -> nomatch
	end;
re_apply({comp_class, L}, More, [C|String], Position, direct) -> 
	case in_char_class(C, L) of
	false -> match_start(Position, re_apply_list(More, String, Position+1, direct));
	true -> nomatch
	end;

%%% OPTIMIZATION: this clause can be removed
re_apply({const, [C]}, More, [C|String], Position, Direction) -> 
	match_start(Position,  re_apply_list(More, String, Position + 1, Direction));
re_apply({const, [_]}, _, _, _, _) -> nomatch;

re_apply({const, [C, C1]}, More, [C, C1|String], Position, Direction) -> 
	match_start(Position,  re_apply_list(More, String, Position + 2, Direction));
re_apply({const, [_, _]}, _, _, _, _) -> nomatch;

% fast say no in some cases
re_apply({const, [C|_]}, _, [D|_], _, _) when C /= D -> 
	nomatch;
re_apply({const, [C, C1|_]}, _, [D, D1|_], _, _) when (C /= D) or (C1 /= D1) -> 
	nomatch;
%%% OPTIMIZATION END

re_apply({const, Text}, More, String, Position, Direction) -> 
	case split_prefix(Text, String) of
	{true, Tail, TextLen} -> 
%		TextLen = length(Text),					% just for test
%		Tail = lists:nthtail(TextLen, String),
		match_start(Position,  re_apply_list(More, Tail, Position + TextLen, Direction));
	false -> nomatch
	end;



%%%
%%% match BeginOfString only in position 1
%%%
re_apply(bos, More, String, 1, direct) -> 
	match_start(1, re_apply_list(More, String, 1, direct));


re_apply({match, RE}, More, String, Position, Direction) -> 
	re_apply_list(RE ++ [{endgroup, Position}] ++ More, String, Position, Direction);

re_apply({endgroup, Pos}, More, String, Position, Direction) ->
	case re_apply_list(More, String, Position, Direction) of
	nomatch -> nomatch;
	{match, _StartPos, EndPos, Group} ->
		{match, Pos, EndPos, [ {Pos, Position, []} | Group ]}
	end;

%%%
%%% kclosure_ng OPTIMIZATION
%%%
%%% check validity!!!
re_apply({kclosure_ng, [any_symbol]}, [ {const, Text} | _ ] = More, String, Position, Direction) -> 
	re_apply_try_kclosure_ng_const(Text, More, String, Position, Direction);
re_apply({kclosure_ng, [any_symbol]}, [ {endgroup, _}, {const, Text} | _ ] = More, String, Position, Direction) -> 
	re_apply_try_kclosure_ng_const(Text, More, String, Position, Direction);
re_apply({kclosure_ng, [any_symbol]}, [ {match, [{const, Text}, _]} | _ ] = More, String, Position, Direction) -> 
	re_apply_try_kclosure_ng_const(Text, More, String, Position, Direction);
% {kclosure_ng, [{char_class ... or {kclosure_ng, [{comp_class or {kclosure_ng, [ {const should be optimized in a same way
%%% OPTIMIZATION END

%%%
%%% quantifier clauses
re_apply({kclosure, RE}, More, String, Position, Direction) -> 
	re_apply_try_kclosure(RE, More, String, Position, Direction);
re_apply({kclosure_ng, RE}, More, String, Position, Direction) -> 
	re_apply_try_kclosure_ng(RE, More, String, Position, Direction);
re_apply({closure, {0, Max}, RE}, More, String, Position, Direction) -> 
	re_apply_try_re(Max, RE, More, String, Position, Direction).	% try apply RE 0 to Max times


%% customized version of lists:prefix to avoid cutting String twice
%% split_prefix(split_prefix, List) -> (true | false)
%%
split_prefix(X, Y) ->
	split_prefix(X, Y, 0). 
	
split_prefix([X|PreTail], [X|Tail], SubLen) -> % {{{
    split_prefix(PreTail, Tail, SubLen+1);
split_prefix([], List, SubLen) -> {true, List, SubLen};
split_prefix(_, _, _) -> false. % }}}

% it's a GREEDY function, we will try from Max to 0
% try apply RE Max to 0 times
%
% XXX: optimiziation idea - first of all test it 1 time,
% if it fail - then we will fail apply it Max times too
% in worst case {0,2} it will increase checks by 4/3, 
% but in most cases it will speedup {} processing a lot
re_apply_try_re(Max, RE, More, String, Position, Direction) -> % {{{
	re_apply_try_re(lists:duplicate(Max, RE), More, String, Position, Direction).

re_apply_try_re([], More, String, Position, Direction) ->
	re_apply_list(More, String, Position, Direction);
re_apply_try_re(REList, More, String, Position, Direction) ->
	case re_apply_list(REList ++ More, String, Position, Direction) of
	nomatch -> re_apply_try_re(tl(REList), More, String, Position, Direction); % apply one less RE
	{match, _, _, _} = M -> M
	end. % }}}

%%%
%%% apply RE (in GREEDY fasion :) as much as we can, then try More regexp 
%%% if not matched - go one step ack and try again and so on
re_apply_try_kclosure(RE, More, String, Position, Direction) -> % {{{
	case re_apply_list(RE, String, Position, Direction) of
	nomatch -> re_apply_list(More, String, Position, Direction);	% we cannot expand kclosure RE here, return result of More regexp
	{match, StartPos, StartPos, _} -> 	% one char match processing
		case re_apply_try_kclosure(RE, More, tl(String), Position + 1, Direction) of
		nomatch -> re_apply_list(More, String, Position, Direction);
		{match, _, _, _} = M -> M
		end;
	{match, StartPos, EndPos, _} -> % we are GREEDY, try expand as much as possible
		Len = EndPos - StartPos,
		case re_apply_try_kclosure(RE, More, lists:nthtail(Len, String), Position + Len, Direction) of
		nomatch -> re_apply_list(More, String, Position, Direction);
		{match, _, _, _} = M -> M
		end
	end. % }}}

%%%
%%% apply RE in non-greedy manner
%%% try apply More regexp, if not matched, apply one RE regexp 
%%% and try again, stop at first match
re_apply_try_kclosure_ng(RE, More, String, Position, Direction) -> % {{{
	case re_apply_list(More, String, Position, Direction) of
	nomatch -> % we should expand kclosure now
		case re_apply_list(RE, String, Position, Direction) of
		nomatch -> nomatch;	% we cannot expand it here
		{match, StartPos, StartPos, _} -> 	
			re_apply_try_kclosure_ng(RE, More, tl(String), Position + 1, Direction);
		{match, StartPos, EndPos, _} -> 	
			Len = EndPos - StartPos,
			re_apply_try_kclosure_ng(RE, More, lists:nthtail(Len, String), Position + Len, Direction)
		end;
	{match, _, _, _} = M -> M
	end. % }}}

%%%
%%% optimize .*?text patterns
%%% XXX: write tests for IT!!!!
re_apply_try_kclosure_ng_const(Text, More, String, Position, Direction) -> % {{{
%	erlang:display({kclosure_ng, Text, More, string:str(String, Text) }),
	case string:str(String, Text) of
	0 -> nomatch;
	N when length(Text) == 1 -> 
		TextAfter = lists:nthtail(N-1, String),
%		erlang:display({matched, lists:sublist(String, 20), Text, {lists:sublist(TextAfter, 20)}, N, More}),
		case re_apply_list(More, TextAfter, Position+N-1, Direction) of
		nomatch -> % we should lookup next text match now
			% XXX: text this position bug with 2 character strings too! 
			re_apply_try_kclosure_ng_const(Text, More, tl(TextAfter), Position+N, Direction);
		Match -> match_start(Position+N-1, Match)
		end;
	N -> % we found text, now prove, that remaining RE will also match
		TextAfter = lists:nthtail(N-1, String),
%		erlang:display({matchedN, lists:sublist(String, 20), Text, {lists:sublist(TextAfter, 20)}, N, More}),
		case re_apply_list(More, TextAfter, Position+N-1, Direction) of
		nomatch -> % we should lookup next text match now
			re_apply_try_kclosure_ng_const(Text, More, tl(TextAfter), Position+N, Direction);
		Match -> match_start(Position+N-1, Match)
		end

	end. % }}}

%%% do arithmerics on match tuples
match_start(_, nomatch) -> nomatch; % {{{
match_start(N, {match, _StartPos, StopPos, Groups}) -> {match, N, StopPos, Groups}. % }}}
%match_start(_, nomatch, _) -> nomatch; % {{{
%match_start(N, {match, _StartPos, StopPos, Groups}, EndOffset) -> {match, N, StopPos+EndOffset, Groups}. % }}}
%match_end(_, nomatch) -> nomatch; % {{{
%match_end(N, {match, StartPos, StopPos, Groups}) -> {match, StartPos, StopPos+N, Groups}. % }}}

%%% -type in_char_class(Char, Class) -> true | false
%%% decide if Char is in Class character class
%%% Char -> char()
%%% Class -> char() | list() | tuple()
in_char_class(C, C) -> true; % check one-symbol char class % {{{
in_char_class(_, N) when is_integer(N) -> false;
% check in list presense
in_char_class(C, {Low, Hi}) when (C >= Low) and (C =< Hi) -> true;
in_char_class(_, {_, _}) -> false;
in_char_class(C, [{Low, Hi}]) when (C >= Low) and (C =< Hi) -> true;
in_char_class(_, [{_, _}]) -> false;
in_char_class(C, L) when is_list(L) -> lists:member(C, L);
% XXX: because of limited size of tuple we will have problems with unicode chars
in_char_class(C, T) when is_tuple(T) -> element(C+1, T). % }}}

%%% span over string until we CAN _NOT_ match this class
%%% stop at first occurence
lookup_char_class(_, [], _) -> nomatch; % {{{
lookup_char_class(Class, [C|Tail], Position) ->
	case in_char_class(C, Class) of
	true -> {Tail, Position+1};
	false -> lookup_char_class(Class, Tail, Position+1)
	end.  % }}}
lookup_comp_class(_, [], _) -> nomatch; % {{{
lookup_comp_class(Class, [C|Tail], Position) ->
	case in_char_class(C, Class) of
	false -> {Tail, Position+1};	% inverse logic! 
	true -> lookup_comp_class(Class, Tail, Position+1)
	end.  % }}}


%%%
%%% Compiler ????
%%% 

%start_with_token({concat, X, _}) ->  % {{{
%	start_with_token(X);
%start_with_token({const, _} = X) -> X;
%start_with_token(I) when is_integer(I) -> {const, [ I ]};
%start_with_token({X, _}) -> start_with_token(X);
%start_with_token(_) -> regexp. % }}}
%end_with_token({X, _}) ->  % {{{
%	end_with_token1(X).
%
%end_with_token1({concat, _, X}) -> 
%	end_with_token1(X);
%end_with_token1({const, _} = X) -> X;
%end_with_token1(I) when is_integer(I) -> {const, [ I ]};
%end_with_token1({_, X}) -> end_with_token1(X);
%end_with_token1(_) -> regexp. % }}}
%
%compile(RegExp) -> % {{{
%	StartToken = start_with_token(RegExp),
%	EndToken = end_with_token(RegExp),
%	erlang:display({start_with, StartToken, end_with, EndToken}).
%%	compile(element(1, RegExp), StartToken, EndToken, []). 
%
%
%-record(re_state, {begin_pos, end_pos}).
%
%compile(RegExp, {const, XStr}, EndToken, []) ->
%	F = fun(_State, Str) ->
%		case string:str(Str, XStr) of
%		0 -> nomatch;
%		N -> {replace, string:sub_string(Str, N)}
%		end
%	end,
%	
%	compile(RegExp, none, EndToken, [ F ]);
%
%compile(RegExp, BeginToken, {const, XStr}, Stack) ->
%	F = fun(_State, Str) ->
%		case string:rstr(Str, XStr) of
%		0 -> nomatch;
%		N -> {replace, string:sub_string(Str, 1, N + length(XStr) - 1)}
%		end
%	end,
%	_N = #re_state{},
%	compile(RegExp, BeginToken, none, [ F | Stack ]);
%	
%compile(RegExp, StartWith, EndWith, Stack) when is_atom(StartWith) and is_atom(EndWith) -> 
%	lists:reverse([ compile1(RegExp) | Stack]).
%
%%compile1({const, XStr}) ->
%%	fun (_, Str) -> 
%%	case string:sub_string(Str,  of
%%	r ++ _ -> matched;
%%	_ -> nomatch
%%	end
%%	end;
%
%compile1({concat, X, Y}) ->
%	XC = compile1(X),
%	YC = compile1(Y),
%
%	F = fun (State, Str) ->
%		case XC(State, Str) of
%		nomatch -> nomatch;
%		A -> erlang:display({concat, re, some, A}),
%			case YC(State, Str) of
%			nomatch -> nomatch;
%			B -> erlang:display({concat, re, some1, B}),
%				 B
%			end
%		end
%	end,
%	F;
%compile1(RegExp) ->
%	erlang:display({compile, RegExp}),
%	fun(_, _) -> nomatch end.
%
%
%%re_apply(Str, RE) ->
%%	re_apply(Str, RE, #re_state{}).
%%
%%re_apply(Str, [ ], _State ) -> Str;
%%re_apply(Str, [ H | RE ], State ) ->
%%	case H(State, Str) of
%%	nomatch -> nomatch;
%%	{replace, Str1} -> re_apply(Str1, RE, State)
%%	end. % }}}


