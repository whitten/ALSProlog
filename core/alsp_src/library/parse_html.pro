/*=================================================================*
 |			parse_html.pro
 |		Copyright (c) 1999-2004 Applied Logic Systems, Inc.
 |	
 |		Parse tokenized html into pxml prolog terms
 |		  -- assumes the html was originally cleaned up by HTMLTidy
 |
 |	Authors: Ken Bowen & Chuck Houpt
 |
 |	This software is copyrighted by Applied Logic Systems, Inc.,
 |	and other parties.  The following terms apply to all files 
 |	associated with the software unless explicitly disclaimed in 
 |	individual files.
 |
 |	The authors hereby grant permission to use, copy, modify, distribute,
 |	and license this software and its documentation for any purpose, provided
 |	that existing copyright notices are retained in all copies and that this
 |	notice is included verbatim in any distributions. No written agreement,
 |	license, or royalty fee is required for any of the authorized uses.
 |	Modifications to this software may be copyrighted by their authors
 |	and need not follow the licensing terms described here, provided that
 |	the new terms are clearly indicated on the first page of each file where
 |	they apply.
 |	
 |	IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
 |	FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 |	ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
 |	DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
 |	POSSIBILITY OF SUCH DAMAGE.
 |	
 |	THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
 |	INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
 |	FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
 |	IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
 |	NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 |	MODIFICATIONS.
 *=================================================================*/

module pxml.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Parser:  HTML --> PXML
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export grab_pxml/2.
export parse_pxml/2.
export read_pxml_term/3.
export read_pxml_comment/3.

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
grab_pxml(Path, PXML)
    :-
/*
    grab_lines(Path, RawLines),
    cut_cmts_js(RawLines, ScriptlessLines),
    read_tokens_lines(ScriptlessLines, Tokens),
    parse_pxml(Tokens, PXML).
*/
    grab_html_tokens(Path, Tokens),
    parse_pxml(Tokens, PXML).


/*
export grab_pxml/2.
grab_pxml(Path, PXML)
	:-
	open(Path, read, IS, [write_eoln_type(lf)]),
	unwind_protect(r_pxml(IS, PXML), 
	close(IS)).

export r_pxml/2.
r_pxml(S, L)
	:-
	read_tokens(S, Tokens),
	parse_pxml(Tokens, L).
*/


	%%------------------------------------------
	%% Parse the token stream
        %%  -- assumes the html was originally 
	%%     cleaned up by HTMLTidy; in particular,
	%%     <p> is a binary tag, so it must always
	%%     be matched by </p>
	%%------------------------------------------
/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
parse_pxml([], []).
parse_pxml(Tokens, [Term | RestTerms])
	:-
	read_pxml_term(Tokens, Term, RestTokens),
	parse_pxml(RestTokens, RestTerms).

read_pxml_term([string(StringAtom) | RestTokens], StringAtom, RestTokens)
	:-!.

read_pxml_term(['<', InTag,'>','<', InTag,'>' | Tokens], Term, RestTokens)
	:-
	make_lc_sym(InTag, html),
	!,
	read_pxml_term(['<', InTag,'>' | Tokens], Term, RestTokens).

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
read_pxml_term(['<', InTag | Tokens], Term, ['<' | RestTokens])
	:-
	make_lc_sym(InTag, Tag),
	Tag = meta,
	!,
	read_pxml_terms_to(Tokens, '<', Features, RestTokens),
	Term  =.. [Tag, Features, []].

read_pxml_term(['<', InTag | Tokens], Term, ['<' | RestTokens])
	:-
	make_lc_sym(InTag, Tag),
	Tag = style,
	parse_style,
	!,
	read_pxml_eqs_to(Tokens, '>', Features, InterTokens),
	read_style_terms_to(InterTokens, Styles, RestTokens),
	Term  =.. [style, Features, Styles].




read_pxml_term(['<', InTag | Tokens], Term, RestTokens)
	:-
	make_lc_sym(InTag, Tag),
	unary_tag(Tag),
	!,
	read_pxml_eqs_to(Tokens, '>', Features, RestTokens),
	Term  =.. [Tag, Features, []].

read_pxml_term(['<', '!--' | Tokens], Term, RestTokens)
	:-
	read_pxml_comment(Tokens, Features, RestTokens),
	Term  =.. [comment, Features, []].

read_pxml_term(['<', InTag | Tokens], Term, RestTokens)
	:-!,
	make_lc_sym(InTag, Tag),
	read_pxml_eqs_to(Tokens, '>', Features, InterTokens),
	read_to_close_html(InterTokens, SubTerms, Tag, RestTokens),
	Term  =.. [Tag, Features, SubTerms].

read_pxml_term(Tokens, List, ['<' | RestTokens])
	:-
	read_pxml_terms_to(Tokens, '<', List, RestTokens).

read_pxml_terms_to([], Terminator, [], []).
read_pxml_terms_to([Terminator | Tokens], Terminator, [], Tokens)
	:-!.
read_pxml_terms_to(['/','>' | Tokens], _, [], Tokens)
	:-!.
read_pxml_terms_to([T0 | Tokens], Terminator, [T0 | Terms], RestTokens)
	:-
	read_pxml_terms_to(Tokens, Terminator, Terms, RestTokens).

read_pxml_eqs_to([], Terminator, [], []).
read_pxml_eqs_to([Terminator | Tokens], Terminator, [], Tokens)
	:-!.
read_pxml_eqs_to(['/','>' | Tokens], _, [], ['/','>' | Tokens])
	:-!.
read_pxml_eqs_to(['<' | Tokens], '>', [], ['<' | Tokens])
	:-!.
read_pxml_eqs_to([T0, '=', T1 | Tokens], Terminator, 
				 [(Tag = Value) | Terms], RestTokens)
	:-!,
	make_lc_sym(T0, Tag),
	read_tag_value(T1, Tokens, Value, InterTokens),
	read_pxml_eqs_to(InterTokens, Terminator, Terms, RestTokens).
read_pxml_eqs_to([T0 | Tokens], Terminator, [T0 | Terms], RestTokens)
	:-
	read_pxml_eqs_to(Tokens, Terminator, Terms, RestTokens).

read_tag_value('"', Tokens, Value, InterTokens)
	:-
	consume_tokens_to_q2(Tokens, Head, InterTokens),
	catenate(Head, Value0),
	unescape_quotes(Value0, Value).
read_tag_value(Value, Tokens, Value, Tokens).

consume_tokens_to_q2([], Head, []) :-!, fail.
%consume_tokens_to_q2(['"' | InterTokens], [Tok], InterTokens)
%	:-!.
consume_tokens_to_q2(['"' | InterTokens], ['""'], InterTokens)
	:-!.
consume_tokens_to_q2([Item | Tokens], [Tok], InterTokens)
	:-
	sub_atom(Item, Bef,1,Aft, '"'),
	!,
	sub_atom(Item,0,Bef,_,Tok),
	(Aft = 0 ->
		InterTokens = Tokens
		;
		B1 is Bef+1,
		sub_atom(Item,B1,_,0,RTok),
		InterTokens = [RTok | Tokens]
	).


consume_tokens_to_q2([Item | Tokens], [Item | Head], InterTokens)
	:-
	consume_tokens_to_q2(Tokens, Head, InterTokens).

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
read_pxml_comment([], [], []).
read_pxml_comment(['--','>' | Tokens], [], Tokens)
	:-!.
read_pxml_comment(['!--','>' | Tokens], [], Tokens)
	:-!.
read_pxml_comment(['//--','>' | Tokens], [], Tokens)
	:-!.
read_pxml_comment([Token | Tokens], [Token | Features], RestTokens)
	:-
	read_pxml_comment(Tokens, Features, RestTokens).


/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
read_to_close_html([], [], _, []).
read_to_close_html(['<','=' | Tokens], [], Tag, Tokens)
	:-!,
	read_to_close_html(Tokens, [], Tag, Tokens).

read_to_close_html(['/','>' | Tokens], [], Tag, Tokens)
	:-!.
read_to_close_html(['/','>' | Tokens], [], Tag, Tokens)
	:-!.
read_to_close_html(['<','/',InTag0,'>','<','/',InTag1,'>' | Tokens], 
                       [], Tag, Tokens)
	:-
	Tag=font,
	make_lc_sym(InTag0, Tag),
	make_lc_sym(InTag1, Tag),
	!.
read_to_close_html(['<','/',InTag0,'>','<','/',InTag,'>' | Tokens], 
                       [], Tag, Tokens)
	:-
	make_lc_sym(InTag0, font),
	member(Tag,[td,tr,table]),
	make_lc_sym(InTag, Tag),
	!.

read_to_close_html(['<','/',InTag,'>' | Tokens], [], Tag, Tokens)
	:-
	make_lc_sym(InTag, Tag),
	!.
read_to_close_html(['<',InTag,'>' | Tokens], [], Tag, 
					['<',ContainingTag,'>' | Tokens])
	:-
	make_lc_sym(InTag, ContainingTag),
	start_can_terminate(ContainingTag, Tag),
	!.


read_to_close_html(['<','/',InTag,'>' | Tokens], [], Tag, 
					['<','/',ContainingTag,'>' | Tokens])
	:-
	make_lc_sym(InTag, ContainingTag),
	end_can_terminate(ContainingTag, Tag),
	!.

%% Text appearance tag which has been close by earlier // structure:  a, b, /a, /b
read_to_close_html(['<','/',InTag,'>' | InterTokens], [], CurTag, Tokens)
	:-
	make_lc_sym(InTag, LCInTag),
	text_appearance_tag(LCInTag),
	end_can_terminate(CurTag, LCInTag),
	!,
	read_to_close_html(InterTokens, [], CurTag, Tokens).

	%% on Yahoo, sometimes:
read_to_close_html(['<','/',InTag,',','>' | Tokens], [], Tag, Tokens)
	:-
	dmember(Tag, [td,a]),
	make_lc_sym(InTag, Tag).

read_to_close_html(Tokens, [Term | SubTerms], Tag, RestTokens)
	:-
	read_pxml_term(Tokens, Term, InterTokens),
	read_to_close_html(InterTokens, SubTerms, Tag, RestTokens).

/*---------------------------------------------------------------------
	Handle <style>
 *--------------------------------------------------------------------*/
	%% Option to control whether <style> is parsed:
export do_parse_style/0.
export no_parse_style/0.
do_parse_style
	:-
	parse_style, !.
do_parse_style
	:-
	assert(parse_style).
no_parse_style
	:-
	parse_style, 
	!,
	retract(parse_style).
no_parse_style.

:-dynamic(parse_style/0).
%parse_style.

read_style_terms_to(['<', '/', style, '>' |  RestTokens], [], RestTokens)
	:-!.

read_style_terms_to([=,'@media' | Tokens], [MediaEntry], RestTokens)
	:-!,
	read_style_body_entries_to([=,'@media' | Tokens], '}', MediaEntry, InterTokens),
	read_to_close_html(InterTokens, SubTerms, Tag, RestTokens).

read_style_terms_to(Tokens, [Style | Styles], RestTokens)
	:-
	read_style_term(Tokens, Style, InterTokens),
	read_style_terms_to(InterTokens, Styles, RestTokens).

read_style_term([= | Tokens], Style, RestTokens)
	:-
	read_style_term(Tokens, Style, RestTokens).

read_style_term(Tokens, Style, RestTokens)
	:-
	read_style_heads_list(Tokens, StyleHeads, InterTokens),		% will consume {
	read_style_body_list(InterTokens, StyleBodyList, RestTokens),	% will consume }
	Style = style(StyleHeads, StyleBodyList).
	
read_style_heads_list(Tokens, StyleHeads, RestTokens)
	:-
	read_pxml_terms_to(Tokens, '{', StyleHeads, RestTokens).

read_style_body_list(Tokens, StyleBodyList, RestTokens)	% will consume }
	:-
	read_style_body_entries_to(Tokens, '}', StyleBodyList, RestTokens).


read_media_kind(['(' | Tokens], [], Tokens)
	:-!.

read_media_kind([Token | InterTokens], [Token | RestMediaKinds], RestTokens)
	:-
	read_media_kind(InterTokens, RestMediaKinds, RestTokens).

read_media_size([')', '{' | RestTokens], [], RestTokens)		% consumes ) and {
	:-!.

read_media_size([Tag, ':', Value | InterTokens], [size(SizeTag, Value) | RestMediaSize],  RestTokens)		% consumes ) and {
	:-
	catenate(Tag, ':', SizeTag),
	read_media_size(InterTokens, RestMediaSize, RestTokens).

read_style_body_entries_to(['}' | RestTokens], '}', [], RestTokens)
	:-!.

read_style_body_entries_to([=,'}' | RestTokens], '}', [], RestTokens)
	:-!.

read_style_body_entries_to([=,'@media' | InterTokens], '}', MediaEntry, RestTokens)
	:-
	read_media_kind(InterTokens, MediaKinds, InterTokens2),		% consumes (
	read_media_size(InterTokens2, MediaSize, InterTokens3),		% consumes ) and {
	read_style_body_entries_to(InterTokens3, '}', StyleBodyList, RestTokens),
	MediaEntry = mediaEntry(MediaKinds, MediaSize, StyleBodyList).

read_style_body_entries_to([=,Tag,':' | InterTokens], '}', [Entry | StyleBodyList], RestTokens)
	:-
	read_style_tag_value_list(InterTokens, ValueList, InterTokens2),
	Entry = style_entry(Tag, ValueList),
	read_style_body_entries_to(InterTokens2, '}', StyleBodyList, RestTokens).

read_style_body_entries_to([=,Tag,'{' | InterTokens], '}', [Entry | Entries], RestTokens)
	:-
	read_style_body_entries_to(InterTokens, '}', Styles, InterTokens2),
	Entry =.. [Tag, [], Styles],
	read_style_body_entries_to(InterTokens2, '}', Entries, RestTokens).

read_style_tag_value_list([= | Tokens], [], [= | Tokens])
	:-!.

read_style_tag_value_list([Token | InterTokens], [Token | RestValueList], RestTokens)
	:-
	read_style_tag_value_list(InterTokens, RestValueList, RestTokens).


	%%------------------------------------------
	%% Syntactic roles of tags:
	%%------------------------------------------
unary_tag(hr).
unary_tag(br).
%unary_tag(p).
unary_tag('!doctype').
unary_tag(meta).
unary_tag(img).
unary_tag(input).
unary_tag(frame).
unary_tag(link).
%unary_tag(option).

%unary_tag(style).

containing_tag(html, X).
containing_tag(body, X)
	:-
	X \= html.
containing_tag(head, X)
	:-
	X \= html,
	X \= body.
containing_tag(table, X)
	:-
	table_tag(X).
containing_tag(table, X)
	:-
	text_appearance_tag(X).
containing_tag(form, X)
	:-
	table_tag(X).
containing_tag(form, X)
	:-
	text_appearance_tag(X).

containing_tag(tr, td).
containing_tag(tr, X)
	:-
	text_appearance_tag(X).
containing_tag(td, X)
	:-
	text_appearance_tag(X).

table_tag(tr).
table_tag(td).

text_appearance_tag(font).
text_appearance_tag(b).
text_appearance_tag(i).

start_can_terminate(td, td).
start_can_terminate(tr, td).
start_can_terminate(tr, tr).

end_can_terminate(tr, td).
end_can_terminate(table, td).
end_can_terminate(form, td).
end_can_terminate(table, tr).
end_can_terminate(form, tr).

end_can_terminate(table, center).
end_can_terminate(tr, center).
end_can_terminate(td, center).

end_can_terminate(table, font).
end_can_terminate(tr, font).
end_can_terminate(td, font).

end_can_terminate(map, area).

end_can_terminate(style, head).

end_can_terminate(td, X) :- text_appearance_tag(X).
end_can_terminate(tr, X) :- text_appearance_tag(X).
end_can_terminate(table, X) :- text_appearance_tag(X).
end_can_terminate(a, X) :- text_appearance_tag(X).

end_can_terminate(font, T2) 
	:-
	text_appearance_tag(T2).

end_can_terminate(table, span).
end_can_terminate(tr, span).
end_can_terminate(td, span).

end_can_terminate(body, X) :-
	X \= head.
end_can_terminate(html, _).

start_can_terminate(dd, dt).
start_can_terminate(dt, dd).
end_can_terminate(dl, dd).

start_can_terminate(li, li).
start_can_terminate(ul, li).
start_can_terminate(ol, li).
end_can_terminate(ul, li).
end_can_terminate(ol, li).

endmod.

