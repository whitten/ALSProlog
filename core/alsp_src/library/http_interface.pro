
module pxml.
export fetch_url_raw/2.
export fetch_page/4.
export build_http_request/2.
export process_raw_page/3.
export process_raw_page/2.
export skip_http_header_lines/3.
export skip_http_header_lines/2.
export getResponseStatus/2.

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
fetch_url_raw(RequestDescription, RawResponse)
        :-
        build_http_request(RequestDescription, HTTP_Request),
        dmember(host=Host, RequestDescription),
        (dmember(port=Port, RequestDescription) -> true ; default_port(Port)),
        (dmember(timeout=Timeout, RequestDescription) -> true ; default_timeout(Timeout)),
	(dmember(verbose=Verbosity, RequestDescription) -> true ; default_verbosity(Verbosity)),

        nsocket(internet, stream, 0, Socket),
        open(nsocket(Socket), read, RS, []),
        open(nsocket(Socket), write, WS, []),
        nsocket_connect(Socket, Host, Port),
		%% when using over MiFi, I (sometimes) need a pause here; not needed when using satellite:
		dopause,
        (Verbosity==true -> 
		printf(user,'%t\nHost: %t\nUser-Agent: alspro\nAccept: */*\n\n', [HTTP_Request, Host])
		; true),
        unwind_protect(
		(
		printf(WS,'%t\nHost: %t\nUser-Agent: alspro\nAccept: */*\n\n', [HTTP_Request, Host]),
        	flush_output(WS)
		),
		close(WS)
	),
		%% IS THIS NEEDED?::
%       nsocket_select([RS], [], [], [Mark], _, _, Timeout:0),
%       Mark = set,
        !,
        get_lines(RS, RawResponse),
	close(RS),
	nsocket_close(Socket),
        (dmember(headers=ShowHeaders, RequestDescription) -> true ; default_headers(ShowHeaders)),
	(ShowHeaders=true -> 
        	skip_http_header_lines(RawResponse, _, HLs),
		write(user, 'Headers:'),nl(user),
		write_lines(user, HLs), nl(user)
		; true ).

fetch_page(RequestDescription, Response, HeaderLines, Status)
        :-
	fetch_url_raw(RequestDescription, InitRawResponse),
	InitRawResponse = [InitStatusLine | _],
	!,
	getResponseStatus(InitStatusLine, InitStatus),
	dispatch_status(InitStatus, RequestDescription, InitRawResponse, Response, HeaderLines, Status).

dispatch_status(InitStatus, InitRequestDescription, InitRawResponse, Response, HeaderLines, InitStatus)
	:-
	200 =< InitStatus, InitStatus < 300,
	!,
    	skip_http_header_lines(InitRawResponse, Response, HeaderLines).

dispatch_status(InitStatus, InitRequestDescription, InitRawResponse, Response, HeaderLines, Status)
	:-
	300 =< InitStatus, InitStatus < 400,
        dmember(location=follow, InitRequestDescription),
	!,
        dmember(host=InitHost, InitRequestDescription),
	redirect_target(InitHost, InitRawResponse, NextTarget),
	list_delete(InitRequestDescription, host=_, HostlessRequestDescription),
	NextRequestDescription = [host = NextTarget | HostlessRequestDescription],

	fetch_page(NextRequestDescription, Response, HeaderLines, Status).
	
dispatch_status(InitStatus, InitRequestDescription, InitRawResponse, Response, HeaderLines, InitStatus)
	:-
	300 =< InitStatus, InitStatus < 400,
	!,
    	skip_http_header_lines(InitRawResponse, Response, HeaderLines).

redirect_target(Host, InitRawResponse, NextTarget)
	:-
	find_location(InitRawResponse, RD),
	(sub_atom(RD, 0, Length, After, 'https://'), sub_atom(RD, Length, _, 0, NT), !;
	    sub_atom(RD, 0, Length, After, 'http://'), sub_atom(RD, Length, _, 0, NT) ),

	(sub_atom(NT, LL, 1, 0, '/') -> sub_atom(NT, 0, LL, 1, NextTarget) ; NextTarget = NT).

redirect_target(Host, _, _)
	:-
	sprintf(atom(ErrMsg), 'Redirect failure: %t\n', [Host]),
	Ball =.. [http_error, ErrMsg],
	throw(Ball).

find_location([], _)
	:-!,
	fail.

find_location([Line | Lines], RedirectTarget)
	:-
	sub_atom(Line, 0, Length, After, 'Location: '),
	!,
	sub_atom(Line, Length, _, 0, RedirectTarget).
	
find_location([Line | Lines], RedirectTarget)
	:-
	find_location(Lines, RedirectTarget).

build_http_request(RequestDescription, HTTP_Request)
        :-
        (dmember(method=Method, RequestDescription) -> true ; default_method(Method)),
        dmember(docpath=DocPath, RequestDescription),
        http_level(HTTPLev),
        sprintf(atom(HTTP_Request), '%t %t %t',[Method,DocPath,HTTPLev]).


%http_level('HTTP/1.0').

http_level('HTTP/1.1').

default_method('GET').
default_port(80).
default_timeout(300).  %% time in secs
default_verbosity(false).
default_headers(false).

process_raw_page(RawResponse, PXMLTerms)
    :-
    process_raw_page(RawResponse, PXMLTerms, _).

process_raw_page(RawResponse, PXMLTerms, HeaderLines)
    :-
    skip_http_header_lines(RawResponse, RawLines, HeaderLines),
    read_tokens_lines(RawLines, HTMLPageTokens),
    parse_pxml(HTMLPageTokens, PXMLTerms),
    !.

process_raw_page(RawResponse, PXMLTerms)
        :-
        write('parse_pxml FAILED'),nl.

skip_http_header_lines([], [], []).
skip_http_header_lines(['' | Ls], Ls, [])
        :-!.
skip_http_header_lines([L | Ls], Result, [L | HLs])
        :-
        skip_http_header_lines(Ls, Result, HLs).

getResponseStatus(StatusLine, Status)
    :-
    asplit(StatusLine, 0' , Left, Right),
    asplit(Right, 0' , StatusAtom, _),
    atom_chars(StatusAtom, NumberChars),
    number_chars(Status, NumberChars).

dopause :- dp(100000).
dp(0) :-!.
dp(N)
    :- 
    M is N - 1,
    K is 4937596748/48576,
    dp(M).

endmod.
