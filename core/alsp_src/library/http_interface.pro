
module pxml.
export fetch_url/2.
export fetch_url_raw/2.
export fetch_url/4.
export ffetch_url/4.
export build_http_request/2.
export process_raw_page/3.
export process_raw_page/2.
export skip_http_header_lines/3.
export skip_http_header_lines/2.
export getResponseStatus/2.

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
fetch_url(RequestDescription, Response)
        :-
	fetch_url(RequestDescription, Response, _, _).

fetch_url(RequestDescription, Response, HeaderLines, Status)
        :-
	fetch_url_raw(RequestDescription, RawResponse),
    	skip_http_header_lines(RawResponse, Response, HeaderLines),
	HeaderLines = [StatusLine | _],
	getResponseStatus(StatusLine, Status).

fetch_url_raw(RequestDescription, RawResponse)
        :-
        build_http_request(RequestDescription, HTTP_Request),
        dmember(host=Host, RequestDescription),
        (dmember(port=Port, RequestDescription) -> true ; Port = 80),
        (dmember(timeout=Timeout, RequestDescription) -> true ; default_timeout(Timeout)),
        nsocket(internet, stream, 0, Socket),
        open(nsocket(Socket), read, RS, []),
        open(nsocket(Socket), write, WS, []),
        nsocket_connect(Socket, Host, Port),
%% when using over MiFi, I (sometimes) need a pause here; not needed when using satellite:
dopause,
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
	nsocket_close(Socket).

	%% Tries to follow 301s:
ffetch_url(RequestDescription, Response, HeaderLines, Status)
        :-
	fetch_url_raw(RequestDescription, InitRawResponse),
	InitRawResponse = [InitStatusLine | _],
	getResponseStatus(InitStatusLine, InitStatus),
	dispatch_ffetch_url(InitStatus, RequestDescription, InitRawResponse, Response, HeaderLines, Status).
		
dispatch_ffetch_url(InitStatus, InitRequestDescription, InitRawResponse, Response, HeaderLines, Status)
	:-
	300 =< InitStatus, InitStatus < 400,
	!,
        (dmember(method=Method, InitRequestDescription) -> true ; Method = 'GET'),
        dmember(docpath=DocPath, InitRequestDescription),
        dmember(host=InitHost, InitRequestDescription),
        (dmember(port=Port, InitRequestDescription) -> true ; Port = 80),
        (dmember(timeout=Timeout, InitRequestDescription) -> true ; default_timeout(Timeout)),

	    %% Do we only strip 'www.', or any prefix (including
	    %% multiple dots) before the core url?
	SubAtom = 'www.',
	sub_atom(InitHost,0,Length,After,SubAtom),
	!,
	sub_atom(InitHost, Length, _, 0, Host),
	RequestDesc = [method=Method,docpath=DocPath,host=Host,port=Port,timeout=Timeout],
	fetch_url_raw(RequestDesc, RawResponse),
    	skip_http_header_lines(RawResponse, Response, HeaderLines),
	HeaderLines = [StatusLine | _],
	getResponseStatus(StatusLine, Status).
	
dispatch_ffetch_url(Status, RequestDescription,  InitRawResponse, Response, HeaderLines, Status)
	:-
    	skip_http_header_lines(InitRawResponse, Response, HeaderLines),
	HeaderLines = [StatusLine | _],
	getResponseStatus(StatusLine, Status).



build_http_request(RequestDescription, HTTP_Request)
        :-
        (dmember(method=Method, RequestDescription) -> true ; Method = 'GET'),
        dmember(docpath=DocPath, RequestDescription),
        http_level(HTTPLev),
        sprintf(atom(HTTP_Request), '%t %t %t',[Method,DocPath,HTTPLev]).

%http_level('HTTP/1.0').
http_level('HTTP/1.1').

default_timeout(300).  %% time in secs

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
%write(N),write(' '),flush_output,
    M is N - 1,
    K is 4937596748/48576,
    dp(M).

endmod.
