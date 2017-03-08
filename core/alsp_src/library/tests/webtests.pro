t1 :-
    Docpath = '/', Host='www.alsprolog.com',
    RequestDescription = [ docpath = Docpath, host=Host ],
    fetch_url(RequestDescription, Response, HeaderLines, Status),
    write(status=Status),nl,
    read_tokens_lines(Response, HTMLPageTokens),
    parse_pxml(HTMLPageTokens, PXMLTerms),
    writeq(PXMLTerms),nl.

t1a :-
    Docpath = '/', Host='alsprolog.com',
    RequestDescription = [ docpath = Docpath, host=Host ],
    fetch_url(RequestDescription, Response, HeaderLines, Status),
    write(status=Status),nl,
    read_tokens_lines(Response, HTMLPageTokens),
    parse_pxml(HTMLPageTokens, PXMLTerms),
    writeq(PXMLTerms),nl.

t1d :-
    do_parse_style,
    Docpath = '/', Host='alsprolog.com',
    RequestDescription = [ docpath = Docpath, host=Host ],
    fetch_url(RequestDescription, Response, HeaderLines, Status),
    write(status=Status),nl,
    read_tokens_lines(Response, HTMLPageTokens),
    parse_pxml(HTMLPageTokens, PXMLTerms),
    writeq(PXMLTerms),nl.

t1f :-
    Docpath = '/', Host='www.alsprolog.com',
    RequestDescription = [ docpath = Docpath, host=Host ],
    ffetch_url(RequestDescription, RawResponse, HeaderLines, Status),
    process_raw_page(RawResponse, PXMLTerms, _),
    HeaderLines = [StatusLine | _],
    write(status=Status),nl,
    writeq(PXMLTerms),nl.

t2 :-
    Docpath = '/', Host='www.example.com',
    RequestDescription = [ docpath = Docpath, host=Host ],
    fetch_url(RequestDescription, Response, HeaderLines, Status),
    write(status=Status),nl,
    read_tokens_lines(Response, HTMLPageTokens),
    parse_pxml(HTMLPageTokens, PXMLTerms),
    writeq(PXMLTerms),nl,nl.

t2d :-
    do_parse_style,
    Docpath = '/', Host='www.example.com',
    RequestDescription = [ docpath = Docpath, host=Host ],
    fetch_url(RequestDescription, Response, HeaderLines, Status),
    write(status=Status),nl,
    read_tokens_lines(Response, HTMLPageTokens),
    parse_pxml(HTMLPageTokens, PXMLTerms),
    writeq(PXMLTerms),nl,nl.

t2a :-
    Docpath = '/', Host='example.com',
    RequestDescription = [ docpath = Docpath, host=Host ],
    fetch_url(RequestDescription, Response, HeaderLines, Status),
    write(status=Status),nl,
    read_tokens_lines(Response, HTMLPageTokens),
    parse_pxml(HTMLPageTokens, PXMLTerms),
    writeq(PXMLTerms),nl,nl.

t2f :-
    Docpath = '/', Host='www.example.com',
    RequestDescription = [ docpath = Docpath, host=Host ],
    ffetch_url(RequestDescription, Response, HeaderLines, Status),
    write(status=Status),nl,
    read_tokens_lines(Response, HTMLPageTokens),
    parse_pxml(HTMLPageTokens, PXMLTerms),
    writeq(PXMLTerms),nl,nl.
