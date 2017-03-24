/*==================================================================
 |		pdoc_html_desc.pro
 |	Copyright (c) 1991-2004 Applied Logic Systems, Inc.
 |
 |   Creation of HTML code for description of an individual predicate
 |	-- originally part of libmaint.pro, separated in 1993
 |	-- HTML-oriented version replaced original 2003-4
 |   Emulates Suns JavaDoc: Draws on JavaDoc generated code.
 |
 |	Date: October, 1991
 |	Author: Ken Bowen
 |	Revised: May 1993; Dec 1995; 2003-4
 *==================================================================*/

module prologdoc.

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
create_html_desc(ModPredsLists, Data, General, OneLiners)
	:-
	dmember(datetime=DateTime, General),
	create_html_desc0(ModPredsLists, Data, DateTime, OneLiners).

create_html_desc0([], Data, DateTime, []).
create_html_desc0([Mod-ModPreds | ModPredsLists], Data, DateTime, [Mod-ModOneLiners | RestOneLiners])
	:-
write(module=Mod),nl,
	create_html_descs(ModPreds, Mod, Data, DateTime, ModOneLiners),
	create_html_desc0(ModPredsLists, Data, DateTime, RestOneLiners).

create_html_descs([], Mod, Data, DateTime, []).
create_html_descs([Doc | ModPreds], Mod, Data, DateTime, [PredOneLiner | RestModOneLiners])
	:-
	html_desc(Doc, Mod, DateTime, Data, PredOneLiner),
	!,
	create_html_descs(ModPreds, Mod, Data, DateTime, RestModOneLiners).
create_html_descs([Doc | ModPreds], Mod, Data, DateTime, [PredOneLiner | RestModOneLiners])
	:-
	get_doc_preddesc(Doc, PredDesc),
        get_doc_file(Doc, File),
	printf(user_output, 
	       '!!FAILURE creating html description for: %t in module = %t from file = %t -- skipping\n', 
	       [PredDesc, Mod, File]),
	create_html_descs(ModPreds, Mod, Data, DateTime, RestModOneLiners).

html_desc(Doc, Module, DateTime, Data, PredText-ShortNote)
	:-
	get_doc_preddesc(Doc, PredDesc),
	PredDesc = Pred/Arity,
	sprintf(atom(PredText), '%t%t', [Pred, Arity]),
	get_mod_path(Module, Data, ModPath),
	sprintf(atom(HTMLFilePath), '%t/%t%t.html', [ModPath,Pred,Arity]),
	open(HTMLFilePath, write, OS),
	unwind_protect(
	   write_html_desc( PredDesc, Doc, DateTime, Module, HTMLFilePath, OS, ShortNote),
	   close(OS)  ).


write_html_desc( PredDesc, Doc, DateTime, Module, HTMLFilePath, OS, ShortNote)
	:-
	init_html_seg( PredDesc, Doc, DateTime, Module, HTMLFilePath, OS),

	central_html_seg( PredDesc, Doc, DateTime, Module, OS, ShortNote),

 	final_html_seg( PredDesc, Doc, DateTime, Module, HTMLFilePath, OS).


init_html_seg( PredDesc, Doc, DateTime, Module, HTMLFilePath, OS)
	:-
	codesweep([
	'<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">',
	'<!--NewPage-->',
	'<HTML>',
	'<HEAD>',
	'<!-- Generated by prologdoc () on %t -->'	+ [DateTime],
	'<TITLE>',
	'%t'						+ [PredDesc],
	'</TITLE>',
	'',
	'<META NAME="keywords" CONTENT="%t:%t">'	+ [Module, PredDesc],
	'',
	'<LINK REL ="stylesheet" TYPE="text/css" HREF="../stylesheet.css" TITLE="Style">',
	'',
	'<SCRIPT type="text/javascript">',
	'function windowTitle()',
	'{',
	'    parent.document.title="%t:%t";'		+ [Module, PredDesc],
	'}',
	'</SCRIPT>',
	'',
	'</HEAD>',
	'',
	'<BODY BGCOLOR="white" onload="windowTitle();">',
	'',
	'',
	'<!-- ========= START OF TOP NAVBAR ======= -->',
	'<A NAME="navbar_top"><!-- --></A>',
	'<A HREF="#skip-navbar_top" title="Skip navigation links"></A>',
	'<TABLE BORDER="0" WIDTH="100%" CELLPADDING="1" CELLSPACING="0" SUMMARY="">',
	'<TR>',
	'<TD COLSPAN=3 BGCOLOR="#EEEEFF" CLASS="NavBarCell1">',
	'<A NAME="navbar_top_firstrow"><!-- --></A>',
	'<TABLE BORDER="0" CELLPADDING="0" CELLSPACING="3" SUMMARY="">',
	'  <TR ALIGN="center" VALIGN="top">',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="../overview-summary.html"><FONT CLASS="NavBarFont1"><B>Overview</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="module-summary.html"><FONT CLASS="NavBarFont1"><B>Module</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#FFFFFF" CLASS="NavBarCell1Rev"> &nbsp;<FONT CLASS="NavBarFont1Rev"><B>Predicate</B></FONT>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="../index-all.html"><FONT CLASS="NavBarFont1"><B>Index</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="../index-kwic.html"><FONT CLASS="NavBarFont1"><B>KWIC Index</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="../files-desc.html"><FONT CLASS="NavBarFont1"><B>Files</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="../help-prologdoc.html"><FONT CLASS="NavBarFont1"><B>Help</B></FONT></A>&nbsp;</TD>',
	'  </TR>',
	'</TABLE>',
	'</TD>',
	'<TD ALIGN="right" VALIGN="top" ROWSPAN=3><EM>',
	'</EM>',
	'</TD>',
	'</TR>',
	'',
/*
	'<TR>',
	'<TD BGCOLOR="white" CLASS="NavBarCell2"><FONT SIZE="-2">',
	'&nbsp;PREV PRED&nbsp;',
	'&nbsp;NEXT PRED</FONT></TD>',
	'<TD BGCOLOR="white" CLASS="NavBarCell2"><FONT SIZE="-2">',
	'  <A HREF="../index.html" target="_top"><B>FRAMES</B></A>  &nbsp;',
	'&nbsp;<A HREF="%t" target="_top"><B>NO FRAMES</B></A>  &nbsp;'			+ [HTMLFilePath],
	'&nbsp;<SCRIPT type="text/javascript">',
	'  <!--',
	'  if(window==top) {',
	'    document.writeln(\'<A HREF="../allmodules-noframe.html"><B>All Modules</B></A>\');',
	'  }',
	'  //-->',
	'</SCRIPT>',
	'<NOSCRIPT>',
	'  <A HREF="../allmodules-noframe.html"><B>All Modules</B></A>',
	'</NOSCRIPT>',
	'',
	'</FONT></TD>',
	'</TR>',
	'<TR>',
	'<TD VALIGN="top" CLASS="NavBarCell3"><FONT SIZE="-2">',
	'  SUMMARY:&nbsp;NESTED&nbsp;|&nbsp;FIELD&nbsp;|&nbsp;<A HREF="#constructor_summary">CONSTR</A>&nbsp;|&nbsp;<A HREF="#method_summary">METHOD</A></FONT></TD>',
	'<TD VALIGN="top" CLASS="NavBarCell3"><FONT SIZE="-2">',
	'DETAIL:&nbsp;FIELD&nbsp;|&nbsp;<A HREF="#constructor_detail">CONSTR</A>&nbsp;|&nbsp;<A HREF="#method_detail">METHOD</A></FONT></TD>',
	'</TR>',
*/
	'</TABLE>',
	'<A NAME="skip-navbar_top"></A>',
	'<!-- ========= END OF TOP NAVBAR ========= -->\n'
	  ], OS).



final_html_seg( PredDesc, Doc, DateTime, Module, HTMLFilePath, OS)
	:-
	codesweep([
	'<!-- ======= START OF BOTTOM NAVBAR ====== -->',
	'<A NAME="navbar_bottom"><!-- --></A>',
	'<A HREF="#skip-navbar_bottom" title="Skip navigation links"></A>',
	'<TABLE BORDER="0" WIDTH="100%" CELLPADDING="1" CELLSPACING="0" SUMMARY="">',
	'<TR>',
	'<TD COLSPAN=3 BGCOLOR="#EEEEFF" CLASS="NavBarCell1">',
	'<A NAME="navbar_bottom_firstrow"><!-- --></A>',
	'<TABLE BORDER="0" CELLPADDING="0" CELLSPACING="3" SUMMARY="">',
	'  <TR ALIGN="center" VALIGN="top">',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="../overview-summary.html"><FONT CLASS="NavBarFont1"><B>Overview</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="module-summary.html"><FONT CLASS="NavBarFont1"><B>Module</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#FFFFFF" CLASS="NavBarCell1Rev"> &nbsp;<FONT CLASS="NavBarFont1Rev"><B>Predicate</B></FONT>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="../index-all.html"><FONT CLASS="NavBarFont1"><B>Index</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="../index-kwic.html"><FONT CLASS="NavBarFont1"><B>KWIC Index</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="../files-desc.html"><FONT CLASS="NavBarFont1"><B>Files</B></FONT></A>&nbsp;</TD>',
	'  <TD BGCOLOR="#EEEEFF" CLASS="NavBarCell1">    <A HREF="../help-prologdoc.html"><FONT CLASS="NavBarFont1"><B>Help</B></FONT></A>&nbsp;</TD>',
	'  </TR>',
	'</TABLE>',
	'</TD>',
	'<TD ALIGN="right" VALIGN="top" ROWSPAN=3><EM>',
	'</EM>',
	'</TD>',
	'</TR>',
	'',
/*
	'<TR>',
	'<TD BGCOLOR="white" CLASS="NavBarCell2"><FONT SIZE="-2">',
	'&nbsp;PREV PRED&nbsp;',
	'&nbsp;NEXT PRED</FONT></TD>',
	'<TD BGCOLOR="white" CLASS="NavBarCell2"><FONT SIZE="-2">',
	'  <A HREF="../index.html" target="_top"><B>FRAMES</B></A>  &nbsp;',
	'&nbsp;<A HREF="%t" target="_top"><B>NO FRAMES</B></A>  &nbsp;'			+ [HTMLFilePath],
	'&nbsp;<SCRIPT type="text/javascript">',
	'  <!--',
	'  if(window==top) {',
	'    document.writeln(\'<A HREF="../allmodules-noframe.html"><B>All Modules</B></A>\');',
	'  }',
	'  //-->',
	'</SCRIPT>',
	'<NOSCRIPT>',
	'  <A HREF="../allclasses-noframe.html"><B>All Classes</B></A>',
	'</NOSCRIPT>',
	'',
	'</FONT></TD>',
	'</TR>',
	'<TR>',
	'<TD VALIGN="top" CLASS="NavBarCell3"><FONT SIZE="-2">',
	'  SUMMARY:&nbsp;NESTED&nbsp;|&nbsp;FIELD&nbsp;|&nbsp;<A HREF="#constructor_summary">CONSTR</A>&nbsp;|&nbsp;<A HREF="#method_summary">METHOD</A></FONT></TD>',
	'<TD VALIGN="top" CLASS="NavBarCell3"><FONT SIZE="-2">',
	'DETAIL:&nbsp;FIELD&nbsp;|&nbsp;<A HREF="#constructor_detail">CONSTR</A>&nbsp;|&nbsp;<A HREF="#method_detail">METHOD</A></FONT></TD>',
	'</TR>',
*/
	'</TABLE>',
	'<A NAME="skip-navbar_bottom"></A>',
	'<!-- ======== END OF BOTTOM NAVBAR ======= -->',
	'',
	'<HR>',
	'',
	'</BODY>',
	'</HTML>'
	  ], OS).

central_html_seg( PredDesc, Doc, DateTime, Module, OS, ShortNote)
	:-
	get_doc_descgroup(Doc, DescGroup),
	arg(1, DescGroup, CallingForm),
	get_doc_briefdesc(Doc, ShortNote),
	codesweep([
	'<HR>',
	'<!-- ======== START OF PREDICATE DATA ======== -->',
	'<H2>%t</H2>'				+ [PredDesc],
	'<H3>&nbsp;&nbsp;module %t</H3>'	+ [Module],
	'<HR>',
	'<P>',
	'%t'					+ [ShortNote],
	'<P>',

	'<!-- ========== CALLING FORM =========== -->',
	'<A NAME="calling form" <!--  --></A>',
	'<TABLE BORDER="1" WIDTH="100%" CELLPADDING="3" CELLSPACING="0" SUMMARY="">',
	'<TR BGCOLOR="#CCCCFF" CLASS="TableHeadingColor">',
	'<TD COLSPAN=2><FONT SIZE="+1">',
	'<B>Calling Form</B></FONT></TD>',
	'</TR>',
	'<TR BGCOLOR="white" CLASS="TableRowColor">',
	'<TD><CODE><B>%t</B></CODE>'			+ [CallingForm],
	'<BR>',
	'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</TD>',
	'</TR>',
	'</TABLE>',
	'&nbsp;',

	'<!-- ========== I/O PATTERNS =========== -->',
	'<A NAME="io patterns" <!--  --></A>',
	'<TABLE BORDER="1" WIDTH="100%" CELLPADDING="3" CELLSPACING="0" SUMMARY="">',
	'<TR BGCOLOR="#CCCCFF" CLASS="TableHeadingColor">',
	'<TD COLSPAN=2><FONT SIZE="+1">',
	'<B>I/O Patterns</B></FONT></TD>',
	'</TR>',
	'<TR BGCOLOR="white" CLASS="TableRowColor">',
	'<TD><CODE><B>'
	  ], OS),
	get_doc_calliopat(Doc, CallingForm_IOPatterns),
	one_per_line_br(CallingForm_IOPatterns, OS),
	codesweep([
	'</B></CODE>',
	'<BR>',
	'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</TD>',
	'</TR>',
	'</TABLE>',
	'&nbsp;',

	'<!-- ========== DESCRIPTION =========== -->',
	'<A NAME="description" <!--  --></A>',
	'<TABLE BORDER="1" WIDTH="100%" CELLPADDING="3" CELLSPACING="0" SUMMARY="">',
	'<TR BGCOLOR="#CCCCFF" CLASS="TableHeadingColor">',
	'<TD COLSPAN=2><FONT SIZE="+1">',
	'<B>Description</B></FONT></TD>',
	'</TR>',
	'<TR BGCOLOR="white" CLASS="TableRowColor">',
	'<TD>'
	  ], OS),
	get_doc_extdesc(Doc, ExtendedDesc),
	one_per_line(ExtendedDesc, OS),
	get_doc_file(Doc, File),
	codesweep([
	'<BR>',
	'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</TD>',
	'</TR>',
	'</TABLE>',
	'&nbsp;',

	'<!-- ========== SOURCE FILE =========== -->',
	'<A NAME="source file" <!--  --></A>',
	'<TABLE BORDER="1" WIDTH="100%" CELLPADDING="3" CELLSPACING="0" SUMMARY="">',
	'<TR BGCOLOR="#CCCCFF" CLASS="TableHeadingColor">',
	'<TD COLSPAN=2><FONT SIZE="+1">',
	'<B>Source File</B></FONT></TD>',
	'</TR>',
	'<TR BGCOLOR="white" CLASS="TableRowColor">',
	'<TD><CODE><B>%t</B></CODE>'			+ [File],
	'<BR>',
	'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</TD>',
	'</TR>',
	'</TABLE>',
	'&nbsp;'
	  ], OS),
	get_doc_addlinfo(Doc, AdditionalInfo),
	(AdditionalInfo = [] ->
		true
		;
		codesweep([
		'<!-- ========== SOURCE FILE =========== -->',
		'<A NAME="additional info" <!--  --></A>',
		'<TABLE BORDER="1" WIDTH="100%" CELLPADDING="3" CELLSPACING="0" SUMMARY="">',
		'<TR BGCOLOR="#CCCCFF" CLASS="TableHeadingColor">',
		'<TD COLSPAN=2><FONT SIZE="+1">',
		'<B>Additional Information</B></FONT></TD>',
		'</TR>',
		'<TR BGCOLOR="white" CLASS="TableRowColor">'
	  		], OS),
		addl_info_per_line(AdditionalInfo, OS),
		codesweep([
		'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</TD>',
		'</TR>',
		'</TABLE>',
		'&nbsp;'
	  		], OS)
	).

one_per_line_br([], OS).
one_per_line_br([Line], OS)
	:-!,
	codesweep([
	'%t'		+ [Line]
	  ], OS).
one_per_line_br([Line | Lines], OS)
	:-
	codesweep([
	'%t'		+ [Line],
	'<BR>'
	  ], OS),
	one_per_line_br(Lines, OS).

one_per_line([], OS).
one_per_line([Line | Lines], OS)
	:-
	codesweep([
	'%t'		+ [Line]
	  ], OS),
	one_per_line(Lines, OS).

addl_info_per_line([], _).
addl_info_per_line([Tag = Value | AdditionalInfo], OS)
	:-
	codesweep([
		'<TD><CODE><B>%t: %t</B></CODE>'	+ [Tag, Value],
		'<BR>'
	  ], OS),
	addl_info_per_line(AdditionalInfo, OS).

endmod.