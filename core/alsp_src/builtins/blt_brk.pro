/*=====================================================================
 | 		blt_brk.pro 	
 | 	Copyright (c) 1987-95 Applied Logic Systems, Inc.
 |
 |	Basic break handler for ^C or ^Break interrupts
 |
 | Author: Ken Bowen (based on code from Kevin Buettner)
 | Date  : 7/29/91
 |
 | Revised 4/1/93 - Ken Bowen 
 |   -- changed to library menu routines
 |   -- changed to stream I/O routines
 |   -- removed see/tell info from breakpoint record
 *===================================================================*/
module builtins.
use windows.

	%% now in object als_shl_mgr as:
	%% break_level			  %% break shell level (old global BreakLevel)
%:- make_gv('BreakLevel').
%:- setBreakLevel([b(0,user,true)]).


breakhandler(M,G) 
	:- 
		%% getBreakLevel(BreakList),
	get_primary_manager(ALSIDE),
	accessObjStruct(break_level, ALSIDE, BreakList),
	BreakList = [b(OldLevel,_,_) | _],
	NewLevel is OldLevel+1,
	%% setBreakLevel([b(NewLevel,M,G) | BreakList]),
	setObjStruct(break_level, ALSIDE, [b(NewLevel,M,G) | BreakList]),
	catch(breakhandler0(M,G,ALSIDE),
		  breakhandler(NewM,NewG),
		  breakhandler0(NewM,NewG,ALSIDE) ), !.

breakhandler(_,_) 
	:- 
		%% getBreakLevel(BreakList),
	get_primary_manager(ALSIDE),
	accessObjStruct(break_level, ALSIDE, BreakList),
	!,
	(BreakList = [b(0,user,true)] ->
		true
		;
		BreakList = [_ | BreakListTail],
			%% setBreakLevel(BreakListTail),
		setObjStruct(break_level, ALSIDE, BreakListTail),
		fail
	).
	
hot_break_handler(Resp,M,G)
	:-
		%% getBreakLevel(BreakList),
	get_primary_manager(ALSIDE),
	accessObjStruct(break_level, ALSIDE, BreakList),
	BreakList = [b(OldLevel,_,_) | _],
	NewLevel is OldLevel+1,
	%% setBreakLevel([b(NewLevel,M,G) | BreakList]),
	setObjStruct(break_level, ALSIDE, [b(NewLevel,M,G) | BreakList]),

	catch(break_handler(Resp,M,G,ALSIDE),
		  breakhandler(NewM,NewG),
		  breakhandler0(NewM,NewG,ALSIDE) ), !.

hot_breakhandler(_,_,_) 
	:- 
		%% getBreakLevel(BreakList),
	get_primary_manager(ALSIDE),
	accessObjStruct(break_level, ALSIDE, BreakList),
	!,
	(BreakList = [b(0,user,true)] ->
		true
		;
		BreakList = [_ | BreakListTail],
			%% setBreakLevel(BreakListTail),
		setObjStruct(break_level, ALSIDE, BreakListTail),
		fail
	).
	
listOfCodes([ a, b, c, d, e, f, p, s, t, ?]).

choiceItems( [
		 'Abort Computation',
		 'Break shell',
		 'Continue',
		 'Debug',
		 'Exit Prolog',
		 'Fail',
		 'Return to Previous Break Level',
		 'Show goal broken at',
		 'Stack trace',
		 'This message'] ).

responses( M, G, [
		 abort,
		 break_shell,
		 continue(M,G),
		 debug(M,G),
		 exit_prolog, 	% was: exit,
		 fail,
		 previous,
		 show(M,G),
		 stack_trace,
		 help ] ).

options(ListOfCodes, Responses, Prompt,
			[codes		= ListOfCodes,
			 responses	= Responses,
			 default	= 'Abort Computation',
			 title		= 'Break Handler',
			 indent		= '    ',
			 prompt		= Prompt ] ).

breakhandler0(M,G,ALSIDE) 
	:- 
	%% getBreakLevel([b(Level,_,_)|_]),
	accessObjStruct(break_level, ALSIDE, BreakList),
	BreakList = [b(Level,_,_)|_],
	catenate(['Break(',Level,') >'],Prompt),
	listOfCodes(ListOfCodes),
	choiceItems(ChoiceItems),
	responses(M, G, Responses),
	options(ListOfCodes, Responses, Prompt, Options0),
	Options = [io_streams=(warning_input,warning_output) | Options0],
		%% Note: menu/4 defined in Library in iolayer.pro:
	simple_menu(ChoiceItems, Response, [0-'No Choice - Exit menu' | Options]),
	position(ChoiceItems, Response, PosN),
	nth(PosN, Responses, Resp),
	break_handler(Resp,M,G,ALSIDE).

break_handler(abort,M,G,ALSIDE) 
	:-!, 
	%% setBreakLevel([b(0,user,true)]),
	setObjStruct(break_level, ALSIDE, [b(0,user,true)]),
		%% abort_ctlc: "Aborting from Control-C or Control-Break.\n"
	prolog_system_error(abort_ctlc, []),
	throw(abort).

break_handler(alsdev_shell,M,G,ALSIDE) 
	:-!,
	builtins:prolog_shell(user_input,user_output,alsdev),
	breakhandler0(M,G,ALSIDE).
break_handler(break_shell,M,G,ALSIDE) 
	:-!,
	prolog_shell,
	breakhandler0(M,G,ALSIDE).
break_handler(continue(M,G),M,G,ALSIDE) 
	:-!,
	%% getBreakLevel([_ | PrevList]),
	accessObjStruct(break_level, ALSIDE, BreakList),
	BreakList = [_ | PrevList],
	%% setBreakLevel(PrevList),
	setObjStruct(break_level, ALSIDE, PrevList),
	M:G.

break_handler(debug(M,G),M,G,ALSIDE) 
	:-!,
	catch(trace(M,G),
		  breakhandler_debug(NewM,NewG),
		  breakhandler0(NewM,NewG,ALSIDE) ).
break_handler(exit_prolog,M,G,ALSIDE) 
	:-!, 
		%% exit_ctlc: "Exiting Prolog from Control-C or Control-Break.\n"
	prolog_system_error(exit_ctlc, []),
	halt.
break_handler(exit,M,G,ALSIDE) 
	:-!, 
	%% getBreakLevel([b(Level,_,_)|_]),
	accessObjStruct(break_level, ALSIDE, BreakList),
	BreakList = [b(Level,_,_)|_],
	(Level < 0 ->
			%% exit_ctlc: "Exiting Prolog from Control-C or Control-Break.\n"
		prolog_system_error(exit_ctlc, []),
		halt
		;
		break_handler(previous,M,G,ALSIDE) 
	).
break_handler(fail,M,G,ALSIDE) 
	:-!,
	fail.
break_handler(previous,M,G,ALSIDE) 
	:-!, 
	get_shell_level(CurLevel),
	get_shell_prompts( CurPromptsStack ),
	%% getBreakLevel([_ | PrevList]),
	accessObjStruct(break_level, ALSIDE, BreakList),
	BreakList = [_ | PrevList],
	%% setBreakLevel(PrevList),
	setObjStruct(break_level, ALSIDE, PrevList),
	PrevList = [b(PrevLevel, PrevM, PrevG) | _],
	(   PrevLevel < 1 ->  
			%% no_prev_lev: "No previous level!!\n"
		prolog_system_error(no_prev_lev, []),
	    	set_shell_prompts( [('?- ','?-_')] ),
	    	abort
		;
	    throw(breakhandler(PrevM,PrevG))
	).
break_handler(show(M,G),M,G,ALSIDE) 
	:-!,
	als_advise('Break at: %t:%t\n',[M,G]),
	breakhandler0(M,G,ALSIDE).
break_handler(stack_trace,M,G,ALSIDE) 
	:-!,
	stack_trace(break_handler),
	breakhandler0(M,G,ALSIDE).

break_handler(Otherwise,M,G,ALSIDE) 
	:- 
	als_advise('\n    Bad input! Please re-enter.\n\n',[]),
	!,
	breakhandler0(M,G,ALSIDE).

stack_trace(Context) 
	:-
	stack_trace(1, Context).

stack_trace(30, Context) :- !.
stack_trace(N, Context) 
	:-
	frame_info(N,FI),
	!,
	disp_stack_trace(FI,N, Context).

disp_stack_trace((builtins:GG),_,_) 
	:-
	functor(GG,do_shell_query,_),
	!,
	printf(debugger_output,'--Bottom of Stack--\n',[],[quoted(true),maxdepth(8)]).

disp_stack_trace(FI, N, _)
	:-
	FI=(builtins:breakhandler(Mod,Goal)),
	!,
	printf(debugger_output,'(%d) %t\n(%d) %t:%t\n',[N,FI,N,Mod,Goal],[quoted(true),maxdepth(8)]),
	NN is N+1,
	stack_trace(NN, Context).

disp_stack_trace(FI, N, debugger)
	:-
	FI=(builtins:stack_trace(_,debugger)),
	!,
	NN is N+1,
	stack_trace(NN, debugger).

disp_stack_trace(FI, N, debugger)
	:-
	FI=(debugger:dogoal(Box,Depth,_,Mod,Goal)),
	!,
	(Box == Depth ->
		printf(debugger_output,'(%d) %t:%t\n',[N,Mod,Goal],[quoted(true),maxdepth(8)])
		;
		printf(debugger_output,'(%d) %t\n',[N,dogoal(Box,Depth,_,Mod,Goal)],[quoted(true),maxdepth(8)])
	),
	NN is N+1,
	stack_trace(NN, debugger).
disp_stack_trace(FI, N, debugger)
	:-
	FI=(debugger:_),
	!,
	NN is N+1,
	stack_trace(NN, debugger).
disp_stack_trace(FI, N, debugger)
	:-
	FI= (builtins:catch0(debugger,?,?)),
	!,
	NN is N+1,
	stack_trace(NN, debugger).

disp_stack_trace(FI, N, Context)
	:-
	printf(debugger_output,'(%d) %t\n',[N,FI],[quoted(true),maxdepth(8)]),
	NN is N+1,
	stack_trace(NN, Context).
stack_trace(_, _).

endmod.
