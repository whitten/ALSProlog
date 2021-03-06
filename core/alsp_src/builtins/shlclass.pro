/*================================================================*
 |		shlclass.pro
 |	Copyright (c) 1998 Applied Logic Systems, Inc.
 |
 |	Common class stuff for blt_shl/blt_dvsh/blt_cslt
 |	The ObjectPro class compiler is temporaily loaded to compile
 |	and assert (at compile time) the given class definitions;
 |	then the class compiler is removed along with the aux
 |	predicates defined here.
 |
 |	Original Creation Date: 7/98
 *================================================================*/

module builtins.

:- 
%	compiletime,

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%%%%   ALS_SHL_MGR  ObjectPro CLASS DEFINITIONS    %%%%%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	defineClass(builtins,
	[   name = als_shl_mgr,
		subClassOf = genericObjects,
		export = yes,
		addl_slots =
		[ 
			shell_module, 	  	  %% module for the shell (alsshell/alsdev)
			prolog_library, 	  %% path to the ...
			initial_dir,    	  %% initial directory we wake up in
			initial_search_dirs,  %% initial search list
			source_mgrs,	   	  %% list of managers for consulted files
			cslt_ctxt,		   	  %% (list) stack of "current source_mgr" 
			break_level			  %% break shell level (old global BreakLevel)
		],
		defaults = [ 
			shell_module = alsshell,   %% make alsdev reset this...
			source_mgrs = [],
			cslt_ctxt	= [],
			break_level = [b(0,user,true)]
		]
	]).

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%%%%%  	Consulted File Manager CLASS DEFINITIONS    %%%%%
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- 
%	compiletime,

        %% The manager for individual consulted files:
	defineClass(builtins,
	[   name=source_handler,
		subClassOf=genericObjects,
		export = yes,
		addl_slots=
		[ 
			source_type,		%% file/anon win/....
			source_file, 		%% OS path to the ...
			base_file,			%% underlying file name
			ext,				%% underlying extension
			obp_file,			%% OS path to obp file if exists, or nil
			fcg, 				%% File clause group # for this (consulted) file
			consult_mode,		%% normal/debug
			last_consult		%% Time of last consult,
		],
		defaults= [ 
			source_type		= file,
			ext				= '',
			fcg				= 0,
			consult_mode	= normal,
			last_consult	= 0
			]
	]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% GETTING A SOURCE MANAGER
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% In alspro, so nothing to do:
als_shl_mgrAction(refresh_wins, State).

als_shl_mgrAction(obtain_src_mgr(BaseFileName, FileMgr), State) 
	:-!,
	accessObjStruct(source_mgrs, State, PrevMgrsList),
	finish_obtain_src_mgr(BaseFileName, PrevMgrsList, State, FileMgr).

finish_obtain_src_mgr(BaseFileName, PrevMgrsList, State, FileMgr)
	:-
	dmember(fm(BaseFileName, FileMgr), PrevMgrsList),
	!.

finish_obtain_src_mgr(BaseFileName, PrevMgrsList, State, FileMgr)
	:-
	accessObjStruct(shell_module, State, ShellModule),
	(clause(alsdev_running,true) -> 
		Class = source_trace_mgr 
		; 
		Class = shl_source_handler
	),
	ShellModule:create_object(
		[instanceOf = Class,
		 handle = true,
		 values =
			[ 	source_type = file,
				base_file = BaseFileName 
			]
		],
		FileMgr ),
	!,
	(clause(alsdev_running,true) -> 
		accessObjStruct(debugger_mgr,  State, DBGMGR),
		setObjStruct(debugger_mgr,  FileMgr, DBGMGR)
		;
		true
	).

als_shl_mgrAction(record_src_mgr(BaseFileName, FileMgr), State) 
	:-!,
	accessObjStruct(source_mgrs, State, PrevMgrsList),
	finish_record_src_mgr(BaseFileName, PrevMgrsList, State, FileMgr).

finish_record_src_mgr(BaseFileName, PrevMgrsList, State, FileMgr)
	:-
	dmember(fm(BaseFileName, FileMgr), PrevMgrsList),
	!.
finish_record_src_mgr(BaseFileName, PrevMgrsList, State, FileMgr)
	:-
	setObjStruct(source_mgrs, State, [fm(BaseFileName, FileMgr) | PrevMgrsList]).

als_shl_mgrAction(obtain_src_mgr_by_cg(CG, FileMgr), State) 
	:-!,
	accessObjStruct(source_mgrs, State, PrevMgrsList),
	finish_obtain_src_mgr_by_cg(PrevMgrsList, CG, State, FileMgr).

finish_obtain_src_mgr_by_cg([fm(_,FileMgr) | List], CG, State, FileMgr)
	:-
	accessObjStruct(fcg, FileMgr, CG),
	!.

finish_obtain_src_mgr_by_cg([_ | List], CG, State, FileMgr)
	:-
	finish_obtain_src_mgr_by_cg(List, CG, State, FileMgr).

als_shl_mgrAction(remove_mgr(BaseFileName, FileMgr), State) 
	:-!,
	accessObjStruct(source_mgrs, State, PrevMgrsList),
	list_delete(PrevMgrsList, fm(BaseFileName, FileMgr), NewMgrsList),
	setObjStruct(source_mgrs, State, NewMgrsList).

als_shl_mgrAction(insert_src_mgr_by_cg(CG, FileMgr), State) 
	:-!,
	(clause(alsdev_running,true) -> 
		accessObjStruct(debugger_mgr,  State, DBGMGR),
		setObjStruct(debugger_mgr,  FileMgr, DBGMGR),
		send(DBGMGR, insert_by_fcg(CG, FileMgr))
		;
		true
	).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% RECORDING LOADING INFO (note_loaded)
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

source_handlerAction(note_loaded(CG, Path), State)
	:-
	setObjStruct(source_file, State, Path),
	setObjStruct(fcg, State, CG).

source_handlerAction( update_errors_wins(_), State).

source_handlerAction( clear_errors_display, State).




endmod.

module alsdev.

:- defineClass(alsdev,
	[   name=als_ide_mgr,
		subClassOf=als_shl_mgr,
		module = alsdev,
		addl_slots=
		[ 
			debugger_mgr,   %% debugger state object
			cur_project,    %% current project manager object    
			cur_cref_mgr,    %% current cref suite manager object    
			edit_files,     %% list of files open for editing
			non_file_edits  %% list of non-file (new) windows open for editing
			],
		defaults= [ 
			edit_files = [], 
			non_file_edits = [] 
		]
	]).

        %%   SHL_SOURCE_HANDLER:
:- defineClass(alsdev,
	[   name=shl_source_handler,
		subClassOf=source_handler,
		module = alsdev,
		export = yes,
		addl_slots= [ 
			tcl_doc_path,		%% Tcl id of edit window
			errors_display		%% nil / non-empty errs list
		],
		defaults= [ 
			tcl_doc_path		= nil,
			errors_display 		= nil
		]
	]).

        %%   SOURCE_TRACE_MGR:
:- defineClass(alsdev,
	[   name=source_trace_mgr,
		subClassOf=shl_source_handler,
		module = alsdev,
		addl_slots=
		[
			debugger_mgr,		%% home to daddy...
			last_visual_load,	%% Time of last load of file text widget
			num_lines,			%% num lines in the file
			linesizes,			%% list of num chars in each line
			invlineindex,		%% list of char offsets to start of each line
			head_tag,			%% i(S,E) = last colored "matching head (aph)tag" lcn
			call_tag			%% i(S,E) = last colored "matching_call (apg)tag" lcn
		],
		defaults= [ 
			visible				= false,
			last_visual_load 	= 0,
			num_lines			= 0,
			head_tag			= 0,
			call_tag			= 0
			]
	]).

:-defineClass(alsdev,
	[   name=debugger_mgr,
		subClassOf=genericObjects,
		module = alsdev,
		addl_slots=
		[ 
			debug_main_win, 		%% path to the ...
			debug_visible,  		%% true/false: debug_main_win visible
			src_trace_mgrs_by_file,		%% list of active mgrs, by file path
			fcg_index_size,			%% size of array for src_trace_mgrs_by_fcg
			src_trace_mgrs_by_fcg,		%% array (term) of active mgrs, by fcg
			mrfcg,				%% most recent file clause group touched
			stack_display_size,		%% size of stack to display
			stack_display_stream,		%% stream to write stack to
			stack_display_list		%% listbot to write stack to
		],
		defaults= [ 
			debug_main_win		= '.debugwin',
			debug_visible		=  false,
			src_trace_mgrs_by_file	= [],
			src_trace_mgrs_by_fcg	= [],
			mrfcg 			= 0,
			stack_display_size	= 20,
			stack_display_stream	= debugger_output,
			stack_display_list	= '.debugwin.stacklist'
			]
	]).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%  Project Manager ObjectPro CLASS DEFINITIONS  %%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		%% Note: Only one project can be open at a time;
		%% The manager for the project is read in from
		%% the project file when the (existing) project is
		%% opened, and is written back out to the project
		%% file when the project is closed.


:- defineClass(
	[   name=gen_project_mgr,
		subClassOf=genericObjects,
		addl_slots=
		[
			internal_name,
			title,
			project_file,
			primary_project_dir,	% normally where project_file is
			list_of_files_slots,
			list_slots,
			text_slots,
			search_dirs,
			search_trees,
			gui_spec,
			slot_names
			], 
		defaults=
		[
			title = '',
			project_file = '',
			list_of_files_slots = [],
			list_slots = [],
			text_slots = [],
			search_dirs = [],
			search_trees = [],
			slot_names = []
		] 
	]).

		%% The manager for a prolog/tcl/c project:
:- defineClass(
	[   name=project_mgr,
		subClassOf=gen_project_mgr,
		addl_slots=
		[
			addl_text_slots,
			production_goal,
			debug_goal,
			executable_name,
			stub_name,
			distdir_name,
			prolog_files,
			library_files,
			file_types,
			default_dirs,
			project_loaded			%% true/fail
			], 
		defaults=
		[
			project_loaded = fail,
			list_of_files_slots = [
				prolog_files
%				,library_files,
				],
			list_slots = [ ],
			text_slots = [],
			addl_text_slots = [ 
				production_goal, 
				debug_goal, 
			       	executable_name, 
				stub_name, 
				distdir_name
				          ],
			production_goal = prodGoal,
			debug_goal = debugGoal,
			executable_name = execName, 
			stub_name = stubName,
			distdir_name = 'MyDistDir',
			prolog_files = [],
			library_files = [],
			file_types =  [ 
				[['Prolog Files', ['.pro', '.pl'] ]],
				[prolog_library_files, ['.pro'] ]
			],
			default_dirs = [],
			slot_names = [
				[production_goal,	'Startup Goal:'],
				[debug_goal, 		'Debug Goal:'],
				[executable_name, 	'Image Name:'],
				[stub_name, 		'Stub Name:'],
				[distdir_name,		'Dist Dir Name:'],
				[prolog_files, 		'Prolog Files:'],
				[library_files, 	'Library Files:']
			]
		]
	]).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%  Cref Suite ObjectPro CLASS DEFINITIONS  %%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		%% Note: Only one suite can be open at a time;
		%% The manager for the suite is created on the
		%% fly using the suite values when an (existing)
		%% cref suite is opened, or a blank suite is
		%% created on the fly when a new suite is requested;
		%% the suite values are written back out to the 
		%% suite file when the suite is closed (via a 
		%% button on the Cref panel.

:- defineClass(
	[   name=cref_panel_mgr,
		subClassOf=genericObjects,
		addl_slots=
		[
			internal_name,
			title,
			suite_file,	% *.crf
			suite_dir,	% where suite_file is
			list_of_files,
			src_dir,	% (internal) dir where files reside
			target,
			gui_spec
			], 
		defaults=
		[
			title = '',
			suite_file = '',
			list_of_files = [],
			target = ''
		] 
	]).

endmod.

