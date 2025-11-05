-module(eMake).
-include_lib("kernel/include/file.hrl").

-export([
	main/1
]).

-export([
	compileWorker/8
	, readEMake/0
	, readEMake/2
	, readEMake/3
	, saveEMake/2
]).

-define(MakeOpts, [noexec, load, netload, noload]).
-define(EMakefile, "Emakefile").
-define(OnceCnt, 16).

-define(help(), <<"
-nfo            清理redis
-sfo Num        清理redis
-nfa            清理redis
-nall           编译所有
-nprint         打印编译文件与编译时长
-sprint  opts|time   打印编译文件,编译时长与编译选项
-nnohrl         增量编译不检查头文件
-semakefile Makefile   指定编译的Makefile文件
-iworkcnt Num   编译进程最大数量
-ioncecnt Num   单次批量编编译的文件数
-sopts String   编译选项字符串
-ssopts String  设置本地编译选项字符串
-ndopts         删除本地编译选项字符串
-ngopts         获取编译选项字符串
-sgopts DirString 获取编译选项字符串
-h              帮助\n"/utf8>>).

main(Args) ->
	MapArgs = parseArgs(Args),
	process_flag(trap_exit, true),
	case MapArgs of
		#{"fo" := true} ->
			os:cmd("redis-cli FLUSHDB"),
			ok;
		#{"fo" := DBNumStr} ->
			os:cmd("redis-cli -n " ++ DBNumStr ++ " FLUSHDB"),
			ok;
		#{"fa" := true} ->
			os:cmd("redis-cli FLUSHALL"),
			ok;
		#{"gopts" := true} ->
			io:format("~s", [readEMake(localOpts, "[]")]),
			ok;
		#{"gopts" := DirString} ->
			io:format("~s", [readEMake(DirString, localOpts, "[]")]),
			ok;
		#{"dopts" := true} ->
			saveEMake(localOpts, "[]"),
			ok;
		#{"sopts" := OptsStr} ->
			saveEMake(localOpts, OptsStr),
			ok;
		#{"-h" := true} ->
			io:format("~ts", [?help()]);
		_ ->
			IsAll = maps:is_key("all", MapArgs),
			IsPrint = maps:get("print", MapArgs, false),
			IsNoHrl = maps:is_key("nohrl", MapArgs),
			EMakeFile = maps:get("emakefile", MapArgs, ?EMakefile),
			WorkCnt = maps:get("workcnt", MapArgs, erlang:system_info(schedulers) - 1),
			OnceCnt = maps:get("oncecnt", MapArgs, ?OnceCnt),
			OptsStr = maps:get("opts", MapArgs, "[]"),
			{ok, Opts} = strToTerm(OptsStr),
			LocalOptsStr = readEMake(localOpts, "[]"),
			{ok, LocalOpts} = strToTerm(LocalOptsStr),
			make(max(1, WorkCnt), max(1, OnceCnt), EMakeFile, LocalOpts ++ Opts, IsAll, IsNoHrl, IsPrint)
	end.

%% 解析命令行参数成map
parseArgs(Args) ->
	parseArgs(Args, #{}).
parseArgs([], Ret) -> Ret;
parseArgs([Flag | Rest], Ret) ->
	case Flag of
		[$-, $n | Left] ->
			case Rest of
				[] ->
					parseArgs(Rest, Ret#{Left => true});
				_ ->
					[Value | LRest] = Rest,
					case Value of
						[$- | _] ->
							parseArgs(Rest, Ret#{Left => true});
						_ ->
							parseArgs(LRest, Ret#{Left => Value})
					end
			end;
		[$-, $s | Left] ->
			[Value | LRest] = Rest,
			parseArgs(LRest, Ret#{Left => Value});
		[$-, $i | Left] ->
			[Value | LRest] = Rest,
			parseArgs(LRest, Ret#{Left => list_to_integer(Value)});
		[$-, $a | Left] ->
			[Value | LRest] = Rest,
			parseArgs(LRest, Ret#{Left => list_to_atom(Value)});
		[$-, $f | Left] ->
			[Value | LRest] = Rest,
			parseArgs(LRest, Ret#{Left => list_to_float(Value)});
		[$-, $b | Left] ->
			[Value | LRest] = Rest,
			parseArgs(LRest, Ret#{Left => list_to_binary(Value)});
		_ ->
			parseArgs(Rest, Ret#{Flag => true})
	end.

%% Base62 字符集 (0-9, A-Z, a-z)
-define(BASE62_CHARS, {
	48, 49, 50, 51, 52, 53, 54, 55, 56, 57,
	65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
	97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122
}).

%% 将整数编码为Base62字符串
enBase62(0) -> <<"0">>;
enBase62(N) -> enBase62(N, <<>>).
enBase62(0, BBinAcc) -> BBinAcc;
enBase62(N, BBinAcc) -> Index = (N rem 62) + 1, Char = element(Index, ?BASE62_CHARS), enBase62(N div 62, <<BBinAcc/binary, Char:8>>).
md5ToBase62(Bin) -> enBase62(binary:decode_unsigned(Bin)).

eMakeFile() ->
	{ok, CurDir} = file:get_cwd(),
	eMakeFile(CurDir).
eMakeFile(CurDir) ->
	Md5 = erlang:md5(CurDir),
	{ok, [[HomeDir]]} = init:get_argument(home),
	Md5Base62 = md5ToBase62(Md5),
	filename:join(HomeDir, <<".eMake/", Md5Base62/binary>>).

readEMake() ->
	try {ok, [MapValue]} = file:consult(eMakeFile()), MapValue of
		Value ->
			Value
	catch _:_ ->
		#{}
	end.

readEMake(Key, DefValue) ->
	try {ok, [MapValue]} = file:consult(eMakeFile()), maps:get(Key, MapValue, DefValue) of
		Value ->
			Value
	catch _:_ ->
		DefValue
	end.

readEMake(Dir, Key, DefValue) ->
	try {ok, [MapValue]} = file:consult(eMakeFile(Dir)), maps:get(Key, MapValue, DefValue) of
		Value ->
			Value
	catch _:_ ->
		DefValue
	end.

saveEMake(Key, Value) ->
	try
		FileName = eMakeFile(),
		filelib:ensure_dir(FileName),
		MapValue = readEMake(),
		NMapValue = MapValue#{Key => Value},
		WriteStr = iolist_to_binary(io_lib:format("~0p", [NMapValue])),
		file:write_file(FileName, <<WriteStr/binary, ".">>)
	catch _:_ ->
		ok
	end.

make(WorkerCnt, OnceCnt, EMakeFile, Opts, IsAll, IsNoHrl, IsPrint) ->
	io:format("compile start use EMakefile: ~ts ~n", [EMakeFile]),
	StartTime = erlang:system_time(second),
	{MakeOpts, CompileOpts} = splitOpts(Opts, [], []),
	LastTime = readEMake(lastTime, 0),
	LIsAll = IsAll orelse (LastTime /= 0 andalso StartTime =< LastTime),
	case readEMakefile(EMakeFile, CompileOpts, LIsAll) of
		{ok, Files} ->
			Ret = forMake(Files, WorkerCnt, OnceCnt, lists:member(noexec, MakeOpts), load_opt(MakeOpts), LIsAll, IsNoHrl, IsPrint, []),
			EndTime = erlang:system_time(second),
			saveEMake(lastTime, EndTime),
			case Ret of
				ok ->
					io:format("compile over all is ok use time: ~ps IsAll:~p ~n", [EndTime - StartTime, LIsAll]);
				{error, _Err} ->
					io:format("compile abort why: ~p~n", [_Err])
			end;
		_Err ->
			_Err
	end.

splitOpts([], Make, Compile) ->
	{Make, lists:reverse(Compile)};
splitOpts([H | T], Make, Compile) ->
	case lists:member(H, ?MakeOpts) of
		true ->
			splitOpts(T, [H | Make], Compile);
		false ->
			splitOpts(T, Make, [H | Compile])
	end.

%% term反序列化, string转换为term
strToTerm(String) ->
	case erl_scan:string(String ++ ".") of
		{ok, Tokens, _} ->
			erl_parse:parse_term(Tokens);
		_Err ->
			{error, _Err}
	end.

%% Any flags that are not recognixed as make flags are passed directly
%% to the compiler.
%% So for example make:all([load,debug_info]) will make everything
%% with the debug_info flag and load it.
load_opt(Opts) ->
	case lists:member(netload, Opts) of
		true ->
			netload;
		false ->
			case lists:member(load, Opts) of
				true ->
					load;
				_ ->
					noload
			end
	end.

%%% 读取给定的 Emakefile 并返回一个元组列表： [{Mods,Opts}]
%%% %%% Mods 是模块名称（字符串）的列表
%%% %%% Opts 是编译 Mods 时要使用的选项列表
readEMakefile(EMakefile, Opts, IsAll) ->
	case file:consult(EMakefile) of
		{ok, EMake} ->
			Ret = transform(EMake, Opts, IsAll, []),
			erase(),
			{ok, Ret};
		{error, enoent} ->
			%% 没有EMakefile 仅仅编译当前没有了下的文件 如果想要编译所有子目录下的文件 使用 filelib:wildcard("./**/*.erl")
			Mods = [filename:rootname(F) || F <- filelib:wildcard("*.erl")],
			{ok, [{Mods, Opts}]};
		{error, Other} = _Err ->
			io:format("the Emakefile:~s is error:~p~n", [EMakefile, Other]),
			_Err
	end.

transform([], _Opts, _IsAll, Files) ->
	lists:reverse(Files);
transform([{Mod, ModOpts} | EMake], Opts, IsAll, Files) ->
	case expand(Mod) of
		[] ->
			transform(EMake, Opts, IsAll, Files);
		Mods ->
			NewOpts = Opts ++ ModOpts,
			ensureOutDir(NewOpts, IsAll),
			transform(EMake, Opts, IsAll, [{Mods, NewOpts} | Files])
	end;
transform([Mod | EMake], Opts, IsAll, Files) ->
	case expand(Mod) of
		[] ->
			transform(EMake, Opts, IsAll, Files);
		Mods ->
			ensureOutDir(Opts, IsAll),
			transform(EMake, Opts, IsAll, [{Mods, Opts} | Files])
	end.

ensureOutDir(Opts, IsAll) ->
	case lists:keysearch(outdir, 1, Opts) of
		{value, {outdir, OutDir}} ->
			IsAll andalso file:del_dir_r(OutDir),
			code:add_path(OutDir),
			filelib:ensure_dir(filename:join(OutDir, "xxx.beam"));
		_ ->
			ignore
	end.

expand(Mod) when is_atom(Mod) ->
	expand(atom_to_list(Mod));
expand(Mods) when is_list(Mods), not is_integer(hd(Mods)) ->
	lists:append([expand(Mod) || Mod <- Mods]);
expand(Mod) ->
	case lists:member($*, Mod) of
		true ->
			foldErl(filelib:wildcard(Mod ++ ".erl"), []);
		_ ->
			M = filename:rootname(Mod, ".erl"),
			case get(M) of
				undefined ->
					put(M, 1),
					[M];
				_ ->
					[]
			end
	end.

foldErl([], Acc) ->
	Acc;
foldErl([OneFile | Left], Acc) ->
	M = filename:rootname(OneFile),
	case get(M) of
		undefined ->
			put(M, 1),
			foldErl(Left, [M | Acc]);
		_ ->
			foldErl(Left, Acc)
	end.

forMake([], _WorkerCnt, _OnceCnt, _NoExec, _Load, IsAll, IsNoHrl, IsPrint, AllWorkPids) ->
	case AllWorkPids of
		[] ->
			ok;
		_ ->
			receive
				{mOverCompile, WPid} ->
					NewAllWorkPids = lists:delete(WPid, AllWorkPids),
					case NewAllWorkPids of
						[] ->
							ok;
						_ ->
							forMake([], _WorkerCnt, _OnceCnt, _NoExec, _Load, IsAll, IsNoHrl, IsPrint, NewAllWorkPids)
					end;
				{mCompileError, Err} ->
					errorStop(Err, AllWorkPids);
				_Other ->
					io:format("forMake [] receive unexpect msg:~p ~n", [_Other]),
					{error, _Other}
			end
	end;
forMake([{Mods, Opts} | Rest], WorkerCnt, OnceCnt, NoExec, Load, IsAll, IsNoHrl, IsPrint, AllWorkPids) ->
	case Mods of
		[] ->
			forMake(Rest, WorkerCnt, OnceCnt, NoExec, Load, IsAll, IsNoHrl, IsPrint, AllWorkPids);
		_ ->
			case WorkerCnt > 0 of
				true ->
					{Files, More} = splitMods(Mods, OnceCnt),
					WPid = spawn_link(?MODULE, compileWorker, [Files, Opts, self(), NoExec, Load, IsAll, IsNoHrl, IsPrint]),
					case More of
						over ->
							forMake(Rest, WorkerCnt - 1, OnceCnt, NoExec, Load, IsAll, IsNoHrl, IsPrint, [WPid | AllWorkPids]);
						_ ->
							forMake([{More, Opts} | Rest], WorkerCnt - 1, OnceCnt, NoExec, Load, IsAll, IsNoHrl, IsPrint, [WPid | AllWorkPids])
					end;
				_ ->
					receive
						{mOverCompile, WPid} ->
							{Files, More} = splitMods(Mods, OnceCnt),
							erlang:send(WPid, {mNewFile, Files, Opts}),
							case More of
								over ->
									forMake(Rest, WorkerCnt, OnceCnt, NoExec, Load, IsAll, IsNoHrl, IsPrint, AllWorkPids);
								_ ->
									forMake([{More, Opts} | Rest], WorkerCnt, OnceCnt, NoExec, Load, IsAll, IsNoHrl, IsPrint, AllWorkPids)
							end;
						{mCompileError, Err} ->
							errorStop(Err, AllWorkPids);
						_Other ->
							io:format("forMake xx receive unexpect msg:~p ~n", [_Other]),
							{error, _Other}
					end
			end
	end.

splitMods(Mods, OnceCnt) ->
	{CurList, Index, LeftList} = splitFiles(OnceCnt, Mods, 0, []),
	case Index < OnceCnt of
		true ->
			{TCurList, _TIndex, TLeftList} = splitFiles(1, Mods, 0, []),
			case TLeftList of
				[] ->
					{TCurList, over};
				_ ->
					{TCurList, TLeftList}
			end;
		_ ->
			{CurList, LeftList}
	end.

splitFiles(0, List, Index, Acc) ->
	{lists:reverse(Acc), Index, List};
splitFiles(_Cnt, [], Index, Acc) ->
	{lists:reverse(Acc), Index, []};
splitFiles(Cnt, [H | T], Index, R) ->
	splitFiles(Cnt - 1, T, Index + 1, [H | R]).

errorStop(Err, AllWorkPids) ->
	[exit(OnePid, kill) || OnePid <- AllWorkPids],
	case Err of
		{File, Errors, Warnings} ->
			io:format("the file:~ts compile error:~p wrar:~p ~n", [File, Errors, Warnings]);
		File ->
			io:format("the file:~ts compile error please check ~n", [File])
	end,
	{error, compile_error}.

compileWorker(Files, Opts, Parent, NoExec, Load, IsAll, IsNoHrl, IsPrint) ->
	put(isPrint, IsPrint),
	compileWork(Files, Opts, Parent, NoExec, Load, IsAll, IsNoHrl).

compileWork([], _Opts, Parent, NoExec, Load, IsAll, IsNoHrl) ->
	erlang:send(Parent, {mOverCompile, self()}),
	receive
		{mNewFile, Files, Opts} ->
			compileWork(Files, Opts, Parent, NoExec, Load, IsAll, IsNoHrl);
		_Other ->
			io:format("compileWorker [] receive unexpect msg:~p ~n", [_Other])
	end;
compileWork([OneFile | Files], Opts, Parent, NoExec, Load, IsAll, IsNoHrl) ->
	case tryCompile(coerce_2_list(OneFile), NoExec, Load, IsAll, IsNoHrl, Opts) of
		error ->
			Parent ! {mCompileError, OneFile},
			exit(error);
		{error, Errors, Warnings} ->
			Parent ! {mCompileError, {OneFile, Errors, Warnings}},
			exit(error);
		_ ->
			compileWork(Files, Opts, Parent, NoExec, Load, IsAll, IsNoHrl)
	end.

tryCompile(File, NoExec, Load, IsAll, IsNoHrl, Opts) ->
	case IsAll of
		false ->
			ObjName = lists:append(filename:basename(File), code:objfile_extension()),
			ObjFile =
				case lists:keysearch(outdir, 1, Opts) of
					{value, {outdir, OutDir}} ->
						filename:join(coerce_2_list(OutDir), ObjName);
					false ->
						ObjName
				end,
			case exists(ObjFile) of
				true ->
					reCompile(File, NoExec, Load, Opts, ObjFile, IsNoHrl);
				false ->
					doCompile(File, NoExec, Load, Opts)
			end;
		_ ->
			doCompile(File, NoExec, Load, Opts)
	end.

reCompile(File, NoExec, Load, Opts, ObjFile, IsNoHrl) ->
	case file:read_file_info(lists:append(File, ".erl")) of
		{ok, #file_info{mtime = SrcTime}} ->
			case file:read_file_info(ObjFile) of
				{ok, #file_info{mtime = ObjTime}} ->
					case SrcTime > ObjTime of
						true ->
							doCompile(File, NoExec, Load, Opts);
						_ ->
							IsNoHrl /= true andalso ckIncludeRecompile(ObjTime, File, NoExec, Load, Opts)
					end;
				{error, _} ->
					doCompile(File, NoExec, Load, Opts)
			end;
		{error, _} ->
			doCompile(File, NoExec, Load, Opts)
	end.

%% recompile2(ObjMTime, File, NoExec, Load, Opts)
%% Check if file is of a later date than include files.
ckIncludeRecompile(ObjMTime, File, NoExec, Load, Opts) ->
	IncludePath = include_opt(Opts),
	case check_includes(lists:append(File, ".erl"), IncludePath, ObjMTime) of
		true ->
			doCompile(File, NoExec, Load, Opts);
		false ->
			false
	end.

include_opt([{i, Path} | Rest]) ->
	[Path | include_opt(Rest)];
include_opt([_First | Rest]) ->
	include_opt(Rest);
include_opt([]) ->
	[].

%% recompile(File, NoExec, Load, Opts)
%% Actually recompile and load the file, depending on the flags.
%% Where load can be netload | load | noload

doCompile(File, true, _Load, _Opts) ->
	io:format("Out of date: ~ts\n", [File]);
doCompile(File, false, noload, Opts) ->
	StartTime = erlang:system_time(millisecond),
	CRet = compile:file(File, [report_errors, report_warnings, error_summary | Opts]),
	doPrint(get(isPrint), File, StartTime, [report_errors, report_warnings, error_summary | Opts]),
	CRet;
doCompile(File, false, load, Opts) ->
	StartTime = erlang:system_time(millisecond),
	CRet = c:c(File, Opts),
	doPrint(get(isPrint), File, StartTime, Opts),
	CRet;
doCompile(File, false, netload, Opts) ->
	StartTime = erlang:system_time(millisecond),
	CRet = c:nc(File, Opts),
	doPrint(get(isPrint), File, StartTime, Opts),
	CRet.

doPrint(false, _File, _StartTime, _Opts) ->
	ignore;
doPrint(true, File, StartTime, _Opts) ->
	io:format("Recompile: ~ts use time:~pms\n", [File, erlang:system_time(millisecond) - StartTime]);
doPrint("time", File, StartTime, _Opts) ->
	io:format("Recompile: ~ts use time:~pms\n", [File, erlang:system_time(millisecond) - StartTime]);
doPrint(_, File, StartTime, Opts) ->
	io:format("Recompile: ~ts use time:~pms opts:~0p\n", [File, erlang:system_time(millisecond) - StartTime, Opts]).

exists(File) ->
	case file:read_file_info(File) of
		{ok, _} ->
			true;
		_ ->
			false
	end.

coerce_2_list(X) when is_atom(X) ->
	atom_to_list(X);
coerce_2_list(X) ->
	X.

%%% If you an include file is found with a modification
%%% time larger than the modification time of the object
%%% file, return true. Otherwise return false.
check_includes(File, IncludePath, ObjMTime) ->
	Path = [filename:dirname(File) | IncludePath],
	case epp:open(File, Path, []) of
		{ok, Epp} ->
			check_includes2(Epp, File, ObjMTime);
		_Error ->
			false
	end.

check_includes2(Epp, File, ObjMTime) ->
	case epp:parse_erl_form(Epp) of
		{ok, {attribute, 1, file, {File, 1}}} ->
			check_includes2(Epp, File, ObjMTime);
		{ok, {attribute, 1, file, {IncFile, 1}}} ->
			case file:read_file_info(IncFile) of
				{ok, #file_info{mtime = MTime}} when MTime > ObjMTime ->
					epp:close(Epp),
					true;
				_ ->
					check_includes2(Epp, File, ObjMTime)
			end;
		{ok, _} ->
			check_includes2(Epp, File, ObjMTime);
		{eof, _} ->
			epp:close(Epp),
			false;
		{error, _Error} ->
			check_includes2(Epp, File, ObjMTime);
		{warning, _Warning} ->
			check_includes2(Epp, File, ObjMTime)
	end.