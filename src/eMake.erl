-module(eMake).
-include_lib("kernel/include/file.hrl").

-export([
   main/1
]).

-export([
   compileWorker/6
   , readEMake/0
   , saveEMake/1
]).

-define(MakeOpts, [noexec, load, netload, noload]).

main(Args) ->
   process_flag(trap_exit, true),
   case Args of
      [] ->
         all(max(1, erlang:system_info(schedulers) - 1), "./Emakefile", []);
      [EMakeFileOrWorkCnt] ->
         try list_to_integer(EMakeFileOrWorkCnt) of
            Cnt ->
               all(max(1, Cnt), "./Emakefile", [])
         catch _:_ ->
            all(max(1, erlang:system_info(schedulers) - 1), EMakeFileOrWorkCnt, [])
         end;
      [EMakeFile, WorkCntStr] ->
         all(max(1, list_to_integer(WorkCntStr)), EMakeFile, []);
      [EMakeFile, WorkCntStr, OptsStr] ->
         {ok, Opts} = strToTerm(OptsStr),
         all(max(1, list_to_integer(WorkCntStr)), EMakeFile, Opts)
   end.

eMakeFile() ->
   {ok, CurDir} = file:get_cwd(),
   Md5 = erlang:md5(CurDir),
   filename:join(code:root_dir(), <<".eMake/", (base64:encode(Md5))/binary>>).

readEMake() ->
   try {ok, [LastTime]} = file:consult(eMakeFile()), LastTime of
      Value ->
         Value
   catch _:_ ->
      0
   end.

saveEMake(NowTime) ->
   try
      FileName = eMakeFile(),
      filelib:ensure_dir(FileName),
      file:write_file(FileName, <<(integer_to_binary(NowTime))/binary, ".">>)
   catch _:_ ->
      ok
   end.

all(WorkerCnt, EMakeFile, Opts) ->
   io:format("compile start use EMakefile: ~ts~n", [EMakeFile]),
   StartTime = erlang:system_time(second),
   {MakeOpts, CompileOpts} = splitOpts(Opts, [], []),
   case readEMakefile(EMakeFile, CompileOpts) of
      {ok, Files} ->
         LastTime = readEMake(),
         IsAll = LastTime /= 0 andalso StartTime =< LastTime,
         forMake(Files, WorkerCnt, lists:member(noexec, MakeOpts), load_opt(MakeOpts), IsAll, []),
         EndTime = erlang:system_time(second),
         saveEMake(EndTime),
         io:format("compile over all is ok use time: ~ps ~n", [EndTime - StartTime]);
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
readEMakefile(EMakefile, Opts) ->
   case file:consult(EMakefile) of
      {ok, EMake} ->
         Ret = transform(EMake, Opts, []),
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

transform([], _Opts, Files) ->
   lists:reverse(Files);
transform([{Mod, ModOpts} | EMake], Opts, Files) ->
   case expand(Mod) of
      [] ->
         transform(EMake, Opts, Files);
      Mods ->
         NewOpts = Opts ++ ModOpts,
         ensure_dir(NewOpts),
         transform(EMake, Opts, [{Mods, NewOpts} | Files])
   end;
transform([Mod | EMake], Opts, Files) ->
   case expand(Mod) of
      [] ->
         transform(EMake, Opts, Files);
      Mods ->
         ensure_dir(Opts),
         transform(EMake, Opts, [{Mods, Opts} | Files])
   end.

ensure_dir(Opts) ->
   case lists:keysearch(outdir, 1, Opts) of
      {value, {outdir, OutDir}} ->
         BeamDir = filename:join(OutDir, "xxx.beam"),
         filelib:ensure_dir(BeamDir);
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

-define(OnceCnt, 8).
forMake([], _Worker, _NoExec, _Load, IsAll, AllWorkPids) ->
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
                     forMake([], _Worker, _NoExec, _Load, IsAll, NewAllWorkPids)
               end;
            {mCompileError, Err} ->
               errorStop(Err, AllWorkPids);
            _Other ->
               io:format("forMake [] receive unexpect msg:~p ~n", [_Other])
         end
   end;
forMake([{Mods, Opts} | Rest], Worker, NoExec, Load, IsAll, AllWorkPids) ->
   case Mods of
      [] ->
         forMake(Rest, Worker, NoExec, Load, IsAll, AllWorkPids);
      _ ->
         case Worker > 0 of
            true ->
               {Files, More} = splitMods(Mods),
               WPid = spawn_link(?MODULE, compileWorker, [Files, Opts, self(), NoExec, Load, IsAll]),
               case More of
                  over ->
                     forMake(Rest, Worker - 1, NoExec, Load, IsAll, [WPid | AllWorkPids]);
                  _ ->
                     forMake([{More, Opts} | Rest], Worker - 1, NoExec, Load, IsAll, [WPid | AllWorkPids])
               end;
            _ ->
               receive
                  {mOverCompile, WPid} ->
                     {Files, More} = splitMods(Mods),
                     erlang:send(WPid, {mNewFile, Files, Opts}),
                     case More of
                        over ->
                           forMake(Rest, Worker, NoExec, Load, IsAll, AllWorkPids);
                        _ ->
                           forMake([{More, Opts} | Rest], Worker, NoExec, Load, IsAll, AllWorkPids)
                     end;
                  {mCompileError, Err} ->
                     errorStop(Err, AllWorkPids);
                  _Other ->
                     io:format("forMake xx receive unexpect msg:~p ~n", [_Other])
               end
         end
   end.

splitMods(Mods) ->
   case length(Mods) =< ?OnceCnt of
      true ->
         {Mods, over};
      _ ->
         lists:split(?OnceCnt, Mods)
   end.

errorStop(Err, AllWorkPids) ->
   [exit(OnePid, kill) || OnePid <- AllWorkPids],
   case Err of
      {File, Errors, Warnings} ->
         io:format("the file:~ts compile error:~p wrar:~p ~n ", [File, Errors, Warnings]);
      File ->
         io:format("the file:~ts compile error please check ~n ", [File])
   end.

compileWorker([], _Opts, Parent, NoExec, Load, IsAll) ->
   erlang:send(Parent, {mOverCompile, self()}),
   receive
      {mNewFile, Files, Opts} ->
         compileWorker(Files, Opts, Parent, NoExec, Load, IsAll);
      _Other ->
         io:format("compileWorker [] receive unexpect msg:~p ~n", [_Other])
   end;
compileWorker([OneFile | Files], Opts, Parent, NoExec, Load, IsAll) ->
   case compile(coerce_2_list(OneFile), NoExec, Load, IsAll, Opts) of
      error ->
         Parent ! {mCompileError, OneFile},
         exit(error);
      {error, Errors, Warnings} ->
         Parent ! {mCompileError, {OneFile, Errors, Warnings}},
         exit(error);
      _ ->
         compileWorker(Files, Opts, Parent, NoExec, Load, IsAll)
   end.

compile(File, NoExec, Load, IsAll, Opts) ->
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
               reCompile(File, NoExec, Load, Opts, ObjFile);
            false ->
               recompile(File, NoExec, Load, Opts)
         end;
      _ ->
         recompile(File, NoExec, Load, Opts)
   end.

reCompile(File, NoExec, Load, Opts, ObjFile) ->
   {ok, #file_info{mtime = SrcTime}} = file:read_file_info(lists:append(File, ".erl")),
   {ok, #file_info{mtime = ObjTime}} = file:read_file_info(ObjFile),
   case SrcTime > ObjTime of
      true ->
         recompile(File, NoExec, Load, Opts);
      _ ->
         ckIncludeRecompile(ObjTime, File, NoExec, Load, Opts)
   end.

%% recompile2(ObjMTime, File, NoExec, Load, Opts)
%% Check if file is of a later date than include files.
ckIncludeRecompile(ObjMTime, File, NoExec, Load, Opts) ->
   IncludePath = include_opt(Opts),
   case check_includes(lists:append(File, ".erl"), IncludePath, ObjMTime) of
      true ->
         recompile(File, NoExec, Load, Opts);
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

recompile(File, true, _Load, _Opts) ->
   io:format("Out of date: ~ts\n", [File]);
recompile(File, false, noload, Opts) ->
   % io:format("Recompile: ~ts\n", [File]),
   compile:file(File, [report_errors, report_warnings, error_summary | Opts]);
recompile(File, false, load, Opts) ->
   % io:format("Recompile: ~ts\n", [File]),
   c:c(File, Opts);
recompile(File, false, netload, Opts) ->
   % io:format("Recompile: ~ts\n", [File]),
   c:nc(File, Opts).

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
         check_includes2(Epp, File, ObjMTime)
   end.