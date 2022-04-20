-module(eMake).
%% 多进程编译,修改自otp/lib/tools/src/make.erl
%% 解析Emakefile,根据获取{mods, options}列表,
%% 按照次序编译每项(解决编译顺序的问题)
%% 其中mods也可以包含多个模块,当大于1个时,
%% 可以启动多个process进行编译,从而提高编译速度.

-include_lib("kernel/include/file.hrl").

-export([
   main/1
   , all/3
]).

-define(MakeOpts, [noexec, load, netload, noload]).

main(Args) ->
   io:format("~p~n", [Args]),

   case Args of
      [] ->
         all(max(1, erlang:system_info(schedulers) - 1), "./Emakefile", []);
      [EMakeFileOrCnt] ->
         try list_to_integer(EMakeFileOrCnt) of
            Cnt ->
               all(max(1, Cnt), "./Emakefile", [])
         catch _:_ ->
            all(max(1, erlang:system_info(schedulers) - 1), EMakeFileOrCnt, [])
         end;
      [EMakeFile, CntStr] ->
         all(max(1, list_to_integer(CntStr)), EMakeFile, []);
      [EMakeFile, CntStr, OptsStr] ->
         {ok, Opts} = strToTerm(OptsStr),
         all(max(1, list_to_integer(CntStr)), EMakeFile, Opts)
   end.

all(Worker, EMakeFile, Opts) ->
   {MakeOpts, CompileOpts} = splitOpts(Opts, [], []),
   case readEMakefile(EMakeFile, CompileOpts) of
      Files when is_list(Files) ->
         do_make_files(Worker, Files, MakeOpts);
      error ->
         error
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

do_make_files(Worker, Fs, Opts) ->
   %io:format("worker:~p~nfs:~p~nopts:~p~n", [Worker, Fs, Opts]),
   process(Fs, Worker, lists:member(noexec, Opts), load_opt(Opts)).


%%% 读取给定的 Emakefile 并返回一个元组列表： [{Mods,Opts}]
%%% %%% Mods 是模块名称（字符串）的列表
%%% %%% Opts 是编译 Mods 时要使用的选项列表
readEMakefile(EMakefile, Opts) ->
   case file:consult(EMakefile) of
      {ok, EMake} ->
         transform(EMake, Opts, [], []);
      {error, enoent} ->
         %% 没有EMakefile 仅仅编译当前没有了下的文件 如果想要编译所有子目录下的文件 使用 filelib:wildcard("./**/*.erl")
         Mods = [filename:rootname(F) || F <- filelib:wildcard("*.erl")],
         [{Mods, Opts}];
      {error, Other} ->
         io:format("the Emakefile:~s is error:~p~n", [EMakefile, Other]),
         error
   end.

transform([{Mod, ModOpts} | Emake], Opts, Files, Already) ->
   case expand(Mod, Already) of
      [] ->
         transform(Emake, Opts, Files, Already);
      Mods ->
         transform(Emake, Opts, [{Mods, ModOpts ++ Opts} | Files], Mods ++ Already)
   end;
transform([Mod | Emake], Opts, Files, Already) ->
   case expand(Mod, Already) of
      [] ->
         transform(Emake, Opts, Files, Already);
      Mods ->
         transform(Emake, Opts, [{Mods, Opts} | Files], Mods ++ Already)
   end;
transform([], _Opts, Files, _Already) ->
   lists:reverse(Files).

expand(Mod, Already) when is_atom(Mod) ->
   expand(atom_to_list(Mod), Already);
expand(Mods, Already) when is_list(Mods), not is_integer(hd(Mods)) ->
   lists:concat([expand(Mod, Already) || Mod <- Mods]);
expand(Mod, Already) ->
   case lists:member($*, Mod) of
      true ->
         Fun = fun(F, Acc) ->
            M = filename:rootname(F),
            case lists:member(M, Already) of
               true -> Acc;
               false -> [M | Acc]
            end
               end,
         lists:foldl(Fun, [], filelib:wildcard(Mod ++ ".erl"));
      false ->
         Mod2 = filename:rootname(Mod, ".erl"),
         case lists:member(Mod2, Already) of
            true -> [];
            false -> [Mod2]
         end
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

%% 处理
process([{[], _Opts} | Rest], Worker, NoExec, Load) ->
   process(Rest, Worker, NoExec, Load);
process([{L, Opts} | Rest], Worker, NoExec, Load) ->
   Len = length(L),
   Worker2 = erlang:min(Len, Worker),
   case catch do_worker(L, Opts, NoExec, Load, Worker2) of
      error ->
         error;
      ok ->
         process(Rest, Worker, NoExec, Load)
   end;
process([], _Worker, _NoExec, _Load) ->
   up_to_date.

do_worker(L, Opts, NoExec, Load, Worker) ->

   %% 先开启worker个编译进程
   SplitNum = min(length(L), Worker),
   {L1, L2} = lists:split(SplitNum, L),

   % 启动进程
   Ref = make_ref(),
   Pids =
      [begin
          start_worker([E], Opts, NoExec, Load, self(), Ref)
       end || E <- L1],
   do_wait_worker(length(Pids), L2, Opts, NoExec, Load, Ref).

%% @doc 一个文件编译完成后，立即进行下一个文件编译,不用等待
do_wait_worker(0, [], _Opts, _NoExec, _Load, _Ref) ->
   ok;
do_wait_worker(N, L, Opts, NoExec, Load, Ref) ->
   receive
      {ack, Ref} ->
         case L of
            [H | T] ->
               %% 未编译完成，编译后面的文件
               start_worker(H, Opts, NoExec, Load, self(), Ref),
               do_wait_worker(N, T, Opts, NoExec, Load, Ref);
            [] ->
               %% 等待所有结果返回
               do_wait_worker(N - 1, [], Opts, NoExec, Load, Ref)
         end;
      {error, Ref} ->
         throw(error);
      {'EXIT', _P, _Reason} ->
         do_wait_worker(N, L, Opts, NoExec, Load, Ref);
      _Other ->
         io:format("receive unknown msg:~p~n", [_Other]),
         do_wait_worker(N, L, Opts, NoExec, Load, Ref)
   end.

start_worker(F, Opts, NoExec, Load, Parent, Ref) ->
   Fun =
      fun() ->
         case recompilep(coerce_2_list(F), NoExec, Load, Opts) of
            error ->
               Parent ! {error, Ref},
               exit(error);
            _ ->
               ok
         end,
         Parent ! {ack, Ref}
      end,
   spawn_link(Fun).

recompilep(File, NoExec, Load, Opts) ->
   ObjName = lists:append(filename:basename(File), code:objfile_extension()),
   ObjFile = case lists:keysearch(outdir, 1, Opts) of
                {value, {outdir, OutDir}} ->
                   filename:join(coerce_2_list(OutDir), ObjName);
                false ->
                   ObjName
             end,
   case exists(ObjFile) of
      true ->
         recompilep1(File, NoExec, Load, Opts, ObjFile);
      false ->
         recompile(File, NoExec, Load, Opts)
   end.

recompilep1(File, NoExec, Load, Opts, ObjFile) ->
   {ok, Erl} = file:read_file_info(lists:append(File, ".erl")),
   {ok, Obj} = file:read_file_info(ObjFile),
   recompilep1(Erl, Obj, File, NoExec, Load, Opts).

recompilep1(#file_info{mtime = Te},
   #file_info{mtime = To}, File, NoExec, Load, Opts) when Te > To ->
   recompile(File, NoExec, Load, Opts);
recompilep1(_Erl, #file_info{mtime = To}, File, NoExec, Load, Opts) ->
   recompile2(To, File, NoExec, Load, Opts).

%% recompile2(ObjMTime, File, NoExec, Load, Opts)
%% Check if file is of a later date than include files.
recompile2(ObjMTime, File, NoExec, Load, Opts) ->
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
   io:format("Out of date: ~s\n", [File]);
recompile(File, false, noload, Opts) ->
   io:format("Recompile: ~s\n", [File]),
   compile:file(File, [report_errors, report_warnings, error_summary | Opts]);
recompile(File, false, load, Opts) ->
   io:format("Recompile: ~s\n", [File]),
   c:c(File, Opts);
recompile(File, false, netload, Opts) ->
   io:format("Recompile: ~s\n", [File]),
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
