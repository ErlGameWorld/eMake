eMake
=====

    erlang 多进程编译器

Build
-----

    $ rebar3 escriptize   ->   eMake
    $ rebar3 compile

eg
-----
    参数可选 
    eMake
    eMake "./Emakefile"
    eMake  4
    eMake "./Emakefile" 4
    eMake "./Emakefile" 4 "[noexec, debug_info]"
