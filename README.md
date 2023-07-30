eMake
=====

    erlang 多进程编译器

Build
-----

    $ rebar3 escriptize   ->   eMake
    将编译得到 _build/default/bin 下面的eMake eMake.cmd 文件复制到工作目录或者添加到环境变量即可使用

eg
-----
    参数可选 all nohrl
    eMake
    eMake all nohrl
    eMake "./Emakefile"
    eMake  4
    eMake "./Emakefile" 4
    eMake "./Emakefile" 4 "[noexec, debug_info]"
    可以在编译之后修改代码指定默认的 Emakefile文件
