eMake
=====

    erlang 多进程编译器

Build
-----

    $ rebar3 escriptize   ->   eMake
    将编译得到 _build/default/bin 下面的eMake eMake.cmd 文件复制到工作目录或者添加到环境变量即可使用

eg
-----
    参数的值类型标记F:
        -s 字符串 参数的值会保持字符串 
        -i 整数  参数的值会转成整数
        -a 原子  参数的值会转成原子
        -f 浮点数  参数的值会转成浮点数
        -b 二进制 参数的值会转成binary
        -n 没有值 没有参数值 或者 可以配置占位符
    参数格式: FKey Value
    所有可选参数: 
        -nfo         
        -sfo Num
        -nfa 
        -nall
        -nprint
        -nnohrl 
        -semakefile Makefile
        -iworkcnt Num 编译进程最大数量
        -ioncecnt Num 单次批量编编译的文件数
        -sopts String 编译选项字符串
    eg:
    eMake  -nfo 
    eMake -ifo 1
    eMake -nfa
    eMake -nall -nnohrl -semakefile "./Emakefile" -iworkcnt 4 -sopts "[noexec, debug_info]"
    可以在编译之后修改代码指定默认的 Emakefile文件
    如果cmd下乱码 可以使用chcp 65001命令

    @echo off
    chcp 65001 > nul
    escript.exe "%~dpn0" %*