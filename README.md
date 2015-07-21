recvbench
==========

ErlangでのTCPデータ受信性能測定用のソースコード群。

いろいろと未整理 and テキトウ。

ビルド
------

```bash
$ git clone git://github.com/sile/recvbench.git
$ cd recvbench
$ ./rebar get-deps compile
```

使用例
------

```erlang
$ erl -pz ebin deps/*/ebin +K true -eval 'application:start(recvbench).'

%% TCPサーバ側(データ受信側)
> recv_passive:start_accept(3000). % passiveモード(gen_tcp:recv/3を使ってデータ受信)でサーバ起動
> recv_active:start_accept(3000). % `{active, true}`モードでサーバ起動
> recv_active_one:start_accept(3000). % `{active, one}`モードでサーバ起動
> recv_active_n:start_accept(3000, 32). % `{active, N=32}`モードでサーバ起動
> recv_active:start_accept(3000, [{sockopts, [{buffer, 1024*1024*2}]}]). % ソケットオプションを指定する

%% TCPクライアント側(データ送信側)
> ClientProcessCount = 1000. 
> ByteRate = round(4*1024*1024/8). % 4Mbps
> RequestsPerSec = 10. % 一つのプロセスが一秒間に何回データを送るか
> simple_load:start_link("localhost", 3000, ClientProcessCount, ByteRate, RequestsPerSec). % クライアント群起動
> simple_load:start_load(). % 負荷(データ送信)開始
```

なお各種指標の集計や表示などは行われないので、sarや[bench_util](https://github.com/sile/bench_util)といった別のツールを併用する必要がある。
