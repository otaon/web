---
title: "書籍 Land of Lisp 12章 ストリーム"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
toc: true
tags: ["lisp"]
categories: ["Land-of-Lisp"]
authors:
- otaon
---

REPLによる入出力、ディスク上のファイルの読み書き、LANやインターネットの通信において、  
Common Lispでは **ストリーム** を使用する。

本ドキュメントでは、ストリームの種類、使い方を説明する。

## 12.1 ストリームの種類

Common Lispでは、リソースの種類に合わせて、いくつかのストリーム型が用意されている。  
また、ストリームの向きにも種類がある。

- リソースにデータを書き出す(write)
- リソースからデータを読み込む(read)
- リソースとデータを読み書きする(read/write)

### リソースの種類による分類

リソースの種類に応じて、ストリームの型を分類する。

<dl>
  <dt>コンソールストリーム</dt>
  <dd>標準入出力。</br>REPLとやりよりするのに使っていたストリーム。</dd>
  <dt>ファイルストリーム</dt>
  <dd>ディスク上のファイルの読み書きに使うストリーム。</dd>
  <dt>ソケットストリーム</dt>
  <dd>ネットワークを通じて他のコンピュータと通信するのに使うストリーム。</dd>
  <dt>文字列ストリーム</dt>
  <dd>Lispの文字列からテキストを読み出したり、文字列へと書き込んだりするストリーム。</dd>
</dl>

### ストリームの向きによる分類

リソースに対するストリームの向きによってストリームを分類する。

<dl>
  <dt>出力ストリーム</dt>
  <dd>リソースにデータを書き出すストリーム。</dd>
  <dt>入力ストリーム</dt>
  <dd>リソースからデータを読み込むストリーム。</dd>
</dl>

#### 出力ストリーム

出力ストリームは、REPLに文字を表示したり、ファイルに書き出したり、ソケットを通じてデータを送ったりするのに使われる。  
出力ストリームの最も基本的な操作は下記の2つのみである。  
他のLispのデータ型に比べると、できる操作が限られているが、むしろこれによりストリームが色々と応用できる。

|基本操作                    |コマンド         |
|----------------------------|-----------------|
|出力ストリームか否かを調べる|`output-stream-p`|
|データをストリームへと送る  |`write-char`     |

##### 出力ストリームか否かを調べる

REPLには`*standard-output*`と呼ばれる出力ストリームが結び付けられている。  
次のコードにより、これが有効な出力ストリームか否かを調べることができる。

```lisp
> (output-stream *standard-output*)
T
```

##### データをストリームへと送る

Lispの文字は`write-char`を使って出力ストリームに送ることができる。  
文字`#\x`を`*standard-output*`ストリームに送り出すには、次のコードを実行する。

```lisp
> (write-char #\x *standard-output*)
xNIL
```

このコードは、`x`を標準出力に書き出す。  
この関数の戻り値`nil`が`x`のすぐ次に表示されているが、これは単なる`write-char`の戻り値。

他にも、バイナリデータなどを操作することもできる。

#### 入力ストリーム

入力ストリームは、データを読み出すために使う。  
出力ストリームと同様、入力ストリームに対して行える操作も限られている。

|基本操作                           |コマンド        |
|-----------------------------------|----------------|
|入力ストリームか否かを調べる       |`input-stream-p`|
|ストリームからデータを1つ取り出す  |`read-char`     |

##### 入力ストリームか否かを調べる

REPLには`*standard-input*`と呼ばれる入力ストリームが結び付けられている。  
次のコードにより、これが有効な入力ストリームか否かを調べることができる。

```lisp
> (input-stream-p *standard-input*)
T
```

##### 入力ストリームから1文字取り出す

`read-char`を使って入力ストリームから1文字取り出すことができる。  
次のコードでは、REPLから読み込んでいるため、`[enter]`キーを押すまでデータが標準入力ストリームに届かないことに注意。

```lisp
> (read-char *standard-input*)
123[enter]
#\1
```

`[enter]`を押すと、入力ストリームの先頭にある`#\1`が`read-char`により返される。


*ストリームに使える他のコマンド*  
`write-char`や`read-char`以外にも、Common Lispにはストリームを扱うコマンドが多く備わっている。  
例えば、`print`コマンドに`*standart-output*`を渡して出力先を指定することができる。

```lisp
> (print 'foo *standard-output*)
FOO
```

## 12.2 ファイルの読み書き

ストリームを使うことで、ファイルの読み書きもできる。  
Common Lispでファイルストリームを作成するのに良い方法としては、`with-open-file`コマンドを使うことである。

```lisp
> (with-open-file (my-stream "data.txt" :direction :output)
      (print "my data" my-stream))
```

この例では、出力ストリームを作って`my-stream`という変数に格納している。  
このストリームは`with-open-file`の閉じ括弧まで有効である。  
そして、このストリームに送られたデータは、ディスク上の"data.txt"というファイルに書き出される。

**`with-open-file`の`:direction`に`:output`を渡すと出力ストリームが作られる。**  
**`with-open-file`の`:direction`に`:input`を渡すと入力ストリームが作られる。**  

```lisp
> (with-open-file (my-stream "data.txt" :direction :input)
      (read my-stream))
```

もう少し複雑な例を次に示す。

### リストをファイルに読み書きする

```lisp
> (let ((animal-noises '((dog . woof)
                         (cat . meow))))
    (with-open-file (my-stream "animal-noises.txt" :direction :output)
       (print animal-noises my-stream)))
((DOG . WOOF)(CAT . MEOW))
> (with-open-file (my-stream "animal-noises.txt" :direction :input)
     (read my-stream))
((DOG . WOOF)(CAT . MEOW))
```

### ファイルが既に存在するか否かをチェックする

作ろうとしたファイルが既に存在した場合にどうするかを指定するには`:if-exists`キーワードを指定する。

**ファイルが既に存在した場合はエラーとする**  
```lisp
> (with-open-file (my-stream "data.txt" :direction :output :if-exists :error)
      (print "my data" my-stream))
*** - OPEN: file #P"/home/user/data.txt" already exists
```

**ファイルが既に存在した場合でも強制的に上書きする**  
```lisp
> (with-open-file (my-stream "data.txt" :direction :output :if-exists :supersede)
      (print "my data" my-stream))
"my data"
```

実はCommon Lispにもファイルをオープンしたりクローズする低レベルコマンドはある。  
`with-open-file`はそれらを隠蔽している。  
もしも`with-open-file`中でエラーが発生してもファイルを確実にクローズして、リソースを開放してくれる。

## 12.3 ソケットを使う

標準的なネットワークにあるコンピュータと通信するためには、ソケットを用意する必要がある。  
ANSI Common Lispの仕様化にソケットの標準化は間に合わなかったため、標準の方法は存在しない。  
ここでは、CLISPののソケットコマンドについて説明する。

### ソケットアドレス

ネットワーク上のソケットには **ソケットアドレス** が割り当てられている。  
ソケットアドレスは、次の2つの要素からなる。

<dl>
  <dt>IPアドレス</dt>
  <dd>ネットワーク上でコンピュータ（厳密にはNIC）を一意に指定する番号</dd>
  <dt>ポート番号</dt>
  <dd>プログラムが、同じコンピュータ上の他のプログラムと区別するために使用する番号</dd>
</dl>

### コネクション

2つのプログラム間でソケットを使ってメッセージをやりとりするには、**コネクション** を初期化する必要がある。

1. 一方のプログラムがソケットを作ってそれをListenすることで、もう一方のプログラムが通信を始めるのを待つ  
   (ソケットをListenするプログラムはサーバと呼ばれている)
2. もう一方のプログラムはクライアントと呼ばれ、自分自身が使うソケットを作った後、サーバとコネクションを確立する

### ソケット上でメッセージを送る

まず、2つのCLISPを立ち上げる。  
一方をクライアント、もう一方をサーバとする。

**NOTE** 必ずCLISPを使用すること。

サーバ側では、`socket-server`を呼ぶことで、指定したポートの使用権を得る。

```lisp
> (defparameter my-socket (socket-server 4321))	; ON THE SERVER
MY-SOCKET
```

このコマンドでは、オペレーティングシステムからポート4321を得て、ソケットをそのポートに束縛する。  
作られたソケットは変数`my-socket`に格納され、この後の例で使えるようになる。  

**NOTE** このコマンドは危険である。  
なぜなら、ソケットを使用し終えた後、自分でOSに返却する必要がある。  
さもなくば、他の誰もソケットに結び付けられたポートを使えなくなる。  
もしも何か手違いがありポートに束縛したソケットをおかしくしてしまったら、新しくソケットを作るときは別のポート番号を選ぶか、コンピュータを再起動しなければならないかもしれない。  
（Common Lispの例外システムにより、この問題を回避することはできる。）  
（CLISPプロセスを一度終了すれば、いずれOSはこのポートを再利用するが、ポートの状態によっては再利用できるようになるまでしばらく時間がかかるかもしれない。）

次に、サーバ側で、このソケットに接続したクライアントとの通信を扱うストリームを作る。  
`socket-accept`を実行すると、サーバ側はREPLプロンプトに戻ってこず、クライアントが接続してくるまでlisten中となる。

```lisp
> (defparameter my-stream (socket-accept my-socket)) ; ON THE SERVER
MY-STREAM
```

次は、クライアント側で`socket-connect`コマンドを使ってサーバのソケットに接続する。  
このコマンドを実行したら、すぐにサーバ側の`socket-accept`関数が戻ってきて、`my-stream`変数がセットされる。

```lisp
> (defparameter my-stream (socket-connect 4321 "127.0.0.1")) ; ON THE CLIENT
MY-STREAM
```

**NOTE** IPアドレス`127.0.0.1`は常に現在のコンピュータ自身を指している特殊なIPアドレスである。

ここでCLISPによって作成されたこれらのストリームは、 **双方向ストリーム** である。  
つまり、入力ストリームとしても出力ストリームとしても振る舞い、通信するためにどちらのストリーム用のコマンドも使用できる。

クライアントからサーバに気軽な挨拶を送ってみる。

```lisp
> (print "Yo Server!" my-stream)
"Yo Server!"
```

そしてサーバ側では次のコマンドを実行する。

```lisp
> (read my-stream)
"Yo Server!"
```

次は、サーバ側で次のようにタイプする。

```lisp
> (print "What up, Client!" my-stream)
"What up, Client!"
```

クライアント側に戻って、これを実行する。

```lisp
> (read my-stream)
"What up, Client!"
```

一連の手順を終えると、サーバ側、クライアント側のプロンプトには次のようになっている。

**サーバ側**  
```lisp
> (defparameter my-socket (socket-server 4321))
MY-SOCKET
> (defparameter my-stream (socket-accept my-socket))
MY-STREAM
> (read my-stream)
"Yo Server!"
> (print "What up, Client!" my-stream)
"What up, Client!"
```

**クライアント側**  
```lisp
> (defparameter my-stream (socket-connect 4321 "127.0.0.1")) ; ON THE CLIENT
MY-STREAM
> (print "Yo Server!" my-stream)
"Yo Server!"
> (read my-stream)
"What up, Client!"
```

今ソケットで送信したのは文字列だったが、他にも標準のLispデータ構造なら何でも全く同じようにやりとりできる。

### 遊んだ後はお片付け（ストリームを閉じる）

以上の例で作成したリソースをきちんと開放しておくことは重要である。  
次のコマンドをクライアントとサーバ双方で実行して、両端のストリームを閉じる。

```lisp
> (close my-stream)
T
```

次に、サーバ側で`socket-server-close`を実行し、ポートを返却してソケットを開放する。  
さもなくば、リブートするまでポート4321が使えなくなる。

```lisp
> (socket-server-close my-socket)
NIL
```

## 12.4 異端児の文字列ストリーム

大抵のストリームは、Lispプログラムが *外の世界* とやりとりするために使うものである。  
しかし、文字列ストリームは例外で、これは単に文字列をストリームのように見せるだけのものである。  
他のストリームが外部のリソースを読み書きするのと同じ方法で、文字列ストリームは文字列を読み書きできる。

文字列ストリームは`make-string-output-stream`と、`make-string-input-stream`で作ることができる。  
次の例では、`make-string-output-stream`を使っている。

```lisp
> (defparameter foo (make-string-output-stream))
> (princ "This will go into foo. " foo)
> (princ "This will also go into foo. " foo)
> (get-output-stream-string foo)
"This will go into foo. This will also go into foo."
```

Lispは文字列を直接操作できるのに、なぜこのような機能が必要なのか？  
しかし、文字列ストリームには利点がある。これらの利点を次に示す。

### 関数にストリームを渡す

ストリームを引数に期待している関数に対して、文字列ストリームを渡すことができる。  
これは、ファイルやソケットを読み書きする関数をデバッグする際にとても役立つ。  
なぜなら、本物のファイルやソケットの代わりに文字列を入出力データとして与えたり受け取ったりできるからである。  

例えば、`write-to-log`という関数があったとする。  
普通はログ情報はファイルストリームへと送って、ファイルにセーブされるようにするだろう。  
しかし、この関数をデバッグする際には、代わりに文字列ストリームを渡してやれば出力された文字列を読むことで動作確認できる。  
`write-to-log`が常にファイルに出力されるようにハードコードしてしまうと、こういった柔軟性がなくなってしまう。  
**関数を書くときは、外部リソースを直接リソースを直接アクセスするのではなく、可能な限りストリームを使うように書いておく方が良い。**

### 長い文字列を作る

非常に長い文字列を作る場合、文字列ストリームを使う方が効率の良いコードになる。  
たくさんの文字列を1つずつ繋いでいくのは非常に効率が悪くなる。これは、文字列を繋ぐ度に文字列用のメモリをアロケートするからだ。  
**NOTE** このため、多くのプログラミング言語では **文字列ビルダ** と呼ばれる機能を用意して、このオーバヘッドを避けている(Javaの`StringBuilder`など)。

### コードの読みやすさとデバッグ

文字列ストリームを、特に`with-output-to-string`と一緒に使うと、読みやすくデバッグしやすいコードが書ける。  
ここで、`with-output-to-string`を使ったコードを次に示す。

```lisp
> (with-output-to-string (*standard-output*)
    (princ "the sum of ")
    (princ 5)
    (princ " and ")
    (princ 2)
    (princ " is ")
    (princ (+ 2 5)))
"the sum of 5 and 2 is 7"
```

`with-output-to-string`マクロは、コンソール、REPL、他のストリームに向かうはずだった出力を横取りして、それを文字列として格納して返す。  
上の例では`with-output-to-string`の本体内で`princ`により出力されるデータが自動的に文字列ストリームへと向けられる。  
`with-output-to-string`の本体の実行が終わると、文字列ストリームに蓄積された出力が文字列として返される。

`with-output-to-string`は、また、長く複雑な文字列を組み立てるときにも使える。  
本体中で文字列の部分部分を`print`していって、最後に集められた出力を文字列として得られる。  
文字列の断片を`concatenate`していくよりも読みやすく効率の良いコードになる。

**NOTE** `with-output-to-string`は関数プログラミングの精神とは逆行している。


