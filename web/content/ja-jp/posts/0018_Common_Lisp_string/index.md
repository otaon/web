---
title: "Common Lisp 文字列操作方法を学ぶ"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
toc: true
tags: ["lisp", "basic"]
categories: ["Notes"]
authors:
- otaon
---

# 本記事の目的
この記事では、文字列表示、および、文字列操作において基本的な関数の使い方について説明する。

# 文字列出力用の関数
## `print`関数
`print`関数は下記の順で文字を表示する。

1. 現在の表示位置が行頭でない場合、改行する
1. 引数を表示する
1. 空白を表示する

例:

```cl
(progn (print "this")
       (print "is")
       (print "a")
       (print "test"))
; => "this"_
;    "is"_
;    "a"_
;    "test"_
; (_ = 空白)
```

## `prin1`関数
`prin1`関数は下記の順で文字を表示する。

1. 引数を表示する

例:

```cl
(progn (prin1 "this")
       (prin1 "is")
       (prin1 "a")
       (prin1 "test"))
; => "this""is""a""test"
```

## `princ`関数
人間が見やすい形式で文字を表示する。

例:

```cl
(princ '3) ; => 3
(print '3) ; => 3

(princ '3.4) ; => 3.4
(print '3.4) ; => 3.4

(princ 'foo) ; => FOO
(print 'foo) ; => FOO

(princ '"foo") ; => foo
(print '"foo") ; => "foo"

(princ '#\a) ; => a
(print '#\a) ; => #\a
```

## `#\newline`
`#\newline`を用いることで、文字列中に改行を入れられる。

```cl
(progn (princ "This sentence will be interrupted")
       (princ #\newline)
       (princ "by an annoying newline character."))
; => This sentence will be interrupted
;    by an annoying newline character.
```

## `prin1-to-string`関数
シンボルを含むリストを文字列に変換する。

```cl
(print1-to-string '(a b c d))
; => "(A B C D)"

(cons 'foo (list (prin1-to-string '(a b c d))))
; => (FOO "(A B C D)")
```

## `princ-to-string`関数
上記の`prin1-to-string`関数の出力を、人間が読みやすい形式にしたもの。

```cl
(princ-to-string '(a b c d))
; => "(A B C D)"

(princ-to-string '(a b "c" d))
; => "(A B c D)"
```

## `write-to-string`関数
`prin1-to-string`関数や`printc-to-string`関数の一般形。

```cl
(write-to-string object :escape t)
; =
(print1-to-string object) ; escape文字が出力される

; 例
(write-to-string "abc" :escape t)
; => "\"abc\""

(write-to-string object :escape nil :readably nil) ; escape文字が出力されない
; =
(princ-to-string object)

; 例
(write-to-string "abc" :escape nil :readably nil)
; => "abc"
```

# 文字入力用の関数
## `read`関数
標準入力から得たものを返す。文字列からS式になる。

構文:

```cl
(read &optional input-stream eof-error-p eof-value recursive-p) ; => Object
```

<dl>
<dt>&optional</dt>
<dd>ここから右の引数は任意。</dd>

<dt>input-stream</dt>
<dd>ストリーム指定。<br>
    デフォルト : 標準入力</dd>

<dt>eof-error-p</dt>
<dd>eof検出時の処理指定。<br>
    t   : デフォルト。エラーを吐く。<br>
    nil : エラーを吐かない。</dd>

<dt>eof-value</dt>
<dd>eof検出時の返り値指定。<br>
    デフォルト : nil</dd>

<dt>recursive-p</dt>
<dd>これがnil以外の場合、readを再帰呼び出しする。<br>
    デフォルト : nil</dd>
</dl>

例:

```cl
(read)
; a
; => A

(read)
; 'a
; => 'A

(read)
; (a)
; => (A)

(read)
; "a"
; => "A"

; readの挙動について細かく見るための例を以下に示す。
(setq a "line 1
   line2")
; 1の後ろに改行文字があることに注意

; => "line 1
;    line2"

(read (setq input-stream (make-string-input-stream a)))
; => LINE

(read input-stream)
; => 1

(read input-stream nil nil)
; => LINE2

(read input-stream nil nil)
; => NIL
```

## `read-line`関数
入力から1行ずつ得たものを文字列として返す。

**NOTE:** newlineからeofまで見る。

**NOTE:** 
`read-line`関数は、実行するたびに新規に文字列を生成する。
したがって、大量の行を読み込むとガベージコレクションの実行時間が長くなりすぎる。
`read-sequence`を使用する方が実用的である。
しかし、`read-sequence`は使用が面倒である。

構文:

```cl
(read-line &optional input-steam eof-error-p eof-value recursive-p) ; => line, missing-newline-p
```

<dl>
<dt>&optional</dt>
<dd>ここから右の引数は任意。</dd>

<dt>input-stream</dt>
<dd>ストリーム指定。<br>
    デフォルト : 標準入力</dd>

<dt>eof-error-p</dt>
<dd>eof検出時の処理指定。<br>
    t   : デフォルト。エラーを吐く。<br>
    nil : エラーを吐かない。</dd>

<dt>eof-value</dt>
<dd>eof検出時の返り値指定。<br>
    デフォルト : nil</dd>

<dt>recursive-p</dt>
<dd>これがnil以外の場合、readを再帰呼び出しする。<br>
    デフォルト : nil</dd>

<dt>返り値2: missing-newline-p</dt>
<dd>次の行があるか否かを表す。<br>
    read-line実行時にnewlineで終わっていたらnilを返す。<br>
    eofで終わっているか、第1引数がeof検出時の返り値ならtを返す。</dd>
</dl>

**例:**

```cl
(read-line)
; abc
; => "abc";
;    NIL

(read-line)
; "abc"
; => "\"abc\"";
;    NIL

(setq a "line 1
   line2")
; 1の後ろに改行文字があることに注意

; => "line 1
;    line2"

(read-line (setq input-stream (make-string-input-stream a)))
; => "line 1";
;    NIL
(read-line input-stream nil nil)
; => "line2";
;    T
(read-line input-stream nil nil)
; => NIL;
;    T
```

## `read-from-string`関数
文字列から、それを入力とみなして返す。

構文:

```cl
(read-from-string string 
                  &optional 
                  eof-error-p 
                  eof-value 
                  &key 
                  start 
                  end 
                  preserve-whitespace)
; => Objects, position
```

<dl>
<dt>eof-error-p</dt>
<dd>eof検出時の処理指定。<br>
    t   : デフォルト。エラーを吐く。<br>
    nil : エラーを吐かない。</dd>
<dt>eof-value</dt>
<dd>eof検出時の返り値指定。<br>
    デフォルト : nil</dd>
<dt>start end</dt>
<dd>文字列の左端と右端を指定する。<br>
    デフォルト : start = 0, end = nil</dd>
<dt>preserve-whitespace</dt>
<dd>whitespaceを保持するか否かを指定する。<br>
    t   : 保持する。<br>
    nil : デフォルト。保持しない。</dd>
<dt>返り値 position</dt>
<dd>読まれなかった文字列の最初の箇所を指す。<br>
もし全体が読み込まれた場合、positionは文字数か、文字数 + 1を示す。</dd>
</dl>

例:

```cl
(setq foo "(a b c d)")
; => FOO

(read-from-string foo)
; => (A B C D);
     9
```

# 文字列編集用の関数
## `string-trim`関数
第1引数の文字を、第2引数の文字列の両端から除く。
第1引数の文字は1つずつ独立して評価される。
つまり、第1引数に"abc"が指定されている場合、
両端から「a」「b」「c」の文字のいずれにもマッチしない文字が現れるまで文字を取り除き続ける。

```cl
(string-trim "abc" "abcaakaaaabckabcbaaa")
; "abcaakaaaabckabcbaaa"
;  ~~~~~kaaaabck~~~~~~~ (~の部分が除かれる)
; => kaaaabck

(string-trim '(#\Space #\e #\t) " trim me ")
; => "rim m"
```

**NOTE:** スペース、タブ、改行を削除対象に指定したい場合は下記のようにする。

```cl
; 下記のリストを指定する。
(string-trim '(#\Space #\newline #\tab #\IDEOGRAPHIC_SPACE) str)
```

## `string-left-trim`関数
第1引数の文字を、第2引数の文字列の左端から除く。

## `string-right-trim`関数
第1引数の文字を、第2引数の文字列の右端から除く。

## `concatenate`関数
文字列やリストを結合する。

```cl
(concatenate 'string "Hello " "world.")
; => "Hello World."

(concatenate 'list '(a b) '(c d) '(e f))
; => (A B C D E F)
```

**NOTE:** ただし、リストの場合は`append`を用いれば良い。

```cl
(append '(a b) '(c) '(d))
; => (A B C D)
```

## `substitute-if`関数

与えられたテスト関数の結果によって値を置き換える。
(substitute : 〜を代用する。代理をする。)

構文:

```cl
(substitute-if new-item predicate sequence &key from-end start end count key)
; => result-sequence
```
<dl>
<dt>new-item</dt>
<dd>置換後の要素。</dd>
<dt>predicate</dt>
<dd>述語。#'関数を描く要素に照らし合わせて真偽を判断する。</dd>
<dt>sequence</dt>
<dd>proper sequence(適切なシーケンス)<br>
    1次元配列や、popしていくと()になるリスト。<br>
    つまり、巡回リスト以外のリスト、および、最後のセルの参照先が非nil以外のリスト。</dd>
<dt>count</dt>
<dd>置換する要素数の上限。<br>
    デフォルト : nil</dd>
<dt>from-end</dt>
<dd>countがセットされていれば働く。右端から上限判定を行う。<br>
    デフォルト : false</dd>
<dt>start end</dt>
<dd>置換範囲は、startより大きく、end以下までの中に制限される。<br>
    デフォルト : start = 0, end = nil<br>
    例: :start 2 :end 5 なら (x x o o o x x)</dd>
</dl>

例:

```cl
(substitute-if #\e #'digit-char-p "I'm a l33t hack3r!")
; => I'm a leet hacker!
```

## `subseq`関数
引数の文字列から、両端の各々任意の文字数を取り除いた文字列を返す。
**NOTE:** endの一つ前までしか表示されないことに注意。

構文:

```cl
(subseq sequence start-position end-position)
; end-position は optional で、デフォルトではnil
```

例:

```cl
; "h e l l o   w o r l d"
;  0 1 2 3 4 5 6 7 8 9 10

(subseq "hello world" 3)
; => "lo world"

(subseq 3 5)
; => "lo"
```

## `length`関数
文字列の長さを返す。

```cl
(length "abcdefg")
; => 7
```

## `fresh-line`関数
行頭に出力ストリームがない場合、newlineを出力する。

```cl
; 下記のようなプロンプトの状態になっているときを考える。
; 前の出力 CLISP>

(fresh-line)
; => プロンプトが下記のように改行される。
; 前の出力
; CLISP>
```

## `terpri`関数
出力ストリームにnewlineを出力する。

# ファイル操作用の関数
## `with-open-file`関数
関数内でfileをopenし、streamで制御する。
処理終了時には、fileをcloseする。

構文:

```cl
(with-open-file (my-stream
                 "file-name.txt"
                 :direction :output
                 :if-exsts :supersede)
   (procedure))
```

<dl>
<dt>my-stream</dt>
<dd>ストリーム名の指定</dd>
<dt>"file-name.txt"</dt>
<dd>open対象のファイル名</dd>
<dt>:direction</dt>
<dd>方向(入出力)の指定<br>
    output : 出力<br>
    input  : 入力</dd>
<dt>:if-exsts</dt>
<dd>ファイルが既存のとき、<br>
    :supersede : 置き換える</dd>
<dt>procedure</dt>
<dd>ファイルオープン後の処理</dd>
</dl>

例:

```cl
(with-open-file (my-stream
                 "file-name.txt"
                 :direction :output
                 :if-exsts :supersede)
   (princ "Hello File!" my-stream))
; => princでmy-streamに投げた文字列を、そのmy-streamに関連付けたファイルに書き出す。
```
