---
title: "書籍 Land of Lisp 13章 Webサーバを作ろう！"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
toc: true
tags: ["lisp"]
categories: ["Land-of-Lisp"]
authors:
- otaon
---

## 13.1 Common Lispでのエラー処理

Webサーバのように外部とやりとりする場合、予想外の自体が起きる可能性がある。  
Common Lispにはコード内で例外を扱う機能が豊富に備わっている。  
Common Lispの例外システムは柔軟である。

### コンディションを通知する`(error)`

関数内で何か問題が起きた時、Lisp関数はLispの実行環境に問題が発生したことを伝える。
この手段が **コンディションを通知する** ことである。
(コンディションは、他の言語では例外(exception)と呼ばれるオブジェクトと同じようなもの)

自分で書いたコードで、どうしても処理を続けられない場合が、コンディションを通知するときである。  
自分の書くコードから直接コンディションを通知するには、`error`コマンドを使う。
`error`コマンドは、他の場所でエラーを横取りしていなければ、Lispプログラムの実行を中断する。  

コンディションを通知して、エラーの説明メッセージとして"foo"を表示してみる。

```lisp
> (error "foo")
*** - foo
The following restarts are available:
ABORT       R1:      Abort main loop
>
```

上の例の通り、コンディションの通知によってLispシステムはプログラムを中断し、メッセージ"foo"を出力した後、REPLにエラープロンプトを表示する。
(CLISPでは、この時点で`:a`をタイプすればプログラムの実行を放棄して通常のREPLに戻る。)

### 自前のコンディションを作る`(define-condition)`

最初の例では、コンディションを説明する文字列を`error`関数に渡した。
しかし、単にテキストでエラーメッセージを表示するだけでは、どういったコンディションかを判断するのは難しい。
そこで、Common Lispでは、コンディションの型を定義して、その型に応じて異なる処理をすることができる。

最初に次の例のように`define-condition`でコンディションの型を定義する。
ここではコンディションを`foo`と名付けた。

```lisp
> (define-condition foo () ()
    (:report (lambda (condition stream)
               (princ "Stop FOOing around, numbskull!" stream))))
FOO
```

定義したコンディションが通知されたときにどう表示されるかを制御する、専用の関数を定義できる。  
上の例では、`lambda`を使ってその関数を定義した。
`lambda`関数の中では、専用のエラーメッセージを表示するようにした。

このコンディションを通知してみる。

```lisp
[5]> (error 'foo)

*** - Stop FOOing around, numbskull!
The following restarts are available:
ABORT          :R1      Abort main loop
Break 1 [6]> :a
[7]>
```

この通り、専用のメッセージが表示された。この方法を使えば、コンディションの型に応じてより分かりやすいメッセージを表示できる。

### コンディションを横取りする`(handler-case)`

`define-condition`でコンディション型を定義したときに名前（上の例では`foo`）を与えた。
この名前を使えば、この型のコンディションが通知されたときに、プログラムを中断する代わりに実行する処理を、プログラムの上位層で書いておくことができる。
そのためのコマンドが`handler-case`である。

`handler-case`コマンドの第1引数には、横取りしたいコンディションを通知するかもしれないコードを与える。（下の例では`bad-function`）  
`handler-case`の残りの部分には、特定のコンディションが通知されたときに何をすべきかを列記する。

```lisp
> (defun bad-function ()
     (error 'foo))
BAD-FUNCTION
> (handler-case (bad-function)
     (foo () "somebody signaled foo!")
     (bar () "somebody signaled bar!"))
"somebody signaled foo!"
```

この`handler-case`が呼び出されると`bad-function`が呼び出され、その中の`(error 'foo)`によって`foo`コンディションが通知される。  
もし`handler-case`がなかったら、この時点でプログラムが中断されてREPLにエラープロンプトが表示されることになっていたが、
この例では、`handler-case`が`foo`コンディションを横取りして、プログラムは中断されることなく、`"somebody signaled foo!"`という結果が返る。

### 予想外のコンディションからリソースを保護する`(unwind-protect)`

予想外の例外が発生した場合、プログラムがクラッシュしたり、下手すると外部のリソースを壊してしまう。  
例えば、ファイルやソケットストリームに何かを書いている最中に例外が発生したと想定する。
この時、ストリームを正しくクローズしてファイルハンドルやソケットを解放してやる必要がある。
リソースが正しい手順でクリーンアップされないと、そのリソースをユーザが再び使いたい場合はコンピュータをリブートする必要がある、という場合もある。

このような「想定外のコンディションからリソースを保護する」ために使うのが、`unwind-protect`コマンドである。
このコマンドは、Common Lispコンパイラに「このコードだけは絶対に実行しろ」と伝えるものである。  
下記の通り、`unwind-protect`の中でゼロ除算を行った場合、コンディションを通知する。  
しかし、エラープロンプトからCLISPに実行の放棄を指示した後、重要なメッセージが表示されていることがわかる。

```lisp
> (unwind-protect (/ 1 0)  ; division by zero
    (princ "I need to say 'flubyduby' matter what"))
*** - /: division by zero
The following restarts are available:
ABORT          :R1      Abort main loop
Break 1 [8]> :r1
I need to say 'flubyduby' matter what
[9]> 
```

Common Lispの`with-`マクロを使っている場合、そのマクロが内部で`unwind-protect`を呼んでくれることが多いため、直接`unwind-protect`を使用する場面はあまりない。
（16章では`unwind-protect`のようなマクロを実際に作成する）

## 13.2 ゼロからWebサーバを書く

### Webサーバの仕組み

HTTP(Hypertext Transfer Protocol)は、Webページをやりとりするために使われるインターネットのプロトコルである。
確立されたソケットコネクションを通じて、TCP/IPの上でページをやりとりするを定義している。
クライアント上で走っているプログラム(Webブラウザなど)が定められた形式に沿ったリクエストを送ると、サーバは要求されたページを作り出して、ソケットストリームを通じてレスポンスを返す。

**NOTE:** このWebサーバはRon Garretのhttp.lispを元にしている。

例えば、ブラウザがクライアントとして、`lolcats.html`というページを要求したとする。
リクエストメッセージは次のような内容になっているはずである。
これらのサーバに送られるメッセージ全体は **リクエストヘッダ** と呼ばれる。

```http
GET /lolcats.html HTTP/1.1
Host: localhost:8080
User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.5)
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-us,en;q=0.5
Accept-Encoding: gzip,deflate
Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7
Keep-Alive: 300
Connection: keep-alive
```

最初の行は **リクエストライン** と呼ばれる。
ここには、リクエストの種類(GET)と、要求するページの名前(lolcats.html)が含まれている。

```http
GET /lolcats.html HTTP/1.1
```

2行目以降は、 **HTTPヘッダフィールド** と呼ばれる。
行頭からコロンまでの箇所にヘッダ、コロンの右側に内容がある。

```http
Host: localhost:8080
User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.5)
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-us,en;q=0.5
Accept-Encoding: gzip,deflate
Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7
Keep-Alive: 300
Connection: keep-alive
```

リクエストヘッダに続いて、 **リクエストボディ** と呼ばれる部分を使って他の情報を送ることもできる。

サーバは、クライアントからリクエストを受け取ったら、 **レスポンスヘッダ** (Webブラウザは受け取ったドキュメントに関する追加情報)と **レスポンスボディ** (Webページを表現するHTMLドキュメント)を返信する。
ただし、今回作っているWebサーバでは、ヘッダを生成せずにただボディだけを返す。

**レスポンスボディ** の一例を示す。

```html
<html>
  <body>
    Sorry dudez, I don't have any LOLZ for you today :(
  </body>
</html>
```

### リクエストパラメータ

ここで、Webサイトに次のログインフォームを作ることを考える。

```
--------------------------------
| userid    [                ] |
| password  [                ] |
|                     [submit] |
--------------------------------
```

サイトを訪れた人がSubmitボタンをクリックすると、ブラウザはPOSTリクエストやGETリクエストをWebサーバに送信する。  

#### POSTリクエストパラメータ

POSTリクエストは前節で説明したGETリクエストによく似ている。
ただ、POSTリクエストはサーバにあるデータに変更を加えたいときに使われる。

今のログインフォームの例では、訪問者がフォームのテキストフィールドに記入したユーザIDとパスワードをサーバに送る必要がある。
フィールドに記入された値は、POSTリクエストの **リクエストパラメータ** として送られる。
つまり、POSTリクエストヘッダの後ろにある、リクエストボディに当たる部分が使われる。

次に、このログインフォームによって送られるPOSTリクエストの例を示す。

```http
POST /lolcats.html HTTP/1.1
Host: www.mywebsite.com
User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.5)
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-us,en;q=0.5
Accept-Encoding: gzip,deflate
Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7
Keep-Alive: 300
Connection: keep-alive
Content-Length: 39

userid=foo&password=supersecretpassword
```

最後の行は、リクエストパラメータである。  
POSTリクエストのヘッダに追加の情報が付加されている。
Content-Lengthは、リクエストのボディに含まれるデータの長さを表す。
ここではContent-Length: 39となっているので、リクエストパラメータの大きさが39バイトであることをサーバに知らせている。

#### GETリクエストパラメータ

リクエストパラメータは主としてPOSTリクエストでサーバにデータを送るために使われている。
しかし、GETリクエストにもリクエストパラメータを入れることもできる。
POSTリクエストでは、パラメータはリクエストボディの中に隠されているが、GETリクエストでは、パラメータはリクエストのURLに含まれる。

例えば、Googleで"dogs"と検索したい場合、リクエストされるページのURLに`?q=dogs`といった値が入っている。
これがリクエストパラメータである。

ここで作るWebサーバは、POSTリクエストパラメータと、GETリクエストパラメータの両方とも扱えるようにする。

### リクエストパラメータから値を取り出す

HTTPでフォームのデータを送る場合、通常のアルファベット以外の文字はHTTPエスケープコードと呼ばれる特殊形式に変換される(RFC3986)。
エスケープコードを使うことで、HTTPフォーマットでは特別な文字を持つような文字もデータとして送ることができる。

例えば、ユーザが`foo?`とテキストフィールドにタイプした場合、リクエストには`foo%3F`という文字列が送られる。
ここではクエスチョンマークがエスケープされている。
Webサーバは、このようなエスケープされた文字をデコードできなければならない。

では、デコードする関数を次に示す。

**英語版リクエストパラメータデコーダ**

```lisp
(defun http-char (c1 c2 &optional (default #\Space))
  "16進数で表されたASCIIコードをデコードする
   c1: 2桁目の数値となる文字
   c2: 1桁目の数値となる文字"
  ;; 16進数の文字列を整数へと変換する
  (let ((code (parse-integer
                (coerce (list c1 c2) 'string)
                :radix 16            ; 数の基数を指定
                :junk-allowed t)))	 ; 数値の解釈を失敗した時、エラー通知ではなくnilを返す
    ;; 整数への変換が成功したら、そのコードに対応した文字を返す
    ;; 整数への変換が失敗したら、default値を返す
    (if code
        (code-char code)
        default)))


(defun decode-param-en (s)
  "httpエスケープされているリクエストパラメータをデコードする(ASCIIコードのみ対応)"
  ;; f: 文字のリストを再帰的に処理するローカル関数
  (labels ((f (lst)
              (when lst
                ;; 文字が%なら、次に2桁の16進数で表されるASCIIコードをデコードする
                ;; 文字が+なら、空白文字として解釈する
                ;; 他の文字なら、そのまま出力する
                (case (car lst)
                    ;; リストの先頭の文字を処理し、残りの文字列（処理済み）と組み合わせる
                    (#\% (cons (http-char (cadr lst) (caddr lst))
                               (f (cdddr lst))))
                    (#\+ (cons #\space
                               (f (cdr lst))))
                    (otherwise (cons (car lst)
                               (f (cdr lst))))))))
    ;; リストの要素を文字列として結合する
    (coerce (f (coerce s 'list)) 'string)))
```

**日本語版リクエストパラメータデコーダ**

```lisp
;; 文字ごとではなく、バイトごとにデコードする(URLの正式なエンコーディング準拠)
(defun http-byte (c1 c2 &optional (default #\Space))
  "16進数で表された文字をバイト数値にデコードする
   c1: 2桁目の数値となる文字
   c2: 1桁目の数値となる文字"
  ;; 16進数の文字列を整数へと変換する
  (let ((code (parse-integer
                (coerce (list (code-char c1) (code-char c2)) 'string)
                :radix 16            ; 数の基数を指定
                :junk-allowed t)))	 ; 数値の解釈を失敗した時、エラー通知ではなくnilを返す
    ;; 整数への変換が成功したら、そのコードに対応したバイト数値を返す
    ;; 整数への変換が失敗したら、default値を返す
    (or code default)))


(defun decode-param-ja (s)
  "httpエスケープされているリクエストパラメータをデコードする(マルチバイト文字対応)"
  ;; f: 文字のリストを再帰的に処理するローカル関数
  (labels ((f (lst)
              (when lst
                ;; 文字が%なら、次に2桁の16進数で表されるASCIIコードをデコードする
                ;; 文字が+なら、空白文字として解釈する
                ;; 他の文字なら、そのまま出力する
                (case (car lst)
                    ;; リストの先頭の文字を処理し、残りの文字列（処理済み）と組み合わせる
                    (#.(char-code #\%) (cons (http-byte (cadr lst) (caddr lst))
                                             (f (cdddr lst))))
                    (#.(char-code #\+) (cons #.(char-code #\space)
                                             (f (cdr lst))))
                    (otherwise (cons (car lst)
                               (f (cdr lst))))))))
    ;; リストの要素を文字列として結合する
    (ext:convert-string-from-bytes
      (coerce (f (coerce (ext:convert-string-to-bytes s charset:utf-8) 'list)) 'vector)
      charset:utf-8)))
```

**NOTE:** CLISPで端末のエンコーディングを設定するには、下記コマンドを使う。

```lisp
;; charsetには下記などが使える。
;; charset:utf-8
;; charset:euc-jp
;; charset:shift-jis
> (setf *terminal-encoding* charset:utf-8)
```

**NOTE:** Webサーバで日本語を表示するためには、ソケットの文字エンコーディングも指定する必要がある。
`serve`コマンド（後述）を起動する前に、REPL上で次のコマンドを実行すること。

```lisp
> (setf *default-file-encoding* charset:utf-8)
#<ENCODING CHARSET:UTF-8 :UNIX>
```

**NOTE:** ここで扱っているHTTPエスケープコードは、Lisp文字列のエスケープ文字とは無関係。

### リクエストパラメータのリストをデコードする

リクエストパラメータには、"name=bob&age=25&gender=male"といった具合に、名前/値の組が複数含まれている。  
このようなパラメータは、Webページの末尾にもよく含まれている。  
ここでは、これらの組をリストとして取り出す。  
データ構造としては連想リスト(alist)と同じである。
そこで、リクエストパラメータの文字列を解釈してalistを返す関数を作る。

```lisp
(defun parse-params (s)
  "リクエストパラメータのalistを返す
   s: リクエストパラメータの文字列
   ret: リクエストパラメータのalist"
  (let ((i1 (position #\= s))	; リクエストパラメータ中の=の位置
        (i2 (position #\& s)))  ; リクエストパラメータ中の&の位置
    (cond (i1 (cons	; 名前と値の各コンスセルをコンスする
                (cons (intern (string-upcase (subseq s 0 i1)))	; car部：名前をシンボルに変換したもの
                      (decode-param (subseq s (1+ i1) i2)))		; cdr部：値のhttpエスケープをデコードしたもの
                (and i2 (parse-params (subseq s (1+ i2))))))	; 残りのリクエストパラメータに対して処理
          ((equal s "") nil)	; リクエストパラメータが空になったらリストを閉じるためにnilを返す
          (t s))))	; リクエストパラメータの書式ではない文字列の場合、文字列をそのまま返す
```

`decode-param`では、文字列を文字のリストとして変換してから処理した。
`parse-params`では、文字列をそのまま扱う。

`position`関数は、文字列から指定した文字を探してその位置を返す関数である。
これを使って、渡された文字列から`&`と`=`の位置を求めている。

`i1`が`nil`でない、つまり、`=`が見つかったら、それは文字列中に名前/値のペアが見つかったということになる。
この場合、`subseq`を使って名前と値それぞれを切り出す。
名前部分については`intern`関数を使って文字列をLispのシンボルに変換する。
値部分についてはhttpエスケープをデコードする。

これらを実行すると、次のような結果になる。
このようにリクエストのパラメータををalistに治すことで、後から特定のパラメータの値を取り出しやすくなる。

```lisp
> (parse-params "name=bob&age=25&gender=male")
((NAME . "bob") (AGE . "25") (GENDER . "male"))
```

**NOTE:** 上の`parse-param`関数では、簡略化のために、名前部分がエスケープされている可能性を無視していることに注意。

### リクエストヘッダを解析する

#### リクエストラインを解析する

次は、リクエストヘッダの最初の行(リクエストライン)である、`GET /lolcats.html HTTP/1.1`といった文字列を解析する。
次に示す`parse-request-line`関数によって行う。

```lisp
(defun parse-request-line (s)
  "リクエストヘッダのリクエストラインからURLを取り出す
   s: リクエストライン
   ret: url本体部とリクエストパラメータ部とのコンスセル"
  (let* ((url (subseq s
                      (+ 2 (position #\space s))          ; スペース位置から2つ進んだ箇所(`/`の次)
                      (position #\space s :from-end t)))  ; 文字列の後ろから見てスペースのある箇所
         (x (position #\? url)))  ; URL中のリクエストパラメータの開始位置
    (if x    ; リクエストパラメータがある
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))    ; url本体部とリクエストパラメータ部とのコンスセル
        (cons url '()))))    ; url本体部と空リストとのコンスセル
```

この関数では、まず、リクエストヘッダのリクエストラインを受け取り、最初にスペースを探し出して、URL部分を抜き出す。  
次に `?`を探し、もし存在すればそれ以降はリクエストパラメータなので、切り出して`parse-params`に渡す。  

```lisp
GET /lolcats.html HTTP/1.1
     ^^^^^^^^^^^^
     car部
> (parse-request-line "GET /lolcats.html HTTP/1.1")
("lolcats.html")
> (parse-request-line "GET /lolcats.html?extra-funny=yes HTTP/1.1")
("lolcats.html" (EXTRA-FUNNY . "yes"))
```

#### HTTPヘッダフィールドを解析する

次に、リクエストヘッダのHTTPヘッダフィールドを処理する。
次に示す`get-header`は、リクエストヘッダの残りの行を読み込んでalistにして返す関数である。

```lisp
(defun get-header (stream)
  "リクエストヘッダのHTTPヘッダフィールドからリクエストパラメータを返す
   stream: HTTPヘッダフィールド
   ret: リクエストパラメータと値とのコンスセル"
  (let* ((s (read-line stream))  ; 入力ストリームから得た文字列1行分
         (h (let ((i (position #\: s)))  ; コロンの位置
              (when i	; コロンがある場合、コロンを区切りとしたリクエスト名/値のコンスセルを作る
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    ;; コンスセルができたら、残りのリクエストも処理する
    ;; コンスセルができなかったら、それ以降はリクエストは無いなずなので、処理を終わる
    (when h
      (cons h (get-header stream)))))
```

#### 文字列ストリームを使って`get-header`をテストする

`get-header`関数はソケットストリームから直接データを読み込む想定である。
したがって、そのままではREPLでテストできない……と思うかもしれない。  
ここで、前章でやったことを利用する。

Common Lispでは、ソケット以外にも異なる種類のリソースを扱う何種類化のストリームが有る。
ストリームはどれも同じインターフェースでアクセスできるため、ソケットストリームの代わりに文字列ストリームを渡して、`get-header`をテストできる。

```lisp
> (get-header (make-string-input-stream "foo: 1
bar: abc,123

"))
((FOO . "1") (BAR . "abc,123"))
```

`make-string-input-stream`関数で、リテラル文字列から入力ストリームを作り出している。
この例では、文字列は2つのキー(fooとbar)を含み、HTTPヘッダの形式通り、空行で終わっている。
(Common Lispではリテラル文字列を複数行に渡って書くことができる。)

### (POSTリクエストの場合)リクエストボディの解析

POSTリクエストでは、パラメータはリクエストヘッダの後、リクエストボディやリクエストコンテントと呼ばれる領域を使って送られる。  
次の`get-content-params`関数によって、そこからパラメータを取り出す。

```lisp
(defun get-content-params (stream header)
  "リクエストヘッダの後にあるリクエストボディから、パラメータを取り出す
   stream: ストリーム
   header: HTTPヘッダフィールドの連想リスト"
  (let ((length (cdr (assoc 'content-length header))))  ; HTTPヘッダフィールドからコンテンツの長さを取得する
    ;; もしcontent-lengthがHTTPヘッダフィールドにあれば、リクエストパラメータの連想リストを作る
    (when length
      (let ((content (make-string (parse-integer length))))  ; 与えられた長さの文字列を`make-string`で作成する
        (read-sequence content stream)  ; ストリームからデータを読み込んで、contentを満たす
        (parse-params content)))))      ; リクエストパラメータの連想リストを作る
```

この関数は、リクエストボディに含まれるパラメータの長さを示す`content-hength`ヘッダを探す。
もし`content-length`ヘッダがリクエストヘッダに見つかれば、処理すべきリクエストパラメータが存在するということになる。
その場合、与えられた長さの文字列を`make-string`で作成し、`read-sequence`を使ってストリームからデータを読み込む。
最後に、読み込まれた文字列に対して`parse-params`を使って、リクエストパラメータの連想リストを作る。

### 最後の仕上げのサーバ関数

ここまでで必要な機能は実装した。
ここでは、Webサーバの核となる`serve`関数を実装する。
この関数は、引数にとったリクエストハンドラに、パス、HTTPヘッダフィールド、パラメータを使った処理を委譲する。

```lisp
(defun serve (request-handler)
  "request-handler: リクエストハンドラ。解析したリクエストを使う。"
  (let ((socket (socket-server 8080)))  ; サーバのポート番号
    (unwind-protect  ; 例外時にソケットが確実に閉じられるようにする
      (loop (with-open-stream (stream (socket-accept socket))  ; 接続が確立したらソケットオブジェクトをstreamにセットする
              (let* ((url    (parse-request-line (read-line stream)))  ; streamからURLとリクエストパラメータを得る
                     (path   (car url))            ; URLのパス部
                     (header (get-header stream))  ; HTTPヘッダフィールド
                     (params (append (cdr url)     ; URL末尾(GET用)とリクエストボディ(POST用)のリクエストパラメータ
                                     (get-content-params stream header)))
                     (*standard-output* stream))   ; ストリームを標準出力に設定
                (funcall request-handler path header params))))  ; 
      (socket-server-close socket))))
```

## 13.3 動的なWebサイトを作る

ここまでで作ったWebサーバを動かしてみる。

```lisp
(defun hello-request-handler (path header params)
  "名前を問いかけて、得られたその名前を使って挨拶する
   CAUTION! リクエストパラメータをサニタイズしていないため、WANでの使用不可
   path: URLのパス部分
   header: HTTPヘッダフィールド
   params: URL末尾(GET用)とリクエストボディ(POST用)のリクエストパラメータ
   ret: レスポンスするHTMLドキュメント"
  (declare (ignore header))  ; 本関数ではHTTPヘッダフィールドは無視する
  ;; "/greeting"ページのみ提供する
  (if (equal path "greeting")
      ;; ページが"greeting"ならパラメータに合わせて表示処理を行う
      (let ((name (assoc 'name params)))
        (if (not name)
            ;; パラメータにnameが無ければ、もう一度名前を問いかける
            (princ "<html><form>What is your name?<input name='name' /></form></html>")
            ;; パラメータにnameがあれば、挨拶を表示する
            (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      ;; ページが"greeting"でなければ、要求されたページが無い旨を表示する
      (princ "Sorry... I don't know that page.")))
```


