---
title: "Common Lisp 基本構文を学ぶ"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
toc: true
tags: ["lisp", "basic"]
categories: ["lisp"]
authors:
- otaon
---

# 本記事の構成
- 001_Common-Lisp-基本構文説明.md
- 002_Common-Lisp-リスト操作説明.md
- 003_Lisp処理系起動時のディレクトリパス-カレントディレクトリパスを取得-設定する.md

# 目的
- Common Lisp の基本的なシンタックスをすぐに確認するため。
- 自分用の備忘録。

# 前提条件
- Common Lisp および Lisp が、括弧だらけのプログラミング言語だと知っていること。  
- Common Lisp の実行環境を持っていること。(CLisp、Steel Bank Common Lisp など)

# Lisp = ListProcessor (List構造を理解しよう)
## LispとAST
LispはAST(抽象構文木)を直接記述するかの如くプログラミングする言語である。
換言すれば、ソースコードが殆どASTと等価であると見做せる。
また、Lispは、そのインタプリタによって、逐次的にソースコードを読み込み、評価し、表示する。
そして、この手順を繰り返してソースコード全体を処理する。
この仕組みを、REPL(Read–Eval–Print Loop)という。

## コンスセル
上記のとおり、Lispのソースコードは殆どASTであるから、データ構造も命令と同様の表現を取る。
即ち、全ての表現を同様に記述できる。
これを実現するために、Lispでは「コンスセル」という概念を導入している。
これは、単純化すれば、2つの値を代入できる__箱__である。
視覚化するならば、`[a|b]`のようになる。このaおよびbに、値が入る。
ここで、値とは、シンボルへの参照と、コンスセルへの参照を意味する。
即ち、Lispでは、ソースコードのアトムとしてシンボルがあり、それらが構造を成す際にコンスセルを用いているといえる。

## リスト表現の例
下記に、Lispに於けるリスト表現を樹形図として表す。

```
'(A)
*--nil
|
A

'((A B) (C D))
*----------*-----nil
|          |
*--*--nil  *--*--nil
|  |       |  |
A  B       C  D
```

# リストの記述方法
リストは`(list hoge foo bar)`の様に記述する。
またはquote(`'`)を使って、`'(hoge foo bar)`の様に記述する。

```cl
'(a b c)
; => (A B C)

```cl
(defun describe-path (edge)
 `(there is a ,(caddr edge) going ,(cadr edge) from here.))

`(describe-path '(garden west door))
; => (THERE IS A DOOR GOING WEST FROM HERE.)
```

# コンスセル・リストの操作方法
下記に、コンスセル、および、リストの操作方法を示す。

```cl
(cons 'x 'y)
; => (x . y)
(cons 'x nil)
; => (X)
(cons 'x ())
; => (X)

; 先頭のスロットのデータを取り出す
(car '(a b c d))
; => A

; 2番めのスロットのデータを取り出す
(cdr '(a b c d))
; => (b c d)

; car cdr を同時に行う関数
(cadr '(a b c))
= (car (cdr '(a b c)))
; => 'B
```

# キーワードシンボル
コロンを先頭に付けたシンボルの使い方の1つ。
シンボルが、そのシンボルそのままの名前を評価値に持つ。
用途は以下がある。

- 普通の変数により、値を持たせる

```cl
(let ((cigar 5))
   cigar)
; => 5

:cigar
; => CIGAR
```

- 定数として使う

定数として使うと、Lispが処理をより効率的に最適化してくれる。

```cl
(let ((:cigar 5))
   :cigar)
; => LET :CIGAR is constant ...
```

# 変数
Lispに於ける変数は、他の言語と同様に、グローバル変数とローカル変数が存在する。

### グローバル変数
上書き|定義
---|---
する |`(defparameter *global-value* num)`	
しない|`(defvar *global-value* num)`

### ローカル変数
```cl
(let ((variable-name1 value1)
      (variable-name2 value2))
  ...)
```

例:

```cl
(let ((a 5)
      (b 6))
  (+ a b))
```

## 関数
Lispに於ける関数は、グローバル関数とローカル関数が存在する。


### グローバル関数
```cl
(defun function-name (arguments)
  ...)
```

例

```cl
(defun function-name (arguments)
       (ash (+ *small* *big*) - 1))
; (ash x n) := xを2進nビット左にずらす
```

### ローカル関数
#### 基本的な定義方法

```cl
(flet ((function-name1 (arguments)
        ...)
       (function-name2 (arguments)
        ...))
      ...)

```

例:

```cl
(flet function-name (arguments)
       (ash (+ *small* *big*) - 1))
; (ash x n) := xを2進nビット左にずらす
```

#### ローカル関数定義中、同じスコープで定義されるローカル関数を使う場合


```cl
(labels ((function-name1 (arguments)
       ...)
      (function-name2 (arguments)
       (function-name1 *arguments*))
     ...)
```

例:

```cl
(labels ((a (n)
          (+ n 5))
         (b (n)
          (+ (a n) 6)))
 (b 10))
; => 21
```
## 関数をパラメータとして渡す
### functionパラメータ
下記の表記を使うことで、関数をパラメータとして渡すことができる。
これを使って、`mapcar`関数などに使用できる。

```cl
; (方法1)
#'関数名

; (方法2)
(function 関数名)
```

## 無名関数(`Lambda`)
一連の処理を纏める。
関数名を定義するまでもない場合に有効。

構文:

```cl
(lambda (args) (procedure))
; => <FUNCTION :LAMBDA...>
```

例:

```cl
; 下記のように通常どおり処理を纏めると関数名が必要となる。
(defun half (n)
   (/ n 2))
; => <FUNCTION :HALF...>

; 下記のように書けば関数名は不要となる。
(lambda (n)
   (/ n 2))

(mapcar (lambda (n) (/ n 2)) '(2 4 6))
; => (1 2 3)
```

## 返り値が複数ある関数
Lispでは、返り値を複数持つ関数が存在する。例えば`read-line`がそれにあたる。
通常は1つ目の返り値しか使われない。

```cl
(read-line)
; abc
; => "abc";
     NIL
```

### 複数の返り値を持つ関数の作成方法
下記のように`values`を用いる。

```cl
(defun foo ()
   (values 3 7))

(foo)
; => 3;
;    7
```

### 複数の返り値を受け取る方法
`multiple-value-bind`を用いる。

`multiple-value-bind`の構文:

```cl
(multiple-value-bind '(binding-vals) (func-call))
```

<dl>
<dt>'(binding-vals)</dt>
<dd>(func-call)の返り値を束縛するための変数のリスト。</dd>
<dt>(func-call)</dt>
<dd>複数の返り値を持つ関数の呼び出し。</dd>
</dl>

例:

```cl
(defun foo ()
   (values 3 7))

(multiple-value-bind (a b) (foo)
   (list a b))
; => ( 3 7)

; fooの返り値を、aとbに束縛している。
```

# 制御構文
## 条件分岐
### `if`
```cl
(if (審議を返すリスト)
  (真の時に実行するリスト)
  (偽の時に実行するリスト))
```

例:

```cl
(if (= (+ 1 2) 3)
  'yup
  'nop)
; => YUP
```

### `when`
```cl
(when (条件式)
  (処理1)
  (処理2)
  ...
  (処理n))
```

例:

```cl
(when (oddp 5)
  (setf *number-is-odd* t)
  'odd-number)
; => ODD-NUMBER
*number-is-odd*
; => T
```
### `unless`
```cl
(unless (条件式)
	(処理1)
	(処理2)
	...
	(処理n))
```

例:

```cl
(unless (oddp 4)
  (setf *number-is-odd* nil)
  'even-number)
; => EVEN-NUMBER
*number-is-odd*
; => nil
```

### `progn`
上記に示した`if`などの制御構文では、実行処理するリストが1つのみ持てる構造となっている。
しかし、一般的には複数の処理を行いたいことが多い。
prognは、中のリストを順繰りに評価して、最後の評価値をprognフォームの値として持つ。

例:

```cl
(if '(oddp 5)
  (progn (setf *number-was-odd* t)
   'odd-number)
   'even-number)
; => ODD-NUMBER
*number-was-odd*
; => T
```


### `case`
condは、C言語の`switch-case`文のようなものである。
つまり、複数のシンボルに対するテストを行う。
caseのシンボルと、それぞれのシンボルをeqlを使って評価する。
もし偽と評価されれば、otherwiseに分岐される。

```cl
(case シンボル
  ((分岐のシンボル) (処理1)
                  (処理2)
                  ...
                  (処理n))
  ((分岐のシンボル) (処理1)
                  (処理2)
                  ...
                  (処理n))
  (otherwise      (処理1)
                  (処理2)
                  ...
                  (処理n)))
```


### `cond`
condは、caseと同じく、C言語の`switch-case`文のようなものである。
__ただし、C言語の`switch-case`文とは異なり、シンボルではなく条件式を列挙する。__

```cl
(cond ((条件式1) (処理1)
                (処理2)
                ...
                (処理n))
      ((条件式2) (処理1)
                (処理2)
                ...
                (処理n))
      (t        (処理1)
                (処理2)
                ...
                (処理n)))
; 条件式nが全て偽だった時、tが実行される。
```

## ループ処理
### `loop`
loopは、第1引数の値によって、異なるループ方法を指定できる。

```cl
(loop repeat 10
   collect 1)
; => (1 1 1 1 1 1 1 1 1 1)

(loop for n from 1 to 10
   collect n)
; => (1 2 3 4 5 6 7 8 9 10)

(loop for n from 1 to 10
   collect (+ 100 n))
; => (101 102 103 104 105 106 107 108 109 110)
```

## 論理演算
C言語の`||`や`&&`のように、制御構文の条件式に使用できる。

```cl
(or (oddp 4) (setf *it-is-even* t))
; => T
*it-is-even*
; => T
```

短絡評価:
下記のように、lispは短絡評価する。

```cl
(or (oddp 5) (setf *it-is-even* t))
; => T
*it-is-even*
; => NIL
```

# シンボル・リストを比較、検査する
ここでは、シンボル、または、リストの値が他のリストの値と等しいか、また、リストの中に目当ての値が入っているかなどを調べるための方法を示す。

## `eq`,`equal`,`eql`,`equalp`,`=`,`string-equal`,`char-equal`関数
シンボルやリストが等しいか否かを調べる関数。

関数名|比較方法
---|---
eq|シンボル同士はeqで比較する
equal|シンボル同士では無い場合はequalで比較する
eql|eqと似ているが、更に数値と文字を比較できる
equalp|equalと似ているが、少し緩い評価をする
=|数値用のequal
string-equal|文字列用のequal
char-equal|文字用のequal

例:

```cl
(defparameter *fruit* 'apple)
=> *FRUIT*
(cond	((eq *fruit* 'apple) 'its-an-apple)
		((eq *fruit* 'orange) 'its-an-orange))
```

```cl
(equal 'apple 'apple)
=> T
(equal 'apple 'apple)
=> T
(equal (list 1 2 3) (list 1 2 3))
=> T
(equal '( 1 2 3) (cons 1 (cons 1 (cons 2 (cons 3 nil)))))
=> T	; 中身が同じなら同一とみなす
(equal 5 5)
=> T	; 整数同士
(equal 2.5 2.5)
=> T	; 浮動小数点同士
(equal "foo" "foo")
=> T	; 文字列同士
(equal #\a #\a)
=> T	; 文字同士
```

```cl
(eql 'foo 'foo)
=> T	; シンボル同士
(eql 3.4 3.4)
=> T	; 数値同士
(eql #\a #\a)
=> T	; 文字列同士
```

```cl
; equalpの使い方
(equalp "Bob Smith" "bob smith")
=> T	; 大文字、小文字を無視した文字列同士
(equalp 0 0.0)
=> T	; 整数と浮動小数点数
```

## `coerce`関数
オブジェクトを指定した形式へ強制変換する。
(coerce : 〜を無理にdoさせる。)

```cl
(coerce '(a b c) 'vector) ; => #(A B C)
(coerce 'a 'character) ; => #\A
(coerce 4.56 'complex) ; => #C(4.56 0.0)
(coerce 4.5s0 'complex) ; => #C(4.5s0 0.0s0)
(coerce 7/2 'complex) ; => 7/2
(coerce 0 'short-float) ; => 3.5L0
(coerce 3.5L0 'float) ; => 3.5L0
(coerce (cons 1 2) t) ; => (1 . 2)
; NOTE: タイプtは、全てのオブジェクトのタイプの集合。
;       従って、この場合は、引数をそのまま返す。
```

## `complement`関数
補集合。(高階関数)

例:

```cl
(complement #'alphanumericp)
; => アルファベットでも数字でもない
```
