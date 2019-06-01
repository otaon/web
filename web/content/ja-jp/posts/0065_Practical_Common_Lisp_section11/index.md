---
title: "書籍 実践 Common Lisp 第11章 コレクション"
date:    2019-06-01T00:00:00+09:00
lastmod: 2019-06-01T00:00:00+09:00
draft: false
toc: true
tags: ["lisp", "実践-Common-Lisp"]
categories: ["実践-Common-Lisp"]
authors:
- otaon
---

## この章について
この章では、複数の値を1つのオブジェクトにまとめたデータ型(コレクション)について説明する。

## 11.1 ベクタ
ベクタは、整数でインデックスされたコレクション。

ベクタには、固定サイズのベクタと可変サイズのベクタがある。

### 固定サイズのベクタ
特定の値を含む固定サイズのベクタは`vector`関数で作ることができる。  
`vector`は任意個の引数を取り、引数で与えた要素を含む固定サイズのベクタを新しく確保する。

```lisp
(vector) ; #()
(vector 1) ; #(1)
(vector 1 2) ; #(1 2)
```

`#(...)`というシンタックスは、lispの印字器や読み取り器が使用するベクタのリテラル表記。  
すなわち、この表記を使えば`print`や`read`を使って読み書きできる。

`#(...)`リテラルを使って定義したベクタを変更したときの動作は**未定義**であるため、変更予定のあるベクタを定義する場合は`vector`関数か`make-array`関数を使用する必要がある。

`make-array`関数は`vector`関数よりも汎用的に使用できる。  
固定サイズ、可変サイズの両方のベクタを作成でき、さらに、任意次元の配列も作成できる。  
したがって、ベクタを作成する場合は1次元を表す数を引数にとる。  
次元を表す引数にはリストか、1次元の場合は数値を渡す。  
`:initial-element`引数を渡せば、すべての要素に初期値を設定できる。

```lisp
(make-array 5 :initial-element nil) ; #(nil nil nil nil nil)
```

`make-array`を使うと、フィルポインタを持ったベクタを作成できる。  
フィルポインタとは「次に要素を追加した時に埋める(fillする)位置」のこと。  
すなわち、フィルポインタは「今ベクタに格納されている要素の数」と等しい。

下記プログラムでは、要素5つ分の領域を持つベクタを作成する。  
なお、フィルポインタが`0`であるため、何も格納されていないように見える。

```lisp
(make-array 5 :fill-pointer 0) ; #()
```

フィルポインタを持ったベクタの末尾に要素を追加するには、`verctor-push`関数を使用する。  
`vector-push`は下記動作を行う。

1. フィルポインタの値が示す場所に要素を追加する
1. フィルポインタの値をインクリメントする
1. 新しく追加された要素のインデックスを返す

フィルポインタを持ったベクタの末尾要素を取り出すには、`verctor-pop`関数を使用する。  
`vector-pop`は下記動作を行う。

1. フィルポインタの値をデクリメントする
1. 最後に追加された要素を返す

```lisp
(defparameter *x* (make-array 5 :fill-pointer 0))

(vector-push 'a *x*) ; 0
*x* ; #(A)
(vector-push 'b *x*) ; 1
*x* ; #(A B)
(vector-push 'c *x*) ; 2
*x* ; #(A B C)
(vector-pop *x*) ; C
*x* ; #(A B)
(vector-pop *x*) ; B
*x* ; #(A)
(vector-pop *x*) ; A
*x* ; #()
```

`make-array`で可変サイズのベクタ(**可変ベクタ : adjustable vector**)を作るには、下記の通り定義する。

- **`:fill-pointer`**を引数に渡してフィルポインタを使用させる
- **`:adjustable t`**を引数に渡して拡張可能にする

```lisp
(make-array 5 :fill-pointer 0 :adjustable t) ; #()
```

可変ベクタに要素を追加するには`vector-push-extend`を使う。  
この関数は`vector-push`と同様に動作するが、フィルポインタの値が下層のストレージサイズと同じ状態で呼び出されると、配列を自動的に拡張する。


**NOTE:**  

- `:fill-pointer`を指定せずに`:adjustable t`のみを指定することもできる。  
- ただし、`vector-push`、`vector-pup`はフィルポインタを持ったベクタにのみ使用可能。  
- また、`vector-push-extend`はフィルポインタを持った可変ベクタに対してのみ使用可能。  
- `adjust-array`関数を使用すると、ベクタのサイズを拡張する以外にも、様々な方法で可変配列を変更できる。

## 11.2 特殊ベクタ
特殊ベクタ(specialized vector)とは、特定の型のデータのみを持つように制限されたベクタのこと。  
データ型を制限されると、データの容量削減や、アクセスの高速化が可能になる。

特殊ベクタの例としては、文字列がある。文字列は、特別に専用の表記方法がある。  
しかし、文字列もベクタの部分型であるから、ベクタに対する関数は文字列に対しても使用できる。

`"foo"`のような文字列リテラルは、`#()`というベクタのリテラルと似ており、変更を加えてはいけない(イミュータブルとして扱う必要がある)。  
ただし、`make-array`に別のキーワード引数`:element-type`を指定すると、サイズ変更可能な文字列を作成できる。  
文字列の場合は`:element-type 'character`を指定する。

```lisp
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character) ; ""
```

文字列以外の特殊なベクタとしては、**ビットベクタ**がある。これは要素の値が`0`か`1`であるベクタ。 
ビットベクタの場合は`:element-type 'bit`を指定する。  
`#*00001111`のような読み取り及び印字のシンタックスと、ビット配列の論理積などの演算用の関数が多数用意されている。

## 11.3 シーケンスとしてのベクタ
ベクタとリストは、いずれもシーケンスという型の部分型だ。  
ここからの節で説明する関数はすべてシーケンス関数であり、ベクタとリストに対して使用可能。

### シーケンス関数 `length`
`length`は、シーケンスの長さを返す。

```lisp
(defparameter *x* (vector 1 2 3))
(length *x*) ; 3
```

### シーケンス関数 `elt`
`elt`は、シーケンスの指定要素を返す。(elt: elementの略)

```lisp
(defparameter *x* (vector 1 2 3))
(elt *x* 0) ; 1
(elt *x* 1) ; 2
(elt *x* 2) ; 3
(elt *x* 3) ; エラー発生
```

`elt`によって返される値に対して`setf`を使用することが可能。

```lisp
(defparameter *x* (vector 1 2 3))
(setf (elt *x* 0) 10)
*x* ; #(10 2 3)
```

## 11.4 シーケンス反復関数
`length` `elt` `setf` を使用すれば如何なるget setも可能だが、利便性のために更に高機能な関数が用意されている。

|名前|&nbsp;引数|&nbsp;戻り値|
|----|----------|------|
|`count`|&nbsp;`(count item sequence)`|&nbsp;アイテムのシーケンス中における出現回数|
|`find`|&nbsp;`(find item sequence)`|&nbsp;アイテム、見つからなかったら`nil`|
|`position`|&nbsp;`(position item sequence)`|&nbsp;指定アイテムの最初のインデックス、<br/>見つからなかったら`nil`|
|`remove`|&nbsp;`(remove item sequence)`|&nbsp;アイテムを除いたシーケンス|
|`substitute`|&nbsp;`(substitute new-item target-item sequence)`|&nbsp;`target-item`を`new-item`に置き換えたシーケンス|

```lisp
(count 1 #(1 2 1 2 3 1 2 3 4)) ; 3

(remove 1 #(1 2 1 2 3 1 2 3 4)) ; #(2 2 3 2 3 4)
(remove 1 '(1 2 1 2 3 1 2 3 4)) ; (2 2 3 2 3 4)
(remove #\a "foobarbaz") ; "foobrbz"

(substitute 10 1 #(1 2 1 2 3 1 2 3 4)) ; #(10 2 10 2 3 10 2 3 4)
(substitute 10 1 '(1 2 1 2 3 1 2 3 4)) ; (10 2 10 2 3 10 2 3 4)
(substitute #\x #\b "foobarbaz") ; "fooxarxaz"

(find 10 #(1 2 1 2 3 1 2 3 4)) ; nil
(find 1 #(1 2 1 2 3 1 2 3 4)) ; 0
```

**NOTE** `remove`と`substitute`は引数のシーケンスと同じ型のシーケンスを戻り値として返す。

### デフォルトの挙動を変更する
上記の関数はキーワード引数によって挙動を変えられる。  
例えば、デフォルトではシーケンス中の要素とアイテムの引数との比較に`eql`を使用するが、これをキーワード引数によって変更できる。  

#### `:test 引数2つの関数`
引数を2つ持ち真偽値を返す関数を指定して、デフォルトの比較方法を変更する。

```lisp
(count #\b "abcabcB") ; 2
(count #\b "abcabcB" :test #'char-equal) ; 3

(count "foo" #("foo" "bar" "baz") ; 0
(count "foo" #("foo" "bar" "baz") :test #'string=) ; 1
```

**NOTE** `:test`に与えた比較関数の**否定**を比較関数としたい場合、 `complement`を使うこと。(非推奨のため`test-not`を使用してはいけない。)  
※`complement`関数は、「与えられた関数の否定を返す関数」を返す。

```lisp
(count #\b "abcabcB" :test (complement #'char-equal)) ; 4
```

#### `:key 引数1つの関数`
引数を1つ持つ関数を指定して、シーケンス中の要素に対して適用された値が、シーケンス側の比較対象となる。

```lisp
(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first) ; (c 30)
```

#### `:start 数値` `:end 数値`
シーケンス関数の範囲をサブシーケンスに限定したい場合、`:start`、`:end`を使用する。

```lisp
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :start 1) ; (a 30)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :end 0)   ; nil
```

### `:from-end 数値`
`:from-end`に`nil`以外の値を与えると、シーケンス内の要素を逆順に探索する。

```lisp
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t) ; (a 30)
(remove #\a "foobarbaz" :count 1) ; "foobrbaz"
(remove #\a "foobarbaz" :count 1 :from-end t) ; "foobarbz"
```

### `:count 数値`
`:count`は、`remove`または`substitute`する要素の個数を指定する。

```lisp
(remove #\a "foobarbaz") ; "foobrbz"
(remove #\a "foobarbaz" :count 1) ; "foobrbaz"
```

### キーワード引数一覧
|引数|&nbsp;意味|&nbsp;デフォルト|
|----|----------|----------------|
|`:test`|&nbsp;パラメータで渡した値と要素との比較に使う関数|&nbsp;`eql`|
|`:key`|&nbsp;実際のシーケンスの要素から、キーとなる値を取り出すための関数|&nbsp;`nil`|
|`:start`|&nbsp;部分シーケンスの開始位置|&nbsp;`0`|
|`:end`|&nbsp;部分シーケンスの終了位置|&nbsp;`nil`|
|`:from-end`|&nbsp;真ならばシーケンスの末尾から遡って走査する|&nbsp;`nil`|
|`:count`|&nbsp;削除もしくは置換する要素の最大数|&nbsp;`nil`|

## 11.5 さまざまな高階関数
Common Lispには高階関数(`xxx-if`,`xxx-if-not`)が用意されている。  
高階関数の引数には、シーケンスの各要素に対して呼び出される関数を渡す。

```lisp
(count-if #'evenp #(1 2 3 4 5)) ; 2
(count-if-not #'evenp #(1 2 3 4 5)) ; 3
(position-if #'digit-char-p "abcd0001") ; 4
(remove-if-not #'(lambda (x) (char= (elt x 0) #\f))
  #("foo" "bar" "baz" "foom")) ; #("foo" "foom")
```

言語標準では`xxx-if-not`は非推奨(deprecated)とされているが、実際には普通に使用されており、特に`remove-if-not`は広く使われている。  
`remove-if-not`は引数の述語を満たさない要素をシーケンスから取り除いたシーケンスを返す。  
すなわち、述語を満たす要素のみをシーケンスから抽出してシーケンスを作る関数だと言える。

これらの高階関数は、`:test`以外のキーワード引数が使用できる。

```lisp
(count-if #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first) ; 2
(count-if-not #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first) ; 3
(remove-if-not #'alpha-char-p #("foo" "bar" "1baz")
  :key #'(lambda (x) (elt x 0))) ; #("foo" "bar")
```

`remove-duplicates`関数はシーケンス中の重複した要素を1つだけにしたシーケンスを返す。

```lisp
(remove-duplicates #(1 2 1 2 3 1 2 3 4)) ; #(1 2 3 4)
```

## 11.6 シーケンス全体の操作
`copy-seq`関数は引数に渡したシーケンスをコピーして、そのシーケンスを返す。  
`reverse`関数は引数に渡したシーケンスを**逆順にして**コピーして、そのシーケンスを返す。  

どちらの関数も、**複製されるのはシーケンス(容器の部分)のみであり、要素自体はコピーされず、元のものが参照される**。

`concatenate`関数は複数のシーケンスをつなぎ合わせた新しいシーケンスを作る。  
`concatenate`関数では、作成するシーケンスの型を指定する必要がある。

```lisp
(concatenate 'vector #(1 2 3) '(4 5 6))    ; #(1 2 3 4 5 6)
(concatenate 'list #(1 2 3) '(4 5 6))      ; (1 2 3 4 5 6)
(concatenate 'string "abc" '(#\d #\e #\f)) ; "abcdef"
```

## 11.7 ソートとマージ
### ソート
シーケンスの方法には、非安定ソートと安定ソートの2種類が用意されている。どちらも**破壊的関数**。

- `sort` : 非安定ソート
- `stable-sort` : 安定ソート

```lisp
(sort (vector "foo" "bar" "baz")) ; #("bar" "baz" "foo")
```

ソート関数は破壊的関数であるから、下記を絶対に守ること。

- ベクタはリテラルではなく`vector`関数で作成すること。
- ソート関数呼び出し後は元の変数を使わないこと。つまり、必ず**関数の戻り値**を使用すること。
- ソート関数呼び出しにも使用したいデータは直接渡さず、コピーを作ってそれをソート関数に渡すこと。

```lisp
; 下記のように書くこと
(setf my-sequence (sort my-sequence #'string<))

; 下記のように書いてはいけない
(sort my-sequence #'string<)
```

ソート関数はキーワード引数`:key`を取る。`:key`には、実際の要素の代わりにソートのための述語に渡す値を得る関数を指定する。

```lisp
(princ (sort (vector '(c 1) '(a 2) '(b 3)) #'string< :key #'first))
; =>
; #((A 2) (B 3) (C 1))
; #((A 2) (B 3) (C 1))
```

### マージ
`merge`関数は、2つのシーケンスを指定の述語に則ってマージして、1つの新しいシーケンスを生成して返す。  
引数には、生成されるシーケンスの型、2つのシーケンス、述語を渡す。  
`merge`関数が返すシーケンスは、引数の各々のシーケンスを述語に則ってソートした結果をマージしたシーケンスに等しい。

```lisp
(merge 'vector #(1 3 5) #(2 4 6) #'<) ; #(1 2 3 4 5 6)
(merge 'list #(1 3 5) #(2 4 6) #'<)   ; (1 2 3 4 5 6)
```

## 11.8 部分シーケンスの操作
### `subseq`
`subseq`は、シーケンスから指定したインデックス分の部分シーケンスを返す。

```lisp
(subseq シーケンス 先頭インデックス 末尾インデックス)
; 末尾インデックスより1つ手前までが部分シーケンスの範囲になる

(subseq "foobarbaz" 3) ; "barbaz"
(subseq "foobarbaz" 3 6) ; "bar"
```

`subseq`が返す値に対して`setf`できるが、これによってシーケンス長は変化しない。  
また、新しくセットする値の長さが、セット対象の部分シーケンスの長さと異なる場合、短い方の長さ分のみがセット対象となる。

```lisp
(defparameter *x* (copy-seq "foobarbaz"))

; 部分シーケンスと新しい値が同じ長さ
(setf (subseq *x* 3 6) "xxx")
*x* ; "fooxxxbaz"

; 新しい値が長すぎるため、余分な文字は無視されれる
(setf (subseq *x* 3 6) "abcd")
*x* ; "fooabcbaz"

; 新しい値が短すぎるため、2文字だけ変更される
(setf (subseq *x* 3 6) "xx")
*x* ; "fooxxcbaz"
```

### `fill`
`fill`は、シーケンス中の複数要素をある1つの数に設定する。  
`:start`、`:end`で設定範囲を指定できる。

```lisp
(fill #(a b c d e) 1) ; #(1 1 1 1 1)
(fill #(a b c d e) 1 :start 1 :end 4) ; #(A 1 1 1 E)
```

### `search`
`search`は、シーケンスの中から部分シーケンスを探し出し、マッチしたときのインデックスを返す。  
マッチしなかった場合は`nil`を返す。

```lisp
(search シーケンス 探索対象の部分シーケンス)
(search "foobarbaz" "bar") ; 3
```

### `mismatch`
`mismatch`は、共通のプリフィックス(つまり先頭から等しい部分シーケンス)を持つ2つのシーケンスに対して、枝分かれする場所を探し出し、そのインデックスを返す。  
枝分かれする場所がなかったら`nil`を返す。  
また、標準的なキーワード引数を指定できる。`:key`によりシーケンスから要素を取り出す関数を、`:test`により比較関数を、`:start1`、`:end1`、`:start2`、`:end2`引数により2つのシーケンスの比較範囲、`:from-end`により探索の向きを指定できる。

```lisp
(mismatch シーケンス 探索対象の部分シーケンス)
(mismatch "foobarbaz" "foom") ; 3
(mismatch "foobar" "bar" :from-end t) ; 3
```

## 11.9 シーケンス述語
シーケンス全体に対して評価・比較するための関数がある。

### `every`
`every`は、「`∀x. P(x)`」と同等。述語が1回でも`nil`を返すと即`nil`を返す。

```lisp
(every #'evenp #(1 2 3 4 5)) ; nil
```

### `some`
`some`は、「`∃x. P(x)`」と同等。述語が1回でも`t`を返すと即`t`を返す。

```lisp
(some #'evenp #(1 2 3 4 5)) ; t
```

### `notany`
`notany`は、「`∀x. ¬P(x)`」と同等。1回でも`t`になると即`nil`を返す。

```lisp
(notany #'evenp #(1 2 3 4 5)) ; nil
```

### `notevery`
`notevery`は、「`∃x. ¬P(x)`」と同等。1回でも`nil`になると即`t`を返す。

```lisp
(notevery #'evenp #(1 2 3 4 5)) ; t
```

### 2つのシーケンスに対するシーケンス述語
要素数が等しい2つのシーケンスに対して、述語を満たすのか否かを評価する。  
それぞれの関数の意味は上記説明を参照。

```lisp
(every #'> #(1 2 3 4) #(5 4 3 2)) ; nil
(some #'> #(1 2 3 4) #(5 4 3 2)) ; t
(notany #'> #(1 2 3 4) #(5 4 3 2)) ; nil
(notevery #'> #(1 2 3 4) #(5 4 3 2)) ; t
```

## 11.10 シーケンスマッピング関数(`map`など)
### `map`
`map`は、各要素に関数を適用し、その結果を含む新しいシーケンスを返す。  
`map`には、どの型のシーケンスを返すのか指定する必要がある。

```lisp
(map 'vector #'* #(1 2 3 4 5) #(10 9 8 7 6)) ; #(10 18 24 28 30)
```

### `map-into`
`map-into`は、第1引数に指定したシーケンスに値を設定する。戻り値は、シーケンスに設定した値。  

```lisp
(defparameter *a* (vector 1 2 3))
(defparameter *b* (vector 10 20 30))
(defparameter *c* (vector 100 200 300))
(map-into *a* #'+ *a* *b* *c*) ; #(111 222 333)
*a* ; #(111 222 333)
```

各シーケンスの長さが異なる場合は、最も短いシーケンス分のみ計算して設定する。  
ただし、マッピングの結果になるシーケンス(つまり第1引数)がフィルポインタ付きのベクタだった場合、計算される範囲は、フィルポインタには制限されず、実際のベクタのサイズで決まる。

`map-into`の呼び出し後には、フィルポインタはマッピングされた要素の個数になる。しかし、`map-into`は可変ベクタを拡張しない。

```lisp
(defun f()
  (defparameter *a* (make-array 3 :fill-pointer 0 :adjustable t))
  (vector-push-extend 1 *a*)
  (vector-push-extend 2 *a*)
  (vector-push-extend 3 *a*)
  (defparameter *b* (make-array 4 :fill-pointer 0 :adjustable t))
  (vector-push-extend 10 *b*)
  (vector-push-extend 20 *b*)
  (vector-push-extend 30 *b*)
  (vector-push-extend 40 *b*)
  (defparameter *c* (make-array 5 :fill-pointer 0 :adjustable t))
  (vector-push-extend 100 *c*)
  (vector-push-extend 200 *c*)
  (vector-push-extend 300 *c*)
  (vector-push-extend 400 *c*)
  (vector-push-extend 500 *c*))
(f)
(map-into *a* #'+ *a* *b* *c*) ; #(111 222 333)
(f)
(defparameter *ret* (make-array 2 :fill-pointer 0 :adjustable 0))
(map-into *ret* #'+ *a* *b* *c*) ; #(111 222) ←*ret*を拡張していない
```

### `reduce`
`reduce`は、シーケンスの先頭の2つの要素に対して2つの引数を持つ関数を適用する。これをシーケンスの末尾まで繰り返して最終的な結果を返す。  
したがって、`reduce`は、第1引数の関数をシーケンスの各要素に適用して1つの値を得る関数。

```lisp
(reduce #'+ #(1 2 3 4 5 6 7 8 9 10)) ; 55
(reduce #'max #(1 2 3 4 5 6 7 8 9 10)) ; 55
```

`reduce`はキーワード引数を指定できる。

- `:key` : シーケンスから要素を取り出すために使われる関数
- `:from-end` : `nil`以外を指定すると末尾から走査する
- `:start` : シーケンスの対象範囲の開始点を指定する
- `:end` : シーケンスの対象範囲の終了点を指定する(`:end`より1つ前まで範囲)
- `:initial-value` : シーケンスの先頭よりも論理的に前に来る値を指定する

## 11.11 ハッシュテーブル
