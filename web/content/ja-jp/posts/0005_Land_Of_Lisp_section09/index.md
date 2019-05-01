---
title: "書籍 Land of Lisp 9章 より進んだデータ型とジェネリックプログラミング"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
toc: true
tags: ["lisp"]
categories: ["Notes"]
authors:
- otaon
---

Common Lisp では、下記のようなデータ型が使用できる。

- コンスセル
- シンボル
- 文字列
- 数値
- 配列
- ハッシュテーブル
- 構造体

## 9.1 配列

### 配列を使う

配列を作成するには`make-array`を使用する。

```lisp
> (make-array 3)
#(NIL NIL NIL)
```

配列を作成する。

```lisp
> (defparameter x (make-array 3))
X
```

配列の1番目の要素をgetする。

```lisp
> (aref x 1)
NIL
```

### ジェネリックなセッター

Common Lispは*ジェネリックなセッター*をサポートしている。  
つまり、あらゆるデータ型に対してセッターが同じ形式で書ける。

例として、`setf`によって、配列の特定の要素を変更する。

```lisp
> (defparameter x (make-array 3))
> (setf (aref x 1) 'foo)
FOO
> x
#(NIL 'FOO NIL)
> (aref x 1)
FOO
```

上記の例では、配列の第1要素を変更した。  
下記の通り、配列以外のデータ型に対しても、setfは同様の操作によって値をセットできる。

```lisp
> (setf foo (make-array 4))
#(NIL NIL NIL NIL)
> (setf (aref foo 2) (list 'x 'y 'z))
(X Y Z)
> foo
#(NIL NIL (X Y Z) NIL)
> (setf (car (aref foo 2)) (make-hash-table))
#S(HASH_TABLE)
> (setf (gethash 'zoink (car (aref foo 2))) 5)
5
> foo
#(NIL NIL (#S(HASH-TABLE (ZOINK . 5)) Y Z) NIL)
```

### 配列とリスト

配列とリストでは、特定の要素にアクセスする方法が異なる。  
具体的には、下記の違いがある。

|      |n番目の要素へのアクセス|
|------|-----------------------|
|リスト| `(nth 数字 リスト)`   |
|配列  | `(aref 配列 数字)`    |


## 9.2 ハッシュテーブル

### ハッシュテーブルを使う

ハッシュテーブルを作成するには`make-hash-table`を使用する。

```lisp
> (make-hash-table)
#S(HASH-TABLE ...)
```

alistと同じく、ハッシュテーブルは要素をキーとバリューの組み合わせで格納する。  
`gethash`関数を使って要素を取り出すことができる。

```lisp
> (defparameter x (make-hash-table))
#S(HASH-TABLE ...)
> (gethash 'yup x)
NIL ;
NIL
```

`gethash`は2つの値を返す関数である。
1つ目の値は、キーに対応する値。
2つ目の値は、キーに対応する値があるか否か。

配列と同じように、データを取り出す関数(ここでは`gethash`)を、`setf`を組み合わせてデータをセットできる。

```lisp
> (defparameter x (make-hash-table))
#S(HASH-TABLE ...)
> (setf (gethash 'yup x) '25)
25
> (gethash 'yup x)
25 ;
T
```

同じデータを連想リストとハッシュテーブルで作成してみる。

*alist*

```lisp
> (defparameter *drink-order* '((bill . double-espresso)
                              (lisa . small-drip-coffee)
                              (john . medium-latte)))
> (cdr (assoc 'lisa *drink-order*))
(LISA . SMALL-DRIP-COFFEE)
```

*hash-table*

```lisp
> (defparameter *dring-order* (make-hash-table))
#S(HASH-TABLE ...)
> (setf (gethash 'bill *drink-order*) 'double-espresso)
DOUBLE-ESPRESSO
> (setf (gethash 'lisa *drink-order*) 'small-drip-coffee)
SMALL-DRIP-COFFEE
> (setf (gethash 'john *drink-order*) 'medium-latte)
MEDIUM-LATTE

(gethash 'lisa *drink-order*)
SMALL-DRIP-COFFEE ;
T
```

### 複数の値を返す

Common Lispでは、複数の値を返す関数を定義できる。  
既成の関数でも、複数の値を返すものがある。

```lisp
> (round 2.4)
2 ;
0.4
```

複数の値を返す関数を自作するには、`(values)`を使用する。

```lisp
> (defun foo ()
    (values 3 7))
FOO
> (foo)
3 ;
7
```

1番目の値を使用する方法は、単数の返り値の扱い方と変わらない。

```lisp
> (+ (foo) 5)
8
```

2番目以降の値を使用するには`(multiple-value-bind)`を使用する。

```lisp
> (multiple-value-bind (a b) (foo)
    (* a b))
21
```

## 9.3 構造体

### 構造体を使う

構造体は、OOPに見られるように、属性を持つオブジェクトを表現するために使用される。  
構造体を定義するには`(defstruct)`を使用する。  
スロットに初期値を与える場合、括弧で囲う。

```lisp
> (defstruct person
    name
    age
    waist-size
    favorite-color)
PERSON

> (defstruct human
    (name "John Doe")
    age
    waist-size
    favorite-color)
HUMAN
```

上記の例では、`person`は4つの属性(lispにおいては*スロット*と呼ばれる)を持つ。

- name   名前
- age   年齢
- waist-size    ウェストサイズ
- favorite-color   好きな色

構造体を定義する(`defstruct`を呼ぶ)と、下記が自動的に生成される。

#### 構造体のインスタンスを作成する関数

`(make-person)`関数

```lisp
> (defparameter *bob* (make-person :name "Bob"
                                   :age 35
                                   :waist-size 32
                                   :favorite-color "blue"))
*BOB*
```

#### 各スロットへのゲッター

`(person-age)`関数

```lisp
> (person-age *bob*)
35

> (setf (person-age *bob*) 36)
36
```

LispのReaderは、`person`の出力表記を読み込み、`person`のインスタンスを生成できる。

```lisp
> (defparameter *that-guy* #S(PERSON :NAME "bob" :AGE 35 :WAIST-SIZE 32 :FAVORITE-COLOR "blue"))
> (person-age *that-guy*)
35
```

### 構造体をいつ使うか

仮に、構造体を使用せずにリストでデータの塊を管理することを考える。  
この場合、インスタンスを作成する関数や、各スロットへのゲッターは下記のとおり書ける。

```lisp
> (defun make-person (name age waist-size favorite-color)
    (list name age waist-size favorite-color))
MAKE-PERSON

> (defun person-age (person)
    (cadr person))
PERSON-AGE
```

見て分かる通り、どの属性が`person`リストのどの位置にあるのかを意識する必要がある。  
したがって、リストで沢山の属性を管理するのはバグの原因となる。  
また、構造体の方がリストよりも属性へのセット、ゲットのコードが簡潔に書ける。  
したがって、複数の属性を持つミュータブルなデータを管理したい場合、リストよりも構造体が適している。

## 9.4 データをジェネリックに扱う

ジェネリック = 一般的。  
Common Lispでは、様々なデータ型を意識せずに、数値を統一的に操作できる。  
そのための道具立てとして、下記のようなものが用意されている。

- ジェネリックライブラリ関数
- 型述語
- `defmethod`
- ジェネリックアクセサ

### シーケンスを使う

引数のデータ型によらず動作するコードを*手軽に*書くには、型チェックを別の関数に任せれば良い。  
Common Lispには、ジェネリックな関数が既に用意されている(e.g. シーケンス関数)。  

シーケンス関数は、Lispにおける3つの主要なシーケンスを統一的に扱える(e.g. `length`関数)。  
(シーケンス: リスト 配列 文字列)

```lisp
> (length '(a b c))
3
> (length "blub")
4
> (length (make-array 5))
5
```

*補足:*  
Common Lispにも、リスト専用の長さを求める関数`list-length`がある。  
ジェネリックな関数よりも処理が速いが、始めから使用する必要はない。  
処理の最適化のフェーズで明確に必要だと分かったら使用すれば良い。

### 探索のためのシーケンス関数

シーケンス関数の中には、シーケンスから何かを探し出すためのものがある。

- `find-if`    与えられた述語を満たす最初の要素を見つける
- `count`    特定の要素がいくつシーケンス中にあるか数える
- `position`    特定の要素がシーケンスのどの位置にあるか返す
- `some`    シーケンス中に条件を満たす要素が存在するか返す
- `every`    シーケンス中の全要素が条件を満たすか返す

上記の関数の実行例を示す。

```lisp
> (find-if #'numberp '(a b 5 d))
5
> (cound #\s "mississippi")
4
> (position #\4 "2kewl4skewl")
5
> (some #'numberp '(a b 5 d))
T
> (every #'numberp '(a b 5 d))
NIL
```

### シーケンスの要素について繰り返す関数の例

#### `reduce`

 ジェネリックなシーケンス関数において、`reduce`はとりわけ便利である。

```lisp
> (reduce #'+ '(3 4 6 5 2))
20
```

`reduce`の第1引数に渡す関数を、縮約関数(reduction function)と呼ぶ。  
上記の例では、`+`が縮約関数である。

`reduce`では、`initial-value`というキーワード引数を使って、初期値を与えられる。  
初期値を与えなかった場合は、シーケンスの最初の要素を初期値とする。

下記に、リスト`(a b c)`に初期値`x`を与えなかった場合と与えた場合の処理の違いを示す。  

|初期値|処理内容(tは一時変数)|
|------|---------------------|
|無し  |t=a t=t+b t=t+c      |
|有り  |t=x t=t+a t=t+b t=t+c|


下記の通り、初期値を与えないと、aが初期値として設定されたまま結果として返されてしまう。  
例えば、縮約関数が「シーケンスの中で最大の偶数を見つける」だった場合、初期値は必須である。

```lisp
(lambda (best item)
  (if (and (evenp item) (> item best))
    item
    best))
; initial-valueが無い場合
; '(7 4 6 5 2)
; 7 <- (> item best)がTとならないため

; initial-valueが0の場合
; '(7 4 6 5 2)
; 6 <- 正しく評価できた
```

`reduce`をpythonで手続き的に記載すると、下記のようになる。

```python
def func(a, b):
    return a + b

def reduce(func, lst, **kwargs):
    if 'initial_value' in kwargs.keys():
        lst.insert(0, kwargs['initial_value'])

    temp = lst[0]
    for i in range(len(lst) - 1):
        temp = func(temp, lst[i + 1])
    
    return temp

lst = [i + 1 for i in range(10)]

print(reduce(func, lst))
# => 55
print(reduce(func, lst, initial_value=10))
# => 65
```

#### `map`

`map`は`mapcar`と同じく、各要素を引数に渡した関数を呼んで結果を集める。  
しかし、`map`は任意のシーケンスに対して使用できる。  
また、`map`は返り値としてどのシーケンス型の値を返すかという引数を取る。

```lisp
> (map 'list
    (lambda (x)
      (if (eq x #\s)
          #\S
          x))
    "this is a string")
(#\t #\h #\i #\S #\  #\i #\s #\  #\a #\  #\S #\t #\r #\i #\n #\g)
```

#### `subseq`

`subseq`関数は始点と終点を指定してシーケンスの一部分を取り出すのに使える。  
位置は0から数え始め、始点は含まれ、終点は含まれない。

```lisp
> (subseq "america" 2 6)
"eric"
```

#### `sort`

`sort`関数は任意の比較関数を渡してシーケンスをソートする。

```lisp
> (sort '(5 8 2 4 9 3 6) #'<)
(2 3 4 5 8 9)
```

### 型述語を使って自分でジェネリック関数を作る

Common Lispは動的型付け言語であるため、ある変数のデータ型を調べる関数が揃っている。  
例えば数値かどうかは`numberp`によって調べられる。

```lisp
> (numberp 5)
T
```

よく使う型述語には下記がある。

- `arrayp`
- `characterp`
- `consp`
- `functionp`
- `hash-table-p`
- `listp`
- `stringp`
- `symbolp`

これらを使えば、色々な型の引数をジェネリックに取る関数を自分で書ける。  
例えば、数値同士とリスト同士を「足す」関数を作るとする。  
単純に関数定義するなら、下記のようになる。

```lisp
> (defun add (a b)
    (cond ((and (numberp a) (numberp b)) (+ a b))
          ((and (listp a) (listp b)) (app a b))))
ADD
> (add 3 4)
7
> (add '(a b) '(c d))
(A B C D)
```

上記の関数は、複数の型に対する処理が固まっているため、保守性が低い。  
そこで、lispは関数の **多重定義(オーバーロード)** が可能なので、これを利用する。  
`defmethod`を使うと各々の型に特化した複数の関数を定義できる。
`defmethod`によって定義された関数が呼ばれたとき、Lispは自動的に引数の型を調べ、対応する関数本体を呼び出す。  
このように、インタプリタ/コンパイラは複数の関数本体から引数の型に応じたものを選び出すことを、 **型によるディスパッチ(type dispatching)** と呼ぶ。

`defmethod`を使うと、上記の`add`は下記のようになる。

```lisp
> (defmethod add ((a number) (b number))
    (+ a b))
ADD
> (defmethod add ((a list) (b list))
    (append a b))
ADD
> (add 3 4)
7
> (add '(a b) '(c d))
(A B C D)
```

`defmethod`は、上記9.3章の`defstruct`で定義した構造体に対しても使用できる。  
これを使用して、簡単なオブジェクトシステムを実装することも出来る。

