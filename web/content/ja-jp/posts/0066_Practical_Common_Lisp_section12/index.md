---
title: "書籍 実践 Common Lisp 第12章 リスト処理：やつらがLISPと呼ぶ理由"
date:    2019-06-02T00:00:00+09:00
lastmod: 2019-06-02T00:00:00+09:00
draft: false
toc: true
tags: ["lisp", "実践-Common-Lisp"]
categories: ["実践-Common-Lisp"]
authors:
- otaon
---

## この章について
この章では、リストついて説明する。

## 12.1 「リストなんてない」
Common Lispにおいては、リストというデータ構造が構築されているわけではない。  
実際には、コンスセルによって構築されたデータ構造があり、それをリスト操作用の関数群によってそう見せているに過ぎない。

```lisp
; コンスセルの生成
(cons 1 2) ; (1 . 2)
```

コンスセルを構成する各値を`(a . b)`のように括弧とドットで区切って表現したものをドット対(dotted pair)と呼ぶ。  
なお、コンスの語源はconstruct。

コンスセルの左側の値にアクセスするには`car`を用いる。  
コンスセルの右側の値にアクセスするには`cdr`を用いる。

```lisp
(car (cons 1 2)) ; 1
(cdr (cons 1 2)) ; 2
```

`car`も`cdr`も`setf`可能。すなわち、下記のような記述ができる。

```lisp
(defparameter *cons* (cons 1 2))
*cons*
; (1 . 2)
(setf (car *cons*) 10)
; 10
*cons*
; (10 . 2)
(setf (cdr *cons*) 20)
; 20
*cons*
; (10 . 20)
```

**NOTE** `setf`マクロの中では`car`は`rplaca`に、`cdr`は`rplacd`に変換される。

```lisp
(macroexpand-1 '(setf (car *x*) 100))
; (SB-KERNEL:%RPLACA *X* 100)
(macroexpand-1 '(setf (cdr *x*) 100))
; (SB-KERNEL:%RPLACD *X* 100)
```

リストとは、コンスセルを鎖の様に連結することで得られるデータ構造だと言える。  
リスト中の特定の要素の値は、その要素のコンスセルの`car`によって参照され、次の要素はコンスセルの`cdr`によって参照される。  
最後の要素のコンスセルは`cdr`に`nil`を持つ。  
したがって、あるデータがリストであるとは、そのデータが`nil`(空リスト)かコンスセルへの参照を持っていることと同義だ。

リストを扱うときに`car`と`cdr`を使用すると意味が伝わりにくいため、`car`には`first`を`rest`には`rest`を代わりに使うほうが良い。

```lisp
(defparameter *list* (list 1 2 3 4))
(car *list*) ; 1
(first *list*) ; 1
(cdr *list*) ; (2 3 4)
(rest *list*) ; (2 3 4)
```

## 12.2 関数プログラミングとリスト
Common Lispにおいては、リストを操作する関数は、リストを無駄に生成しないように、リストを構成するコンスセルの一部を共有してリストを生成することがある。  
したがって、共有されたリストを変更すると、意図しないリストに対しても影響してしまう可能性がある。

例えば、`append`は、与えられた引数のうち、最後の引数に対してはリストの複製を行わず、そのリストの先頭に対して、他の引数のリストの`cdr`の参照を繋げるのみの操作をする。これにより、下図のようなデータ構造が作成される。

{{<figure src="lists.svg" alt="litst" width="400" align="aligncenter">}}

このデータ構造を考慮せずに`*list-2*`を変更すると、意図せずして`*list-3*`も変更してしまう。

```lisp
(defparameter *list-1* (list 1 2))
(defparameter *list-2* (list 3 4))
(defparameter *list-3* (append *list-1* *list-2*))
; (1 2 3 4)
(setf (first *list-2*) 0)
; 0
*list-2*
; (0 4)
*list-3*
; (1 2 0 4) ; *list-3*も変更されている
```



## 12.3 破壊的な操作
関数には、副作用によってリストを破壊するものがある。これを破壊的な操作と呼ぶことにする。  
これは2種類に大別できる。

### 副作用を目的に使用される操作
そもそも副作用を目的として使用される操作がある。これは**非**関数プログラミングのスタイルの操作。  
12.1で示した例のとおり、この種類の操作は、共有されているリストを書き換えてしまわないように注意する必要がある。

代表的なものには`setf`がある。  
また、`vertor-push`や`vector-pop`のように、内部で`setf`を呼ぶような操作も破壊的な操作。  


### 「リサイクルする」操作
リサイクル操作はこの本の造語。  
この種類の操作は、操作の結果を生成する際に、引数に渡されたリストのメモリ領域を再利用する。  
また、この種類の操作は、**関数的**なコードの中で利用することを想定されている。  

例えば`reverse`は関数的な操作だ。すなわち、引数に渡されたリストは変更されない。  
一方、これのリサイクル版の`nreverse`は破壊的な操作だ。すなわち、結果が、引数に渡されたリストのメモリ領域に上書きされる。  

```lisp
(defparameter *list* (list 1 2 3 4))

; reverse
; 新しいメモリ領域にデータを格納して、参照を付け替えている
(setf *list* (reverse *list*))
*list* ; (4 3 2 1)

(defparameter *list* (list 1 2 3 4))

; nreverse
(nreverse *list*)
*list* ; (4 3 2 1)
```

上記コードにおいて、`(setf *list* (reverse *list*))`の方も`*list*`を上書きしているには違いない。  
しかし、こちらは`*list*`を新しいメモリ領域にアサインし、そこに`reverse`の戻り値を格納している。  
したがって、元々の`*list*`のメモリ領域もガベージコレクションが働くまで確保された状態になる。  
一方で`nreverse`は元々の`*list*`のメモリ領域のみ使用しているため、無駄にメモリを消費することがない。

### 関数的な操作と破壊的な操作の対応
大抵の破壊的な操作には、共通して語頭に`n`がついている。`n`は`non-consing`、すなわち、新しいコンスセルを確保しないという意味。

大抵の破壊的な操作には、同じ演算結果を返す非破壊的な関数が存在し、その関数名の語頭には`n`がついていない。
しかし、下記のように例外もある。

- 関数的な操作 : `append`  
  破壊バージョン : `nconc`
- 関数的な関数 : `remove`,`remove-if`,`remove-if-not`,...  
  破壊バージョン : `delete`,`delete-if`,`delete-if-not`,...

大抵の破壊的な操作は、その操作による副作用が厳密に規定されていないため、その副作用を目的に使用できない。  
ただし、例外があり、`nconc`,`nsubstitute`,`nsubstitute-if`,...などは副作用の結果が規定されており、それを利用できる。

`nconc`は、下記の通り動作する。

1. 渡された空ではない全てのリストについて、そのリストの最後のコンスセルの`cdr`に、次の引数のリストの先頭を参照させる。
1. 上記を繰り返す。
1. 最後に最初のリストの先頭を返す。

`nsubstutute`とその亜種は、下記の通り動作する。

1. 引数のリストをトラバースする。
1. 古い値を保持しているコンスセルの`car`を新しい値に`setf`する。

上記のように、副作用が明確に規定されたものもあるが、通常はこの副作用を期待したコードは書くべきではない。

## 12.4 リサイクルな関数と構造共有の組み合わせ
リサイクルな関数を使う際には、その入力となるデータ構造が変更されることに注意する必要がある。  
しかし、大抵の場合、メモリ使用量の節約などのために、プログラムはデータ構造を共有している。

データ構造を共有している状況でリサイクルな関数を使用するには、リサイクルされたデータを今後使用しないことを注意深く確認する必要がある。しかし、リサイクルな関数の仕様、挙動を全て理解するのは難しい。  
そこで、安全な使用方法をイディオムとして覚えてしまうのが一番効率が良い。

### イディオム1:リストを`push`と`nreverse`で作る
下記に示すように、空のリストの先頭に対してどんどん`cons`(`push`も用いられる)していき、最後に`nreverse`することで、追加した順番になったリストを得る事ができる。  
`nreverse`は、リストの先頭から、コンスセルの`cdr`を直前のコンスセルへの参照に変更しながらトラバースする。
したがって、`nreverse`のオーダは、要素を追加していくときのオーダと等しくO(n)のため、この方法はそれなりに効率が良い。

```lisp
(defun upto (max)
  (let ((result nil))
    (dotimes (i max)
      (push i result))
    (nreverse result)))

(upto 10) ; (0 1 2 3 4 5 6 7 8 9)
```

### イディオム2:リサイクルな関数の戻り値を、入力となっていた値のメモリ領域に即`setf`する
これは、「関数的なスタイルで変数を束縛するように見せて、実は同じメモリ領域を使い回す」ということを実現するためのイディオム。

```lisp
(setf foo (delete nil foo))
```

**注意**  
上記の`foo`が、どこか他のデータと構造を共有していた場合、そのデータを破壊してしまう可能性がある。  
例として、先述のリスト`*list-2`と`*list-3*`を例に挙げる。

```lisp
*list-2* ; (0 4)
*list-3* ; (1 2 0 4) ; (0 4) の部分は*list-2*と共有している

(setf *list-3* (delete 4 *list-3*)) ; (1 2 0)

*list-2* ; (0) ←*list-2*のデータ構造が壊れた
```

`delete`ではなく`remove`を使用していれば、`*list-3*`のデータ構造は一切変更されず、新規に`(1 2 0)`が作成されることになる(一部のデータ構造が共有されるかもしれないが)。  
上記のように、`setf`を用いたイディオムは絶対に安全というわけではなく、使用状況をよく理解する必要がある。

### `sort`,`stable-srot`,`merge`もリサイクルな関数
11章に登場した`sort`,`stable-srot`,`merge`もリサイクルな関数だ。  
さらに、これらの関数には、非破壊的なバージョンが存在しない。  
したがって、これらの関数の入力となるデータを破壊したくなければ、`copy-list`で予めデータのコピーを作成しておく必要がある。

```lisp
(defparameter *list* (list 4 3 2 1))
(sort *list* #'<) ; (1 2 3 4)
*list* ; (4) ←破壊されている
```

## 12.5 リスト操作関数
Common Lispには、リストを操作する関数群が豊富に用意されている。  

- `first`, `second`, `third`, ...., `ninth`, `tenth`で、1-10番目の要素を取得できる
- `(nth n list)`は、任意の場所の値を取得できる
  - **`n`は0始まり**
  - `(elt list n)`も同様の事が可能だが、`nth`の方がリスト専用であるため高速
- `(nthcdr n list)`は、`cdr`を`n`回呼び出したのと同じ値を返す
  - `(nthcdr 0 list)`は`list`と等しい
  - `(nthcdr 1 list`は`(cdr list)`や`(rest list)`と等しい
- `(last list n)`は、後ろからn番目の要素を取得できる
  - **`n`は1始まり**
  - `n`を省略すると末尾要素
  - `(last '(0 1 2 3 4 5) 4) ; (2 3 4 5)`

`car`と`cdr`を合成した関数も存在する。例えば`cadr`は、`(car (cdr list))`と等しい。  
合成関数は合計28個存在する。


### `(last list [n])`
リストの末尾要素を返す。オプションの`n`指定で最後のn個のコンスセルを返す。

### `(butlast list [n])`
最後のコンスセルを除いたリストの**コピー**を返す。  
オプションの`n`指定で最後のn個のコンスセルを含めない。

### `(nbutlast list [n])`
`butlast`のリサイクルバージョン。

### `(ldiff list object)`
指定された`object`までのリストのコピーを返す。
`object`が`list`を構成するコンスセルそのものの場合に真となる。*1

- *1[Common Lisp Hyper Spec - ldiff](http://clhs.lisp.se/Body/f_ldiffc.htm)

```lisp
(let ((lists '#((a b c) (a b c . d))))
  (dotimes (i (length lists)) ()
    (let ((list (aref lists i)))
      (format t "~2&list=~S ~21T(tailp object list)~
                 ~44T(ldiff list object)~%" list)
      (let ((objects (vector list ; (a b c)
                             (cddr list) ; (c)
                             (copy-list (cddr list)) ; (c) (a b c)と別のコンスセル
                             '(f g h)
                             '()
                             'd
                             'x)))
        (dotimes (j (length objects)) ()
          (let ((object (aref objects j)))
            (format t "~& object=~S ~21T~S ~44T~S"
                    object (tailp object list) (ldiff list object))))))))

>>  list=(A B C)         (tailp object list)    (ldiff list object)
>>   object=(A B C)      T                      NIL
>>   object=(C)          T                      (A B)
>>   object=(C)          NIL                    (A B C)
>>   object=(F G H)      NIL                    (A B C)
>>   object=NIL          T                      (A B C)
>>   object=D            NIL                    (A B C)
>>   object=X            NIL                    (A B C)
>>  
>>  list=(A B C . D)     (tailp object list)    (ldiff list object)
>>   object=(A B C . D)  T                      NIL
>>   object=(C . D)      T                      (A B)
>>   object=(C . D)      NIL                    (A B C . D)
>>   object=(F G H)      NIL                    (A B C . D)
>>   object=NIL          NIL                    (A B C . D)
>>   object=D            T                      (A B C)
>>   object=X            NIL                    (A B C . D)
=>  NIL
```

### `(tailp object list)`
指定された`object`がリストを構成するコンスセルの一部である時真を返す。*1

### `(list* list+)`
1. 指定された引数のうち最後のもの以外をすべて含むリストを生成する。
1. 最後の引数を、生成したリストの最後のセルの`cdr`にする。*2

- \*2[Common Lisp Hyper Spec - list*](http://clhs.lisp.se/Body/f_list_.htm)

{{<figure src="list.svg" alt="litst" width="400" align="aligncenter">}}

### `(make-list size [:initial-element elm])`
n個の要素から成るリストを生成する。  
`:initial-element`で初期値を指定(既定値は`nil`)

### `(revappend list object)`
`reverse`と`append`の合成関数。  
最初の引数を反転し、2つ目の引数を結合する。

```lisp
(revappend '(1 2 3) '(a b c)) =>  (3 2 1 a b c)
(revappend '(1 2 3) '()) =>  (3 2 1)
```

### `(nreconc list object)`
`revappend`のリサイクルバージョン。

```lisp
 (let ((list-1 '(1 2 3))
       (list-2 '(a b c)))
   (print (nreconc list-1 list-2))
   (print (equal list-1 '(1 2 3)))
   (print (equal list-2 '(a b c))))
>>  (3 2 1 A B C)
>>  NIL
>>  T
=>  T
```

### `(consp object)`
オブジェクトがコンスセルのときに真を返す述語。

### `(atom object)`
オブジェクトがコンスセルでないときに真を返す述語。

### `(listp object)`
オブジェクトがコンスセルか`nil`のときに真を返す述語。

### `(null object)`
オブジェクトが`nil`のときに真を返す述語。  
`not`と等価だが、空リストか否かのテストの際には、意図を表すためにこちらを使うべき。
