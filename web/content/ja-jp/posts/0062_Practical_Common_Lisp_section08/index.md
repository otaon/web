---
title: "書籍 実践 Common Lisp 第8章 マクロ：自分で定義しよう"
date:    2019-05-27T00:00:00+09:00
lastmod: 2019-05-27T00:00:00+09:00
draft: false
toc: true
tags: ["lisp", "実践-Common-Lisp"]
categories: ["実践-Common-Lisp"]
authors:
- otaon
---

## この章について
この章では、マクロの定義の方法について示す。

## 8.1 マックはじめて物語
省略。

## 8.2 マクロ展開時 vs. 実行時
マクロを書く時に自分で記述しているコードは、コンパイラがコンパイルするコードを生成するためのコード。  
全てのマクロが完全に展開されて、初めて結果として得られるコードがコンパイルされて実行される。  

**マクロ展開時(macro expansion time)**
: マクロが動作する時間。

**実行時(runtime)**
: 通常のコード(マクロによって生成されたコードも含む)が実行される時間。

マクロ展開時に動作するコードは、実行時とは全く異なる環境で動作する。
すなわち、マクロ展開時は、ソースコードに予め存在しているデータのみ扱うことが可能。

```lisp
(defun foo (x)
  (when (> x 10)
    (print 'big)))
```

上記のコードの場合、マクロ展開時にマクロ`when`に渡されるのは、`x`が評価された値ではなく、`(when (> x 10)`や`(print 'big)`のような「ソースコードを表すLispのリスト」。

```lisp
(defmacro when (condition &rest body)
  `(if ,condition (progn ,@body)))

; マクロ展開されたコード
(if (> x 10) (progn (print 'big)))
```

マクロ展開時と実行時は、Lispがインタプリタ上で動作している場合は境界が曖昧。  
更に、言語仕様ではインタプリタのマクロの扱い方について(逐次解釈の間に全マクロが展開されて最終的に出来上がったコードが実行されるor逐次解釈中に出会ったマクロをその場で展開する)を正確に規定していない。

## 8.3 `defmacro`
マクロは`defmacro`を使って定義する。

```lisp
(defmacro name (parameter*)
  "documentation comment"
  (body-form*))
```

**マクロフォーム**
: 先頭の要素がマクロの名前になっているLispフォーム。

マクロは、マクロフォームを変換して、実行可能なコードに仕上げる。  
すなわち、マクロを作成するときは、下記の通りに進めるのが分かりやすい。  
下記の1.と2.は逆転させても良い。

1. マクロを呼び出す例を1つ以上記述する。
1. その例がマクロによって展開されるべきコードを記述する。
1. 実際にマクロを記述する。

## 8.4 試しにマクロを書いてみる & 8.5 マクロのパラメータ
実際に、素数をイテレートする`do-primes`というマクロを定義してみる。  
準備として、下記の2つの補助関数を定義する。

**`primep`**
: 与えられた数が素数かどうかを判定する。

```lisp
(defun primep (number)
  "与えられた数が素数かどうかを判定する"
  (when (> number 1)
    (loop for fac from 2 to (isqrt number)
          never (zerop (mod number fac)))))
```

**`next-prime`**
: 与えられた数以上の素数を返す。

```lisp
(defun next-prime (number)
  "与えられた数以上の素数を返す"
  (loop for n from number
        when (primep n) return n))
```

### 1. マクロを呼び出す例を1つ以上記述する
ここでは、例えば、0以上19以下の素数それぞれに対してループの本体が1回だけ実行されるような呼び出しを記述する。

```lisp
(do-primes (p 0 19)
  (format t "~d " p))
```

### 2. その例がマクロによって展開されるべきコードを記述する
上記の`do-primes`コードが展開された状態を記述する。

```lisp
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))
```

### 3. 実際にマクロを記述する
上記の1.から2.に変換するマクロを記述する。  
両者を見比べると、それぞれ対応する箇所が分かる。これを、字句的に変換すれば良い。


```lisp
(defmacro do-primes (var-and-range &body body)

  (let ((var   (first var-and-range))
        (start (second var-and-range))
        (end   (third var-and-range)))
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
    ,@body)))
```

**NOTE:**  
マクロの引数の`&body`と`&rest`は等価だが、大抵の開発環境では`&body`パラメータがあるとマクロで使うインデントを調整してくれるようになる。


上記の例は、引数にリストを与えて、それをマクロ内部でわざわざ分配して束縛する必要がある。これを簡略化する方法がある。  
マクロのパラメータのリストは**分配パラメータリスト(destructuring parameter list)**と呼ばれ、パラメータリストを入れ子にして指定できる。  
すなわち、入れ子にしたパラメータリスト内のパラメータは、それぞれに対応する位置の値に束縛される。  
これにより、例えば上記のマクロ定義は下記のように簡略化できる。

```lisp
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
    ,@body))
```

**NOTE:**  
さらに、分配パラメータリストを用いると、下記のような恩恵を受けられる。
- 自動的に引数の個数や構造が誤っていた時にエラーを発生させてくれる。
- SLIMEなどの開発環境で引数をサジェストしてくれるようになる。

## 8.6 展開系の生成
シンプルなマクロでは、上記のようにバッククォート式を用いたシンタックスが適している。  
バッククォート式(\`で始まるリスト)はクォート式('で始まるリスト)に似ているが、`,`(または`,@`)を使って部分式を「アンクォート」できる。アンクォートのそれぞれの違いは下記の通り。

- `,`を用いた場合、`,`に続く部分式は単純にその場所に埋め込まれる。
- `,@`を用いた場合、`,@`に続く部分式(リストである必要がある)は、その括弧が外れて埋め込まれる。

下記にバッククォート式の使用例を示す。

|バッククォートシンタックス&nbsp;|&nbsp;等価なリスト構築コード&nbsp;|&nbsp;結果|
|--------------------------|----------------------|----|
|``` `(a (+ 1 2) c) ```&nbsp;|&nbsp;`(list 'a '(+ 1 2) 'c)`&nbsp;|&nbsp;`(a (+ 1 2) c)`|
|``` `(a ,(+ 1 2) c) ```&nbsp;|&nbsp;`(list 'a (+ 1 2) 'c)`&nbsp;|&nbsp;`(a 3 c)`|
|``` `(a (list 1 2) c) ```&nbsp;|&nbsp;`(list 'a '(list 1 2) 'c)`&nbsp;|&nbsp;`(a (list 1 2) c)`|
|``` `(a ,(list 1 2) c) ```&nbsp;|&nbsp;`(list 'a (list 1 2) 'c)`&nbsp;|&nbsp;`(a (1 2) c)`|
|``` `(a ,@(list 1 2) c) ```&nbsp;|&nbsp;`(append (list 'a) (list 1 2) (list 'c)`&nbsp;|&nbsp;`(a 1 2 c)`|

## 8.6-2 マクロの検証方法(`macroexpand`と`macroexpand-1`)
マクロは`macroexpand`関数または`macroexpand-1`関数を使って展開できる。

- `macroexpand` : マクロを多段的に展開する。
- `macroexpand-1` : マクロを1段階だけ展開する。

これらの使用方法は下記の通り。

```lisp
(macroexpand 'macro-form)

; 使用例
(macroexpand '(do-primes (p 0 19) (format t "~d " p)))
; =>
; (do ((p (next-prime 0) (next-prime (1+ p))))
;     ((> p 19))
;   (format t "~d " p))
```

## 8.7 漏れをふさぐ
ここでは、マクロで抽象化しきれていないことを「漏れ」と呼ぶ。

### 「多重評価」による漏れ
マクロは字句的な変換であるため、1回のみ評価されることを期待する式を、予期せず複数回評価するようにマクロ展開してしまうことがある。  
例えば、`do-primes`の引数に、`19`のような**リテラル**ではなく、`(random 100)`のような評価するたびに値が変わる**式**を渡したとする。  
このときの`do-primes`のマクロ展開結果を確認すると、下記の通りになる。  
この例では、`end-test`部分に`(random 100)`があるため、終了判定の度に`(random 100)`が評価されることになる。  
したがって、下記のループは、  
「**ループ中の現在の素数が、100以下のある数(乱数で最初に決定)以下になるまで繰り返す**」  
ではなく、  
「**ループ中の現在の素数が、100以下の乱数(毎ループ変化する)以下になるまで繰り返す**」  
という挙動になる。

```lisp
(macroexpand '(do-primes (p 0 (random 100)) (format t "~d " p)))
; =>
; (do ((p (next-prime 0) (next-prime (1+ p))))
;     ((> p (random 100)))
;   (format t "~d " p))
```

上記のような多重評価を防ぐには、最初に評価済みの値を別の変数に束縛すれば良い。  
`do`マクロには`init-form`部分があるから、この部分で値を評価させて変数に束縛させれば良い。

```lisp
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end)) ; init-form部でendを評価
       ((> ,var ending-value))
    ,@body))
```

### 「異なる評価順序」による漏れ
マクロに渡した引数と同じ順番で値を評価されるようにする必要がある。  
さもなくば、副作用があったときに予期しない評価結果になる。

:o: **引数と本体の評価順序が一致しているコード**

```lisp
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end)) ; init-form部でendを評価
       ((> ,var ending-value))
    ,@body))
```

:x: **引数と本体の評価順序が一致していないコード**

```lisp
(defmacro do-primes ((var start end) &body body)
  `(do ((ending-value ,end) ; init-form部でendを評価
        (,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
    ,@body))
```

### 「変数名の重複」による漏れ
単純にマクロ中で変数名を定義すると、変数名がマクロに渡されたコードやマクロ呼出しのコンテキストに影響してしまう。  
例えば、下記のコードはマクロで使用している`ending-value`を引数に渡してしまっているため正しく動作しない。

```lisp
(do-primes (ending-value 0 10)
  (print ending-value))
; =>
; (do ((ending-value (next-prime 0) (next-prime (1+ ending-value)))
;      (ending-value 10))
;     ((> ending-value ending-value))
;   (print ending-value))
```

これを解決するには`gensym`関数を用いる。`gensym`は、呼び出される度にユニークなシンボルを返すことを保証している。  
このシンボルはLispの読み取り器には決して読み取られないため、どのパッケージでもインターンされない。  
この関数を使えば、`ending-value`のようなリテラル名ではなく、`do-primes`が展開される度に新しいシンボルを生成できる。

:o: 全ての「漏れ」が塞がれたコード

```lisp
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
      `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
            (,ending-value-name ,end))
       ((> ,var ,ending-value-name))
    ,@body)))
```

`gensym`を呼び出している箇所が、展開系に含まれていないことに注意。  
`gensym`はマクロ展開の一部として実行されるため、マクロが展開される度に新しいシンボルが生成される。

`var`の値はマクロフォームが読み込まれた時に読み取り器によって作られるが、`ending-value-name`の値はマクロのコードが実行される時にプログラムで生成される。

**マクロのコードが実行されるとき**

```lisp
(defmacro do-primes ((var start end) &body body) ; var start endが束縛される
  (let ((ending-value-name (gensym))) ; gensymが評価された値を束縛される
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
      ,@body)))
```

**マクロフォームが読み込まれたとき**

```lisp
(do-primes (ending-value 0 10) (print ending-value))
; ↓マクロ展開
(do ((ending-value (next-prime 0) (next-prime (1+ ending-value)))
     (#:G567 10))
    ((> ending-value #:G567))
  (print ending-value))
```

### マクロを書くルール
- 基本的には、マクロ呼び出し時に、出現時と同じ順序で評価されるように、全ての部分フォームを配置する。
- 部分フォームが一度だけ評価されるようにする。  
  このためには、展開形の中で引数のフォームを評価した値を保持する変数を作り、展開形の別の場所で引数を使う必要がある場合はその変数を使うようにする。
- 展開系の中で使う変数の名前は、マクロ展開時に`gensym`を使って作る。

## 8.8 マクロを書くマクロ
関数を定義する際に補助関数を使用するように、もちろん、マクロを定義する際に補助のマクロを定義することもできる。  
ここでは、指定した変数を`gensym`で束縛させる処理を簡単に記述するためのマクロを定義する。

### 1. マクロを呼び出す例を1つ以上記述する
マクロ`with-gensyms`を呼び出す例を下記の通り記述する。

```lisp
(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
      ,@body)))
```


### 2. その例がマクロによって展開されるべきコードを記述する
マクロ展開した結果を下記の通り記述する。

```lisp
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
      `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
            (,ending-value-name ,end))
       ((> ,var ,ending-value-name))
    ,@body)))
```

### 3. 実際にマクロを記述する
`with-gensyms`を定義する。

```lisp
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
```

`with-gensyms`で使用した`loop`は下記の通りの挙動をする。

```lisp
(loop for n in '(a b c) collect `(,n (gensym)))
; =>
; ((A (GENSYM)) (B (GENSYM)) (C (GENSYM)))
```

`do-primes`において`with-gensym`を使った場合、`do-primes`の`defmacro`をコンパイルすると、その`with-gensym`が下記の通りに展開される。

```lisp
(let ((ending-value-name (gensym)))
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (,ending-value-name ,end))
       ((> ,var ,ending-value-name))
     ,@body)
```

上記は`let`を手動で記述したものと全く等しい。  
すなわち、`do-primes`を使う関数をコンパイルするときは、**`with-gensyms`で生成されたコード**が`do-primes`の展開系を生成するために実行される。  
しかし、`with-gensyms`自体は`do-primes`がコンパイルされた時点で既に展開されている。


## 8.9 シンプルなマクロの先へ
次章では、マクロを使ったテストフレームワークについて説明する。

## コラム `once-only` - **マクロを書くマクロ**の例
ここで挙げる`once-only`は、生成されたコードが特定の順番で一度だけ評価されるようなコードを生成するもの。  

```lisp
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names
                       collect (gensym))))
    `(let (,@(loop for g in gensyms
                   collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms
                       for n in names
                       collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names
                         for g in gensyms
                         collect `(,n ,g)))
             ,@body)))))
```

### マクロの使い方
`once-only`の引数に渡した値は即座に左から評価され、内部で一時変数に束縛される。(マクロ展開結果を見れば分かる。)

```lisp
(defmacro do-primes ((var start end) & body body)
  (once-only (start end)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))
```

### マクロ展開結果

```lisp
(macroexpand-1 '(once-only (a b c) (princ "Test")))
; =>
(let ((#:G543 (gensym)) (#:G544 (gensym)) (#:G545 (gensym))) ; (1)
  `(let (,`(,#:G543 ,a) ,`(,#:G544 ,b) ,`(,#:G545 ,c)) ; (2)
     ,(let ((a #:G543) (b #:G544) (c #:G545)) ; (3)
        (princ "Test")))) ; (4)
```

1. マクロの引数の個数分の一時変数を用意する
1. マクロの各々の引数を評価して、一時変数に代入する(束縛する)
1. マクロの各々の引数と同名の引数を作り、一時変数の値を束縛する
1. この時点で、`once-only`の第一引数に指定された変数は、同名の変数によってシャドーイングされており、  
   その値は既に評価済みとなっている。
