---
title: "書籍 実践 Common Lisp 第7章 マクロ：標準的な制御構文の構築"
date:    2019-05-26T00:00:00+09:00
lastmod: 2019-05-26T00:00:00+09:00
draft: false
toc: true
tags: ["lisp", "実践-Common-Lisp"]
categories: ["実践-Common-Lisp"]
authors:
- otaon
---

## この章について
この章では、Common Lispに組み込まれているマクロの例と、その使い方について示す。

## 7.1 `when`と`unless`
先述の通り、`if`は特殊オペレータによって提供される。  
`condition`が評価され、その値が非`nil`なら`then-form`が評価されてその値が返る。  
`nil`なら、`else-form`が評価されてその値が返る。

```lisp
(if condition then-form [else-form])
```

この構文の欠点としては、`then-form`と`else-form`にそれぞれ1つのフォームしか記述できない点。  
愚直に解決するなら、`progn`を用いて、下記の通り記述する必要がある。

```lisp
(if (spam-p current-message)
  (progn
    (file-in-spam-folder current-message)
    (update-spam-database current-message)))
```

Common Lispには、`when`という標準のマクロが存在する。これを使うと、上記の式は下記の通り記述できる。

```lisp
(when (spam-p current-message)
  (file-in-spam-folder current-message)
  (update-spam-database current-message))
```

`when`に対応するマクロとして`unless`がある。これは条件が`when`と反転しており、`condition`が`nil`のときに本体のフォームが評価される。すなわち、下記と同等。

```lisp
(defmacro unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))
```

## 7.2 `cond`
条件分岐が複数ある場合は、`cond`を用いると便利。  
デフォルトの場合(必ず実行される場合)を用意する場合は、testに`t`つまり必ず真と評価される式を記述すれば良い。

```lisp
(cond
  (test-1 form*)
  (test-2 form*)
  ...
  (test-N form*))
```

## 7.3 `and`, `or`, `not`
`and`, `or`, `not`のうち、`not`は関数で、残りはマクロ。  
`and`, `or`はマクロであるため、短絡評価できる(関数だと評価を途中で止めることはできない)。

## 7.4 繰り返し
Common Lispにおける特殊オペレータには、ループ構文を直接サポートするものは一つもない。  
全てのループ制御構造は、原始的な`goto`の機能を提供する2つの特殊オペレータ(`tagbody`と`go`)を用いて、多重に抽象化されて構成されている。

上記の２つの特殊オペレータを別にすると、抽象化の最下層は非常に汎用的(で難解)なループ構文である`do`だ(7.6章参照)。  
この`do`を用いて、幾分か使いやすい`dolist`や`dotimes`が定義されている。  
また、さらに多機能なマクロとして`loop`がある。

## 7.5 `dolist`と`dotimes`
`dolist`は、`list-form`から次々に変数値を取り出しながら`var`に代入し、`body-form*`を実行していく。  
これをリスト全体に対してループする。

```lisp
(dolist (var list-form)
  body-form*)
```

リストの途中でループから抜けたいなら、`return`を使用する。

```lisp
(dolist (x '(1 2 3))
  (print x)
  (if (evenp x)
    (return)))
; =>
; 1
; 2
; NIL
```

`dotimes`は、指定回数(`count-form`)だけループする。  
`count-form`は必ず整数値に評価される必要がある。

```lisp
; 0からcount-form - 1までループする
(dotimes (var count-form)
  body-form*)
```

```lisp
(dotimes (i 4)
  (print i))
; =>
; 0
; 1
; 2
; 3
; NIL
```

## 7.6 `do`
`do`は汎用的なループを提供するマクロだが、シンタックスが少々複雑。

```lisp
(do (variable-definition*)
    (end-test-form result-form*)
  statement*)
```

各`variable-definition`では、ループの本体で使用する変数を定義する。  
`variable-definition`一つの定義は下記の通り。

```lisp
; variable-definition
(var init-form step-form)
```

`init-form`がループの開始時に評価されて、その値が`var`に入る。  
次のループの繰り返し毎に`step-form`が評価され、その新しい値が`var`に代入される。  
`init-form`を省略すると、`var`は`nil`に束縛される。

`step-form`は省略可能で、省略した場合は、`body-form`で新たに値を代入するまで`var`の値は変化しない。

毎回のループ開始時には、全てのループ変数に新しい値が与えられてから`end-test-form`が評価される。  
その値が**`nil`**である限りループが繰り返され、`statements`が順番に実行される。

`end-test-form`が真になったら`result-form`が評価されて、その最後のフォームの値が`do`の値として返される。

`do`の構文は以上のとおり覚えにくい。ここで、**`do`において絶対に必要となる括弧**を下記に示す。

```lisp
(do (variable-definition*)
    (end-tet-form result-form*)
  statement*)
```

`result-form`を省略した`do`も記述できる。

```lisp
(do ((i 0 (1+ i)))
    ((>= i 4))
  (print i))

; 上記と同等のコード
(dotimes (i 4)
  (print i))
```

`statement`を省略した`do`も記述できる。  
下記は10番目のフィボナッチ数を返す例。

```do
(do ((n 0 (1+ n)) ; variable-definition : index
     (cur 0 next) ; variable-definition : 現在のフィボナッチ数
     (next 1 (+ cur next))) ; variable-definition : 次のフィボナッチ数
    ((= 10 n) cur))	; result-form : 10番目のフィボナッチ数を返す
```

変数を一つも束縛しない`do`も記述できる。  
下記は現在時刻がダイナミック変数の値より小さい間、1分に1回"Waiting"と表示する。  
**1つもループ変数を使わなくとも、空の変数リストが必要である点に注意。**

```lisp
(do ()
    ((> (get-universal-time) *some-future-date*))
  (format t "Waiting~%")
  (sleep 60))
```

## 7.7 `loop`
`loop`マクロは、できる事が多い。多すぎて全てを把握し切るには時間と労力が必要となる。

非常に単純なループは、何も変数を束縛しない無限ループで、下記の通り。

```lisp
(loop
  body-form*)
```

先述の`do`の無限ループを`loop`で書き直すと下記の通り。

```lisp
(loop
  (when (> (get-universal-time) *some-future-date*)
    (return))
  (format t "Waiting~%")
  (sleep 60))
```

「ループキーワード」と呼ばれる`loop`マクロ特有のキーワードを使用すると、`loop`でかなり複雑な繰り返しを実現できる。

例として、1から10までの数をリストに集めるループを示す。

```lisp
(loop for i from 1 to 10 collecting i)
; => (1 2 3 4 5 6 7 8 9 10)

; 上記と同等のdoループ
(do ((nums nil)
     (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums))
; => (1 2 3 4 5 6 7 8 9 10)
```

**最初の10個の平方数を合計する例**

```lisp
(loop for x from 1 to 10 summing (expt x 2))
; => 385
```

**文字列中の母音を数える例**

```lisp
(loop for x across "the quick brown fox jumps over the lazy dog"
      counting (find x "aeiou"))
; => 11
```

**11番目のフィボナッチ数を計算する例**

```lisp
(loop for i below 10
      and a = 0 then b
      and b = 1 then (+ b a)
      finally (return a))
```

上記の通り、`loop`マクロには様々なループキーワードを用いることができる。
