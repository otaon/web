---
title: "書籍 Land of Lisp 11章 format関数でテキストを表示する"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
tags: ["lisp"]
categories: ["Notes"]
authors:
- otaon
---

## T.O.C.

- [11.1 `format`関数の呼び出し方](#111-format関数の呼び出し方)
- [11.2 制御シーケンス: *Lispの値* を表示する](#112-制御シーケンス-lispの値-を表示する)
- [11.3 制御シーケンス: 数値を整形する](#113-制御シーケンス-数値を整形する)
- [11.4 複数行出力](#114-複数行出力)
- [11.5 テキストを揃える](#115-テキストを揃える)
- [11.6 制御シーケンス: 繰り返し](#116-制御シーケンス-繰り返し)
- [11.7 綺麗な表を作るクレージーな整形トリック](#117-綺麗な表を作るクレージーな整形トリック)

----

`format`関数の構文と制御シーケンスを説明する。

## 11.1 `format`関数の呼び出し方

下記に`format`関数の構文を示す。

```lisp
(format t "Add onion rings for only ~$ dollars more!" 1.5)
```

### 第1引数:出力先

<dl>
  <dt>nil</dt>
  <dd>生成されたテキストを文字列として返す。</dd>
  <dt>t</dt>
  <dd>結果をコンソールに出力する。返り値はnilとなる。</dd>
  <dt>stream</dt>
  <dd>データを出力ストリームに書き出す。</dd>
</dl>

### 第2引数:制御文字列

`"......"`の部分は制御文字列といい、原則としてテキストはそのまま出力される。  
ただし、制御文字列の中に **制御シーケンス** を使用することで、出力形式に影響を与える。  
制御シーケンスは常に`~`で始まる。

### 第3?引数:値引数

制御文字列の後ろの引数は、実際の値、つまり整形され表示されるデータである。  
制御文字列に従ってこれらの値は解釈、整形される。

----

ここからは、制御シーケンスについて解説する。

## 11.2 制御シーケンス: *Lispの値* を表示する

<dl>
  <dt>~s</dt>
  <dd>(print1)と同じく、Lispが後から読み込めるような区切り文字も入っている。</dd>
  <dt>~a</dt>
  <dd>(princ)と同じく、人間が読みやすい形式で表示する。</dd>
</dl>

```lisp
> (format t "I am printing ~s in the middle of this sentence." "foo")
I am printing "foo" in the middle of this sentence.
> (format t "I am prining ~a in the middle of this sentence." "foo")
I am printing foo in the middle of this sentence.
```

### `~s`および`~a`制御シーケンスのパラメータ

<dl>
  <dt>~aや~sの前の整数n (例:~10a)</dt>
  <dd>出力の最小値の指定。値をフォーマットした文字列が整数nに満たなければ、スペースが右側に追加される。</dd>
</dl>

**`~a`の例** 制御シーケンスの部分が10文字になるように、`foo`の右に空白が7個追加される。

```lisp
> (format t "I am prining ~10a within ten spaces of room." "foo")
I am printing foo        within ten spaces of room.
                 ^^^^^^^
```

#### パラメータ一覧

<dl>
  <dt>第1パラメータ</dt>
  <dd>整数n</br>出力の最小幅を指定する。パディングにはスペースが使われる。</dd>
  <dt>第2パラメータ</dt>
  <dd>整数n</br>パディングのステップ数を指定する。</br>パディングは全体の表示幅が第1パラメータ以上になるまで続く。</dd>
  <dt>第3パラメータ</dt>
  <dd>整数n</br>パディング文字数の下限を指定する。</br>全体の表示幅ではなく、パディング文字数自体の下限であることに注意。</dd>
  <dt>第4パラメータ</dt>
  <dd>'文字</br>パディングに使用する文字を指定する。最初に'をつけることに注意。</dd>
  <dt>, (カンマ)</dt>
  <dd>各パラメータのセパレータ</dd>
  <dt>@ (アットマーク)</dt>
  <dd>パディング文字を左側に挿入することを指定する。</dd>
</dl>

## 11.3 制御シーケンス: 数値を整形する

### 整数の整形

下記の制御シーケンスを用いることで、様々な基数で数値を表示できる。

<dl>
  <dt>~x</dt>
  <dd>16進数で数値を表示する。</dd>
  <dt>~b</dt>
  <dd>2進数で数値を表示する。</dd>
  <dt>~d</dt>
  <dd>10進数で数値を表示する。</dd>
</dl>

数値用の制御シーケンス特有のパラメータが用意されている。

<dl>
  <dt>: (コロン)</dt>
  <dd>制御シーケンス文字の前に:を入れると、3桁ごとにカンマを入れる。</dd>
</dl>

### 浮動小数点の整形

下記の制御シーケンスを用いることで、様々な基数で数値を表示できる。

<dl>
  <dt>~f</dt>
  <dd>浮動小数点を表示する。</dd>
</dl>

浮動小数点用のパラメータを以下に示す。

<dl>
  <dt>第1パラメータ</dt>
  <dd>小数(整数部と小数点を含む)の表示幅。例えばPIに4を指定したら3.14と表示される。</dd>
  <dt>第2パラメータ</dt>
  <dd>小数点以下の表示幅。例えばPIに4を指定したら3.1416と表示される。(四捨五入される!)</dd>
  <dt>第3パラメータ</dt>
  <dd>数値を10^指定値倍する。例えばPIに2を指定したら100倍され314.16と表示される。</dd>
</dl>

### 通貨の整形

下記の制御シーケンスを用いることで、小数を含む通貨表示を指定できる。

<dl>
  <dt>~$</dt>
  <dd>"ドル.セント"の形式で表示する。1.5は1.50と表示される。</dd>
</dl>

## 11.4 複数行出力

Lispのコマンドとして、改行には2つ(`terpri`と`fresh-line`)がある。  

<dl>
  <dt>terpri</dt>
  <dd>現在の行を終了して、続く出力が新たな行に現れるようにする。</dd>
  <dt>fresh-line</dt>
  <dd>現在のカーソルが行頭いないときに限って改行する。</dd>
</dl>

formatコマンドでは、`terpri`と`fresh-line`それぞれに対応する制御シーケンスがある。

<dl>
  <dt>~%</dt>
  <dd>(terpriに相当)</br>現在の行を終了して、続く出力が新たな行に現れるようにする。</dd>
  <dt>~&</dt>
  <dd>(fresh-lineに相当)</br>現在のカーソルが行頭いないときに限って改行する。</dd>
</dl>

さらに、これら二つの制御シーケンスには改行数を指定するパラメータがある。

<dl>
  <dt>第1パラメータ</dt>
  <dd>改行数を指定する。~5%として、5つの空行を出力する。</dd>
</dl>

## 11.5 テキストを揃える

formatコマンドでは、テキストを揃える制御シーケンスがある。  
例えばテーブルを作ったり、センタリングしたりする制御シーケンスがある。  
ここでは下記のリストを使用して説明する。

```lisp
(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))
```

<dl>
  <dt>~t</dt>
  <dd>テキストが現れる位置を指定する。</dd>
  <dt>第1パラメータ</dt>
  <dd>整形後のテキストが現れるカラム位置。カラム位置は行頭から数える。</dd>
</dl>

```lisp
> (loop repeat 10
        do (format t "5t~a ~15t~a ~25t~a~%"
                   (random-animal)
                   (random-animal)
                   (random-animal)))

;     walrus    tick      dog
;     dog       dog       tick
;     tiger     tiger     kangaroo
;     kangaroo  tick      tiger
;     tiger     walrus    tiger
;     dog       tick      kangaroo
;     tiger     walrus    dog
;     walrus    tiger     dog
;     walrus    dog       dog
;     walrus    tick      dog
;NIL
```

文字がなるべく等しい距離をとって表示するようにするには、`~<`と`~>`制御シーケンスを使用する。

<dl>
  <dt>~<, ~></dt>
  <dd>~<と~>で囲まれた文字列を文字寄せする。</dd>
  <dt>~<の第1パラメータその1</dt>
  <dd>整数n</br>~<と~>で囲まれたブロックの幅を指定する。</br>例えば30と指定すると、ブロック全部で30文字分の幅を使用する。</dd>
  <dt>~<の第1パラメータその2</dt>
  <dd>:@</br>行全体に対して値をセンタリングする。文字列ごとではないことに注意。</dd>
  <dt>~;</dt>
  <dd>~<による文字寄せ対象となる新たな値が次に来ることを示す。(~;は文字寄せ用の空白を挿入する、と考えても良い。)</dd>
</dl>

**3つの文字列を30文字分の幅に配置する**

```lisp
(loop repeat 10
      do (format t "~30<~a~;~a~;~a~>~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))

;kangaroo        dog        dog
;tiger      tiger      kangaroo
;tiger      kangaroo     walrus
;tiger     kangaroo    kangaroo
;tick       kangaroo      tiger
;kangaroo      walrus      tick
;walrus      walrus      walrus
;tick        walrus       tiger
;tick         tick        tiger
;walrus       kangaroo      dog
;NIL
```

**3つの文字列を30文字分の幅に中央揃えで配置する**

```lisp
(loop repeat 10
      do (format t "~30:@<~a~;~a~;~a~>~%"
                 (random-animal)
                 (random-animal)
                 (random-animal)))

;     tiger     tick    dog    
;     tiger    dog    tiger    
;    tiger    dog    walrus    
;    dog    kangaroo    tick   
;   kangaroo   tiger   tiger   
;    walrus    tick   walrus   
;    tick    walrus    tick    
;     tiger    tick    tick    
;    dog    tick    kangaroo   
;   walrus   walrus  kangaroo  
;NIL
```

## 11.6 制御シーケンス: 繰り返し

formatではループを実現する制御シーケンスがある。

<dl>
  <dt>~{, ~}</dt>
  <dd>~{と~}で囲まれた制御文字列とリストを与えると、formatはリスト中のデータをループで処理する。</dd>
</dl>

以下にループの例を示す。

```lisp
(defparameter *animals* (loop repeat 10 collect (random-animal)))
*animals*
;("tiger" "dog" "tiger" "tick" "walrus" "walrus" "tiger" "tiger" "dog" "tiger")

;; リスト中の文字列を1ループにつき1つずつ取り出して整形する
(format t "~{I see a ~a!~%~}" *animals*)
;I see a tiger!
;I see a dog!
;I see a tiger!
;I see a tick!
;I see a walrus!
;I see a walrus!
;I see a tiger!
;I see a tiger!
;I see a dog!
;I see a tiger!
;NIL

;; リスト中の文字列を1ループにつき2つずつ取り出して整形する
(format t "~{I see a ~a... or was it a ~a?~%~}" *animals*)
;I see a tiger... or was it a dog?
;I see a tiger... or was it a tick?
;I see a walrus... or was it a walrus?
;I see a tiger... or was it a tiger?
;I see a dog... or was it a tiger?
;NIL
```

## 11.7 綺麗な表を作るクレージーな整形トリック

```lisp
(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))
;| 0  1  2  3  4  5  6  7  8  9 |
;|10 11 12 13 14 15 16 17 18 19 |
;|20 21 22 23 24 25 26 27 28 29 |
;|30 31 32 33 34 35 36 37 38 39 |
;|40 41 42 43 44 45 46 47 48 49 |
;|50 51 52 53 54 55 56 57 58 59 |
;|60 61 62 63 64 65 66 67 68 69 |
;|70 71 72 73 74 75 76 77 78 79 |
;|80 81 82 83 84 85 86 87 88 89 |
;|90 91 92 93 94 95 96 97 98 99 |
;NIL
```

上表は、以下の制御シーケンスによって表示される。

|制御シーケンス|制御内容|
|:------------:|--------|
|`\|`           |最初に`\|`を表示する|
|`~{`          |ループ制御を始める|
|`~<`          |1行ごとの文字揃えを始める|
|`\|~%\|`        |`\|`改行`\|`を表示する|
|`~,33:;`      |33文字分出力したらこの制御シーケンスに先立つ文字列を表示する|
|`~2d `        |2桁の数値と` `を表示する|
|`~>`          |1行ごとの文字揃えを終わる|
|`~}`          |ループ制御を終わる|
|`\|`           |最後に`\|`を表示する|

