---
title: "書籍 Land of Lisp 15章 ダイスオブドゥーム：関数型スタイルでゲームを書こう"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
tags: ["lisp"]
categories: ["Notes"]
authors:
- otaon
---

## T.O.C.

- [15.5 ダイス・オブ・ドゥームを高速化する](#155-ダイスオブドゥームを高速化する)

----

## 15.5 ダイス・オブ・ドゥームを高速化する

### クロージャ

クロージャは、`lambda`で関数が作られるとき、外側の情報を捕獲したものである。

まずは、普通の関数を定義する。これは、`5`を返す関数である。

```lisp
> (defparameter *foo* (lambda ()
                        5))
*FOO*
> (funcall *foo*)
5
```

次に、示す関数は、クロージャの実装例である。  
最初にローカル変数`x`を作り、それに`5`を代入している。
そして、`lambda`の本体から、`x`の値を参照して返している。

```lisp
> (defparameter *foo* (let ((x 5))
                      (lambda ()
                        x)))
*foo*
> (funcall *foo*)
5
```

上の通り、クロージャでは関数が定義された時に参照した変数を捕捉している。

この動作は、Lispがガベージコレクタを持っていることを考えると理解しやすい。  
ガベージコレクタは、アロケートされた変数がどこからも参照されなくなると、メモリを解放する。
上の例では、`let`の中で`lambda`を使っている。
この場合、`let`を抜けても、変数は`lambda`の中から参照されている。
したがって、ガベージコレクタは変数を回収しない。  
そして、`lambda`自身がガベージコレクタに回収されるまでは変数も生き続けることになる。

クロージャを使うことで、関数に紐づけたスタティック変数があるかのような処理を実装できる。  
下の例では、関数が呼ばれる度に、捕捉した行番号を表示しつつインクリメントする。

```lisp
> (let ((line-number 0))
    (defun my-print (x)
      (print line-number)
      (print x)
      (incf line-number)
      nil))
MY-PRINT
> (my-print "this")
0
"this"
nil
> (my-print "is")
1
"is"
nil
> (my-print "a")
2
"a"
nil
> (my-print "a")
3
"test"
nil
```


### メモ化

メモ化とは、関数が受け取った引数と、その結果を記録しておくテクニックである。
このテクニックは、副作用がない関数(=関数型プログラミングによる関数)に対して使える。
また、このテクニックは、クロージャを使って実現できる。  

#### `neighbors`関数をメモ化する

まずは、与えられたマスから攻撃可能な隣り合うマスを計算する`neighbors`関数をメモ化してみる。

```lisp
> (neighbors 0)
(3 1 4)
```

上のとおり、`neighbors`に引数`0`を渡した時の返り値は`(3 1 4)`となる(ゲーム盤が3x3の場合)。  
また、この関数は不変のゲーム盤に対する不変的な位置計算をするものであるため、メモ化の対象とできる。
`neighbors`関数をメモ化したものを下に示す。

```lisp
(let ((old-neighbors (symbol-function 'neighbors))
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (funcall old-neighbors pos)))))
```

最初に定義したレキシカル変数の`symbol-function`は、引数のシンボルに束縛されている関数を取り出すコマンドである。
したがって、`old-neighbors`変数には、この行が評価されるよりも前に定義した`neighbors`が束縛される。
つまり、この後に同名の`neighbors`関数を再定義しても、以前のバージョンの定義にアクセスできるという寸法である。

次に定義したレキシカル変数の`previous`は、渡された引数とその結果とを全て保存していくためのハッシュテーブルである。
このハッシュテーブルは、引数をキー、結果を値とする。

そして、新たに`neighbors`関数を定義して以前のバージョンの`neighbors`を上書きする。
この新しい定義の`neighbors`関数は、以前の定義の`neighbors`関数にメモ化処理を加えたものである。  
この新しい定義の`neighbors`関数は、はじめに、引数`pos`を使ってハッシュテーブルを調べる。  
既に値が登録されていれば、その引数をキーとした値を取り出して返す。
未だ値が登録されていなければ、その引数を使って`old-neighbors`(つまり以前のバージョンの`neighbors`)を呼び出した結果を、引数をキーとしてハッシュテーブルに登録する。  
`setf`はセットされた値を返すから、最後の式では、ハッシュテーブルへ登録すると同時に`old-neighbors`の返り値を返している。

#### ゲーム木をメモ化する

ゲーム木を計算する関数において、同じゲーム木を何度も計算するのは全くの無駄な処理である。  
そこで、`game-tree`関数をメモ化して、同じゲーム木を見つけたらそのゲーム木の枝を共有することとする。  
下に、`game-tree`関数をメモ化するコードを示す。

```lisp
(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))  ; キーの比較関数にequalpを使う
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
        (setf (gethash rest previous) (apply old-game-tree rest)))))
```

ハッシュテーブルのキーの比較関数に`equalp`関数を使用したのは、キーがゲーム盤を含む配列であるからである。
テスト関数に`equalp`を使えば、ゲーム盤の全てのマスの同値性を比較して、完全一致した時に以前の計算結果が使われるようにできる。

また、`old-game-tree`関数には引数が複数あるため、`&rest rest`と表記することでリスト`rest`として扱っている。  
そして、`apply`によりリスト`rest`を個々の引数として`old-game-tree`に適用している。

#### `rate-position`関数をリスト化する

最後に、メモ化する効果が高い`rate-position`をメモ化することを考える。  
メモ化のコードは下のとおりである。

```lisp
;; クロージャとして補足する値: 特定のプレイヤーに対する特定のゲーム木に対応する点数のハッシュテーブル
(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))  ; 引数のプレイヤーについての返り値の記憶を辿る
	  ;; 引数のプレイヤーについての返り値が記憶されていなければ、
      ;; 引数のプレイヤー用のハッシュテーブルを新規作成する
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table))))
      ;; 引数のプレイヤーについて、引数のゲーム木が記憶されていれば、それに対応する値を返す
      ;; 記憶されていなければ、引数のプレイヤーと引数のゲーム木に対応する戻り値を新たに計算して記憶し、
      ;; それを返り値とする
      (or (gethash tree tab)
          (setf (gethash tree tab)
                (funcall old-rate-position tree player))))))
```

`rate-position`には問題がある。  
`rate-position`の引数である`tree`はゲーム木であるため、非常に大きなデータである可能性がある。
また、`game-tree`で使用した`equalp`は同値性の比較をするため、大きなデータに対しては比較コストが非常に高い。  
したがって、これを`game-tree`と同様に`equalp`(同値性比較)で比較すると、キーの比較だけで処理が増大してしまい、メモ化の効果が薄れる可能性がある。

ところで、先程の`game-tree`関数のメモ化によって、同値のゲーム木は必ず同一のインスタンスとなることが保証されている。
そこで、`rate-position`の引数のうち、`tree`は、デフォルトの`eql`(低コストな同一性比較)で済むようにしたい。
なお、残りの引数`player`はシンボルであるため、`player`単体ならば既にデフォルトの`eql`で比較可能である。  
(`tree`と`player`をコンスしたりしてしまうと同一性が保てない点に注意。)

そこで、`rate-position`関数の2つの引数(`tree`と`palyer`)を別々に記憶しておくようにしたい。  
上のコードでは、ネストしたハッシュテーブルを使用してそれを実現している。
下に、このハッシュテーブルの構造を示す。

```lisp
; ネストしたハッシュテーブルの構造
; previous = #S((player1 . tab1)
;               (player2 . tab2))
; tab = #S((tree1 . ret1)
;          (tree2 . ret2))

> previous
#S((プレイヤーID-1 . #S((ゲーム木a . 返り値1-a)
                        (ゲーム木b . 返り値1-b)))
   (プレイヤーID-2 . #S((ゲーム木c . 返り値2-c)
                        (ゲーム木d . 返り値2-d))))
```

**NOTE:** メモ化は、関数型スタイルで書かれたコードの最適化に使えるテクニックであるが、メモ化するコード自体は *以前の計算結果* という状態を持つため、関数型では書けない。


### 末尾再帰最適化

ここでは、 **末尾再帰最適化** と呼ばれる、関数型プログラミングの最適化テクニックを説明する。  
このテクニックを理解するために、リストの長さを求める簡単な関数を考えてみる。

```lisp
> (defun my-length (lst)
    (if lst
        (1+ (my-length (cdr lst)))
        0))
MY-LENGTH
> (my-length '(fie foh fum))
3
```

じつは、この関数はかなり非効率である。  
試しに、とても大きなリストにこの関数を適用すると、CLISPではプログラムがクラッシュする。

```lisp
;; 注意:このプログラムはクラッシュするので実行しないこと!!
> (defparameter *biglist* (loop for i below 100000 collect 'x))
*BIGLIST*
> (my-length *biglist*)

*** - Program stack overflow. RESET
```

なぜクラッシュするのか。
それは、再帰された関数を呼び出す際に現在の関数の情報をスタックに積むからである。  
スタックに積み上げたデータが取り出されるのは、関数が終了した時であるから、関数が再帰的に呼び出され続けていればスタックオーバーフローを起こす。  
ただし、処理系によってはスタックオーバーフローが起こらないように設計されている。

この問題を回避したバージョンの`my-length`を以下に示す。

```lisp
> (defun my-length (lst)
    (labels ((f (lst acc)  ; アキュムレータ
               (if lst  ; このlstはfのローカル変数
                   (f (cdr lst) (1+ acc))
                   acc)))
            (f lst 0)))
MY-LENGTH
> (my-length '(fie foh fum))
3
```

このバージョンでは、リストを走査するローカル変数`f`を定義して、それを再帰的に呼び出している。  
この関数`f`は、入力リストに加え、余分な引数`acc`を取る。  
この`acc`はアキュムレータ(accumlator)と呼ばれる。
引数`acc`は、それまでにいくつのリストの要素に出会ったかを数えている。
一番最初に`f`を呼び出すとき、`acc`は`0`である。  
アキュムレータを使うと、関数`f`が自分自身を再帰的に呼び出す際にその結果を受け取って`1`を加算しなくても良い。
代わりに、引数`acc`に`1`を加算した値を再帰呼び出しの引数へと渡していく。
リストの最後に到達したら(`list`が`nil`)、引数の`acc`はリストの要素数と同じになっているから、この`acc`をそのまま返せば良い。

*(accumlator(アキュムレータ)とは、CPUの演算回路を構成するレジスタの一種で、論理演算や四則演算などによるデータの入出力と結果の保持に用いられるレジスタのことである。)*  
*(accumlate: 蓄積する。)*

このバージョンで大事なのは、「リストが空ではない場合、`f`の最後の処理が **自分自身を呼び出すこと** である」ということである。
Lispの関数が、その最後の処理として自分自身や他の関数を呼び出すとき、それを末尾呼び出しと呼ぶ。  
末尾呼び出しの場合、Lispでは現在の状態をスタックに積み上げず、すぐに`f`の処理に取り掛かる。
これは、C言語の`longjump`やBASICの`GOTO`に相当する動きである。  
現在の状態をスタックに積み上げない場合、スタック操作が無い分非常に速く、そもそもスタックを消費せずに済む。
また、Lispの末尾呼び出しは`longjump`や`GOTO`とは違い、構造化プログラミングの範疇となり、安全な処理のままである。

また、上の例の`lst`は、下の通り2種類の意味で使われている。

- `my-length`の引数
- `f`の引数

したがって、`f`の内部では、`lst`は`f`の引数として扱われる。  
このように、同じ名前の変数があるときに近い方の引数が優先されることを、「変数の **シャドウイング** 」という。

##### Common Lispにおける末尾呼び出しのサポート

Common Lispにおいては、コンパイラ/インタプリタが末尾呼び出しを最適化することを常に期待できない。
何故なら、ANSI Common Lispでは、末尾再帰最適化を要求していないからである。  
(`Scheme`では、その規格において末尾呼び出し最適化を厳密に要求している)  
ただし、ほとんどのCommon Lispの処理系では、末尾呼び出し最適化をサポートしている。

**CLISPでは、末尾呼び出し最適化を有効にするために、以下のコードを実行する必要がある。**

```lisp
(compile 'my-length)
```

わざわざ末尾呼び出し最適化を有効にするためにコード実行が必要である理由としては、末尾呼び出し最適化が性能上の問題を引き起こすケースが存在するからである。  
また、プログラムをデバッグする際には、スタックにはなるべく多くの情報が保存されていた方が良いに決まっているが、末尾呼び出し最適化を施してしまうと、その情報は失われてしまう。

##### ダイス・オブ・ドゥームでの末尾呼び出し最適化

ダイス・オブ・ドゥームで末尾呼び出し最適化の効果が大きく現れるのは、`add-new-dice`関数である。  

まずは、末尾呼び出し最適化していないバージョンの`add-new-dice`関数を示す。

```lisp
(defun add-new-dice (board player spare-dice)
  "ゲーム盤にサイコロを足していく
   board: 現在のゲーム盤情報
   player: 現在のプレイヤーID
   spare-dice: 補給できるサイコロの個数
   ret: サイコロ追加後のゲーム盤情報"
  (labels ((f (lst n)
             ;; lst: ゲーム盤情報(リスト)
             ;; n: 補給できるサイコロの個数

             ;; ゲーム盤情報が無ければ、そのまま無し(nil)を返す
             ;; 補給できるサイコロが無ければ、ゲーム盤情報を返す
             ;; その他の場合、サイコロを補給する
             (cond ((null lst) nil)
                   ((zerop n) lst)
                   (t (let ((cur-player (caar lst))  ; 現在のプレイヤーID
                            (cur-dice (cadar lst)))  ; 着目中のマスのサイコロの個数
                        (if (and (eq cur-player player) (< cur-dice *max-dice*))
                            ;; 着目中のマスが現在のプレイヤーのマス、かつ、
                            ;; マスにおけるサイコロの個数が上限でなければ、
                            ;; サイコロを追加して次のマスへ移動
                            (cons (list cur-player (1+ cur-dice))
                                  (f (cdr lst) (1- n)))
                            ;; そうでなければ、サイコロを追加せずに次のマスへ移動
                            (cons (car lst) (f (cdr lst) n))))))))
    ;; ゲーム盤情報をリストに変換して、
    ;; サイコロを追加して、
    ;; ゲーム盤情報を再び配列に戻す
    (board-array (f (coerce board 'list) spare-dice))))
```

次に、末尾呼び出し最適化を施した`add-new-dice`関数を以下に示す。
```lisp
(defun add-new-dice (board player spare-dice)
  "ゲーム盤にサイコロを足していく
   board: 現在のゲーム盤情報
   player: 現在のプレイヤーID
   spare-dice: 補給できるサイコロの個数
   ret: サイコロ追加後のゲーム盤情報"
  (labels ((f (lst n acc)
             ;; lst: ゲーム盤情報(リスト)
             ;; n: 補給できるサイコロの個数
             ;; acc: 新たなサイコロの追加を考慮された、更新済みのマスのリスト(右下->左上の順)

             (cond
               ;; 補給できるサイコロが無ければ、ゲーム盤情報を返す
               ((zerop n) (append (reverse acc) lst))
               ;; ゲーム盤を最後まで走査したら、サイコロ追加後のゲーム盤情報を返す
               ((null lst) (reverse acc))
               ;; その他の場合、サイコロを補給する
               (t (let ((cur-player (caar lst))  ; 現在のプレイヤーID
                        (cur-dice (cadar lst)))  ; 着目中のマスのサイコロの個数
                    (if (and (eq cur-player player) (< cur-dice *max-dice*))
                        ;; 着目中のマスが現在のプレイヤーのマス、かつ、
                        ;; マスにおけるサイコロの個数が上限でなければ、
                        ;; サイコロを追加して次のマスへ移動
                        (f (cdr lst)  ; サイコロを足していく対象のゲーム盤のうち未走査部分
                           (1- n)  ; 補給できるサイコロを1減らす
                           (cons (list cur-player (1+ cur-dice)) acc))  ; 更新済みのマスのリスト
                        ;; そうでなければ、サイコロを追加せずに次のマスへ移動
                        (f (cdr lst)  ; サイコロを足していく対象のゲーム盤のうち未走査部分
                           n  ; 補給できるサイコロ
                           (cons (car lst) acc))))))))  ; 更新済みのマスのリスト
    ;; ゲーム盤情報をリストに変換して、
    ;; サイコロを追加して、
    ;; ゲーム盤情報を再び配列に戻す
    (board-array (f (coerce board 'list) spare-dice ()))))
```

関数`f`の引数のアキュムレータ`acc`に渡されるのは、新たなサイコロの追加を考慮された、更新済みのマスのリストである。  
`f`の中では、2箇所で`f`自身を末尾呼び出ししており、それぞれ、新たなマスの情報を`acc`に`cons`している。

注意点としては、`acc`には左上から右下に向けて走査しつつ`cons`していっているため、左上の情報はリストの末尾に、右下の情報はリストの先頭にある。
したがって、正しいゲーム盤情報を返すには`acc`を`reverse`する必要がある。
