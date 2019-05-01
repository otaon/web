---
title: "書籍 Land of Lisp 18章 遅延プログラミング"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
toc: true
tags: ["lisp"]
categories: ["Notes"]
authors:
- otaon
---

関数型プログラミングによって、コードが簡潔になることが分かった。
数学的な関数は、同じ引数に対して常に同じ結果を返すことによる利点である。

しかし、15章で関数型プログラミングによってゲームを造ったとき、問題点も明らかになった。
すなわち、引数のみに依存して関数の値を計算しようとすると、引数に **膨大** な情報量を渡す羽目になる。

ダイスオブドゥームでは、ゲーム盤でこれから起き得る全ての状態を表した`game-tree`引数を渡していた。
この引数は、たった3x3のゲーム盤でさえ巨大な構造となっていた。
このときの設計は、コードを簡単でエレガントにしてはいたものの、より大きなゲーム盤に対して容易にはスケールしてくれない。
なぜなら、ゲーム木はゲーム盤が大きくなるにつれて指数関数的に大きくなるからである。

幸いにも、コードの現在のエレガントさを保ったまま(関数型プログラミングのまま)、より大きなゲーム盤で複雑なゲームを実現する方法は存在する。
すなわち、ゲームの最初から全ての可能性を見なくて済むように、 **遅延評価** という機能を使う。
この章では、遅延評価を使ってダイスオブドゥームを改善する。


# 18.1 Lispに遅延評価を足す

遅延評価を使っても、コード上ではゲームの初期化時にゲーム木を作ってしまうことに変わりはない。
ただし、ゲーム木の一部以外は、本物の枝を作る時にやっていたような実際の計算を行わない。
すなわち、実際にゲーム木のその箇所を評価する必要ができてからはじめて計算するのである。
プレーヤーがゲーム中のある手を指さず、AIもその手を考慮しなかったとしたら、プログラムはその枝から先がどうなっているかを計算しなくても良いのである。

計算の必要が出てきてからはじめて計算する部分を、遅延評価における **遅延部分** と呼ぶ。

HaskellやClojureといった言語では、遅延評価が言語のコアでサポートされている。
むしろ、Clojureでは遅延評価が推奨されている。
しかし、残念なことにCommon Lispには遅延評価やそれに類する機能がサポートされていない。
そこで、Common Lispのマクロ機能を使用することで、自分で遅延評価の機能を実現することとする。


## `lazy`コマンドと`force`コマンドを作成する

遅延評価の機能を実現するために、`lazy`コマンドと`force`コマンドを作成する。
まず、`lazy`コマンドは、コードを包むことでLispにそのコードの評価を後回しにするように指示する。
`lazy`の使用例を次に示す。
見て分かる通り、`lazy`コマンドで包まれたコードは関数としてまとめられる。

```lisp
> (lazy ( + 1 2))
#<FUNCTION ...>
```

次に、`force`コマンドは、先程の`lazy`コマンドによってまとめられた関数を実行する。
`force`の使用例を次に示す。

```lisp
> (force (lazy (+ 1 2)))
3
```

ここで重要な点は、「実際の計算が、遅延された値が作られたときではなく、その結果が要求された時にはじめて行われた」という事実である。
これを実感するため、より複雑な例を示す。

```lisp
> (defun add (a b)
    (princ "I am adding now")
    (+ a b))
ADD

> (defparameter *foo* (lazy (add 1 2)))
*FOO*

> (force *foo*)
I am adding now
3
```

この例では、2個の数を足す`add`関数を定義した。
この関数は、評価されるときに副作用としてコンソールにメッセージを表示する。
コンソールにメッセージが表示されているタイミングが、`force`を呼び出したときであることから、`add`の計算が実際にこの部分でなされたことが分かる。

### `lazy`コマンド

`lazy`の簡単な実装は次のとおりである。
`lazy`はマクロによって実現している。

```lisp
(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value  (gensym)))
    `(let ((,forced nil)
           (,value nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))
```

マクロは生成されたコード中で変数を2つ使用するため、`gensym`を使って変数の名前を作り出している。

次に来るのが、マクロが実際に生成するコード本体である(行頭にバッククォートがついている行)。
コード本体の行頭では、`gensym`によって作られた変数名を使って、ローカル変数を2つ生成している(`,forced`と`,value`)。
最初の変数`,forced`は、遅延した値が既に評価されたかどうかのフラグである。
これが`nil`であれば、値はまだ評価されていない。
これが`t`なら、すでに評価済みである。
次の変数`,value`は、評価された関数の戻り値を格納する変数である。

このマクロによって生成された`lambda`は、クロージャの仕組みを使って`,forced`変数と`,value`変数を捕捉している。
さらに、この`lambda`は、マクロの引数に渡された式をそのまま中に取り込んでいる。
これによって、`lambda`は、次の2種類の情報を持っている。

- クロージャによって呼び出されたか否かといった情報を持つ
- `lazy`呼び出し時に渡された式を内部にすべて持つ

ここで、`force`コマンドによって`lambda`が計算されると、次のとおり動作する。

- `,forced`が`nil`(まだ計算されていない場合)
  1. `nil`で初期化しておいた`,value`に計算結果を格納する
  1. `,forced`を`t`に更新する
- `,forced`が`t`(すでに計算されている場合)
  1. `,value`の値を返す(`lambda`は一切評価しない)


### `force`マクロ

次に`force`マクロを実装する。
`lazy`マクロのようなトリッキーな実装ではなく、`force`は非常に素朴な実装である。
単純に、引数に渡された関数を呼び出すだけである。

```lisp
(defun force (lazy-value)
  (funcall lazy-value))
```

## 遅延リストライブラリを作る

先程作ったコマンドを基にして、 **遅延リスト** のライブラリを作ることにする。
この遅延リストライブラリはClojureのものを参考にしている(Clojureでは遅延リストを **遅延シーケンス** と呼ぶ)。

Lispにおいてリストを扱う最も基本的なコマンドは`cons`である。
したがって、遅延リストでは`lazy-cons`コマンドから作成する。
このマクロは`cons`と似ているが、結果を`lazy`マクロで包んで返す。
ついでに、`lazy-car`と`lazy-cdr`も作っておくことにする。

```lisp
(defmacro lazy-cons (a b)
  `(lazy (cons ,a ,b)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))
```

これらの使用例を次に示す。
実行結果から分かるように、`lazy-cons`と`lazy-car`と`lazy-cdr`は、それぞれ、`cons`と`car`と`cdr`と同じように使用できる。

```lisp
> (defparameter *foo* (lazy-cons 4 7))
*FOO*
> (lazy-car *foo*)
4
> (lazy-cdr *foo*)
7
```

これらの単純な関数で、次のような有用な定義を実現できる。
すなわち、全ての正整数のリスト`*integers*`を定義しているのである。
無限長のリストを定義しているにも関わらず、遅延評価を導入したことで全ての評価をしてシステムダウンするような自体を回避できている。

```lisp
(defparameter *integers*
  (labels ((f (n)
             (lazy-cons n (f (1+ n)))))
    (f 1)))
```

実際にこれを評価すると次のとおりになる。

```lisp
> (lazy-car *integers*)
1
> (lazy-car (lazy-cdr *integers*))
2
> (lazy-car (lazy-cdr (lazy-cdr *integers*)))
3
```

マクロを展開すると、次のとおりになる。

```lisp
(lazy-car (lazy-cdr (lazy-cdr *integers*)))

; =>

(lazy-car
  (lazy-cdr
    (lazy-cdr
      (labels ((f (n)
                 (lazy-cons n (f (1+ n)))))
        (f 1)))))

; =>

(car
  (force
    (cdr
      (force
        (cdr
          (force
            (labels ((f (n)
                       (lazy
                         (cons n (f (1+ n))))))
              (f 1))))))))
```

`lazy-`コマンドを使っている限り、この正整数のリスト`*integers*`から、欲しいだけ正整数を取り出すことができる。
取り出したいところまでの整数が、必要に応じて計算されるのである。

このような無限長のリストばかりが遅延リストではない。
すなわち、終端を持つ遅延リストも存在する。

終端を持つ遅延リストを実現するためには、`lazy-nil`コマンドも必要となる。
そして、通常のリストに対して、終端に達したかどうかを調べる`null`関数に対応する、遅延リストの終端を調べる`lazy-null`関数も必要となる。

```lisp
(defun lazy-nil ()
  "forceされるとnilを返す"
  (lazy nil))
```

```lisp
(defun lazy-null (x)
  "遅延リストがnilならtを返す"
  (not (force x)))
```

## 通常のリストと遅延リストとの変換

ここからは、遅延リストの操作に便利な関数を作っていく。

まず必要となるのは、通常のリストを遅延リストに変換する関数である。
これを実現する`make-lazy`関数を実装する。

```lisp
(defun make-lazy (lst)
  (lazy (when lst
          (cons (car lst) (make-lazy (cdr lst))))))
```

この`make-lazy`関数は、大雑把に言えば、再帰で与えられたリストを順に見ていき、それぞれのコンスを`lazy`なマクロで包んでいるということになる。
しかしながら、この関数の実際の意味を正しく理解するには、`lazy`と`force`の意味を考える必要がある。
幸いなことに、遅延リストライブラリを完成させてしまえば、これらの遅延評価にまつわる奇妙さはライブラリの中に隠されることとなる。

`make-lazy`関数は普通のリストを遅延リストに変換した。
では反対に、遅延リストを普通のリストに変換するための`take`および`take-all`関数を実装する。

```lisp
(defun take (n lst)
  "遅延リストから指定した数の要素だけ取り出す"
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst)
          (take (1- n) (lazy-cdr lst)))))
```

```lisp
(defun take-all (lst)
  "遅延リストから全ての要素を取り出す
   無限長の遅延リストには使用禁止"
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))
```

これらを使用すると、次のようになる。

```lisp
> (take 10 *integers*)
(1 2 3 4 5 6 7 8 9 10)
> (take 10 (make-lazy '(q w e r t y u i o p a s d f)))
(Q W E R T Y U I O P)
> (take-all (make-lazy '(q w e r t y u i o p a s d f)))
(Q W E R T Y U I O P A S D F)
```

## 遅延リストに対するマッピングと検索

遅延リストに対して、マップや検索を実現する関数を次に示す。
`mapcar`、`mapcan`、`find-if`、`nth`に対する遅延リスト版の関数を実装する。
これらの関数は、引数に遅延リストを取り、戻り値もリストを返す場合は遅延リストを返す。
これらの関数の実装には、`lazy-null`、`lazy-car`、`lazy-cdr`を使う必要がある。

```lisp
(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fun (lazy-car lst))
                (lazy-mapcar fun (lazy-cdr lst))))))
```

```lisp
(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur)
             (if (lazy-null lst-cur)
                 (force (lazy-mapcan fun (lazy-cdr lst)))
                 (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null lst)
            (f (funcall fun (lazy-car lst)))))))
```

```lisp
(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-null lst)))
      (if (funcall fun x)
        x
        (lazy-find-if fun (lazy-cdr lst))))))
```

```lisp
(defun lazy-nth (n lst)
  (if (zerop n)
      (lazy-car lst)
      (lazy-nth (1- n) (lazy-cdr lst))))
```

上の関数の使い方は、次のとおりである。

```lisp
> (take 10 (lazy-mapcar #'sqrt *integers*))
(1 1.4143135 1.7320508 2 2.236068 2.4494898 2.6457512 2.828427 3 3.1622777)
```

`lazy-mapcar`を使って無限長の正整数リストに`sqrt`をマップすると、全ての正整数の平方根の遅延リストが得られる。

```lisp
> (take 10 (lazy-mapcan (lambda (x)
                          (if (evenp x)
                              (make-lazy (list x))
                              (lazy-nil)))
                        *integers*))
(2 4 6 8 10 12 14 16 18 20)
```

`lazy-mapcan`を使って、各正整数について、それが偶数ならその数だけからなる遅延リストを、それが奇数なら遅延空リストを返す関数を適用している。
ここでは、結果として、無限正整数リストから偶数だけを取り出したリストを、要素10個分だけ返している。

```lisp
> (lazy-find-if #'oddp (make-lazy '(2 4 6 7 8 10)))
7
```

`find-if`を使って、遅延リストから最初の奇数を探している。
この例では、結果として`7`を返している。

```lisp
> (lazy-nth 4 (make-lazy '(a b c d e f g)))
E
```

`lazy-nth`を使って、遅延リストの指定箇所の要素を取り出している。

これら、遅延リスト版の関数を、例えば`lazy.lisp`ファイルに記載しておき、このファイルをロードしていつでも使えるようにしておくと良い。


# 18.2 ダイスオブドゥームVer2

15章で作成したダイスオブドゥームVer1に、遅延リストライブラリを適用する。
まず、ダイスオブドゥームのコードと、遅延リストライブラリをロードする。

```lisp
> (load "dice_of_doom_v1.lisp")
> (load "lazy-lisp")
```

ロードしたコードに変更を加えることで、ゲームを遅延リスト版に変更する。

次に、ゲーム盤の大きさを4x4に拡大する。

```lisp
> (defparameter *board-size* 4)
> (defparameter *board-hexnum* (* *board-size* *board-size*))
```

この大きさのゲームを実用的な速度で実行するには、ゲーム木のそれぞれの枝を遅延リストとして表現する必要がある。
そのためには、バージョン1のゲームのいくつかの関数を遅延リスト関数を使ったものに差し替える必要がある。

まず、与えられたゲーム盤の状態に対して、攻撃と手番終了の手を計算する関数を変更する。

`add-passing-move`関数では、1箇所だけ変更する。
手のリストを遅延リストにするため、可能な手のリストに手番を終える手を加えるのに`lazy-cons`を使う。

```lisp
(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
    moves
    (lazy-cons (list nil
                     (game-tree (add-new-dice board player
                                              (1- spare-dice))
                                (mod (1+ player) *num-players*)
                                0
                                t))
               moves)))
```

`attacking-moves`関数では、多めの変更が必要である。  
まず、遅延リストを返すために、手のリストを組み立てる2箇所の`mapcan`を`lazy-mapcan`に置き換える。  
`lazy-mapcan`関数はその中で作るリストも遅延リストでなければならないので、`make-lazy`関数を使うようにする。  
また、`nil`を返していたところは`lazy-nil`を返すようにする。  
最後に、計算されたゲーム盤のリストも遅延リストにする。
このリストは外側の`lazy-mapcan`に使われる。

```lisp
(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (lazy-mapcan
      (lambda (src)
        (if (eq (player src) cur-player)
          (lazy-mapcan
            (lambda (dst)
              (if (and (not (eq (player dst)
                                cur-player))
                       (> (dice src) (dice dst)))
                (make-lazy
                  (list (list (list src dst)
                              (game-tree (board-attack board
                                                       cur-player
                                                       src
                                                       dst
                                                       (dice src))
                                         cur-player
                                         (+ spare-dice (dice dst))
                                         nil))))
                (lazy-nil)))
          (make-lazy (neighbors src)))
        (lazy-nil)))
      (make-lazy (loop for n below *board-hexnum*
                       collect n)))))
```

次に、人間のプレイヤーに対応する2つの関数に変更を加える。

`handle-human`関数では、ローカル関数`print-moves`を定義している。
これは可能な手のリストを舐めていく関数である。

- リスト終端のチェック
- リストの先頭からの手を取り出す
- リストの残りの部分で再帰する

上の3箇所について、遅延版のコマンドを使うように変更する。
さらに、プレイヤーが選んだ手を可能な手のリストから取り出すところに`lazy-nth`を使うようにする。

```lisp
(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
               (unless (lazy-null moves)
                 (let* ((move (lazy-car moves))
                        (action (car move)))
                   (fresh-line)
                   (format t "~a. " n)
                   (if action
                     (format t "~a -> ~a" (car action) (cadr action))
                     (princ "end turn")))
                 (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))
```

`paly-vs-human`関数では、変更は1箇所だけである。
ゲームの終了状態に達したかどうかを判断するのに、可能な手のリストが空かどうかを調べ、もし空なら勝者を計算する。
この、リストが空かどうかを調べる箇所を、`lazy-null`に置き換える。

```lisp
(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))
```

ここまでの変更で、より大きな盤面を使ったダイスオブドゥームを人間対人間で遊べるようになった。
すなわち、ゲーム技はプレイヤーがその状態を選んだ場合にしか計算されない。
4x4のゲーム盤でゲームを開始するには、バージョン1と同様に、次のコマンドを入力すれば良い。

```lisp
> (play-vs-human (game-tree (gen-board) 0 0 t))
current player = a
        a-1 a-3 a-1 b-2
      b-3 a-3 a-3 a-1
    a-3 a-3 b-1 a-2
  b-3 a-3 a-1 a-3
choose your move:
1. 5 -> 10
2. 6 -> 10
3. 9 -> 10
4. 11 -> 10
5. 15 -> 10
```

# 18.3 大きなゲーム盤でAIを動かす

ここでは、ゲームAIの関数を遅延リストライブラリに対応させる。
また、ついでにAIコードにいくつかの改善をする。

## ゲーム木の刈り込み

ダイスオブドゥームver1のAIコードは、ある意味では最強だった。
というのも、手を決める全ての機会に、AIは **将来起こりうる全ての状態** を調べて、その中で最良手を指していたからである。

しかし、この方法は、ゲーム盤の規模が少し大きくなるだけで計算量が爆発して破綻する。
そもそも、ゲーム木に遅延評価を入れた目的は、全ての枝を計算対象にしたくないからであった。

したがって、このバージョンにおいては、ゲームAIには「最大でも何手までしか計算しなくて良い」と指示できる仕組みが必要となる。

関数型プログラミングスタイルを使ったダイスオブドゥームでは、この変更は非常に簡潔に記述できるが、すぐには思いつかないような方法である。
そこで、ステップバイステップで考えることとする。

すぐに思いつく方法としては、バージョン1の`get-rating`と`rate-position`を変更して、`search-depth`という新しいアキュムレータを引数に足すことである。
そして、これらの関数を呼び出す度に、先読みの最大値に達したかどうかを調べる。

でもこの方法には問題がある。
それぞれの関数が余分な引数を背負わされて、本来の関数の処理が分かりにくくなってしまっている。
本来、盤面の状態を評価することと、なんて先まで読むかを判断することは、別々の関心事のはずである。
つまり、これらの関心事は直行している、といえる。
したがって、各々の処理は別々の関数で扱われるべきである。

ここで、先程の遅延ゲーム木を使うと、探索木を「刈り込む」という仕事だけをする関数を、可能な手を評価して次の手を考えるAIコードとは完全に独立して記述できる。

ゲーム木を刈り込む関数を次に示す。
この関数は、引数を2つだけ取る、かなり簡単な関数である。
返り値は、新しく作られるゲーム木のコピーである。
コピーの枝は、この関数を再帰的に呼んで作成されるが、再帰する度に`depth`をデクリメントする。
`depth`が`0`になったら、そこか刈り込む深さであるから、可能な指し手に対応する遅延ゲーム木を空にする。

```lisp
(defun limit-tree-depth (tree depth)
  "ゲーム木を指定の深さで刈り込む
   tree: 遅延ゲーム木
   depth: 何手先まで読むか(何手先で枝を刈るか)
   ret: 新しく作られる遅延ゲーム木のコピー"
  (list (car tree)  ; プレイヤーID
        (cadr tree) ; ゲーム盤情報
        ;; 刈り込む深さになったら、指し手の遅延リスト部分を空にする
        ;; 刈り込む深さでなかったら、可能な指し手に対応する遅延ゲーム木を取得する
        (if (zerop depth)
            (lazy-nil)  ; 空のゲーム木
            (lazy-mapcar (lambda (move)
                           ;; 指定された指し手に対応する遅延ゲーム木を取得する
                           ;; move: 指し手をキーに持つ遅延ゲーム木のalist
                           ;; ret: 指し手に対応する遅延ゲーム木
                           (list (car move)  ; 指し手
                                 (limit-tree-depth (cadr move) (1- depth)))) ; 指し手に対応するゲーム盤情報
                         ;; 指し手をキーに持つ遅延ゲーム木のalistのリスト
                         (caddr tree)))))
```

他に必要となるのは、ゲームAIが手を評価する直前にこの`limit-tree-depth`を呼んでやることだけである。
`hadle-computer`関数を少し変更すれば実現できる。
すなわち、`get-ratings`を呼んで現在の木から先の手を評価する前に、現在の木を刈り込む。
すると、元のゲーム木の全容をゲームAIは意識しない。
さらに細かな変更として、評価後の手を遅延リストから選び出すために`lazy-nth`を使用するようにした。

```lisp
;;; ゲームAIが先読みする遅延ゲーム木の深さ
(defparameter *ai-level* 4)

(defun handle-computer (tree)
  "ゲームAIを操作する
   tree: 現在の遅延ゲーム木
   ret: ゲームAIの指し手に対応する遅延ゲーム木"
  ;; ratings: 現在のゲーム盤情報における、各指し手に対する点数のリスト
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*)
                              (car tree))))
    ;; 最高得点を得られる指し手を計算し、それに対応する遅延ゲーム木を返す
    (cadr (lazy-nth (position (apply #'max ratings) ratings)
                    (caddr tree)))))
```

さらに、`play-vs-computer`にも1箇所変更がある。
可能な指し手の遅延リストが空であるか確かめるために`lazy-null`を使うように変更する。

```lisp
(defun play-vs-computer (tree)
  "対コンピュータ戦を開始する
   tree: 遅延ゲーム木
   ret: "
  ;; ゲーム情報を表示する
  (print-info tree)
  ;; 指し手をキーとする遅延ゲーム木のalistが空なら、現在のゲーム盤情報から勝者を表示してゲーム終了
  ;; プレイヤーIDが0(人間の手番)なら、人間から指し手を要求してゲーム続行する
  // プレイヤーIDがゲームAIの手番なら、ゲームAIに指し手を計算させてゲーム続行する
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))
```

## ヒューリスティクスを適用する

ここでは、AIを強化する方法について考える。

ゲーム木を刈り込む事により、ゲームAIについて本質的に変化が生じた。
すなわち、刈り込みがなければ完璧なプレイを見せたゲームAIは、いまや、勝てる指し手を「見逃す」可能性を生じるようになった。
これは、性能と引き換えに、完璧な手を指すことを捨てたといえる。

このような状況は、ヒューリスティクスな状況であるといえる。
コンピュータサイエンスにおけるヒューリスティクスは、完全ではないが及第点以上の良い結果を素早く得られるようなプログラミングテクニックを意味する。
ダイスオブドゥームにおいても、簡単なチューニングを実施することで、ゲームAIの性能を大幅に引き上げることができる。

## 大きく勝つか小さく勝つか

ゲーム木の全ての枝について勝敗を評価する場合、ゲームAIはどのくらい差をつけて勝つかを気にする必要はなかった。
つまり、ゲームの終了時点で、相手より1つでも多くのマスを確保していれば勝ちであった。

しかし、今のゲームAIはヒューリスティックなアルゴリズムとなった。
すなわち、ゲームの任意の時点において、どの程度相手をリードしているのかはとても重要な勝因となる。

ここで有効な経験則としては、「今、相手を十分に引き離していれば、たとえ数手先しか読まなくとも相手に追いつかれる確率は低い」というものがある。

このゲームのゲームAIに実装したミニマックスアルゴリズムでは、ゲーム木の「葉」にそれぞれスコアをつけていた。
まず、バージョン1(全ての枝を確認するVer.)では、このスコアは0(AIの負け)、1(AIの勝ち)、1/2(引き分け)という単純なものであった。

しかし、バージョン2においては、評価関数が見ることのできる範囲でのゲーム木の「葉」は、本当のゲームの勝敗を決するものではなく、その先に刈られた枝が続いている。
この場合、スコアの範囲を拡大して、どの手を指すと「より大きく」勝ち、どの手を指すと「より小さく」勝つのかということを判断できるようにしたい。

もっと複雑なヒューリスティクスを使用して、葉の部分のゲーム盤の状態を評価する`score-board`を記述してみる。
`score-board`関数は、ゲーム盤の全てのマスをループして、`loop`マクロの`sum`を使って各マスのスコアを合計する。
プレイヤーが現在のマスを占領していれば正のスコアを加算する。
下記のルールで各マスのスコアを算出する。

- プレイヤーが所有するマスで、より強い敵のマスが隣にない:2
- プレイヤーが所有するマスで、より強い敵のマスが隣にある:1
- 敵が所有するマス:-1

**NOTE** `score-board`はヒューリスティックな関数であって、スコアの付け方に絶対的な正解は無い。

```lisp
(defun score-board (board player)
  "指定のプレイヤーにとっての現在のゲーム盤情報のスコアを算出する
   board: ゲーム盤情報
   player: プレイヤーID
   ret: ゲーム盤情報のスコア"
  ;; ゲーム盤を走査しながら、各マスのスコアを合計する
  (loop for hex across board
        for pos from 0
        ;; 下記のルールで各マスのスコアを算出する
        ;; - プレイヤーが所有するマスで、より強い敵のマスが隣にない:2
        ;; - プレイヤーが所有するマスで、より強い敵のマスが隣にある:1
        ;; - 敵が所有するマス:-1
        sum (if (eq (car hex) player)
                (if (threatened pos board)
                    1
                    2)
                -1)))
```

上の`score-board`関数で使われている`threatened`関数を次に示す。
この関数では、引数で指定したマスの隣を走査して、敵が所有している、かつ、サイコロが引数のマスよりも多いマスが無いかを調べる。

```lisp
(defun threatened (pos board)
  "隣のマスにより強い敵のマスがあるか判定する
   pos: ゲーム盤の位置
   board: ゲーム盤情報
   ret: t:隣により強い敵のマスがある nil:ない"
  (let* ((hex (aref board pos))  ; 引数posで指定したマス情報
         (player (car hex))      ; マスを所有するプレイヤーのID
         (dice (cadr hex)))      ; マスに置かれたサイコロの数
    (loop for n in (neighbors pos)
          do (let* ((nhex (aref board n)) ; posの隣のマス情報
                    (nplayer (car nhex))  ; posの隣のマスを所有するプレイヤーのID
                    (ndice (cadr nhex)))  ; posの隣のマスに置かれたサイコロの数
               ;; posの隣のマスが、異なる所有者でより多くのサイコロを持っていたら、
               ;; 隣のマスにより強い敵のマスがあると評価する
               (when (and (not (eq player nplayer)) (> ndice dice))
                 (return n))))))
```

次に、上の`score-board`と`threatened`を使って、`get-ratings`と`rate-position`を改良してみる。
大きな改良点としては、これ以上続く指し手のないゲーム木に対して、得点をつけていることである。

```lisp
(defun get-ratings (tree player)
  "現在の遅延ゲーム木における指定したプレイヤーが取りうる得点を全パターン返す
   tree: 遅延ゲーム木
   player: 得点を算出したいプレイヤーのID
   ret: 得点のリスト"
  (take-all (lazy-mapcar (lambda (move)
                           ;; 指し手に対応するそのマスの得点を計算する
                           (rate-position (cadr move) player))
                         ;; 可能な全ての指し手
                         (caddr tree))))
```

```lisp
(defun rate-position (tree player)
  "現在のゲーム木から指定プレイヤーの得点を算出する
   tree: 遅延ゲーム木
   player: プレイヤーID
   ret: 得点"
  (let ((moves (caddr tree)))  ; 可能な指し手
    ;; 現在のゲーム木に可能な指し手があれば、次に取りうる全てのゲーム木を見ていき、
    ;; ミニマックスアルゴリズムを適用したときの得点を返す
    ;; 現在のゲーム木に可能な指し手がなければ、現在のゲーム盤の得点を返す
    (if (not (lazy-null moves))
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
          (get-ratings tree player))
        (score-board (cadr tree) player))))
```

これで、ヒューリスティクスを用いたゲームAIが大きな盤面で動かせる。
以前の例と同様に、プレイヤーBの指してはAIアルゴリズムで自動的に計算されたものとなる。

```lisp
> (play-vs-computer (game-tree (gen-board) 0 0 t))
...
...
...
```

## アルファ・ベータ法

**アルファ・ベータ法** はミニマックスアルゴリズムにおける、よく知られた最適化手法である。
最終的なミニマックス評価の結果に影響を及ぼさないと判断した枝を飛ばしてしまう(枝刈りする)ことで処理速度を上げるのである。

ゲーム木のとある枝が最終的な評価に影響を及ぼさないというのはどういう場合か。  
アルファ・ベータ法を理解するため、2x2のゲーム盤でのゲーム木を示した図を見てみる。

![alpha-beta-pruning](alpha-beta-pruning.svg)

**図の意味**

- ゲームは図の一番上からスタートする
- 矢印が可能な手を表す
- 各四角には、どちらが手番化を示してある
- 各ゲーム盤の右下の数字が、(`score-board`関数を使った)最新の`get-ratings`による評価値
  - 葉ノードでは、評価値は`score-board`により直接計算される
  - 葉ノード以外では、数値はミニマックスアルゴリズムにより選ばれる
- ゲーム木の各状態の中で、指し手を選ぶ余地のあるノード(分岐のあるノード)はMAXノード、または、MINと示してある
  - プレイヤーAが選べる分岐はMAXノード
  - プレイヤーBが選べる分岐はMINノード

ミニマックスアルゴリズムは、**深さ優先探索**である。
つまり、ゲーム木を左から右に、深さ優先で、全ての葉を調べていく。
(ここでは、`*ai-level*`が高く設定されていて、木が一切刈り込まれていないとしよう。)
全ての葉を見た後、分岐があるノードについて、最小または最大のスコアを採用する。

ここで、MINの分岐に注目する。
ミニマックスアルゴリズムを適用すると、MINノードの最初(左側)のぶん機のスコアは`8`であることが分かる。
AIエンジンが右側の枝を見に行く際には、スコアが8以下になることだけが重要である。
8とそれより大きい数から最小を取れば常に8であるから、**8より大きな数**は結果に影響しない。

したがって、AIが左の分岐でスコア8を見つけたら、その時点でもうそれ以上右側の枝を調べる必要がない事がわかる。
つまり、ミニマックスアルゴリズムにおいては、図中の点線で示されている部分の枝を調べる必要はないということである。

この例においては、枝刈りできた部分はごく一部分のみであったが、ゲーム木の規模が大きくなれば、大抵は大部分の枝を刈り取ることができる。


### アルファ・ベータ法のライブラリを作る

巷でよく見られるアルファ・ベータ法においては、`alpha`と`beta`という変数を利用する。
つまり、MINノードかMAXノードかによって、`alpha`と`beta`の役割(上限か下限か)を適宜入れ替えて使うことで、同じコードを両方の種類のノードに使えるようにする。  
しかし、ここで作成するコードでは、わかりやすさを優先して、`upper-limit`と`lower-limit`という変数を受け渡していくことにする。
これらは、それぞれ、ゲーム木を調べている最中に気にすべき上限値と下限値を表す。
`alpha`と`beta`を使わないことで、MINとMAXそれぞれの場合分けのコードに重複が生じるが、上限値と下限値を明確にしておくことで、アルファ・ベータ法のコードをより平易にする意味がある。

もう一つの注意として、ここでは、ミニマックスアルゴリズムのコード部分と、アルファ・ベータ法のコード部分を分離しない。
先程のダイスオブドゥームにおける「先読み制限」のコードでは、先読みを制限する`limit-tree-depth`関数をAIコードの残りの部分と独立して実践した。
アルファ・ベータ法も同様に、ゲーム木を変換する独立した関数として実装できなくはない。
しかし、アルファ・ベータ法のコードはミニマックスの計算の中間結果を参照しなければならないので、少しややこしくなる。
もっと進んだAIエンジンなら、それでも分離しておくのが良い設計となるが、この規模のゲームであれば、アルファ・ベータ法のチェックもミニマックスアルゴリズムのコード中に入れてしまっても良いであろう。

ここからは、実装に入る。
まず、`get-ratings`関数を、`ab-get-ratings-max`関数と`ab-get-ratings-min`関数で置き換える。
`get-ratings`関数は、与えられたゲーム盤の状態から、可能な指し手のうち最良のものを計算する関数であった。
そして、これから実装したいのは、「評価関数が「これ以上の指しては存在しない」と判断したら直ちに評価を打ち切る処理」である。
打ち切りの決定ロジックは、今見ているノードがMAX(自分のプレイヤーの手番)か、MIN(相手プレイヤーの手番)かによって異なる。

まず、MAXノードについて計算する関数`ab-get-ratings-max`を実装する。
この関数は引数として`get-ratings`関数が受け取っていた引数に加え、`upper-limit`と`lower-limit`を受け取る。
この関数自身は最大値だけに関心があるため、`lower-limit`は参照しない。
ただし、子ノードにMINノードがあれば、再帰呼出しの先では最小値を求める必要がある。
したがって、再帰呼び出し先のために下限を引数に持つ。

```lisp
(defun ab-get-ratings-max (tree player upper-limit lower-limit)
  "MAXノードにおいて、現在のゲーム盤で取りうるスコアの最大値を計算する
   tree: 現在の遅延ゲーム木
   player: プレイヤーID
   upper-limit: スコアの上限
   lower-limit: スコアの下限
   ret: スコアの最大値"
  (labels ((f (moves lower-limit)
             ;; 可能な指し手の中からスコアの最大値を求める
             ;; moves: 可能な指し手
             ;; lower-limit: 探索すべきスコアの下限
             ;; ret: スコアの最大値
             ;; 可能な指し手があれば、それらに対してスコアの最大値を計算する
             (unless (lazy-null moves)
               ;; x: 未探索の指し手のうち一番左側の指し手のスコアを計算する
               (let ((x (ab-rate-position (cadr (lazy-car moves))
                                          player
                                          upper-limit
                                          lower-limit)))
                 ;; - xが上限以上なら、それ以上探索する必要はないので評価を打ち切る
                 ;; - xがそれ以外なら、残りの枝をさらに探索する必要がある
                 ;;   - xがそれまでのlower-limitより大きければxを新たなlower-limitとして採用する
                 (if (>= x upper-limit)
                     (list x)
                     (cons x (f (lazy-cdr moves) (max x lower-limit))))))))

    ;; 可能な指し手と下限を指定して、スコアの最大値を計算する
    (f (caddr tree) lower-limit)))
```

次に、MINノードについて計算する関数`ab-get-ratings-min`を実装する。
この関数は引数として`get-ratings`関数が受け取っていた引数に加え、`upper-limit`と`lower-limit`を受け取る。
この関数自身は最小値だけに関心があるため、`upper-limit`は参照しない。
ただし、子ノードにMAXノードがあれば、再帰呼出しの先では最大値を求める必要がある。
したがって、再帰呼び出し先のために上限を引数に持つ。

```lisp
(defun ab-get-ratings-min (tree player upper-limit lower-limit)
  "MINノードにおいて、現在のゲーム盤で取りうるスコアの最小値を計算する
   tree: 現在の遅延ゲーム木
   player: プレイヤーID
   upper-limit: スコアの上限
   lower-limit: スコアの下限
   ret: スコアの最大値"
  (labels ((f (moves upper-limit)
             ;; 可能な指し手の中からスコアの最大値を求める
             ;; moves: 可能な指し手
             ;; upper-limit: 探索すべきスコアの上限
             ;; ret: スコアの最大値
             ;; 可能な指し手があれば、それらに対してスコアの最大値を計算する
             (unless (lazy-null moves)
               ;; x: 未探索の指し手のうち一番左側の指し手のスコアを計算する
               (let ((x (ab-rate-position (cadr (lazy-car moves))
                                          player
                                          upper-limit
                                          lower-limit)))
                 ;; - xが下限以下なら、それ以上探索する必要はないので評価を打ち切る
                 ;; - xがそれ以外なら、残りの枝をさらに探索する必要がある
                 ;;   - xがそれまでのupper-limitより大きければxを新たなupper-limitとして採用する
                 (if (<= x lower-limit)
                     (list x)
                     (cons x (f (lazy-cdr moves) (min x upper-limit))))))))

    ;; 可能な指し手と上限を指定して、スコアの最小値を計算する
    (f (caddr tree) upper-limit)))
```

新たな関数`ab-rate-position`では、まず現在のノードが自分の手番化相手の手番化を確認する。
自分の手番であればMAXノードということであるから、処理を`ab-get-ratings-max`に任せる。
相手の手番であればMINノードということであるから、処理を`ab-get-ratings-min`に任せる。
その他の部分は以前の`rate-position`と同じである。


```lisp
(defun ab-rate-position (tree player upper-limit lower-limit)
  ""
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
        (if (eq (car tree) player)
            (apply #'max (ab-get-ratings-max tree
                                             player
                                             upper-limit
                                             lower-limit))
            (apply #'min (ab-get-ratings-min tree
                                             player
                                             upper-limit
                                             lower-limit)))
        (score-board (cadr tree) player))))
```

最後に、ミニマックスアルゴリズムを起動する`handle-computer`関数を、新しい関数を呼ぶように変更する。
この関数は、`ab-get-ratings-max`を呼び出すことでミニマックスアルゴリズムを起動する。
この関数が呼ばれるのは、自分の手番なわけだから、最初に評価されるノードはMAXノードである。

```lisp
(defun handle-computer (tree)
  (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
                                     (car tree)
                                     most-positive-fixnum
                                     most-negative-fixnum)))
    (cadr (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)))))
```

この関数を呼び出すにあたって、`upper-limit`と`lower-limit`の初期値を決めてやらないとならない。
ミニマックスアルゴリズムをこれから開始するわけであるから、上限および上限はできる限り無限に近づけておきたい。
多くのLisp環境では無限大が定義されているが、ANSI Common Lispには無限大が含まれていない。
ただし、規格としては、`most-positive-fixnum`と`most-negative-fixnum`を定めていて、これらはとても大きな絶対値を持つ正負の数である。
今回の目的としてはこれで十分であるため、これらの値を`ab-get-ratings-max`に渡している。

AIエンジンの効率をもう少し上げたいなら、`upper-limit`と`lower-limit`を`score-board`が返しうる最大値と最小値にしておくことも考えられる。
そうすれば、多少は枝刈りできる機会が増えるであろう。
しかし、`score-board`が返す値の範囲はゲーム盤の大きさに依存しており、将来、点数計算を更に最適化したら変化するリスクを持つ。
したがって、今のところは初期値には安全なものを採用することとする。

ここまでの最適化を完了させたところで、ゲーム盤の大きさを5x5に拡張してみる。
ここまでで、下の最適化を実装したAIアルゴリズムであれば、この大きさのゲーム盤でも難なく処理できるであろう。

- 遅延評価
- 先読み制限
- 枝刈り

**5x5ゲーム盤でゲーム開始**

```lisp
(defparameter *board-size* 5)
(defparameter *board-hexnum* (* *board-size* *board-size*))
```



