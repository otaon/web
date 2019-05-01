---
title: "書籍 Land of Lisp 20章 ダイスオブドゥームをさらに面白く"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
toc: true
tags: ["lisp"]
categories: ["Notes"]
authors:
- otaon
---

ダイスオブドゥームのバージョン4を作る。  
今までのバージョンでは、プログラムを簡単にするため、重要なルールを省略していた。  
本章では、ゲームのプレイヤーを増やし、サイコロを振るようにし、さらにいくつかの改良をダイスオブドゥームに施す。

まず、前章で作ったコードをファイルに保存し、呼び出すだけで使用できるようにしておく。

```lisp
> (load "dice_of_doom_v3.lisp")
```

## 20.1 プレイヤーの数を増やす
最初の変更では、プレイヤーを2人から4人に増やす。  
うち3人は、ゲームAIプレイヤーである。

まず、変数`*num-players*`の値を4にし、新たなプレイヤーのためのサイコロの色を追加する。  

```lisp
(defparameter *num-players* 4)
(defparameter *die-colors* '((255 63 63)     ; 赤
                             (63 63 255)     ; 青
                             (63 255 63)     ; 緑
                             (255 63 255)))  ; 紫
```

プレイヤーの数の定数を変更したため、他の定数も変えておく。  
サイコロの最大数を5個に増やし、そしてAIのレベルを4から2に減らした。  
ゲームAIが3人もいるため、対人としては賢さがそれほど必要ではなくなったわけである。

```lisp
(defparameter *max-dice* 5)  ; サイコロの最大数
(defparameter *ai-level* 2)  ; AIが思考するゲーム木の深さ
```

### パラノイド戦略
これまで作ってきたゲームAIプレイヤーは、いわゆる「パラノイド戦略」をとっている。
すなわち、それぞれのAIプレイヤーは「他のプレイヤーはすべて敵で、他人を攻撃することしか眼中にない」と考えている。  
これは必ずしも悪い戦略ではないが、プレイヤーが3人以上になると、他の有効な戦略も存在することは覚えておきたい。
例えば、負けているプレイヤー同士が結託して、トップのプレイヤーを攻撃する、などである。

しかしながら、本書のAIエンジンは、そういった協力プレイは一切計算できない。

## 20.2 サイコロを降る
これまでのゲームにおける重大な欠陥の1つは、サイコロを一切振っていない点である。
これはつまりサイコロのランダム性を全く使っていないということである。

このバージョンにおいては、攻撃にあたって、攻撃元のマスのサイコロ、攻撃先のサイコロ、それぞれをまとめて振り、目の合計の多いほうが勝つ。
目が同じだった場合は、防御側の勝ちとする。
攻撃側が失敗した場合は、攻撃側のマスはサイコロを1つだけ残して、残りを防御側のプレイヤーに渡すルールとする。

上記のルールを実現するためには、AIプログラミング用語でいう**確率ノード**(chance node)をゲーム木に足す必要がある。  
次に、実装を示す。

### 確率ノードを作る
今まで、ゲーム木の次の手を表す遅延リストの要素は、下記の2つの項目を持つリストであった。

- car: 手の記述(攻撃の場合は、攻撃元と攻撃先のマス。手番終了)
- cadr: 手が選ばれた場合の、次のゲーム木のノード

ここに、3つ目の項目として、攻撃が失敗した場合のゲーム木のノードを追加する。  
すなわち、ゲーム木のそれぞれの手から伸びる枝が、攻撃の成否によってさらに2つに分岐することになる。  

では、`attacking-moves`関数を拡張し、それぞれの手が確率ノードとして動作するように要素を付け足していく。  
ここでの変更における新しい変更は、ゲーム木に新たな手を付け加える時にもう一つの枝を足してやることである。

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
              (if (and (not (eq (player dst) cur-player))
                       (> (dice src) 1))
                  (make-lazy (list
                               (list
                                 (list src dst)
                                 (game-tree (board-attack board cur-player
                                                          src dst (dice src))
                                             cur-player
                                             (+ spare-dice (dice dst))
                                             nil)
                                 (game-tree (board-attack-fail board cur-player
                                                               src dst (dice src))
                                            cur-player
                                            (+ spare-dice (dice dst))
                                            nil))))
                  (lazy-nil)))
            (make-lazy (neighbors src)))
          (lazy-nil)))
      (make-lazy (loop for n below *board-hexnum*
                   collect n)))))
```

この確率ノードから伸びる追加の枝のゲーム盤を作るには、次に示す`board-attack-fail`を呼び出してやる。  
`board-attack-fail`は、ゲーム盤を受け取り、そして失敗した攻撃の攻撃元となったマスから、サイコロを1つだけ残して残りを取り上げた状態のゲーム盤を返す。  
この関数は、ゲーム盤をループして、各マスを単純にコピーしている。  
ただし、マスの番号が攻撃元と一致した場合に限り、そこに1個だけサイコロを残すようにする。

```lisp
(defun board-attack-fail (board player src dst dice)
  (board-array (loop for pos from 0
                     for hex across board
                     collect (if (eq pos src)
                                 (list player 1)
                                 hex))))
```

### サイコロを実際に振る
サイコロを振るロジックを実装する。  
次の関数では、引数で与えられた数のサイコロをまとめて振る。
そして、サイコロを振った結果をメッセージに表示し、合計を返す。

```lisp
(defun roll-dice (dice-num)
  (let ((total (loop repeat dice-num
                     sum (1+ (random 6)))))
    (fresh-line)
    (format t "On ~a dice rolled ~a. " dice-num total)
    total))
```

サイコロは常に攻撃側と守備側それぞれで振ることになるため、それらをまとめて行う関数も定義する。  
この関数は単に`roll-dice`を2回呼び、結果を比べるのみである。  
ゲーム木をたどる過程でプレイヤーがサイコロを降る手を選択したらこの関数を呼び出し、結果に応じて勝った場合の枝か、負けた場合の枝のどちらかを次のゲーム木にする。

```lisp
(defun roll-against (src-dice dst-dice)
  (> (roll-dice src-dice) (rill-dice dst-dice)))
```

### ゲームエンジンからサイコロを振るコードを呼び出す
ゲームエンジンにとっては、サイコロを振るのは人間かコンピュータのプレイヤーが手を選んだ時に確率ノードの枝のどちらかを選ぶときだけである。  
この動作は、`pick-chance-branch`関数で実現される。

```lisp
(defun pick-chance-branch (branch move)
  (labels ((dice (pos)
             (cadr (aref board pos))))
    (let ((path (car move)))
      (if (or (null path)
              (roll-against (dice (car path))
                            (dice (cadr path))))
          (cadr move)
          (caddr move)))))
```

この関数は現在のゲーム盤と指し手のエントリを受け取り、指し手が確率ノードを持っていたら、そのどちらの枝を選ぶかを決定する。  
まず、指し手の`car`を、すなわち`path`を見て、これが`nil`でなければこの指し手は攻撃なので、そこから攻撃元`(car path)`と攻撃先`(cadr path)`のマスを取り出し、それぞれのサイコロの個数を求めて`roll-against`を呼び出す。  
`path`が`nil`ならこの手は「手番を終える」手であるため、サイコロを振る必要はない。

サイコロを振って攻撃が成功と出れば、確率ノードの最初のゲーム木を返す。
攻撃が失敗に終われば、確率ノードの2番目のゲーム木を返す。

人間やコンピュータが指し手を選んだ時に、`pick-chance-branch`が呼ばれるようにする。  
まず、人間側を実装する。以前の`web-handle-human`からの変更点は、次のゲームの状態を表すゲーム木を返す箇所に`pick-chance-branch`を足しただけである。

```lisp
(defun web-handle-human (pos)
  (cond ((not pos) (princ "Please choose a hex to move from:"))
        ((eq pos 'pass) (setf *cur-game-tree*
                              (cadr (lazy-car (caddr *cur-game-tree*))))
         (princ "Your reinforcements have been placed.")
         (tag a (href (make-game-link nil))
              (princ "continue")))
        ((not *from-tile*) (setf *from-tile* nil)
         (princ "Move cancelled."))
        (t (setf *cur-game-tree*
                 (pick-chance-branch
                   (cadr *cur-game-tree*)
                   (lazy-find-if (lambda (move)
                                   (equal (car move)
                                          (list *from-tile* pos)))
                                 (caddr *cur-game-tree*))))
        (setf *from-tile* nil)
        (princ "You may now ")
        (tag a (href (make-game-link 'pass))
          (princ "pass"))
        (princ " or make another move:"))))
```

コンピュータ側の`handle-computer`も同様に変更する。  
関数の最後に`pick-chance-branch`を加えている。

```lisp
(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*) (car tree))))
    (pick-chance-branch
      (cadr tree)
      (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)))))
```

ここまでの変更により、新しいダイスオブドゥームをプレイできるようになっているはずである。  
ただ、このコードでは、ゲームAIは確率ノードのことを考慮できておらず、全ての攻撃が成功すると思って手を計算してしまう。  
そこで、次章ではAIエンジンを改良して、サイコロのランダム要素を考慮できるようにする。

### AIの改良
ゲームAIがサイコロについて考慮できるようにするためには、サイコロを振ったときの統計について知っておく必要がある。  
全ての可能なサイコロの個数の組み合わせについて、攻撃が成功する確率を計算したものを表で用意しておく。

```lisp
(defparameter *dice-probability* #(#(0.84 0.97 1.0  1.0)
                                   #(0.44 0.78 0.94 0.99)
                                   #(0.15 0.45 0.74 0.91)
                                   #(0.04 0.19 0.46 0.72)
                                   #(0.01 0.06 0.22 0.46)))
```

この表は、各行が守備側のサイコロの個数(1個〜5個)、各列が攻撃側のサイコロの個数(2個〜5個)の確率を表す。  
例えば、攻撃側が2個、守備側が1個の時、攻撃が成功する確率は84%である。

AIのコードの中心となる関数は`get-ratings`である。  
この関数は、可能な次の手それぞれに点数を与えるものであった。
点数の計算にサイコロを振る成功確率を考慮に入れる変更を施すこととする。
それぞれの攻撃について、成功した場合と失敗した場合それぞれの点数を、`*dice-probability*`から分かる確率を使って結合する。  
この新しい`get-ratings`関数では、攻撃の手について、その成功確率をテーブルから取り出し、攻撃が成功した場合の点数に乗算する。  
また、失敗確率(= 1 - 成功確率)を、失敗した場合の点数に乗算する。  
この両者の我が、攻撃手の点数である。  
これにより、`get-ratings`関数は確率ノードを考慮した点数を返せるようになった。

```lisp
(defun get-ratings (tree player)
  (let ((board (cadr tree)))
    (labels ((dice (pos)
               (cadr (aref board pos))))
      (take-all (lazy-mapcar
                  (lambda (move)
                    (let ((path (car move)))
                       (if path
                           (let* ((src (car path))
                                  (dst (cadr path))
                                  (probability (aref (aref *dice-probability*
                                                           (1- (dice dst)))
                                                     (- (dice src) 2))))
                             (+ (* probability (rate-position (cadr move) player))
                                (* (- 1 probability) (rate-position (caddr move)
                                                                    player))))
                           (rate-position (cadr move) player))))
                (caddr tree))))))
```

ゲームAIを確率ノードに完全に対応させるには、もう1つ小さな変更を行う。  
ゲーム木の大きさを制限する関数は、確率ノードから2つ枝が伸びていることを考慮する。
そして、勝つ場合と負ける場合の両方の枝を刈り込む必要がある。

```lisp
(defun limit-tree-depth (tree depth)
  (list (car tree)
        (cadr tree)
        (if (zerop depth)
            (lazy-nil)
            (lazy-mapcar (lambda (move)
                           (cons (car move)
                                 (mapcar (lambda (x)
                                           (limit-tree-depth x (1- depth)))
                                   (cdr move))))
              (caddr tree)))))
```

各指し手のリスト`(move)`の`cdr`に気を刈り込む関数を`mapcar`することで、確率ノードの両方の枝を刈り込める。  

**NOTE**  
ダイスオブドゥームのバージョン4では、アルファベータ法は使用しない。  
なぜなら、確率ノードがある場合のアルファベータ法は非常に複雑になるためである。

## 20.3 ダイスオブドゥームの補給ルールの改善
これまで、手番を終えた時に補給されるサイコロは、常に`その手番で得たサイコロの総数 - 1`であった。  
この補給ルールは、ゲームが進むに連れて必ずサイコロの総数が減るため、ゲームが必ず終了し、ゲーム木が有限の大きさを持つことを保証できる。  
しかし、バージョン2からゲーム木は遅延ツリーになっているため、大きさが無限になっても全く問題ない。
そこで、補給ルールを変更して、ゲームをより戦略的に面白くしてみよう。

新しいルールでは、補給サイコロの数は、プレイヤーが専有している連続した領域のうち最も大きいものの広さに等しいとする。  
こうすると、プレイヤーは、常に、領域が分断されるリスクを取れるかどうかの判断を迫れられる。あるいは、小さな領域を捨てて特攻攻撃を仕掛けるという手段もある。

新たな補給ルールを実現するため、まず、指定したマスを起点として、現在のプレイヤーが専有する連続した領域のマスのリストを返す`get-connected`を定義する。  
この関数は、8章のGTWと同様のアルゴリズムを用いて、連続するマスを見つけ出す。
すなわち、注目しているマスから隣接するマスへと再帰的に移動しながら、既に見たマスのリストを更新していくわけである。  
`get-connected`関数では、2つのローカルな再帰関数を定義している。

- `check-pos`関数は現在見ているマスがプレイヤーの所有であり、かつまだ見たことがなければそれを`visited`リストに追加する。
- `check-neighbors`関数は隣接したマスのリストを受け取ってその全てをチェックする。

この2つの関数は、相互に再帰して、連続したマスの一塊を見つけ出す。

```lisp
(defun get-connected (board player pos)
  (labels ((check-pos (pos visited)
             (if (and (eq (car (aref board pos)) player)
                      (not (member pos visited)))
                 (check-neighbors (neighbors pos) (cons pos visited))
                 visited))
           (check-neighbors (lst visited)
             (if lst
               (check-neighbors (cdr lst) (check-pos (car lst) visited))
               visited)))
  (check-pos pos '())))
```

相互再帰の起点は、目標のマス1つと、空の`visited`リストで`check-pos`を呼び出すことである。
この関数で連続するマスの1つの領域は見つけられるが、**最大の領域**を見つけるために、`largest-cluster-size`関数が必要となる。

```lisp
(defun largest-cluster-size (boardd player)
  (labels ((f (pos visited best)
             (if (< pos *board-hexnum*)
                 (if (and (eq (car (aref board pos)) player)
                          (not (member pos visited)))
                     (let* ((cluster (get-connected board player pos))
                            (size (length cluster)))
                       (if (> size best)
                           (f (1+ pos) (append cluster visited) size)
                           (f (1+ pos) (append cluster visited) best)))
                     (f (1+ pos) visited best))
               best)))
  (f 0 '() 0)))
```

最後に、この新しい補給ルールを反映するため、`add-new-dice`を変更する。

```lisp
(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n)
             (cond ((zerop n) lst)
                   ((null lst) nil)
                   (t (let ((cur-player (caar lst))
                            (cur-dice (cadar lst)))
                        (if (and (eq cur-player player) (< cur-dice *max-dice*))
                            (cons (list cur-player (1+ cur-dice))
                                  (f (cdr lst) (1- n)))
                            (cons (car lst) (f (cdr lst) n))))))))
  (board-array (f (coerce board 'list)
                  (largest-cluster-size board player)))))
```

新しい`add-new-dice`でも`spare-dice`引数を受け取っているが、これは`add-new-dice`を呼び出している箇所との互換性のためだけで、この引数は無視される。  
すなわち、追加される補給サイコロの数は最も大きな連続領域の大きさのみで決まる。  
`add-new-dice`関数の変更箇所はここのみである。

これで、新たな補給ルールを有効にするための全てのコードが完成した。  
この設計では、ゲームAIのプレイヤーがゲーム木の全てにアクセスできるようになっている。
ゲーム木はこの新たな補給を考慮したデータを持つので、ゲームAIは自動的に新たな補給ルールに合わせた最適な戦略を見つけるようになる。

## 20.4 終わりに
ダイスオブドゥームのゲームは、これにて完成である。  
プレイするには、下記のとおりコマンドを実行する。

```lisp
> (serve #'dod-request-handler)
```

そして、webブラウザで[ゲームページ](http://localhost:8080/game.html)を開く。

