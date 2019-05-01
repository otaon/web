---
title: "書籍 Land of Lisp 19章 ダイスオブドゥームにグラフィカルなWebインターフェースをつける"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
toc: true
tags: ["lisp"]
categories: ["Notes"]
authors:
- otaon
---

18章で作ったダイスオブドゥームVer2では、Ver1よりも大きなゲーム盤でプレイ可能となった。
この規模だと、コンソールでの可視化には視認性に限界がある。  
そこで、この章では、ダイスオブドゥームにグラフィックをつけ、クリックして手が指せるように改造する。

## 19.1 ゲーム盤をSVGフォーマットで描画する

13章でWebサーバを作成し、17章ではDSLを使ってSVGを描画した。
これらを組み合わせれば、ブラウザ上でグラフィック表示を簡単に実現できる。

HTML5の規格では、SVG画像をHTMLドキュメント内に埋め込むことができるから、これを利用する方針とする。

**NOTE** ここからは、18章で作成した`dice of doom version.2`と、13章で作成した`webserver`と、16,17章で作成した`SVG`レンダリングライブラリを使用する。

まず、ゲーム盤の各部の大きさを決める定数を定義する。  
ボードの幅と高さは900x500とする。  
`*board-scale*`は1つの升の幅の半分の長さをピクセル数で表したものである。  
`*top-offset*`は、盤の上に3マス分の空白を開けることを表す。  
`*dice-scale*`は、1つのサイコロの大きさ(幅、高さ)を指定する。  
`*dot-size*`はサイコロの目の点の大きさで、ここではサイコロ自体の大きさの0.05倍としている。

```lisp
(defparameter *board-width* 900)   ; ゲーム盤の横幅(pixel)
(defparameter *board-height* 500)  ; ゲーム盤の高さ(pixel)
(defparameter *board-scale* 64)    ; 1つのマスの幅の半分の長さ(pixel)
(defparameter *top-offset* 3)      ; ゲーム盤の上にあける空白の大きさ(何マス分か)
(defparameter *dice-scale* 40)     ; 1つのサイコロの大きさ(pixel)
(defparameter *dot-size* 0.05)     ; サイコロの目の大きさ(サイコロ自体の何倍か)
```

### サイコロを描く
サイコロを描くコードを示す。ここでは、サイコロを、SVGを使って全てコードとして記載する。

```lisp
(defun draw-die-svg (x y col)
  "指定した座標にサイコロを1つ描画する
   x: サイコロを描画するx座標(pixel)
   y: サイコロを描画するy座標(pixel)
   col: サイコロの色(RGB値)
   ret: -"
  (labels ((calc-pt (pt)
             ;; 描画対象の座標を補正する
             ;; pt:  補正する前の座標コンスセル
             ;; ret: 補正した後の座標コンスセル
             (cons (+ x (* *dice-scale* (car pt)))
                   (+ y (* *dice-scale* (cdr pt)))))
           (f (pol col)
             ;; 指定した頂点座標と色情報をもとにポリゴンを描画する
             ;; pol: ポリゴンの頂点座標
             ;; col: ポリゴンの色情報(RGB値)
             ;; ret: ポリゴンのsvg記述
             (polygon (mapcar #'calc-pt pol) col)))

    ;; サイコロの上面を描画する
    (f '((0 . -1) (-0.6 . -0.75) (0 . -0.5) (0.6 . -0.75))
       (brightness col 40))
    ;; サイコロの左面を描画する
    (f '((0 . -0.5) (-0.6 . -0.75) (-0.6 . 0) (0 . 0.25))
       col)
    ;; サイコロの右面を描画する
    (f '((0 . -0.5) (0.6 . -0.75) (0.6 . 0) (0 . 0.25))
       (brightness col -40))
    ;; サイコロの目を描画する(サイコロ1つの3面分を一気に)
    (mapc (lambda (x y)
            (polygon (mapcar
                       (lambda (xx yy)
                         ;; サイコロの目を描画する
                         (calc-pt (cons (+ x (* xx *dot-size*))
                                        (+ y (* yy *dot-size*)))))
                       ;; サイコロの目のx座標とy座標
                       '(-1 -1 1 1)
                       '(-1 1 1 -1))
                     ;; サイコロの目の色(白)
                     '(255 255 255)))
          ;; サイコロの目のx座標とy座標
          '(-0.05 0.125 0.3 -0.3 -0.125 0.05 0.2 0.2 0.45 0.45 -0.45 -0.2)
          '(-0.875 -0.80 -0.725 -0.775 -0.70 -0.625 -0.35 -0.05 -0.45 -0.15 -0.45 -0.05))))
```

では、`x=50, y=50`の位置に、RGB値`(255 0 0)`(赤)のサイコロを描く。

```lisp
> (svg 100 100 (draw-die-svg 50 50 '(255 0 0)))
; サイコロ1つ分のSVGコードが表示される
```

### マスを描く
次に、6角マスとその上に積み上がったサイコロを描く関数を書こう。

```lisp
(defun draw-tile-svg (x y pos hex xx yy col chosen-tile)
  "六角形のマスとその上に積み上がったサイコロを描く
   x: マスのx座標(マス目)
   y: マスのy座標(マス目)
   pos: 描画対象のマス
   hex: プレイヤーIDとサイコロ数のコンスセル
   xx: マスの描画用x座標(pixel)
   yy: マスの描画用y座標(pixel)
   col: マスとサイコロの色
   chosen-tile: 選択中のマスの番号
   ret: -"
  ;; マスを描く(厚みを持たせるため、縦をずらして2重に描く)
  (loop for z below 2
        do (polygon (mapcar (lambda (pt)
                              (cons (+ xx (* *board-scale* (car pt)))
                                    (+ yy (* *board-scale* (+ (cdr pt) (* (- 1 z) 0.1))))))
                            ;; 六角形のマスの座標(上から時計回り)
                            '((-1 . -0.2) (0 . -0.5) (1 . -0.2) (1 . 0.2) (0 . 0.5) (-1 . 0.2)))
                    ;; 選択中のマスを明るくする
                    (if (eql pos chosen-tile)
                        (brightness col 100)
                        col)))
  ;; サイコロを描く
  (loop for z below (second hex)
        do (draw-die-svg (+ xx
                            (* *dice-scale*
                               0.3
                               ;; サイコロを左右にブレさせる
                               (if (oddp (+ x y z))
                                   -0.3
                                   0.3)))
                         (- yy (* *dice-scale* z 0.8))
                         col)))
```

では、1マス分のタイルを描く。

```lisp
> (svg 300 300 (draw-tile-svg 0 0 0 '(0 3) 100 150 '(255 0 0) nil))
; サイコロ3つが載ったタイル1つ分のSVGコードが表示される
```

### ゲーム盤を描く
ゲーム盤全体をSVG画像として描く。

```lisp
;; サイコロの色(赤と青)
(defparameter *die-colors* '((255 63 63) (63 63 255)))
```

SVGには、webリンクを埋め込むことができる。  
これは、通常のHTMLにおける`<a href="...">`によるハイパーリンクと同様に動作する。  
プレイヤーが次に選択できるマスについて、そのマスのSVGをリンクで囲んでやることにより、マスがクリック可能になる。

ゲーム盤は、斜めから見下ろした形で描画するため、真上からみた形の座標を変換している。  
また、奥に行くにつれてマスを暗くすることにより、奥行きを出している。


```lisp
(defun draw-board-svg (board chosen-tile legal-tiles)
  "ゲーム盤をsvg記述する
   board: ゲーム盤情報
   chosen-tile: 選択中のマス
   legal-tiles: プレイヤーが次に選択可能なマスのリスト
   ret: -"
  ;; ゲーム盤の全マスを走査する
  (loop for y below *board-size*
        do (loop for x below *board-size*
                 ;; 現在のマスの番号
                 for pos = (+ x (* *board-size* y))
                 ;; 現在のマスの情報(プレイヤーIDとサイコロ数)
                 for hex = (aref board pos)
                 ;; 現在のマスの表示座標(x座標)
                 for xx = (* *board-scale* (+ (* 2 x) (- *board-size* y)))
                 ;; 現在のマスの表示座標(y座標)
                 for yy = (* *board-scale* (+ (* y 0.7) *top-offset*))
                 ;; マスとサイコロの色(上の行ほど暗く補正する)
                 for col = (brightness (nth (first hex) *die-colors*)
                                       (* -15 (- *board-size* y)))
                 ;; 現在のマスが、プレイヤーが次に選択可能なマス、または、選択中のマスの場合、
                 ;; リンクで囲ってクリック可能にする
                 ;; 現在のマスが、それ以外の場合、そのまま選択される
                 do (if (or (member pos legal-tiles) (eql pos chosen-tile))
                        ;; リンクの場合は1マス分を<g>タグで囲んでグルーピングする
                        (tag g ()
                             (tag a ("xlink:href" (make-game-link pos))
                                  (draw-tile-svg x y pos hex xx yy col chosen-tile)))
                        (draw-tile-svg x y pos hex xx yy col chosen-tile)))))
```

`make-game-link`は、適切なURLを作って返す関数である。

```lisp
(defun make-game-link (pos)
  "リンクするURLを生成する
   pos: リンク対象のマスの番号
   ret: -"
  (format nil "/game.html?chosen=~a" pos))
```

下記を実行した結果をファイルに保存してwebブラウザで表示すると、ゲーム盤が表示される。

```lisp
> (svg *board-width* *board-height* (draw-board-svg (gen-board) nil nil))
; ゲーム盤のSVGコードが表示される
```

## 19.2 Webサーバインターフェースを作る

### リクエストハンドラの作成
webサーバの中心となる関数は、`dod-request-handler`である。
この関数は、先に作ったwebブラウザからくる全てのリクエストを処理する役割を持つ。
次に示すのが、`dod-request-handler`のコードである。

```lisp
;; 現在のゲーム木
(defparameter *cur-game-tree* nil)
(defparameter *from-tile* nil)
```

```lisp
(defun dod-request-handler (path header params)
  "Webブラウザから来る全てのリクエストを処理する
   path: URL
   header: *未使用*
   params: URLのパラメータ
   ret: -"
  ;; アクセスされたURLがgame.htmlならゲーム処理する
  (if (equal path "game.html")
      ;; doctypeを指定して、html5だと認識させる
      (progn (princ "<!doctype html>")
             (tag center ()
                  (princ "Welcome to DICE OF DOOM!")
                  (tag br ())
                  (let ((chosen (assoc 'chosen params)))
                    ;; どのマスも選択されていないか、ゲーム木が空なら、
                    ;; ゲームを初期化する
                    (when (or (not *cur-game-tree*) (not chosen))
                      (setf chosen nil)
                      (web-initialize))
                    ;; ゲーム木における可能な手が空なら、ゲームを終了させる
                    ;; 人間のプレイヤーの手番なら、パラメータから指し手を取得し、htmlを組み立てる
                    ;; ゲームAIの手番なら、ゲームAIに指し手を選ばせ、htmlを組み立てる
                    (cond ((lazy-null (caddr *cur-game-tree*))
                           (web-announce-winner (cadr *cur-game-tree*)))
                          ((zerop (car *cur-game-tree*))
                           (web-handle-human
                             (when chosen
                               (read-from-string (cdr chosen)))))
                          (t (web-handle-computer))))
                  (tag br ())
                  ;; ゲーム盤を描く
                  (draw-dod-page *cur-game-tree* *from-tile*)))
      (princ "Sorry... I don't know that page.")))
```

`dod-request-handler`では、まず、リクエストされたページが`game.html`であるかどうかをチェックする。  
このページが、webサーバ上でゲームを置いておくことにするページである。  

ページの先頭では、まず*doctype*を指定する。
これにより、webブラウザは返されたページがHTML5であると認識する。  
その後、オープニングメッセージを画面中央に表示するHTMLを出力する。  
### このゲームwebサーバの制限
このwebサーバには、制限が存在する。  
まず、処理を簡単にするため、`dod-request-handler`は誰からのwebリクエストが来たのかを一切チェックしていない。  
したがって、複数のプレイヤーが別々のゲームを同時にプレイしようとしたら、`dod-request-handler`は正常に動作しない。  
マルチユーザ対応したいのであれば、セッション情報をキーとするハッシュテーブルに、グローバル変数の情報を格納してしまうことにより、ユーザごとのゲーム木を保持させることができる。

`dod-request-handler`のもう一つの制限は、URLからの情報を読むために`read-from-string`関数を使っていることである。  
この関数は、悪意のあるLispプログラマであれば、簡単に任意コードを実行されてしまう。  
したがって、このサーバをインターネット上に公開するのは強く非推奨である。

### ゲームを初期化する
新規にダイスオブドゥームを始めるために、ゲームエンジンを初期化する`web-initialize`のコードを次に示す。
`dod-request-handler`では、`param`を見て、ゲーム木が空、あるいは、どのマスも選択されていない場合、`web-initialize`関数を呼んでゲームを新規で開始する。

```lisp
(defun web-initialize ()
  "ゲームエンジンを初期化する
   ret: -"
  ;; ランダムなゲーム盤を作成して保持する
  (setf *from-tile* nil)
  (setf *cur-game-tree* (game-tree (gen-board) 0 0 t)))
```

### 勝者を表示する
webブラウザに勝者を表示する関数を示す。

```lisp
(defun web-announce-winner (board)
  "勝者を表示する"
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w)))))
  (tag a (href "game.html")
       (princ " play again")))
```

### 人間のプレイヤーの処理
`web-handle-human`は、人間のプレイヤーの手番である場合のHTMLページの作成を行う。

```lisp
(defun web-handle-human (pos)
  "人間のプレイヤーを処理する
   pos: 選択したマスの番号"
  (cond
    ;; マスを未選択:
    ;; 攻撃元のマス選択メッセージを表示
    ((not pos) (princ "Please choose a hex to move from:"))
    ;; パスを選択済み:
    ;; プレイヤーの補給が完了したとメッセージを表示
    ;; パラメータにnilを渡すcontinueリンクを表示
    ((eq pos 'pass) (setf *cur-game-tree*
                          (cadr (lazy-car (caddr *cur-game-tree*))))
                    (princ "Your reinforcements have been placed.")
                    (tag a (href (make-game-link nil))
                         (princ "continue")))
    ;; マスを選択済み & 攻撃元のタイルがセットされていない:
    ;; 今選ばれたマスを攻撃元としてセット
    ((not *from-tile*) (setf *from-tile* pos)
                       (princ "Now choose a destination:"))
    ;; 今選択したマスが攻撃元のタイルと同じ:
    ;; 攻撃元のタイルをリセット
    ((eq pos *from-tile*) (setf *from-tile* nil)
                          (princ "Move cancelled."))
    ;; 上記以外(=攻撃元と攻撃先を選択完了した):
    ;; 攻撃元と攻撃先に対応するゲーム木に遷移する
    ;; 次の手を指すかパスするかを選ばせる
    (t (setf *cur-game-tree*
             (cadr (lazy-find-if (lambda (move)
                                   (equal (car move)
                                          (list *from-tile* pos)))
                                 (caddr *cur-game-tree*))))
       (setf *from-tile* nil)
       (princ "You may now ")
       (tag a (href (make-game-link 'pass))
            (princ "pass"))
       (princ " or make another move:"))))
```

### コンピュータプレイヤの処理
`web-handle-computer`は、ゲームAIプレイヤーの手番である場合のHTMLページの作成を行う。

```lisp
(defun web-handle-computer ()
  "ゲームAIプレイヤーを処理する"
  ;; ゲームAIにゲーム木を遷移させる
  (setf *cur-game-tree* (handle-computer *cur-game-tree*))
  (princ "The computer has moved. ")
  ;; webブラウザを5秒毎にリロードさせる
  ;; これによりリロードしたときにはコンピュータの手番とさせるために、chosen=NILとしている
  (tag script ()
       (princ "window.setTimeout('window.location=\"game.html?chosen=NIL\"',5000)")))
```

### HTMLの中にSVGゲーム盤を描く
`draw-dod-page`関数は、ゲームサーバとSVG生成コードとをつなぎ、現在のゲーム盤を描く。

```lisp
(defun draw-dod-page (tree selected-tile)
  "HTMLの中にSVGゲーム盤を描く
   tree: ゲーム木
   selected-tile: タイルを選択中か"
  (svg *board-width*  ; ゲーム盤の幅
       *board-height* ; ゲーム盤の高さ
       (draw-board-svg (cadr tree)
                       selected-tile
                       ;; プレイヤーが選択可能なマスのリストを計算する
                       (take-all (if selected-tile
                                     ;; 攻撃元のタイルを選択中なら、
                                     ;; 有効な攻撃先を全て収集する
                                     (lazy-mapcar
                                       (lambda (move)
                                         (when (eql (caar move)
                                                    selected-tile)
                                           (cadar move)))
                                       (caddr tree))
                                     ;; 攻撃元のタイルを選択していなかったら、
                                     ;; 有効な攻撃から、攻撃元を収集する
                                     (lazy-mapcar #'caar (caddr tree)))))))
```

## 19.3 ダイスオブドゥームVer3をプレイする
サーバ側で下記のコマンドを叩くことでゲームを起動できる。

```lisp
> (serve #'dod-request-handler)
```

次に、クライアント側のwebブラウザで[ゲームページ](http://localhost:8080/game.html)にアクセスする。

