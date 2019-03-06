---
title: "書籍 実践 Common Lisp 第3章 実践：簡単なデータベース"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
tags: ["lisp"]
categories: ["Notes"]
authors:
- otaon
---

# 第3章 実践：簡単なデータベース
ここでは、CD情報をデータベースのレコードとして残すコードを実装しながら、Lispの基本的なデータ操作方法を学ぶ。

## 3.1 CDとレコード
リッピング対象のCDの情報を管理するため、下記の情報を管理するデータベースを作成する。

- CDタイトル
- アーティスト
- CDのレーティング
- リッピング完了したか

CDのデータベースには、keyとvalueの組み合わせを持つリストを使うのが良さそうである。  
Lispでは、連想配列の実現方法として主に2種類のデータ構造が使える。

|名称                                |データ構造       |コード例                 |
|-----------------------------------|----------------|-----------------------|
|連想リスト(association list, alist) | keyとvalueのドットペアをリストの要素に持つ | `'((key1 . value1) (key2 . value2) ...)`|
|属性リスト(property list, plist)    | keyとvalueを交互にリストの要素に持つ | `'(key1 value1 key2 value2 ...)`        |

ここでは、**属性リスト**を採用する。

CDのレコード情報を取り、CDを表す属性リストを返す関数を定義する。

```lisp
(defun make-cd (title artist rating ripped)
  "CDを表す属性リストを返す
   title:  CDタイトル
   artist: アーティスト名
   rating: CDのレーティング
   ripped: リッピング完了したか
   ret: CDを表す属性リスト"
  (list :title title :artist artist :rating rating :ripped ripped))
```

## 3.2 CDのファイリング
複数のレコードを保持するデータ構造には、リストを採用する。

ダイナミック変数として`*db*`を定義する。

```lisp
;; CDデータベース
(defparameter *db* nil)
```

レコードを`*db*`に格納する関数を定義する。

```lisp
(add-record (cd)
  "CD情報をデータベースに追加する
   cd: CDを表す属性リスト
   ret: CD情報追加後のCDデータベース"
  (push cd *db*))
```

## 3.3 データベースの中身を見てみる
CD情報のデータベースを全てダンプする関数を定義する。  
この関数では、`format`関数のフォーマット指定子によって、複数のレコードを1行で表示する。

```lisp
(defun dump-db ()
  "CDデータベースの内容をダンプする
  ret: -"
  ;; 1レコード分の表示例
  ;;   TITLE:  Syro
  ;;   ARTIST: Aphex Twin
  ;;   RATING: 3
  ;;   RIPPED: T
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))
```

## 3.4 ユーザインタラクションを改善する
ユーザフレンドリなレコード追加仕組みを作成する。

ユーザプロンプトを表示して、標準ストリームへの入力を促す関数を定義する。

```lisp
(defun prompt-read (prompt)
  "prompt: ユーザ入力画面のプロンプト文字
   ret: ユーザが入力したデータ1行"
  ;; 標準I/Oストリームを*query-io*に接続し、promptの値を流し込む
  (format *query-io* "~a: " prompt)
  ;; *query-io*ストリームを、改行コードがなくても表示する
  (force-output *query-io*)
  ;; ユーザに*query-io*ストリームへの入力を促す
  (read-line *query-io*))
```

上記の、ユーザへの入力を促す機能を利用して、ユーザにCD1枚分のデータを入力させる関数を定義する。

```lisp
(defun prompt-for-cd ()
  "ユーザにCD1枚分のデータを入力させる
   ret: CDを表す属性リスト"
  (make-cd
    ;; CDタイトル
    (prompt-read "Title: ")
	;; アーティスト名
    (prompt-read "Artist: ")
	;; CDのレーティング
    (or
	  ;; レーティングは数値で取得する
	  ;; レーティングデフォルト値: 0
	  (parse-integer (prompt-read "Rating: ") :junk-allowed t)
	  0)
	;; リッピング完了したか
    (y-or-n-p "Ripped [y/n]: ")))
```

上記の、ユーザにCD1枚分のデータを入力させる関数を利用して、ユーザにCDのデータを好きなだけ入力させる関数を定義する。

```lisp
(defun add-cds ()
  "ユーザにCD情報を登録させる
   ret: -"
  (loop (add-record (prompt-for-cd))
    (if (not (y-or-n-p "Anther? [y/n]: "))
	    (return))))
```

## 3.5 データベースの保存と読み出し
データベースの保存では、単純に`*db*`の中身をファイルに書き出すことにする。  
あとでファイルから変数に代入できるように、あとからREPLが読み直す事のできる`print`関数を定義する。

```lisp
(defun save-db (filename)
  "指定したファイルにデータベースの中身を保存する
   filename: 保存先のファイルパス
   ret: -"
  (with-open-file (out filename
                   :direction :output      ; 出力先とする
				   :if-exists :supersede)  ; 既存のファイルを上書き
    (with-standard-io-syntax  ; print関数に影響する変数を標準に保つ
	  (print *db* out))))     ; REPLで読み直せる形でファイルに書き出す
```

ファイルに書き出したデータベースの情報を`*bd*`に代入する関数を定義する。

```lisp
(defun load-db (filename)
  "ファイルに書き込まれたデータベースを*db*に読み込む
   filename: CDデータベースが保存されたファイルパス
   ret: -"
  (with-open-file (in filename) ; inにfilenameの内容を入力する
    (with-standard-io-sytax		; read関数に影響する変数を標準に保つ
	  (setf *db* (read in)))))
```

## 3.6 データベースにクエリを投げる
特定のアーティストのレコードのみを取得したい場合、`remove-if-not`関数を利用することが簡単に実現できる。

```lisp
(defun select-by-artist (artist)
  "指定したアーティストのレコードのみ返す
   artist: アーティスト名
   ret: レコードのリスト"
  (remove-if-not
    #'(lambda (cd)
	    (equal (getf cd :artist) artist))
	artist))
```

これを応用して、任意のアトリビュートに対して値を指定してレコードを取得する関数を定義する。  
タイトル指定用の関数、アーティスト名指定用の関数を全てのアトリビュート分用意すれば、全てのアトリビュートに対して絞り込みをかけることができる。

**`select`**  

```lisp
(defun select (selector-fn)
  "データベースからアトリビュート指定用の関数が真となるレコードのみ返す
   selector-fn: レコードの検索条件を満たすか判定する関数(セレクタ)
   ret: レコードのリスト"
   (remove-if-not selector-fn *bd*))
```

しかしながら、全てのアトリビュートに対して指定用の関数を定義するのは拡張性や保守性が悪くなる。  
そこで、キーワードパラメータを使用する。キーワードパラメータとは`&key`を使用した引数の指定方法で、これを使用すると関数の引数の個数に依存されることがなくなる。

```lisp
(defun foo (&key a b c)
  (list a b c))
```

この`foo`は、下記のように呼び出せる。  
見ての通り、`&key`に続く仮引数の変数は、引数におけるキーワードに続く値により束縛されている。  
また、引数の値はキーワードとの対応のみによって仮引数に関連付けられており、引数の位置は全く関係しない。

```lisp
> (foo :a 1 :b 2 :c 3)
(1 2 3)
> (foo :c 3 :b 2 :a 1)
(1 2 3)
> (foo :a 1 :c 3)
(1 NIL 3)
> (foo)
(NIL NIL NIL)
```

また、`&key`の後に定義された**キーワードパラメータ**では、単純にキーワードパラメータを指定するだけではなく、  
`(キーワードパラメータ名 デフォルト値 キーワード引数が与えられたかを表す変数)`  
という値を指定できる。  

```lisp
> (defun foo (&key a (b 20) (c 30 c-p))
    (list a b c c-p))

> (foo :a 1 :b 2 :c 3)
(1 2 3 T)
> (foo :c 3 :b 2 :a 1)
(1 2 3 T)
> (foo :a 1 :c 3)
(1 20 3 T)
> (foo)
(NIL 20 30 NIL)
```

この`&key`を利用して、SQLなどの`where`のような機能を実現する。  
where句に相当する関数を返す関数を定義している。
where関数が返す無名関数は、`title`、`artist`、`rating`、`ripped`をwhere関数から保持している。(クロージャ)  

**`where`**  

```lisp
(defun where (&key title artist rating (ripped nil ripped-p))
  "レコードの検索条件を満たすか判定する関数を返す
   &key
   title: タイトル
   artist: アーティスト名
   rating: CDのレーティング
   ripped: リッピング完了したか
   ret: 引数で指定した条件を判定する関数"
  #'(lambda (cd)
      ;; レコードが指定された検索条件に合致するか判断する
      ;; cd: 対象レコード
      ;; ret: レコードが指定された検索条件に合致するか
      (and
        (if title 
          (equal (getf cd :title) title)
          t)
        (if artist
          (equal (getf cd :artist) artist)
          t)
        (if rating
          (equal (getf cd :rating) rating)
          t)
        (if ripped-p
          (equal (getf cd :ripped) ripped)
          t))))
```

キーワードパラメータ`ripped`については、`(ripped nil ripped-p)`という3つの要素を持つリストを指定している。  
これにより、`where`関数の`ripped`引数について下記を区別できるようになる。

- `where`を呼び出した側が、「フィールド`ripped`の値が`nil`のCDを選び出せ」という意味で`:ripped nil`と指定した
- `where`を呼び出した側が、「フィールド`ripped`の値は任意」という意味で、`:ripped`を指定しなかった

これを実行すると、下記のようになる。

```lisp
;;; アーティスト名を指定してレコードを取得する
> (select (where :artist "Dixie Chicks"))
;;; レーティングとリッピング歴を指定してレコードを取得する
> (select (where :rating 10 :ripped nil))
```

## 3.7 既存のレコードを更新する - もう1つの`WHERE`の使い方

ここまでで、SQLクエリの`select`と`where`句に相当する関数を定義できた。

次に、SQLクエリの`update`句に相当する関数を定義する。

**`update`**  

```lisp
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  "特定のレコードを更新する
   selector-fn: レコードの検索条件を満たすか判定する関数(セレクタ)
   &key
   title: 更新後のタイトル
   artist: 更新後のアーティスト名
   rating: 更新後のレーティング
   ripped: 更新後のリッピング歴
   ret: -"
  (setf *db*
        (mapcar
          #'(lambda (row)
              ;; セレクタで条件にマッチするレコード(row)のみ対象とする
              (when (funcall selector-fn row)
                (if title
                    ;; タイトルが指定されている場合、タイトルを更新
                    (setf (getf row :title) title))
                (if artist
                    ;; アーティストが指定されている場合、アーティストを更新
                    (setf (getf row :artist) artist))
                (if rating
                    ;; レーティングが指定されている場合、レーティングを更新
                    (setf (getf row :rating) rating))
                (if ripped-p
                    ;; リッピング歴が指定されている場合、リッピング歴を更新
                    (setf (getf row :ripped) ripped)))
              row)
          *db*)))
```

`update`は下記のように使用できる。

```lisp
;;; update前
> (select (where :artist "Dixie Chicks"))
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 0 :Ripped T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 0 :Ripped T))

> (update (where :artist "Dixie Chicks") :rating 11)
NIL

;;; update後
> (select (where :artist "Dixie Chicks"))
((:TITLE "Home" :ARTIST "Dixie Chicks" :RATING 11 :Ripped T)
 (:TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 11 :Ripped T))
```

`delete`関数を定義するには、下記のようにする。

**`delete`**  

```lisp
(defun delete-rows (selector-fn)
  "特定のレコードを削除する
   selector-fn:  レコードの検索条件を満たすか判定する関数(セレクタ)
   ret: -"
   ;; 古い*db*から値を削除したデータベースを新しい*db*に代入する
  (setf *db* (remove-if selector-fn *db*)))
```

## 3.8 ムダを排除して勝利を収める

ここまでで、`select`、`where`、`update`、`delete`は、全て合わせて50行足らずで定義できた。  
しかしながら、コード上にはまだ重複が存在するので、それを取り除くことにする。

修正対象は`where`関数である。

```lisp
(defun where (&key title artist rating (ripped nil ripped-p))
      (and
        (if title 
          (equal (getf cd :title) title)
          t)
        (if artist
          (equal (getf cd :artist) artist)
          t)
        (if rating
          (equal (getf cd :rating) rating)
          t)
        (if ripped-p
          (equal (getf cd :ripped) ripped)
          t))))
```

`where`関数の下記の部分は、フィールドごとに何度も出てくる。

```lisp
        (if title 
          (equal (getf cd :title) title)
          t)
```

このようなコードの重複は、フィールド構成を変更するときにコードの修正量が多くなるという欠点がある。
そこで、この部分をどうにか短縮することを考える。  

まず、`where`で実現したいことの本質は、「意図したフィールドの値をチェックするコードを生成する」だけである。  
例えば、下記の`select`の呼び出しコードを考える。

```lisp
> (select (where :title "Give Us a Break" :ripped t))
```

このコードの`where`部分は、下記のように書き換えられる。

```lisp
(select
  #'(lambda (cd)
      (and (equal (getf cd :title) "Give Us a Break")
           (equal (getf cd :ripped) t))))
```

つまり、`where`で返される関数内で行われているムダな処理(=引数に渡されていないフィールドの有無まで確認する処理)を省く事ができる。
しかし、全ての`where`関数呼び出し部分でこの修正を加えるのは非常に骨が折れる。

上記の知見から、修正方針をまとめると下記の2点となる。

- `where`には、呼び出し時に注目していない(=ムダな)フィールドの存在をチェックするコードを書きたくない
- `where`の呼び出し部分で、`where`を必要なフィールドのみチェックする関数に手作業で置き換えるのは大変なのでやりたくない

これを実現するために、マクロを使用する。マクロを使用すれば、必要なフィールドのみチェックするコードを持つ`where`関数を生成することが可能となる。

例えば、下記のような`backwords`というマクロを定義すれば、Lispの逆表記バージョンの言語を作ることも可能となる。
このように、マクロを用いることで、REPL評価前の式を変形させることができる。

```lisp
> (defmacro backwords (expr) (reverse expr))
> (backwords ("hello, world" t format))
hello, world
NIL
```

このようにマクロを使用して、参照されるそれぞれのフィールドに対応した式を返す`where`マクロを定義してみる。  
ここからは、下記の手順で`where`の改良版を実現する。

1. `make-comparison-expr`関数：フィールドに対応する比較式を作る
2. `make-comparison-list`関数：`make-comparison-expr`関数を使って、複数のフィールドに対応する比較式を作る
3. `where`マクロ：`make-comparison-list`関数を使って、複数のフィールドに対応する比較式を`AND`で包んだ関数を返す

まず、引数に指定したフィールドに対応した比較の式を返す`make-comparison-expr`関数を定義する。

```lisp
> (defun make-comparison-expr (field value)
    "指定したフィールドに対応する比較式を返す
     field: フィールド名
     value: 値
     ret: フィールドに対応する比較式"
    `(equal (getf cd ,field) ,value))
```

これを使用すると、下記のようになる。

```lisp
> (make-comparison-expr :rating 10)
(EQUAL (GETF CD :RATING) 10)
> (make-comparison-expr :title "Give Us a Break")
(EQUAL (GETF CD :TITLE) "Give Us a Break")
```

次に、`make-comparison-expr`関数が返す比較式を集める`make-comparison-list`関数を定義する。

```lisp
(defun make-comparison-list (fields)
  "複数の指定したフィールドに対応する比較式のリストを返す
   fields: フィールドのキーワードと値が交互に並んだリスト
   ret: フィールドに対応する比較式のリスト"
  (loop while fields
    collecting (make-comparison-expr (pop fields) (pop fields))))
```

最後に、`make-comparison-list`関数が返す比較式のリストを`AND`で包む`where`マクロを定義する。

```lisp
(defmacro where (&rest clauses)
  ;; 複数のフィールドに対応する比較式を`AND`で包んだ関数を返す
  ;; clauses: whereで指定するフィールド(複数)
  ;; ret: 指定された複数フィールドに対応するフィルタ関数
  `#'(lambda (cd)
       (and ,@(make-comparisons-list clauses))))
```

**NOTE**  
`&rest`が引数リストにあると、関数やマクロは任意個の引数をとれるようになり、その引数はリストとして`&rest`の直後にある仮引数を束縛する。  
つまり、下記のようになる。

```lisp
> (defun f (&rest args)
    args)
> (f :a 1 :b 2 :c 3 :d 4)
(:a 1 :b 2 :c 3 :d 4)
```

**NOTE**  
`,@`は、評価された結果のリストを展開する。つまり、下記のようになる。

```lisp
;; ,@を使わない例
`(and ,(list 1 2 3)) => (AND (1 2 3))

;; ,@を使う例
`(and ,@(list 1 2 3)) => (AND 1 2 3)
`(and ,@(list 1 2 3) 4) => (AND 1 2 3 4)
```

`where`マクロを呼び出すとどんなコードが生成されるのかを知るには、`macroexpand-1`関数が使用できる。

```lisp
> (macroexpand-1 '(where :title "Give Us a Break" :ripped t))
#'(LAMBDA (CD)
    (AND (EQUAL (GETF CD :TITLE) "Give Us a Break")
         (EQUAL (GETF CD :RIPPED) T))
T
```

実際に、上記で定義した`where`マクロを試してみる。

```lisp
> (select (where :title "Give Us a Break" :ripped t))
((:TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T))
```

このように、`where`マクロは最初の`where`関数よりも短く簡潔で、かつ、特定のフィールドと結びついていないため、フィールドの構成変更に影響されない。

## 3.9 まとめ
上記では、コードの重複を取り除こうとしたら、同時にコードの汎用性も向上した。何故だろうか？  
なぜなら、マクロはシンタックスレベルでの抽象化を施す仕組みの1つであり、抽象化というのは物事の普遍性を表現する手法の1つであるからである。  

なお、この簡易データベースのコードにおいて、CDとそのフィールド固有のコードを含む箇所は、`make-cd`と`prompt-for-cd`と`add-cd`のみとなっていることにも注目するべきである。
