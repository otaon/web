---
title: "書籍 実践 Common Lisp 第9章 実践：ユニットテストフレームワーク"
date:    2019-05-28T00:00:00+09:00
lastmod: 2019-05-28T00:00:00+09:00
draft: false
toc: true
tags: ["lisp", "実践-Common-Lisp"]
categories: ["実践-Common-Lisp"]
authors:
- otaon
---

## この章について
この章では、マクロを使ってユニットテストフレームワークを作成する。

テストとは、結局のところ**承認(accept)**か**拒否(reject)**のどちらかに評価される。  
しかし、使い勝手を考えていくと、下記のような機能が無いと困ることに気付く。

- どのサブテストが失敗したのかを表示する
- 失敗したときの期待値と実行結果は何だったのかを表示する

## 9.1 最初の試みを2件
副作用を持つ機能に対しては、機能を呼び出した後に、正しい副作用が起こったのかを確かめる必要がある。  
反対に、副作用を持たない機能に対しては、機能の戻り値を受け取って、期待値と比較すれば良い。

例えば、加算`+`が正しく実行されているか確認するための簡単なテストなら、下記のコードで十分だ。

```lisp
(defun test-+ ()
  (and (= (+ 1 2) 3)
       (= (+ 1 2 3) 6)
       (= (+ -1 -3) -4)))

(test-+)
; => T
```

各テストケースで何が起こったのかを確認したいのであれば、下記のように愚直に各方法もある。  
`~:[FAIL~;pass~]`、最初の引数が偽の場合は"FAIL"を、そうでなければ"pass"を表示する。

```lisp
(defun test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3)    '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6)  '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
```

上記のコードでは、下記の点で使いにくい。

- 繰り返し`format`を呼び出している
- テストの式が表示用と評価用で2度記述されている
- 全体として合格か不合格かが分かりにくい(全テストケースがpassであると確認する必要がある)

## 9.2 先述のコードをリファクタリングする
ここで本当に実現したいのは、下記の両立だ。

- 1つ目の例のように、`T`か`NIL`を返す1つ目のバージョンの`test-+`くらい直感的
- 2つ目の例のように、個々のテストケースについての結果も報告される

似たような`format`の呼び出しを繰り返さないようにするには、新しい関数を定義する。

```lisp
(defun report-result (result form)
  (format t "~:[FAIL;pass~] ... ~a~%" result form))
```

`format`の代わりに上記の`report-result`を使うことで、結果のレポート方法の定義を一箇所にまとめられた。

```lisp
(defun test-+ ()
  (report-result (= (+ 1 2) 3)    '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6)  '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
```

次に、テストケースの式が、表示用と評価用で繰り返されている部分をまとめる。  
マクロを使えば簡単に定義できる。

```lisp
(defmacro check (form)
  `(report-result ,form ',form))

(check (= (+ 1 2) 3))
; =>
; (report-result (= (+ 1 2) 3)    '(= (+ 1 2) 3))
```

`check`を使って`test-+`を書き換えると下記の通りになる。


```lisp
(test-+
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))
```

上記を見ると、そもそも`check`を繰り返していることに気付く。これも邪魔なので記述しないように`check`を修正する。  
下記では、**複数のフォームの並びを単一のフォームにするため`progn`で包み込む**というイディオムを利用している。
ここでは必須ではないが、紹介のために使用している。よく使うので覚えると良い。

```lisp
(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms
             collect `(report-result ,f ',f))))
```

こうすると、`test-+`は下記の通りになる。

```lisp
(test-+
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))
```

上記の`check`を展開すると下記の通りになる。

```lisp
(defun test-+ ()
  (progn
    (report-result (= (+ 1 2) 3)    '(= (+ 1 2) 3))
    (report-result (= (+ 1 2 3) 6)  '(= (+ 1 2 3) 6))
    (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4))))
```

## 9.3 戻り値を手直しする
`test-+`を手直しして、全テストケースにパスしたかどうかを戻り値によって示すようにする。  
最終的にテストケースを実行するコードの生成は`check`が担っているため、これを変更すれば良い。

最初に、`report-result`を変更して、レポート対象のテストケースの結果を返すようにする。

```lisp
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)
```

これで各テストケースの結果を`report-result`が返すようになった。  
全てのテストケースを`and`で繋げれば良いと思うところだが、`and`は短絡評価であるため、NGが出た時点でテスト実施が止まってしまう。  
したがって、`and`の短絡評価ではないバージョンのようなものを作成すれば良い。

ここで作るマクロを`combine-results`と命名する。  
このマクロは下記のように記述する。

```lisp
(combine-results
  (foo)
  (bar)
  (baz))
```

このマクロは下記のように展開されることとする。

```lisp
(let ((result t))
  (unless (foo) (setf result nil)) ; fooが失格だった場合、resultにnilをセットする
  (unless (bar) (setf result nil))
  (unless (baz) (setf result nil))
  result)
```

このマクロでは変数を使っている。すなわち、変数名が重複する可能性がある。  
そこで、先述の通り`(gensym)`を使ってユニークな変数を定義する。

```lisp
(defmacro combine-results (&body forms)
  (let ((result (gensym)))
    `(let ((,result t))
       ,@(loop for f in forms
              collect `(unless ,f (setf ,result nil)))
       ,result)))

(macroexpand-1 '(combine-results (= (+ 1 2 3) 6) (= (+ 1 2) 3)))
; =>
(LET ((#:G563 T))
  (UNLESS (= (+ 1 2 3) 6) (SETF #:G563 NIL))
  (UNLESS (= (+ 1 2) 3) (SETF #:G563 NIL))
  #:G563)
```

上記のように定義しても良いが、8章で作成した`with-gensyms`マクロを使えば、少し簡単に定義できる。

```lisp
(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms
               collect `(unless ,f (setf ,result nil)))
       ,result)))
```

上記で定義できた`combine-results`マクロを使って、当初の目的通り`check`を修正する。

```lisp
(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms
             collect `(report-result ,f ',f))))
```

この`check`を使うと、`test-+`は3つのテスト式の結果を表示し、その後で`T`を返して全てのテストがパスしたことを示す。

```lisp
CL-USER> (test-+)
pass ... (= (+ 1 2) 3)
pass ... (= (+ 1 2 3) 6)
pass ... (= (+ -1 -3) -4)
T
```

仮にテストで期待値と結果が異なっていれば、下記の通りどこが間違っているかすぐに分かるように表示される。

```lisp
CL-USER> (test-+)
pass ... (= (+ 1 2) 3)
pass ... (= (+ 1 2 3) 6)
FAIL ... (= (+ -1 -3) -5)
NIL
```

## 9.4 より良い結果レポートのために
テスト対象の関数が1つだけなら、先程のテスト方法で問題ない。  
しかし、複数の関数をテストするなら、もっと体系的なテストを実現したい。

例えば、`*`関数のテストを追加しようと思ったら、下記のような関数で実現する事をすぐ思いつく。

```lisp
(defun test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))
```

テスト関数が複数になったのだから、これらテスト関数をまとめる関数が欲しくなる。

```lisp
(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
```

ここまでで定義したテスト用関数、テスト用マクロをまとめて再掲する。

```lisp
(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(test-+
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(defun test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms
             collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms
               collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)
```

今、何らかのテストケースが失敗して問題を追跡する必要があるとする。  
上記のテスト関数をまとめた場合、大量のテスト結果の表示から、`FAIL`の文字を探す必要がある。  
しかも、`FAIL`の文字を探し出せたとしても、そのテストがどの関数に対するテストだったのかは、実際のテスト対象の式を見て判断するしか無い。

ここで、テスト対象の関数が何なのかが分かるように、一種の**ラベル**を表示する。  
ラベルを表示するためにすぐ思いつくのは、`test-+`や`test-*`の関数を修正して、それぞれの関数を表す文字列を定義する方法だ。
この文字列を最終的に各テストケースに表示できれば、とりあえず各テストケースが何に対するテストなのか分かるようになる。

しかし、`test-+`や`test-*`内でラベルを定義しようとした場合、他にも編集する必要がある。  
例えば`check`マクロに対しては、引数に渡された関数名を考慮してマクロの仕組みを変える必要が出てきてしまう。  
さらに、テスト種別ごとに`check`マクロを呼ぶのであれば、`check`マクロの修正までで良いが、使い勝手が低下する。

そこで、ダイナミック変数を用いて対応することにする。下記の通り、トップレベルで使用できるダイナミック変数を定義しておく。

```lisp
(defvar *test-name* nil)
```

次に、`format`の出力に`*test-name*`が含まれるように`report-result`を修正する。

```lisp
(format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
```

上記の修正によって、`*test-name*`にテスト名が定義されているときにテスト実施すると、そのテスト名が表示されるようになる。

```lisp
(defun test-+ ()
  (let ((*test-name* 'test-+))
    (check
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6)
      (= (+ -1 -3) -4))))

(defun test-* ()
  (let ((*test-name* 'test-*))
    (check
      (= (* 2 2) 4)
      (= (* 3 5) 15))))

(test-arithmetic)
; =>
pass ... test-+: (= (+ 1 2) 3)
pass ... test-+: (= (+ 1 2 3) 6)
pass ... test-+: (= (+ -1 -3) -4)
pass ... test-*: (= (* 2 2) 4)
pass ... test-*: (= (* 3 5) 15)
T
```

## 9.5 抽象化の余地
上記の対応によって、重複するコードが発生してしまった。具体的には下記の2点。

- 各テスト関数において、関数名が2回出現する(`defun`で定義するときと、`*test-name*`で名前を束縛する時)
- 各テスト関数において、`defun`の行と後続行の合計3行が共通して記述される

これらの重複は、他人がテストフレームワークを正しく使用する際の妨げとなる。  
これを解決する効果的な方法としては、使い方を覚えさせるコストを生むのではなく、重複の原因となっているコードをマクロで生成してしまえば良い。

```lisp
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* ',name))
       ,@body)))
```

このマクロを使って`test-+`を書き直すと、下記の通りになる。

```lisp
(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))
; =>
(defun test-+ ()
  (let ((*test-name* 'test-+))
    (check
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6)
      (= (+ -1 -3) -4))))
```

## 9.6 テストの階層
前章までで、テスト関数を定義できるようになった。  

さて、テストが大規模化・複雑化していくと、`test-+`や`test-*`に対する`test-arithmetic`のように、複数のテスト関数をグループ化して階層化していく必要が出てくる。  
テストを階層すると、異なるテストグループから同一のテスト関数を呼び出す場合もあり得るし、さらに、あるテストグループでは成功するテスト関数が、他のテストグループでは失敗するという自体も考えられる。  
こういった場合、テストグループのトップレベルから一番下層のテスト関数までの**フルパス**を表示すれば、各テスト結果がどういう状況で実行されたものなのかを識別できるようになる。  
lispらしく、フルパスには**トップレベルから最下層までの関数名をリスト化したもの**を表示すれば良い。(下記参照)

```lisp
pass ... (TEST-ARITHMETRIC TEST-+): (= (+ 1 2) 3)
```

テスト関数を定義するプロセスは既に抽象化したため、テスト関数のコードは変更不要であり、記録方式の詳細だけを変更すれば良い。  
すなわち、`*test-name*`が最新のテスト関数の名前だけではなく、それまでに呼び出されたテスト関数の名前のリストを保持するようにするには、`let`の部分を下記の通りに変更する。

```lisp
; before
(let ((*test-name* ',name)) ...)

; after
(let ((*test-name* (append *test-name* (list ',name)))) ...)

; 修正後のdeftest
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))
```

`append`を使って、`*test-name*`の末尾にに`name`の評価結果をリスト化したものを結合する。  
しかも、`*test-name*`は`let`を使ってシャドーイングしているだけなので、テスト関数の処理が終わって`let`から抜けていく度に、そのテスト関数名がパスのリストから取り除かれていくという処理になる。  

修正後の`deftest`を使って定義した`test-arithmetic`を実行すると、下記の通りに表示される。

```lisp
pass ... (test-arithmetic test-+): (= (+ 1 2) 3)
pass ... (test-arithmetic test-+): (= (+ 1 2 3) 6)
pass ... (test-arithmetic test-+): (= (+ -1 -3) -4)
pass ... (test-arithmetic test-*): (= (* 2 2) 4)
pass ... (test-arithmetic test-*): (= (* 3 5) 15)
T
```

更にテストが階層化された場合にも既に対応できている。例えば、`test-arithmetic`などをまとめ上げたテスト`test-math`を作成したとする。

```lisp
(deftest test-math ()
  (test-arithmetic))
```

これは、下記の通りに表示される。

```lisp
pass ... (test-math test-arithmetic test-+): (= (+ 1 2) 3)
pass ... (test-math test-arithmetic test-+): (= (+ 1 2 3) 6)
pass ... (test-math test-arithmetic test-+): (= (+ -1 -3) -4)
pass ... (test-math test-arithmetic test-*): (= (* 2 2) 4)
pass ... (test-math test-arithmetic test-*): (= (* 3 5) 15)
T
```

## 9.7 まとめ
上記でユニットテストフレームワークを作成した各ステップについてと、全体としてどの様にリファクタリングを進めてきたかを確認すること。  
ドラスティックな改善が手軽に行えているという、lispでのプログラミングに頻出する事実に気付くはず。