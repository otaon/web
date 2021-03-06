---
title: "書籍 Land of Lisp 16章 マクロの魔法"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
toc: true
tags: ["lisp"]
categories: ["Land-of-Lisp"]
authors:
- otaon
---

**マクロプログラミング** によって、プログラマはLispのコンパイラ/インタプリタの動作に変更を加え、Lispを独自の言語へと変化させられる。


## 16.1 簡単なLispマクロ

例えば、とても簡単な関数を考える。

```lisp
(defun add (a b)
  "2値を加算して、副作用として和をREPLに表示する"
  (let ((x (+ a b)))
    (format t "The sum is ~a" x)
    x))
```

この関数のように、たかだか1つの変数`x`を宣言するためだけに、多くの括弧が必要となっている場面は多い。  
`let`関数の括弧は、いわゆる **視覚ノイズ** の一例である。
この括弧を隠蔽しようと思った時、何か関数を書くことで解決することはできない。
何故なら、`let`は **特殊形式** と呼ばれるコマンドの1つであるからである。  
特殊形式は、言語の根幹に組み込まれており、通常のLisp関数ではできない特別なことができる。

マクロを使えばおの余分な括弧を消すことができる。  
ここで、余計な括弧を削除した`let1`関数を作ってみる。

```lisp
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))
```

見て分かる通り、マクロの定義は関数の定義とよく似ている。
ただし、`defun`の代わりに`defmacro`を使う。  
関数と同様に、マクロは名前(ここでは`let1`)と仮引数を持つ。  
`let1`を上の通り定義したら、括弧の少ない`let`として次の通り使うことができる。

```lisp
> (let ((foo (+ 2 3)))
    (* foo foo))
25
> (let1 foo (+ 2 3)
    (* foo foo))
25
```

### マクロの展開

Lispのコンパイラ/インタプリタは、「標準のLispコード」しか解釈できない。
したがって、マクロ`let1`は解釈できない。  

ここで、Lispのコンパイラ/インタプリタがマクロを解釈する前に、 **マクロ展開** と呼ばれるステップが実施される。
マクロ展開器は、コード中のマクロを探して、それらを標準的なLispコードへど変換する。  
したがって、マクロは関数が実行されるのと異なるタイミングで実行されることが分かる。
すなわち、下のとおりである。

- 通常のLisp関数は、その関数を含むプログラムを実行するタイミング(実行時)で解釈される。
- マクロは、プログラムが実行される前、つまり、Lisp環境でプログラムが読み込まれてコンパイルされるタイミング(マクロ展開時)で解釈される。

### マクロはどんなふうに変換されるか

`defmacro`によって新たなマクロを定義するということは、つまり、Lispのマクロ展開器に対して、新たな変換ルールを教えるということである。
マクロはもとのソースコードをLispの式の形で、引数として受け取る。
マクロの仕事は、尾野本のコードを標準のLispコードに変換することである。

上で定義した`let1`を例に、マクロがどのように変換されるのかを説明する。

*`let1`再掲*

```lisp
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))
```

最初の行は、「`let1`で始まる行があったらそれを標準的なLispコードに変換するためのルールを定義する」と、マクロ変換器に伝えている。
`defmacro`は、また、マクロに渡される引数についても定義している。  
マクロの引数には、マクロが使われている場所に現れるもとのソースコードが渡される。
`let1`マクロの場合は、次の3つの引数を受け取ることになる。


```lisp
> (let1 foo (+ 2 3)
    (* foo foo))
25
```

<dl>
<dt>var</dt>
<dd>
最初の引数は、ローカル変数として定義される名前である。
マクロの中では、引数`var`の値がその名前になっている。
上の呼び出しの例では、名前は`foo`である。
</dd>
<dt>val</dt>
<dd>
2番目の式は、ローカル変数の値を決めるコードである。
上の呼び出しの例では、`(+ 2 3)`となっている。
</dd>
<dt>body</dt>
<dd>
3番目の式は、`let1`の中で実行されるコードの本体である。
このコードの中では、`let1`が作る新しい変数(この例では`foo`)を使用できる。
マクロでは、このコードが引数`body`の値として使える。
</dd>
</dl>

`let`コマンドは本体の中に複数の式を書いておけるから、`let1`も同様に複数の式が書けるようにする。  
`&body`はそれを実現するための特別なシンボルである。
`&body`が書かれていると、マクロ展開時に「マクロの使われている場所に出てくる残りの式の全てを、リストにして次の引数に渡せ」という意味になる。
したがって、`let1`の`body`引数に渡ってくる値は、ネストしたリスト`((* foo foo))`になっているというわけである。

さて、`let1`マクロの引数については分かった。
次に、マクロがその値を使ってどのように`let1`を`let`に変換するのかを見ていく。  
Lispでソースコードを変換する最も簡単な方法は、バッククォート構文を使用することである。
バッククォートを頭につけた準クォートでは、基本はデータモードで、カンマを付けた部分だけコードモードに戻る。

```lisp
`(let ((,var ,val))
```

`let1`マクロは、バッククォートで作られる上のリストを返す。
リストの先頭の要素はシンボル`let`である。続いて、変数の名前と値が置かれる。  
これにより、本来の`let`コマンドの構文どおりに、ネストされたリストに収まっていることが分かる。  
最後に、`let1`に渡された`body`のコードが、`let`コマンドの対応する位置に挿入されている。  
ここで、`body`引数の値を挿入するために、 **スプライシングカンマ** (`,@`)を使用している。
スプライシングカンマを使用することで、カンマの対象範囲となるデータの括弧を取り外す(=スプライスする)。

なぜスプライシングが必要なのかは、`let1`が次のように使われた場合を考えてみると分かりやすい。

```lisp
(let1 foo (+ 2 3)
  (princ "Lisp is awesome!")
  (* foo foo))
List is awesome!
25
```

この例では、`let1`の本体中に複数の式が使われている。  
よくよく考えれば分かるが、`let`コマンドは、暗黙の`progn`コマンドを含んでいて、本体内に複数のLispコマンドを記載できる。
`let1`マクロも、`body`引数の前に特別な`&body`シンボルを置いておいたおかげで同じように複数の式を扱える。  
上の例では、`body`の値は`((princ "Lisp is awesome!") (* foo foo))`となっているため、スプライスすると、`let`に複数の式を渡したことと同等の結果となるわけである。

### 簡単なマクロを使ってみる

`let1`マクロが書けたので、それを使って本章の最初に書いた`add`関数を書き直してみる。

```lisp
(defun add (a b)
  (let1 x (+ a b)
    (format t "The sum is ~a" x)
    x))
```

また、`macroexpand`コマンドを使えば、マクロがどのようなコードを作るのか確かめられる。
マクロの呼び出しコードを、次のように`macroexpand`に渡せば良い。

```lisp
> (macroexpand '(let1 foo (+ 2 3)
                  (* foo foo)))
(LET ((FOO (+ 2 3))
  (* FOO FOO))) ;
T
```

最後の`T`は、`macroexpand`が問題なくマクロを展開できたことを表している。  
**NOTE:** マクロが複雑になるにつれ、`macroexpand`はとても有用なコマンドになる。


## 16.2 もっと複雑なマクロ

ここで、リストの長さを求める`my-length`コマンドを考える。
末尾呼び出し最適化が可能な形で実装したものが次の例である。

```lisp
(defun my-length (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cdr lst) (1+ acc))
                 acc)))
    (f lst 0)))
```

この関数には、特に悪い意味で気になる特徴が2点ある。

- リストをなめていく関数に共通する処理として、次の2つがある
  - リストが空かどうかを調べることと
  - `cdr`でリストの残りを調べること
- わざわざローカル関数を定義していること

これらの問題を緩和するため、ここからはマクロで対処してみる。  
なお、これからの説明は、素朴なマクロ(バグあり)の作成から始めて、段々とブラッシュアップしていく流れになっている。

### リストを分割するマクロ

ここでは、`split`マクロを作成する。
`my-length`のような、リストを頭から順に見ていく関数を簡潔に書けるようにする。

リストをなめていく関数は、常に、まずリストが空かどうかをチェックし、空でなければその頭と残りを`car`と`cdr`で取り出して処理をする。
`split`マクロは、その共通部分をまとめてやってくれるものである。

まずは、`split`マクロの使い方について次に示す。

```lisp
> (split '(2 3)
    (format t "This can be split into ~a and ~a." head tail)
    (format t "This can not be split"))
This can be split into 2 and (3).
> (split '()
    (format t "This can be split into ~a and ~a." head tail)
    (format t "This cannot be split."))
This can not be split.
```

`split`マクロの最初の引数は、頭と残りに分解したいリストである。  
もし分解可能なら、2番目の引数に渡された式が実行される。
このとき、`split`マクロは自動的に2つのローカル変数、`head`と`tail`を作り、リストの頭と残りをそれに格納する。
これにより、関数の中で`car`と`cdr`を呼ぶ手間を省ける。
リストが空だったら、3番目の引数に渡された式が実行される。

次に、`split`マクロのコードを見てみる。
このコードにはバグがある(後述)。

```lisp
;; バグあり
(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
             (tail (cdr ,val)))
         ,yes)
       ,no))
```

`split`マクロは3つの引数を取る。
すなわち、このマクロを使うときには常に3つの引数を渡す必要がある。  
また、リストが空だった場合、`no`の位置からは変数`head`と`tail`は見えないことに注意すること。

`split`マクロを使えば`my-length`関数は少し綺麗になる。
`tail`変数を使うことで、コードが簡潔になっているのが分かる。
このマクロのように、自動的に変数を作り出すマクロは、 **アナフォリックマクロ** と呼ばれる。

**NOTE:** Anaphoric macro. Anaphoric(前方参照)とは、既に出ている話題に言及する際に代名詞などを使うことである。  
ここの例では、分割したリストの頭と残りを、自動的に作られる変数で言及できる。

```lisp
(defun my-length (lst)
  (labels ((f lst acc)
             ;; lst: リスト
             ;; acc: アキュムレータ
             (split lst
               (f tail (1+ acc))
               acc)))
    (f lst 0)))
```


### マクロ中で式が繰り返し実行されるのを防ぐ

マクロでよくあるバグとしては、コードを意図せずに複数回実行してしまうことである。
実際に、上の`split`マクロにもこのバグが存在してしまっている。
例えば、次のコードはそのバグを引き起こす。

```lisp
> (split (progn (princ "Lisp rocks!")
                '(2 3))
    (format t "This can be split into ~a and ~a." head tail)
    (format t "This cannot be split."))
Lisp rocks!Lisp rocks!Lisp rocks!This can be split into 2 and (3).
```

`split`を使ったら、"Lisp rocks!"というメッセージが3回も表示されてしまった。

これは、マクロに渡される引数が生のソースコードであることが原因である。
`split`マクロの展開時に`val`を3回参照するので、`princ`が3回実行されてしまったのである。

実際にマクロがどのように展開されるかは、`macroexpand`を使えば確かめることができる。

```lisp
> (macroexpand (split (progn (princ "Lisp rocks!")
                             '(2 3))
                 (format t "This can be split into ~a and ~a." head tail)
                 (format t "This cannot be split.")))
(IF (PROGN (PRINC "Lisp rocks!") '(2 3))
  (LET ((HEAD (CAR (PROGN (PRINC "Lisp rocks!") '(2 3))))
        (TAIL (CDR (PROGN (PRINC "Lisp rocks!") '(2 3)))))
    (FORMAT T "This can be split into ~a and ~a." HEAD TAIL)
    (FORMAT T "This cannot be split.")) ;
T
```

この問題の解決方法を考えてみると、次のようにローカル変数を使ってみれば良いことに気付く。
(この新しい`split`マクロでは、間に作った`let1`マクロを使ってみている。マクロの中で別のマクロを使うことに問題はない。)  
この定義を使用すれば、valの式は1度しか評価されないから、上のように`princ`が呼ばれることはない。  
**NOTE:** しかしながら、これにはまだバグがある。

```lisp
;; 注意! これにもまだバグがある
(defmacro split (val yes no)
  `(let1 x ,val
     (if x
         (let ((head (car x))
               (tail (cdr x)))
           ,yes)
         ,no)))
```

### 変更捕捉を避ける

上の`split`のバグを見るには、次のコードを実行すれば分かる。

```lisp
> (let1 x 1000
    (split '(2 3)
      (+ x head)
      nil))
*** - +: (2 3) is not a number

> (macroexpand (split '(2 3) (+ x head) nil))
(LET ((X '(2 3)))
  (IF X
      (LET ((HEAD (CAR X))
            (TAIL (CDR X)))
        (+ X HEAD))
        NIL)) ;
T
```

このように、`split`のには`x`の展開が含まれるが、これがマクロに渡したコードと衝突を起こしてしまっている。
この例では、`split`マクロが変数`x`を意図せず捕捉してしまい、見たい値をシャドウしてしまった。
これによって、`split`の外で宣言した`x`には、最初に`1000`を代入したにもかかわらず、`split`の中で`x`をシャドウして、
さらにリスト`'(2 3)`を代入しようとしたために型違いエラーが発生した。

このような変数名の衝突を回避するための素朴な解決策としては、衝突しなさそうな`aeicfnuhaceknf`のようなおかしな名前の変数を使うというものがある。  
これを実現するための仕組みとして、`gensym`関数がCommon Lispには予め備わっている。

```lisp
> (gensym)
#:G8695
```

`gensym`関数が作る名前は、コード中で唯一だと保証される。
また、`gensym`が返した値と同じ名前をコード中に上書き定義できないようにされており、それが分かるようにプレフィックス(`#:`)がつけられている。
したがって、`gensym`を実行してから、その返り値と全く同じ変数名を宣言しても、別々の変数として扱われる。

ここで、`gensym`を使って`split`マクロを変数補足に対して安全になるように修正してみる。

```lisp
;; 安全なバージョン
(defmacro split (val yes no)
  (let1 g (gensym)  ; マクロ展開時にgにシンボル名を代入
    ;; マクロ展開時には既にgはシンボル名に評価されている
    `(let1 ,g ,val
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))

[9]> (macroexpand '(split '(2 3) (+ x head) nil))
(LET ((#:G2985 '(2 3)))
  (IF #:G2985
    (LET ((HEAD (CAR #:G2985))
          (TAIL (CDR #:G2985)))
      (+ X HEAD))
    NIL)) ;
T
[10]> (macroexpand '(split '(2 3) (+ x head) nil))
(LET ((#:G2986 '(2 3)))
  (IF #:G2986
    (LET ((HEAD (CAR #:G2986))
          (TAIL (CDR #:G2986)))
      (+ X HEAD))
    NIL)) ;
T
[11]> (macroexpand '(split '(2 3) (+ x head) nil))
(LET ((#:G2987 '(2 3)))
  (IF #:G2987
    (LET ((HEAD (CAR #:G2987))
          (TAIL (CDR #:G2987)))
      (+ X HEAD))
    NIL)) ;
T
```

上のコードの`(let1 g (gensym))`部分にバッククォートが無い(=準クォートではない)ことに注意すること。
すなわち、この部分は、 **マクロが作り出したコードの実行時** ではなく、 **マクロ自身の展開時** に評価される。
また、マクロが展開されるたびに、`gensym`が異なる変数名を生成していることも分かる。


また、当然だが、変数名が衝突しないことと変数捕捉しないことは同じではない。
このバージョンでも`head`と`tail`という変数を使用しているため、これらの変数を別の意味で使っているコードと混ぜて使用したら、やはり問題は起こる。
しかし、`head`と`tail`に関しては、むしろわざと変数を捕捉しているのだ。
アナフォリックマクロでは、マクロ本体内でこれらの変数を使えるようにわざわざ捕捉しているわけであるから、予め決まっている変数を捕捉するのはバグではなく **仕様** である。

### 再帰呼び出しマクロ

ここで、もう一度、`my-length`を修正する。
前に作った`my-length`を再掲する。

```lisp
(defun my-length (lst)
  (labels ((f lst acc)
             ;; lst: リスト
             ;; acc: アキュムレータ
             (split lst
               (f tail (1+ acc))
               acc)))
    (f lst 0)))
```

先述の通り、このコードにもまだ繰り返し出てくるパターンがある。
すなわち、ローカル関数`f`を定義しているところである。

ここで、再帰部分を隠す`recurse`マクロを次に示す。
まず、`recurse`マクロの使用例を示す。

```lisp
> (recurse (n 9)
    (fresh-line)
    (if (zerop n)
      (princ "lift-off!")
      (progn (princ n)
             (self (1- n)))))
9
8
7
6
5
4
3
2
1
lift-off!
```

`recurse`マクロの最初のパラメータは、変数とその初期値のリストである。
この例では、変数`n`を宣言し、その初期値を`9`に設定している。  
残りの行は再帰関数の本体を構成する。

再帰関数の本体では、まず、改行している。
次に、`n`がゼロになったか否かを調べ、ゼロになっていれば"lift-off!"を表示する。
そうでなければ現在の`n`の値を出力し、自分自身を再帰呼び出しする。
`split`マクロと同様、このマクロもアナフォリックである。
すなわち、`recurse`マクロでは、変数`self`で自分自身の関数を参照できる。
再帰の条件が整ったら、`self`を呼び出せば良い。
この例では`(1- n)`を引数として渡して、カウントダウンを実現している。

では、`recurse`マクロを実装してみる。
まず、変数とその初期値の対を切り出すのに便利なように、補助関数`pairs`関数を定義する。
`pairs`関数は末尾呼び出し最適化可能な、リストを舐める関数である。
この関数を定義するためにローカル関数`f`を定義するはめに陥っているが、後述する方法でこういった関数定義をしなくて良くなる。
この関数`f`の中では、`split`マクロを使ってリストを分解しているが、今回はリストから2つずつ要素を取り出したいため、`tail`が空でないかを改めて調べている。
これにより、リストが空か、要素が1つしか残っていない(`(if tail)`が偽)場合は、蓄積した値を返す。
そうでなければ最初の2つの要素をペアにしてアキュムレータ`acc`に追加し、再帰する。

```lisp
> (defun pairs (lst)
    ;; lst: 2要素ずつコンスセルを作る対象となるリスト
    ;; acc: 作ったコンスセルを格納するアキュムレータ
    (labels ((f (lst acc)
               (split lst
                 (if tail
                   ;; lstが空でなく、かつ、残り部分も空でない場合、
                   ;; => ((head . tail) これまでに作ったコンスセル達)
                   (f (cdr tail) (cons (cons head (car tail)) acc))
                   ;; lstが空ではないが、残り部分が空の場合、
                   ;; これまでに作ったコンスセル達は逆順なので、順序を正してから返す
                   (reverse acc))
                 ;; lstが空の場合、
                 ;; これまでに作ったコンスセル達は逆順なので、順序を正してから返す
                 (reverse acc))))
      (f lst nil)))
PAIRS
> (pairs '(a b c d e f))
((A . B) ( C . D) (E . F))
```

次に、いよいよ`recurse`マクロを定義する。
変数`p`には、`pair`関数を使って最初のリストを変数と初期値のコンスセルのリストにしたものを代入する。
次に、`self`ローカル関数を定義する。
`self`の引数は、最初のリストの基数番目の要素(つまり`recurse`に渡した`vars`のリスト中の変数)を並べたものである。
`self`は、マクロ展開された式の中から参照できる(つまりアナフォリックに参照できる)必要があるため、`(gensym)`を使わず、直接名前を書いている。
そしてマクロの最後で、初期値を引数として`self`を呼び出す。

```lisp
(defmacro recurse (vars &body body)
  ;; p: varsで得られた変数とその初期値のコンスセルのリスト
  (let1 p (pairs vars)
    ;; ローカル関数self
    ;; 引数: varsで得られた変数
    ;; 関数本体 bodyで得られたリスト(複数可)
    `(labels ((self ,(mapcar #'car p)
                ,@body))
       ;; ローカル関数selfに初期値を適用
       (self ,@(mapcar #'cdr p)))))
```

最後に、`recurse`マクロを使って`my-length`関数を更に簡潔にする。
`my-length`に必要な補助関数やマクロ定義も全て示す。

```lisp
(defmacro let1 (var val &body body)
  ;; 変数を1つだけ代入して式を実行する
  ;; var: 代入先の変数
  ;; val: 代入する値
  ;; body: 実行する式(複数可)
  `(let ((,var ,val))
     ,@body))
```

```lisp
(defmacro split (val yes no)
  ;; valに対して頭と残りへの分解を試みる
  ;; val: 分解対象の式
  ;; yes: 分解成功時に実行する式
  ;; no: 分解失敗時に実行する式
  (let1 g (gensym)  ; マクロ展開時にgにシンボル名を代入
    ;; マクロ展開時には既にgはシンボル名に評価されている
    `(let1 ,g ,val
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))
```

```lisp
(defun pairs (lst)
  "2要素ずつコンスセルを作る
   lst: 2要素ずつコンスセルを作る対象となるリスト"
  ;; lst: 2要素ずつコンスセルを作る対象となるリスト
  ;; acc: 作ったコンスセルを格納するアキュムレータ
  (labels ((f (lst acc)
             (split lst
               (if tail
                 ;; lstが空でなく、かつ、残り部分も空でない場合、
                 ;; => ((head . tail) これまでに作ったコンスセル達)
                 (f (cdr tail) (cons (cons head (car tail)) acc))
                 ;; lstが空ではないが、残り部分が空の場合、
                 ;; これまでに作ったコンスセル達は逆順なので、順序を正してから返す
                 (reverse acc))
               ;; lstが空の場合、
               ;; これまでに作ったコンスセル達は逆順なので、順序を正してから返す
               (reverse acc))))
    (f lst nil)))
```

```lisp
(defmacro recurse (vars &body body)
  ;; 再帰処理を定義する
  ;; vars: 変数とその初期値(連続してOK)
  ;; body: 再帰する処理(再帰呼び出しする関数は変数self)
  ;; p: varsで得られた変数とその初期値のコンスセルのリスト
  (let1 p (pairs vars)
    ;; ローカル関数self
    ;; 引数: varsで得られた変数
    ;; 関数本体 bodyで得られたリスト(複数可)
    `(labels ((self ,(mapcar #'car p)
                ,@body))
       ;; ローカル関数selfに初期値を適用
       (self ,@(mapcar #'cdr p)))))
```

```lisp
(defun my-length (lst)
  "リストの長さを返す
   lst: 対象のリスト
   ret: リストの長さ"
  (recurse (lst lst acc 0)
           ;; lst: 走査対象のリスト 初期値lst
           ;; acc: リストの長さ保持用 初期値0
           (split lst
             ;; リストに残りがあれば残りに対して再帰呼び出しする
             (self tail (1+ acc))
             ;; リストが空になったらリストの長さを返す
             acc)))
```

## 16.3 マクロの危険と代替案

マクロはコードを生成するコードを書く手段である。
これにより、Lispはメタプログラミングや新しい言語のアイデアやプロトタイプを作るのに適した言語であるといえる。
しかし、マクロはある意味、小手先のテクニックである。
自作の言語を、標準のLispであるかのようにLispコンパイラ/インタプリタに読み込ませるためのトリックである。
道具立てとしては非常に強力なものだが、エレガントではない。

マクロの一番の欠点は、コードが理解しにくくなることである。
つまり、他のプログラマにとって初見となるプログラミング方言を作っているわけである。
したがって、しばらく後にマクロを駆使したプログラムを読み解くのは非常に骨が折れる作業になる。

初心者Lisperがマクロを書きたくなる場合の多くは、もっとエレガントな解法があるものである。
例えば、`my-length`をマクロを使わずに簡潔に表現する方法は、実は存在する。
次のように、高階関数`reduce`を使えば良いだけである。

```lisp
(defun my-length (lst)
  (reduce (lambda (x i)
            (1+ x))
          lst
          :initial-value 0))
```

高階関数`reduce`は、リストの各要素に対して関数適用するための関数である。
`reduce`関数の第1引数には、縮約を行う関数を渡してやる。
ここでは`lambda`によって無名関数を作っている。
この`lambda`関数の第1引数`x`は、最初に、`reduce`関数のキーワード引数の`:initial-value`の値`0`を束縛する。
そして`lambda`関数本体の処理を行い、その結果をまた次に呼んだ`lambda`関数の`x`に束縛する。
これにより、リストの各要素に対して`lambda`が呼ばれただけ`x`がインクリメントされつつ`lambda`に渡される。
(すなわち`x`はアキュムレータである。)

また、縮約関数には、その時に見ているリストの各要素の値も引数に渡されている。
それが引数`i`である。
ただし、`my-length`関数では`i`を使う必要はない。

このように、高階関数が使える場合はそちらを使った方がくだらないバグに悩まされることもなく、簡単である。
しかし、高階関数が使えない場合もあるから、その場合にマクロを使えるようになっておくことが望ましい。

