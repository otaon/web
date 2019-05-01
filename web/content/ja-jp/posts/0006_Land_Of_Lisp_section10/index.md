---
title: "書籍 Land of Lisp 10章 loopマクロ"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
toc: true
tags: ["lisp"]
categories: ["Notes"]
authors:
- otaon
---

# Loopマクロ周期表

書籍では、`loop`マクロで使えるトークンを周期表のようにまとめていた。  
それだと少々見辛いため、素直な表形式で下記にまとめなおす。

## 参考文献
[独学 Common Lisp - 第6章「繰り返し」](http://lisp.satoshiweb.net/2018/01/iteration.html)

## 基本的なトークン

|トークン|説明|
|----|----|
|`loop`|単純なループ|
|`do` `doing`|繰り返しの中で任意の式を実行する|
|`repeat`|指定した回数ループする|
|`return`|任意の式の実行結果を返してループを抜ける|
|`initially`|ループし始める前に任意の式を実行する|
|`finally`|ループが終わった後に任意の式を実行する|
|         |**ループの途中脱出時には実行されない**|
|`with`|ローカル変数を作成する|
|`into`|結果を格納するローカル変数を作成する|

### `loop`

```lisp
(loop (princ "type something")
      (force-output)
      (read))

; type somethingr
; type somethingf
; type somethingf
; ...
```

### `do`

```lisp
(loop for i below 5 
      do (print i))

; 0 
; 1 
; 2 
; 3 
; 4 
; NIL
; CL-USER> 
```

### `repeat`

```lisp
(loop repeat 5
      do (print "Print five times"))

; "Print five times" 
; "Print five times" 
; "Print five times" 
; "Print five times" 
; "Print five times" 
; NIL
```

### `return`

```lisp
(loop for i below 10
      when (= i 5)
      return 'leave-early
      do (print i))

; 0 
; 1 
; 2 
; 3 
; 4 
; LEAVE-EARLY
```

### `initially`

```lisp
(loop initially (print 'loop-begin)
      for x below 3
	  do (print x))

; LOOP-BEGIN 
; 0 
; 1 
; 2 
; NIL
```

### `finally`

```lisp
(loop for x below 3
      do (printx)
      finally (print 'loop-end))

; 0 
; 1 
; 2 
; LOOP-END 
; NIL
```

### `with`

```lisp
(loop with x = (+ 1 2)
      repeat 5 do (print x))

; 3 
; 3 
; 3 
; 3 
; 3 
; NIL
```

### `into`

```lisp
(loop for i in '(1 1 2 3 5)
      minimize i into lowest
	  maximize i into biggest
	  finally (return (cons lowest biggest)))

; (1 . 5)
```

## ループに対する名前付けとループの脱出

|トークン|説明|
|----|----|
|`named`|ループに任意の名前をつける|
|`return-from`|ループ名を指定してループを抜ける|
|`while`|式が真ならループを続け、nilならループを抜ける|
|`until`|式がnilならループを続け、真ならループを抜ける|

### `named`

```lisp
(loop named outer
      for i below 10
	  do (progn (print "outer")
	            (loop named inner
				      for x below i
					  do (print "**inner")
					  when (= x 2)
					  do (return-from outer 'kicked-out-all-the-way))))

; "outer" 
; "outer" 
; "**inner" 
; "outer" 
; "**inner" 
; "**inner" 
; "outer" 
; "**inner" 
; "**inner" 
; "**inner" 
; KICKED-OUT-ALL-THE-WAY
```

### `return-from`

省略。  
`named`の例を参照のこと。

### `while`

```lisp
(loop for i in '(0 2 4 555 6)
      while (evenp i)
	  do (print i))
; 0 
; 2 
; 4 
; NIL
```

### `until`

```lisp
(loop for i from 0
	  do (print i)
	  until (> i 3))

; 0 
; 1 
; 2 
; 3 
; 4 
; NIL

(loop for i from 0
	  until (> i 3)
	  do (print i))

; 0 
; 1 
; 2 
; 3 
; NIL
```

## ハッシュテーブル関連

|トークン|説明|
|----|----|
|`using`|`hash-key`によりキーを、`hash-value`によりバリューを保持する|
|`being`|ハッシュテーブルから、`being the hash-key of`でキーを、`being the hash-value of`でバリューを取得する|
|`the` `each`|ハッシュテーブルに対して`being the`か`being each`としてアクセスする|
|`hash-keys` `hash-key`|ハッシュキーを取得する際に指定するトークン|
|`hash-values` `hash-value`|ハッシュ値を取得する際に指定するトークン|

下記の例では全て`salary`ハッシュテーブルを使用する。

```lisp
(defparameter salary (make-hash-table)
(setf (gethash 'bob salary) 80)
(setf (gethash 'john salary) 90)
```

### `using`

```lisp
(loop for person being each hash-key of salary
      using (hash-value amt)
	  do (print (cons person amt))))

; (JOHN . 90) 
; (BOB . 80) 
; NIL
```

### `being`

```lisp
(loop for person being each hash-key of salary
	  do (print person))

; JOHN 
; BOB 
; NIL
```

### `the` `each`

```lisp
(loop for person being each hash-key of salary
	  do (print person))

; JOHN 
; BOB 
; NIL
```

```lisp
(loop for person being the hash-keys of salary
	  do (print person))

; JOHN 
; BOB 
; NIL
```

### `hash-key` `hash-keys`

省略。  
`the` `each`の例を参照のこと。

### `hash-value` `hash-values`

```lisp
(loop for amt being each hash-value of salary
	  do (print amt))

; 90 
; 80 
; NIL
```

```lisp
(loop for amt being the hash-values of salary
	  do (print amt))

; 90 
; 80 
; NIL
```


## `for`ループ関連

|トークン|説明|
|----|----|
|`for` `as`|ループ変数を初期化する|
|`in`|リストを`car`したものをループ変数に与える|
|`on`|リストをループ変数に与えた後に`cdr`する|
|`by`|数:指定した数値だけループ変数を増減させる（デフォルト:`1`or`-1`）|
|    |リスト:指定した関数でリストから値を取り出す（デフォルト:`#'cdr`）|
|`then`|`for x = y then z`とすると、`x`に初期値`y`を設定し、式`z`を繰り返し実行する|
|`from`|`for x from y to z`として、ループ変数`x`を数値`y`から増減させる|
|`upfrom`|`for x upfrom y to z`として、ループ変数`x`を数値`y`から増加させる|
|`downfrom`|`for x downfrom y to z`として、ループ変数`x`を数値`y`から減少させる|
|`to`|`for x from y to z`として、ループ変数`x`を数値`z`まで増減させる|
|`upto`|`for x from y to z`として、ループ変数`x`を数値`z`まで増加させる|
|`downto`|`for x from y to z`として、ループ変数`x`を数値`z`まで減少させる|
|`across`|`for x across y`として、シーケンス（文字列を含む）`y`を先頭から`x`に与える|

### `for` `as`

```lisp
(loop for i from 0
	  do (print i)
	  when (= i 5)
	  return 'zuchini)

; 0 
; 1 
; 2 
; 3 
; 4 
; 5 
; ZUCHINI
```

```lisp
(loop as i from 5
	  to 10
	  collect x)

; (0 1 2 3 4 5 6 7 8 9 10)
```

### `in` `on`

```lisp
(loop for i in '(100 20 3)
	  sum i)

; 123
```

```lisp
(loop for x in '(1 3 5)
	  do (print x))

; 1 
; 3 
; 5 
; NIL

(loop for x on '(1 3 5)
	  do (print x))

; (1 3 5) 
; (3 5) 
; (5) 
; NIL
```

### `by`

```lisp
(loop for i from 6 to 8 by 2
	  sum i)

; 14
```

### `then`

```lisp
(loop repeat 5
      for x = 10.0
	  then (/ x 2)
	  collect x)

; (10.0 5.0 2.5 1.25 0.625)
```

### `from`

```lisp
(loop for i from 6 to 8
	  sum i)
; 21
```

### `upfrom`

```lisp
(loop for i upfrom 6 to 8
	  sum i)
; 21
```

### `downfrom`

```lisp
(loop for i downfrom 10 to 7
	  do (print i))

; 10 
; 9 
; 8 
; 7 
; NIL
```

### `to`

省略。  
`from`の例を参照のこと。


### `upto`

```lisp
(loop for i from 6 upto 8
	  sum i)

; 21
```

### `downto`

```lisp
(loop for i from 10 downto 7
	  do (print i))

; 10 
; 9 
; 8 
; 7 
; NIL
```

### `across`

```lisp
(loop for i across #(100 20 3)	; 配列
	  sum i)

; 123
```

## 要素が満たすべき条件を調べる

|トークン|説明|
|----|----|
|`always`|式が真ならばループを続け、`nil`ならばループを抜ける|
|`never`|式が`nil`ならばループを続け、真ならばループを抜ける|
|`thereis`|式が真ならばループを抜ける|
|         |返り値は真偽値ではなく、判定に用いた値自体|

### `always`

```lisp
(loop for i in '(0 2 4 6)
	  always (evenp i))

; T
```

### `never`

```lisp
(loop for i in '(0 2 4 6)
	  never (oddp i))

; T
```

### `thereis`

```lisp
(loop for i in '(0 2 555 6)
	  thereis (oddp i))

; T
```

## 条件分岐

|トークン|説明|
|----|----|
|`if` `when`|式が真ならば、その次の節を実行する|
|`unless`|式が`nil`ならば、その次の節を実行する|
|`and`|条件を満たした時に実行する節を複数記述する場合に、節を連結する|
|`else`|`cond`マクロのように条件節を連結する|
|`end`|複数記述した節の終わりを示す|


### `if`

```lisp
(loop for i below 5
	  if (oddp i)
	  do (print i))

; 1 
; 3 
; NIL
```

### `when`

```lisp
(loop for i below 4
	  when (oddp i)
	  do (print i)
	  do (print "yup"))

; "yup" 
; 1 
; "yup" 
; "yup" 
; 3 
; "yup" 
; NIL
```

### `unless`

```lisp
(loop for i below 4
	  unless (oddp i)
	  do (print i))

; 0 
; 2 
; NIL
```

### `and`

```lisp
(loop for i below 5
	  when (= x 3)
	  do (print "do this")
	  and do (print "also do this")
	  do (print "always do this"))

; "always do this" 
; "always do this" 
; "always do this" 
; "do this" 
; "also do this" 
; "always do this" 
; "always do this" 
; NIL
```

### `else`

```lisp
(loop for i below 5
	  if (oddp i)
	  do (print i)
	  else do (print "w00t"))

; "w00t" 
; 1 
; "w00t" 
; 3 
; "w00t" 
; NIL
```

### `end`

```lisp
(loop for i below 4
      when (oddp i)
	  do (print i)
	  end
	  do (print "yup"))

; "yup" 
; 1 
; "yup" 
; "yup" 
; 3 
; "yup" 
; NIL
```


## 結果の集積

|トークン|説明|
|----|----|
|`count` `counting`|式がnil以外の場合に累積的に数を数える|
|`sum` `summing`|数値を加算していく|
|`minimize` `minimizing`|後ろに続く数が前の数値よりも小さい場合、その数を残す|
|`maximize` `maximizing`|後ろに続く数が前の数値よりも大きい場合、その数を残す|
|`append` `appending`|次に続くリストを結果となるリストに連結する（**非破壊的**）|
|`nconc` `nconcing`|次に続くリストを結果となるリストに連結する（**破壊的**）|

### `count` `counting`

```lisp
(loop for i in '(1 1 1 1)
      count i)

; 4
```

### `sum` `summing`

```lisp
(loop for i below 5
      sum i)

; 10
```

### `minimize` `minimizing`

```lisp
(loop for i in '(3 2 1 2 3)
      minimize i)

; 1
```

### `maximize` `maximizing`

```lisp
(loop for i in '(3 2 1 2 3)
      maximize i)

; 3
```


### `append` `appending`

```lisp
(loop for i below 5
      append (list 'Z i))

; (Z 0 Z 1 Z 2 Z 3 Z 4)
```

### `nconc` `nconcing`

```lisp
(loop for i below 5
      nconc (list 'Z i))

; (Z 0 Z 1 Z 2 Z 3 Z 4)
```

