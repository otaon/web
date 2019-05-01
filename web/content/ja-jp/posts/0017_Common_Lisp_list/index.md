---
title: "Common Lisp リスト操作方法を学ぶ"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
toc: true
tags: ["lisp", "basic"]
categories: ["Notes"]
authors:
- otaon
---

# 目的
この記事では、リスト操作において基本的な関数の使い方について説明する。

# 関数の説明
## `member`関数
リスト中に、或る要素が含まれているか否かを調べる。

```cl
(if (member 1 '(3 4 1 5))
	'one-is-in-the-list
	'one-is-not-in-the-list)
=> ONE-IS-IN-THE-LIST

; memberの実際の返り値
(member 1 '(3 4 1 5))
=> (1 5)
```
上記に示したように、memberの挙動は下記のとおりである。

1. リストをcarして、
1. その要素が目的oの値だったら、
1. そのリストを返す。
1. 違ったら、cdrしたリストを対象にする。
1. 上記1-4を繰り返す。
1. 最後までみつからなかったらNILを返す。

例:

```cl
(member 3 '(1 5 3 4 (1 2) 4))
; => (3 4 (1 2) 4)
(member '(1 2) '(3 (1 2) 4))
; => NIL  ; シンボルでないと駄目らしい
```

## `find-if`関数

第2引数を`car`した値を対象に、第1引数に指定した関数を使って評価し、
真を返した値を返す。
もしも全ての値が偽だった場合は`nil`を返す。

```cl
(find-if #'func '(hoge foo bar))
; => (foo bar)
```
`#'oddp`により、リストから奇数が見つかるまで順に操作して、
真を返す値を見つけたら、その値を返す。
さもなければ、空リストを返す。

例:

```cl
(if (find-if #'null '(2 4 5 6))
	'there-is-an-odd-number
	'there-is-no-odd-number)
; => 'THERE-IS-AN-ODD-NUMBER
```

__注意:nilを探す場合には使えない。__

```cl
(find-if #'null '(2 4 nil 6))
; => NIL
```
## `assoc`関数
連想リスト(association list)から、指定したキーのリストを返す。

```cl
(assoc 'key2 '((key1 (list1)) (key2 (list2)) (key3 (list3))) )
; => (KEY2 (LIST2))
```

例:

```cl
(assoc 'a '((a (1 2)) (b (3 4)) (c (5 6)) (d (7 8))) )
; => (A (1 2))
```

## `mapcar`関数
リストを第n引数(2 <= n <= N)に持ち、これの要素を1つずつ取り出して、第1引数の関数に対して適用する。

```cl
(mapcar #'func '(list-for-arg-1) ... '(list-for-arg-n))
; => `((func '(list-for-arg-1の第1要素) ... '(list-for-arg-nの第1要素))
;      (func '(list-for-arg-1の第2要素) ... '(list-for-arg-nの第2要素))
;      ...
;      (func '(list-for-arg-1の第m要素) ...'(list-for-arg-nの第m要素)))
```

定義:

```cl
(defuc mapcar (func mlist)
  (cond ((null mlist)
          nil)
        (t
          (cons (funcall func (car mlist)) (mapcar func (cdr mlist))) ))

; 上記のmapcarを実行した結果
(mapcar #'+ '(1 2 3))
; =>
; mapcarを定義にしたがって展開する
(cons (funcall #'- (car '(1 2 3))) (mapcar #'- (cdr '(1 2 3))))
; =>
(cons (funcall #'- 1) (mapcar #'- '(2 3)))
; =>
(cons -1 (mapcar #'- '(2 3)) )
; =>
(cons -1 (cons (funcall #'- 2) (mapcar #'- '(3))) )
; =>
(cons -1 (cons -2 (mapcar #'- '(3)) ) )
; =>
(cons -1 (cons -2 (cons (funcall #'- 3) (mapcar #'- nil)) ) )
; =>
(cons -1 (cons -2 (cons -3 (mapcar #'- nil)) ) )
; =>
(cons -1 (cons -2 (cons -3 nil) ) )
; =>
(-1 . (-2 . (-3 . () ) ) )
; =>
(-1 -2 -3)
```

## `append`関数
引数に会う複数のリストを、1つのリストに統合する。
(append = 〜を付け加える。)

```cl
(append '(list1) '(list2) '(list3))
; => (LIST1 LIST2 LIST3) 
```

例:

```cl
(append '(many had) '(a) '(little lamb))
; =>
(MARY HAD A LITTLE LAMB)
```

## `apply`関数
引数にある、複数の要素を持つリスト（要素1つのリストでも良い）の各要素を、
引数にある関数の引数として渡す。

```cl
(apply #'func '(list))
```

例1:

```cl
(apply #'1+ '(2))
; => 3

(apply #'+ '(2 3))
; => 5

(apply #'append '((mary had) (a) (little lamb)) )
; => (MARY HAD A LITTLE LAMB)
```

## `append`関数と`apply`関数の使用例
とあるゲーム(Land of Lispのアレ)を例に示す。

### ゲームの変数を定義する。

- とあるゲームにおいて、各々の場所について(つながっている他の場所 移動方向 移動方法)を定義

```cl
(defparameter *edges* '((living-room (garden west door)
                                    (attic upstairs ladder)
                       (garden      (living-room east door)
                       (attic       (living-room downstairs ladders))))
```

- とあるゲームにおいて、登場するオブジェクトを定義

```cl
(defparameter *objects* '(whiskey bucket frog chain))
```

- とあるゲームにおいて、登場するオブジェクトが置いてある場所を定義

```cl
(defparameter *objects-locations* '((whiskey living-room)
                                    (bucket living-room)
                                    (chain garden)
                                    (frog garden)) )
```

### 関数を定義する。

-  与えられたパスを表示する

```cl
(defun describe-path (edge)
  '(there is a ,(caddr edge) going ,(cadr edge) from here.))

; 使用例:
(describe-path '(garden west door))
; => (THERE IS A DOOR GOING WEST FROM HERE.)
```

- 指定した場所のパスを一覧表示する

```cl
(defun describe-paths (location edges)
  (apply #' append (mapcar #'describe-path (cdr (assoc location edges)) )) )

; 使用例:
(describe-paths 'living-room *edges*)
; => (THERE IS A DOOR GOING WEST FROM HERE. \
;     THERE IS A LADDER GOING UPSTAIRS FROM HERE.)
```

- 指定した場所にあるオブジェクトを一覧表示する

```cl
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
             ; remove-if-not : predicate が偽となる要素を取り除く
             (remove-if-not #'at-loc-p objs)))

; 使用例:
(objects-at 'living-room *objects* *objects-locations*)
; => (objects-at *objects* *objects-locations*)

; => (remove-if-not #'at-loc-p *objects*)

; => 下記の評価を*objects*リストの全要素について行い、
;    真になったものをリストから消す。
;    (eq (cadr (assoc obj obj-locs)) 'living-room)
;                     │   └ '((whiskey living-room) 
;                     │       (bucket living-room) 
;                     │       (chain garden) 
;                     │       (frog garden))
;                     └ 'whiskey
;    => T
;    (eq (cadr (assoc obj obj-locs)) 'living-room)
;                     │   └ '((whiskey living-room) 
;                     │       (bucket living-room) 
;                     │       (chain garden) 
;                     │       (frog garden))
;                     └ 'bucket
;    => T
;    (eq (cadr (assoc obj obj-locs)) 'living-room)
;                     │   └ '((whiskey living-room) 
;                     │       (bucket living-room) 
;                     │       (chain garden) 
;                     │       (frog garden))
;                     └ 'frog
;    => F
;    (eq (cadr (assoc obj obj-locs)) 'living-room)
;                     │   └ '((whiskey living-room) 
;                     │       (bucket living-room) 
;                     │       (chain garden) 
;                     │       (frog garden))
;                     └ 'chain
;    => F
 
; => (WHISKEY BUCKET)
```

- 全てのオブジェクトを一覧表示する

```cl
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
              `(you see a ,obj on the floor.) ))
   (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

; 使用例:
(describe-objects 'living-room *objects* *objects-locations*)
; => (apply #'append (mapcar #'describe-obj (objects-at living-room *objects* *objects-locations*)))
; => (apply #'append (mapcar #'describe-obj '(WHISKEY BUCKET)))
; => (apply #'append '((YOU SEE A WHISKEY ON THE FLOOR.) '(YOU SEE A BUCKET ON THE FLOOR.)))
; => (append '(YOU SEE A WHISKEY ON THE FLOOR.) '(YOU SEE A BUCKET ON THE FLOOR.))
; => (YOU SEE A WHISKEY ON THE FLOOR. YOU SEE A BUCKET ON THE FLOOR.)
```

## `find`関数
リストから引数の要素を探す。

```cl
(find 'keyword 'object :key #'func)
; 'objectの各要素に対してfuncを適用した値がkeywordと等しいとき、その要素を返す。
```

例:

```cl
(find 'y '((5 x) (3 y) (7 z)) :key #'cadr)
; => (3 Y)
```

## `push`関数
引数をリストの先頭に追加する。

```cl
(push val '(list))
; => (val list)
```

下記の2つの処理は同等。

```cl
(push 7 *foo*)

(setf *foo* (cons 7 *foo*))
```

例:

```cl
(defparameter *foo* '(1 2 3))
(push 7 *foo*)
; => (7 1 2 3)
*foo*
; => (7 1 2 3)
```

## `assoc`関数と`push`関数を使った、alist(association list)の値変更
`assoc`関数は、先頭から探索して、最初に見つかった要素を返したら残りは無視する。
従って、`assoc`と`push`を使用すると、alistの値を、さも変更したかのように見せられる。

例:

```cl
(defparameter *foo* '((whiskey living-room) 
                      (bucket living-room) 
                      (chain garden) 
                      (frog garden)))

(assoc 'whiskey *foo*)
; => (WHISKEY LIVING-ROOM)

(push '(whiskey void) *foo*)

(assoc 'whiskey *foo*)
; => (WHISKEY VOID)

*foo*
; => ((WHISKEY VOID) 
;     (WHISKEY LIVING-ROOM) 
;     (BUCKET LIVING-ROOM) 
;     (CHAIN GARDEN) 
;     (FROG GARDEN))
```

