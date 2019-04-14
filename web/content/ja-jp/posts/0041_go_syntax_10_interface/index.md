---
title: "Go言語入門(interface{}型とnil)"
date:    2019-04-13T10:00:00+09:00
lastmod: 2019-04-13T10:00:00+09:00
draft: false
tags: ["Go", "Golang"]
categories: ["Notes"]
authors:
- otaon
---

# 目的
Go言語の基本構文を残す。

# 参考文献
- [スターティング Go言語](https://www.shoeisha.co.jp/book/detail/9784798142418)

# 基本構文
## `interface{}`型と`nil`
`interface{}`は、`{}`も含めて型名。  
`interface{}`は、Goの全ての型と互換性のある型。
すなわち、`int`, `float64`,`string`なども全て`interface{}`と互換性がある。  
近いイメージとしては、C言語における汎用ポインタ`(void*)`や、JavaやC#における`Object`クラスがある。

`interface{}`は他の型と同様に変数定義できる。デフォルト値は`nil`。

```go
var x interface{}
fmt.Println("%#v", x)	// => "<nil>"
```

<u>`nil`とは、Goにおいて**具体的な値を持っていない状態を表す値**で、C言語、Java、C#における`null`のようなもの。</u>

`interface{}`型の変数であれば、次のように、あらゆる型の値を代入できる。

```go
var x interface{}
x = 1
x = 3.14
x = '山'
x = "文字列"
x = [...]uint8{1, 2, 3, 4, 5}
```

一旦`interface{}`型に格納されてしまった値は、元の型情報を失ってしまう。  
そのため、例えば`int`型の値を`interface{}`型の変数に代入すると、`+`演算が出来なくなる。

```go
var x, y interface{}
x, y = 1, 2
z := x + y	// 演算できないためエラーとなる
```

