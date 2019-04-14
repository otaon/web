---
title: "Go言語入門(関数)"
date:    2019-04-13T12:00:00+09:00
lastmod: 2019-04-13T12:00:00+09:00
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
## 関数
 

 ```go
func 関数名([引数名 型名]*) 戻り値の型名* {
	処理
}
 ```

### [引数名 型名]*
同じ型の引数を指定する場合、`x, y int`のように型の指定を一つにまとめることも、`x int, y int`のように変数ごとに型名を指定することもできる。

### 戻り値のない関数
戻り値のない関数は、下記のように戻り値の型名を省略することで定義できる。

```go
func 関数名([引数名 型名]*) {
	処理
}
 ```

### 複数の戻り値を持つ関数
Goでは複数の戻り値を持つ関数を定義できる。

```go
func 関数名([引数名 型名]*) (戻り値の型名1, 戻り値の型名2) {
	処理
	return 戻り値1, 戻り値2
}
```

複数の戻り値を持つ関数の戻り値は下記のように受け取る

```go
a, b := f(x, y)
```

複数の戻り値に不要なものがある場合、下記のように`_`を使うことで破棄できる。

```go
// 2つ目の戻り値を破棄
q, _ := f(x, y)

// 型推論できないためコンパイルエラー
_, _ := f(x, y)

// コンパイルエラーとはならないが、ただの関数呼び出しと等しいため無意味
_, _ = f(x, y)
```

### Goにおけるエラー処理
Goには例外機構がない。  
Goにおける一般的なエラー処理では、下記のイディオムを用いる。  
複数の戻り値を用いて、最後の戻り値でエラー内容を表すことで、関数の呼び出し元に自主的にエラー判定を行わせる。  

```go
result, err := f(x, y)
if (err != nil) {
	// エラー発生時の処理を書く
}
```

この方法は、C言語ライクのエラー判定をやりやすくした程度で原始的だが、単純で良いと思う。  
エラー用の戻り値を読み捨てるような使い方をされる可能性があるが、そもそも`err`は契約的プログラミングでいう事前条件違反を表すものと考えれば、むしろエラーハンドリングの責務を明確にできるのかもしれない。

### 戻り値を表す変数
Goでは戻り値に変数を割り当てることができる。

```go
package main

import (
	"fmt"
)

func f() (a int) {	// 戻り値に変数aを割り当てた
	return
}

func main() {
	fmt.Println(f())	// => 0 (== aのデフォルト値)
}
```

上記の構文は、下記のコードの短縮形と考えられる。

```go
// 短縮形
func f() (a int) {
	return
}

// 短縮していない形
fnc f() int {
	var a int
	return a
}
```

複数の戻り値に対しても変数を割り当てることができる。

```go
// 短縮形
func f() (a, b int) {
	b = 5
	return	// x === 0, y == 5
}

// 短縮していない形
func f() (int, int) {
	var a, b int
	b = 5
	return a, b	// x === 0, y == 5
}
```

### 引数を無視する
戻り値と同じく、引数も破棄することができる。  
インターフェースで定義されているプロトタイプを実装するとき、実装で使用しない場合に、それを明示する場合に使用すると無駄な変数割り当てが発生しない。

```go
func f(_, _ int) int {
	return 1
}

f(2, 3)	// => 1
```

### 無名関数
Goでは、関数を変数に代入できる。  
名前がある関数を代入できるのはもちろんのこと、下記のように関数定義自体をそのまま代入することもできる。

```go
f := func(x, y int) int { return x + y}
```

上記の右辺のように、関数を名前無しで定義して、それ自身を変数に代入できる。  
このとき、右辺を**関数リテラル**と呼ぶ。

関数リテラルの型は、その関数の引数の型と戻り値の型の組となる。  

```go
fmt.Println("%T\n", func(x, y int) int {return x + y})
// => "func(int, int) int"
```

したがって、意味はないが、変数を、この型で定義することもできる。

```go
var f func(int, int) int
f = func(x, y int) int { return x + y}
```

### 高階関数(関数を返す関数)
```go
package main

import (
	"fmt"
)

func F() func() {	// 戻り値の型 func()
	return func() {
		fmt.Println("Hello, World!")
	}
}

func main() {
	function := F()
	function()	// "Hello, World!"

	// F()で帰ってきたfを同時に呼び出す
	F()()	// "Hello, World!"
}
```

### 高階関数(関数を引数に渡す関数)

```go
package main

import (
	"fmt"
)

func F(f func()) {
	f()
}

func main() {
	F(func() { fmt.Println("Hello, World!") })	// "Hello, World!"
}
```

### クロージャ
Goではクロージャも定義できる。

```go
package main

/**
 * "nextに値を渡すとその値を内部に保持し、その前にnextに渡した値を返す"関数を返す
 */
func later() func(string) string {
	var store string
	return func(next string) string {
		s := store
		store = next
		return s
	}
}

func main() {
	f := later()
	f("Golang")	// => ""
	f("is")	// => "Golang"
	f("good")	// => "is"
}
```

#### クロージャでジェネレータを実装する
```go
package main

/**
 * "nextに値を渡すとその値を内部に保持し、その前にnextに渡した値を返す"関数を返す
 */
func integers() func() int {
	index := -1
	return func() int {
		index += 1
		return index
	}
}

func main() {
	f := integers()
	f()	// => 0
	f()	// => 1
	f()	// => 2
	f()	// => 3
	f2 := integers()
	f2()	// => 0
	f2()	// => 1
}
```

