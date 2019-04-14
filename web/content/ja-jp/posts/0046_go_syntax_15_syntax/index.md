---
title: "Go言語入門(制御構文)"
date:    2019-04-13T15:00:00+09:00
lastmod: 2019-04-13T15:00:00+09:00
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
## 制御構文
### `for`
Goにおけるループ用の制御構文は`for`のみ。

```go
for {
	// ...

	if [条件] {
		break	// 中断
	}

	if [条件] {
		continue	// 次の周回へジャンプ
	}
}

for i := 0; i < 10; i++ {
	// 0から9までの繰り返し
}

for ;; {
	// 無限ループ
}
```

pythonなどにあるような範囲節を用いたループも記述できる。

```go
for [配列のインデックス], [配列の要素] := range [配列型] {
	// ...
}

for i, s := range [3]string{"aaa", "bbb", "ccc"} {
	fmt.Printf("%v:%v\n", i, s)
}
// =>
// 0:aaa
// 1:bbb
// 2:ccc
```

範囲節に指定できる型は下記の通り。

|データ型|型|反復値<br/>(1番目)<br/>== [配列のインデックス]|反復値の型<br/>(1番目)|反復値<br/>(2番目)<br/>== [配列の要素]|反復値の型<br/>(2番目)|
|:---|:---:|:---|:---:|:---|:---:|
|配列型|`[n]E`|インデックス|`int`|配列の要素|`E`|
|配列型のポインタ|`*[n]E`|インデックス|`int`|配列の要素|`E`|
|スライス|`[]E`|インデックス|`int`|の要素|`E`|
|文字列|`string`|インデックス|`int`|ルーン|`rune`|
|マップ|`map[K]V`|マップのキー|`K`|マップの値|`V`|
|チャネル|`chan E`, `<- chan E`|チャネルの値|`E`|無し|-|

- 
- インデックスは0始まり

### `if`
条件文のひとつに`if`がある。

```go
if x == 1 {
	// ...
}

if x == 1 {
	// ...
} else if x == 2 {
	// ...
} else if x == 3 {
	// ...
} else {
	// ...
}

// {}は絶対に省略できない
if x == 1
	// ...	// コンパイルエラー

// 真偽値を返さない条件式は記述できない
if 1 {
	// ...	// コンパイルエラー
}
```

#### 簡易文付き`if`
条件式への使用などのために、条件式の前に単一の文を定義できる。  
簡易文で定義した変数は、`if`の中でのみ使用できる。つまり`if`の外では参照できない。

```go
if [簡易文]; [条件式] {
	// ...
}

if x, y = 1, 2; x < y {
	// ...
}

// ここではx, yを参照できない
```

簡易文付き`if`は、エラー処理をシンプルに記載する際に便利。

```go
if _, err := doSomething(); err != nil {
	// エラー発生時の処理
}
```

### `switch`
#### 式による`switch`
大抵はCにおけるswitchと似ているが、細部の仕様が異なる。

- `if`と同じく簡易文が使える。
- `case`に複数の式をカンマ区切りで指定できる。
- 互換性のある型にはマッチする。
- Goにおける`switch`ではフォールスルー特性がない。  
  すなわち、各caseの処理が完了したら他の`case`に入らず`switch`文から出る。
- `case`に式と値を混在させるとコンパイルエラーとなる。

```go
switch [簡易文;] [式] {
	// ...
}

n := 3
switch n {
case 1, 2:
	fmt.Println("1 or 2")
case 3, 4:
	fmt.Println("3 or 4")
default:
	fmt.Println("other")
}
// =>
// "3 or 4"

n := 3
switch n {
case 1:
	fmt.Println("one")
case 2.0:	// 整数2の場合、ここに入る
	fmt.Println("two")
case 3+0i:	// 整数3と互換の型のため、ここに入る
	fmt.Println("three")
}
// =>
// "three"

// 式を使ったswitch-case文
n := 4
switch n {
case 0 < n && n < 3:
	fmt.Println("0 < n < 3")
case 3 < n && n < 6:
	fmt.Println("3 < n < 6")
}
// =>
// "3 < n < 6"
```


#### **型アサーション**と型による`switch`
型アサーションとは、動的に変数の型をチェックする機能。

1つの戻り値を受け取る場合、型アサーション違反となるとランタイムパニックが発生する。

```go
// 基本的な書式
x.(T)	// 変数xが型Tでなければランタイムパニックを発生させる

var x interface{} = 3

i := x.(int)		// エラー発生せず(i == 3)
f := x.(float64)	// ランタイムパニック発生
```

2つの戻り値を受け取る場合、第2戻り値に型アサーションの結果が真偽値で格納される。

```go
var x interface{} = 3.14

i, isInt := x.(int)	// i == 0, isInt == false
f, isFloat64 := x.(float64)	// f == 3.14, isFloat64 == true
s, isString := x.(string)	// s == "", isString == false


// 実用例
if x == nil {
	fmt.Println("x is nil")
} else if i, isInt := x.(int); isInt {
	fmt.Printf("x is integer : %d\n", i)
} else if s, isString := x.(string); isString {
	fmt.Println(s)
} else {
	fmt.Println("unsupported type")
}
```

上記コードの実用例を`switch`を用いることでさらに簡潔に記述できる。

```go
switch x.(type) {
case bool:
	fmt.Println("x is bool")
case int, uint:
	fmt.Printf("x is integer or unsigned integer")
case string:
	fmt.Println("string")
default:
	fmt.Println("unsupported type")
}
```

簡易文によって、`switch`本体で値を簡単に使用できる。  
このとき、値が明確に型推論可能であれば、`case`内部の値は型情報を与えられる。

```go
// 正しい使用例
switch v := x.(type) {
case bool:
	fmt.Println("x is bool", v)
case int:
	fmt.Printf("x is integer: v^2=", v * v)
case string:
	fmt.Println(v)
default:
	fmt.Printlf("unsupported type: %#v\n", v)
}

// 誤った使用例
switch v := x.(type) {
case int, uint:	// int型かuint型か定まらないためコンパイルエラーとなる
	fmt.Printf("x is integer: v^2=", v * v)
default:
	fmt.Printlf("unsupported type: %#v\n", v)
}
```

### `goto`
`goto`も使用可能。ただし、安全性を保つため下記制約がある。

- ラベルと`goto`の組み合わせは関数内に閉じた範囲で使用できる。
- `goto`によって、`for`文などが構成するブロックの外側から内側にはジャンプできない。
- `goto`によって、**変数定義**を飛び越えられない。

```go
func main() {
	fmt.Println("A")
	goto L
	fmt.Println("B")
L:	// ラベル
	fmt.Prinln("C")
}

// =>
// A
// C
```

### ラベル付き文
`goto`を使わずとも、**ラベル付き文**を用いれば大域脱出が可能。

```go
LOOP:
	for {
		for {
			for {
				fmt.Println("開始")
				break LOOP
			}
			fmt.Println("ここは通らない")
		}
		fmt.Println("ここは通らない")
	}
	fmt.Println("完了")

// =>
// 開始
// 完了
```


### `defer`で関数終了時の処理を登録する
`defer`キーワードを使用することで、関数終了時の処理を予め登録しておける。  
登録しておいた処理は、関数終了時に実行される。

```go
func runDefer() {
	defer fmt.Println("defer")	// runDefer()終了時に呼ばれる処理
	fmt.Println("done")
}
runDefer()

// =>
// done
// defer
```

`defer`を使えば、ファイルのオープン/クローズ処理などの、リソース管理を多少楽に記述できる。  
`defer`は複数回使用できるが、後に登録した処理から先に実行されることに注意。

```go
func foo() {
	file, err := os.Open("path/to/file")
	if err != nil {
		// ファイルのオープンに失敗
		return
	}
	defer file.Close()	// 最後にファイルをクローズするように処理を登録

	// ファイルを使った処理...
}

func foo() {
	defer Println("1")
	defer Println("2")
	defer Println("3")
	
	Println("ABC")
}

// =>
// ABC
// 3
// 2
// 1
```

`defer`で複数の処理を登録したい場合、無名関数を使用すれば良い。  
`defer`は関数呼び出しの形にする必要があるため、無名関数の定義の最後にカッコを付ける必要があることに注意。

```go
defer func() {
	Println("3")
	Println("2")
	Println("1")
	Println("0!")
}()	// 関数呼び出しの形にする必要がある
```

### `panic`と`recover`によるランタイムパニック処理
#### `panic`によってランタイムパニックを発生させる
`panic`はランタイムパニック(C言語のセグメンテーション違反のようなもの)を発生させる関数。

```go
// panicの書式
func panic(v interface{})

// panicの使用例
func main() {
	// panic発生時にもdeferは必ず実行される
	defer fmt.Println("END!!!!")

	panic("runtime error!")
	fmt.Println("Hello, World!")
}

// =>
// END!!!!
// panic: runtime error!
// ....エラー詳細....
// ..................
```

#### `recover`によってランタイムパニックによる中断を回復する
`defer`の中で`recover`関数を登録しておくことで、ランタイムパニックから復帰することができる。

```go
/// recoverの書式
func recover() interface{}
// 戻り値...nil:ランタイムパニック未発生, nil以外:ランタイムパニック発生
```

下記コードは、`panic`と`recover`を組み合わせた使用例。

```go
func testRecover(src interface{}) {
	// エラーハンドリング
	defer func() {
		if x:= recover(); x != nil {
			// 発生したpanicの種類によって処理分岐
			switch v := x.(type) {
			case int:
				fmt.Printf("panic: int=%v\n", v)
			case string:
				fmt.Printf("panic: string=%v\n", v)
			default:
				fmt.Printf("panic: unknown error")
			}
		}
	}

	panic(src)
	return
}

func main() {
	testRecover(128)
	testRecover("foo")
	testRecover([...]int{1, 2, 3})
}

// =>
// panic: int=128
// panic: string=foo
// panic: unknown error
```

**NOTE**  
Goにおいては、この手の例外処理は積極的に使用しないほうが良い。  
むしろ`panic`を起こさないように注意したコーディング(防御的プログラミング的なもの?)を心がけることが良しとされているらしい。

### `go`文による並行処理
`go`文は、**ゴルーチン(goroutine)**を生成して、並行処理されるようにランタイムに追加するための機能。

ゴルーチン
: Goが持つ、スレッドよりも小さい、軽量な処理単位。

`go`文では、`defer`文と同様に、関数呼び出し形式の式を受け取る。  
`go [関数呼び出し]`と記述すると、ゴルーチンを新規に生成して`[関数呼び出し]`を実行する。

```go
func sub() {
	for {	// 無限ループ
		fmt.Println("sub loop")
	}
}

func main() {
	go sub()
	for {	// 無限ループ
		fmt.Println("main loop")
	}
}
```

`runtime`パッケージを使用することで、Goのランタイム自身についての情報を参照したり、動作をコントロールできる。

```go
package main

import (
	"fmt"
	"runtime"
)

func main() {
	fmt.Printf("NumCPU: %d\n", runtime.NumCPU())
	fmt.Printf("NumGoroutine: %d\n", runtime.NumGoroutine())
	fmt.Printf("Version: %d\n", runtime.Version())
}
// =>
// NumCPU: 2
// NUmGoroutine: 1
// Version: go1.6
```

- `NumCPU`...CPUのコアの数
- `NumGoroutine`...Goランタイム上で動作しているゴルーチンの個数

### `init`関数
Goのパッケージにおいて、パッケージの初期化を目的にした特殊な関数`init`を、開発者自身で定義できる。  
`init`は、引数と戻り値を持ってはいけない。

`init`関数は、`main`関数の実行前に実行される。すなわち、`init`関数は、プログラムのメインルーチン実行前に、初期化したい何らかの処理を実行するために使用できる。

```go
package main

package (
	"fmt"
)
func init() {
	fmt.Println("init()")
}

func main() {
	fmt.Println("main()")
}

// =>
// init()
// main()
```

`init()`は複数個定義できる。定義した`init()`は、上から順番に実行される。ただし、明確な目的がない限り、`init`関数を分けない方が良い。

```go
package main

package (
	"fmt"
)

var S = ""

func init() {
	S += "A"
}

func init() {
	S += "B"
}

func init() {
	S += "C"
}

func main() {
	fmt.Println("S")
}

// =>
// ABC
```

