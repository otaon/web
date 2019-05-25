---
title: "Go言語入門(ポインタ)"
date:    2019-04-13T17:00:00+09:00
lastmod: 2019-04-13T17:00:00+09:00
draft: false
toc: true
tags: ["Go", "Golang"]
categories: ["Go"]
authors:
- otaon
---

# 目的
Go言語の基本構文を残す。

# 参考文献
- [スターティング Go言語](https://www.shoeisha.co.jp/book/detail/9784798142418)

# ポインタ・構造体・インターフェース
## ポインタ
Goにも、C言語でいう**ポインタ**がある。

### ポインタを定義する
ポインタ型は、`*int`のようにポインタを使って参照・走査する方の前に「`*`」をつけることで定義できる。  
`*int`は`int`型のポインタ型、`*[3]float64`は`[3]float64`型のポインタ型になる。  
また、定義のみ行ったポインタ型の変数の初期値は`nil`となり、参照型と同じように振る舞う。

```go
var p *int
fmt.Println(p == nil)	// => "true"
```

「ポインタのポインタ」は、C言語と同様に「`*`」を重ねて書くことで定義できる。

```go
var pp **int
var ppp ***int
```

参照型に対しても、そのポインタ型を定義できる。

```go
var (
	s *[]string
	m *map[int]rune
	ch *chan int
)
```

### アドレス演算子とデリファレンス
アドレス演算子`&`を使って、任意の型からそのポインタ型を生成できる。

```go
var i int
p := &i
fmt.Printf("%T\n", p)	// "*int"
pp := &p
fmt.Printf("%T\n", pp)	// "**int"
```

変数に演算子`*`を使って、C言語と同様に、デリファレンスできる。

```go
var i int
p := &i
i = 5
fmt.Println(*p)	// => "5"
*p = 10
fmt.Println(i)	// => "10"
```

配列型のリテラルと`&`を組み合わせて、例えば`[3]int`型の配列を指し示すポインタを定義することが可能。

```go
func pow(p *[3]int) {
	for i := 0; i < 3; i++ {
		// 各要素を累乗する
		(*p)[i] = (*p)[i] * (*p)[i]
	}
}

func main() {
	// 変数pは*[3]int型
	p := &int{1, 2, 3}
	pow(p)
	fmt.Println(p)	// => "&[1, 2, 3]"
}
```

### 配列のポインタ型はデリファレンス(`*`)不要の場合がある
C言語では、配列のポインタ型変数から配列の各要素にアクセスするには、`(*p)[i]`のようにデリファレンスを明示的に行う必要がある。  
しかし、Goにおいては、配列のポインタ型のデリファレンスについて簡潔に書くための特別な方法がある。
すなわち、**配列に対するデリファレンスを省略できる**。

```go
p := &[3]{1, 2, 3}
// C言語ライクな記述方法
fmt.Println((*p)[0])	// => "1"

// Goにおける特別な記述方法(配列には*が不要)
fmt.Println(p[0])	// => "1"

// *を省略した記述方法
a := [3]string{"Apple", "Banana", "Cherry"}
p := &a
fmt.Println(a[1])	// "Banana"
fmt.Println(p[1])	// "Banana"
p[2] = "Grape"
fmt.Println(a[2])	// "Grape"
fmt.Println(p[2])	// "Grape"

// len()やcap()に対しても*を省略できる
p := &[3]int{1, 2, 3}
fmt.Println(len(p))	// => "3"
fmt.Println(cap(p))	// => "3"
fmt.Println(p[0:2])	// => "[1, 2]"

// 範囲説によるforにおけるrageにおいても*を省略できる
p := &[3]string{"Apple", "Banana", "Cherry"}
for i, v := range p {
	fmt.Println(i, v)
}
// =>
// 0 Apple
// 1 Banana
// 2 Cherry
```

### アドレスを確認する
C言語と同様に、`Printf`で`%p`フォーマット指定子を使用して、ポインタのアドレスを確認できる。

```go
i := 5
ip := &i
fmt.Printf("type=%T, address=%p\n", ip, ip)
// =>
// type=*int, address=0xc82000a380
```

### 文字列(`string`)型のポインタに対する特殊な扱い
Goにおいては、`string`型のポインタをとるのは特に問題無い。  
しかし、**文字列の部分参照(例:`s[0]`)をポインタ参照するとコンパイルエラーが発生する**。  
これは、文字列が不変（イミュータブル）であることが原因。つまり、文字列を構成する`byte`型のポインタを定義できてしまうと、その参照先の文字列の本体を破壊してしまい、イミュータブルでなくなってしまう。  

```go
s := "ABC"
s[0]	// 文字列のインデックス参照(byte型)

&s		// 文字列型のポインタ(問題無い)
&s[0]	// コンパイルエラー // connot take the address of s[0]
```

GoとC言語の文字列には大きな違いが有るため、これを覚えておかないと文字列操作を誤る可能性が高い。
具体的には下記の違いがある。

1. Goの文字列はイミュータブルのため、文字列の変換を多く実施すると、途中の文字列を全て生成するため、効率が落ちる。
1. `string`型の値を変数へ再代入したり、関数の引数に渡した場合でも、文字列の実体が別のメモリ領域にコピーされない。

**NOTE**  
Goでは、文字列を効率的に操作するために`bytes`パッケージを使用する。

```go
func printString(s string) {
	fmt.Println(s)
}

func main() {
	s := "Hello, World!"
	printString(s)	// 文字列の実体のコピー処理は発生しない
}
```

```go
// 非常に効率の悪い処理の例
s := ""
for _, v := range []string{"A", "B", "C"} {
	s += v
}
s	// == "ABC"
```

