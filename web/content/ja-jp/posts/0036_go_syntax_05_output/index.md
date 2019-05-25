---
title: "Go言語入門(出力関数)"
date:    2019-04-13T05:00:00+09:00
lastmod: 2019-04-13T05:00:00+09:00
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

# 基本構文
## 出力関数
### `fmt.Println()`
文字列の最後に改行を付加した文字列を標準出力に出力する。

```go
fmt.Println("Hello, Golang!")	// => "Hello, Golang!"
```

複数の引数を渡した場合、各々の文字列をスペースで区切って1行で表示する。

```go
fmt.Println("Hello" "Golang" "!")	// => "Hello Golang !"
```

### `fmt.Printf()`
書式指定子を含んだフォーマット文字列と、可変長引数を渡すと、生成した文字列を標準出力に出力する。

```go
fmt.Printf("数値=%d\n", 5)	// => "数値=5"
```

```go
fmt.Printf("10進数=%d 2進数=%b 8進数=%o 16進数=%x\n", 17, 17, 17, 17)
// => "10進数=17 2進数=10001 8進数=21 16進数=11"
```

可変長引数の個数に過不足がある場合、それを下記のように文字列中で知らせる。

```go
fmt.Printf("%d年%d月%d日\n", 2015, 12)
// => "2015年12月%!d(MISSING)日"

fmt.Printf("%d年%d月%d日\n", 2015, 12, 25, 17)
// => "2015年12月25日%!(EXTRA int=17)"
```

Goのデバッグで有用な書式指定子として`%v`, `%#v`, `%T`がある。

```go
// %v 様々な型のデータを埋め込む
fmt.Printf("数値=%v 文字列=%v 配列=%v\n", 5, "Golang", [...]int{1, 2, 3})
// => "数値=5 文字列=Golang 配列=[1 2 3]"

// %#v Goのリテラル表現でデータを埋め込む
fmt.Printf("数値=%#v 文字列=%#v 配列=%#v\n", 5, "Golang", [...]int{1, 2, 3})
// => "数値=5 文字列="Golang" 配列=[3]int{1 ,2, 3}"

// %T 型情報を埋め込む
fmt.Printf("数値=%v 文字列=%v 配列=%v\n", 5, "Golang", [...]int{1, 2, 3})
// => "数値=int 文字列=string 配列=[3]int"
```

### `print()`, `println()`
組み込み関数として`print()`と`println()`がある。これらは、与えられた文字列を**標準エラー出力**に出力する。
