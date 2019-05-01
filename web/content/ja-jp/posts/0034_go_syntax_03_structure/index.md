---
title: "Go言語入門(文構造)"
date:    2019-04-13T03:00:00+09:00
lastmod: 2019-04-13T03:00:00+09:00
draft: false
toc: true
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
## 文構造
Goでは、各々の文はセミコロン(`;`)によって区切られる。  
ただし、Goは<u>全てのセミコロンが省略できる</u>。  

```go
func main() {
	fmt.Println("Hello, World!")
}
```

上記のコードは、コンパイル時に文末にセミコロンが自動的に挿入され、下記のコードとして扱われる。

```go
func main() {
	fmt.Println("Hello, World!");
}
```

**NOTE**  
基本的に、<u>文末判定は行ごとに行われると考えて良い。</u>したがって、下記のコードはコンパイルエラーとなる。

```go
a := [3]string{
	"foo",
	"bar",
	"baz"	// 行末に;が挿入されてしまう
	}
```

これを防ぐには、下記のように末尾要素の後ろにも`,`をつけて、文末でない事を明示すべき。

```go
a := [3]string{
	"foo",
	"bar",
	"baz",	// 文末と判断されず;は挿入されない
	}
```

