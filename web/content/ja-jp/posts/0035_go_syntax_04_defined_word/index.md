---
title: "Go言語入門(定義済み識別子)"
date:    2019-04-13T04:00:00+09:00
lastmod: 2019-04-13T04:00:00+09:00
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
## 定義済み識別子
下記の定義済み識別子は、変数名や関数名に使用できるが、混乱のもとになるため避けること。

| 識別子の種類 | 識別子 |
|:--------------:|--------|
| 型 | `bool` `byte` `complex64` `complex128` `error` `float32` `float64` `int` `int8` `int16` `int32` `int64` `rune` `string` `uint` `uint8` `uint16` `uint32` `uint64` `uintptr` |
| 定数 | `true` `false` `iota` |
| ゼロ値 | `nil` |
| 関数 | `append` `cap` `close` `complex` `copy` `delete` `imag` `len` `make` `new` `panic` `print` `println` `real` `recover` |
