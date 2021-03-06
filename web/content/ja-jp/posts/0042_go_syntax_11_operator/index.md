---
title: "Go言語入門(演算子)"
date:    2019-04-13T11:00:00+09:00
lastmod: 2019-04-13T11:00:00+09:00
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
## 演算子
`Go`では下記の演算子が使用できる。

|種類|&nbsp;&nbsp;演算子|
|----|------|
|算術|&nbsp;&nbsp;+ - * / % & \| ^ ^& << >> |
|比較|&nbsp;&nbsp;== != < <= > >=|
|論理|&nbsp;&nbsp;\|\| &&|
|単行|&nbsp;&nbsp;+ - ! ^ * & <-|
<br/>

|優先度|&nbsp;&nbsp;演算子|
|------|------|
|高    |&nbsp;&nbsp;* / % << >> & &^|
|      |&nbsp;&nbsp;+ - \| ^|
|      |&nbsp;&nbsp;== != < <= > >=|
|      |&nbsp;&nbsp;&&|
|低    |&nbsp;&nbsp;\|\||

### 演算子の意味と対象の型

|演算子|&nbsp;&nbsp;意味|&nbsp;&nbsp;対象の型|
|------|----|--------|
|+(単項)|&nbsp;&nbsp;意味なし(`0 + x`と等価)|&nbsp;&nbsp;整数、浮動小数点数、複素数|
|-(単項)|&nbsp;&nbsp;符号反転|&nbsp;&nbsp;整数、浮動小数点数、複素数|
|^(単項)|&nbsp;&nbsp;ビットの補数|&nbsp;&nbsp;整数、浮動小数点数|
|+(2項)|&nbsp;&nbsp;和|&nbsp;&nbsp;整数、浮動小数点数、複素数、文字列(文字列結合)|
|-(2項)|&nbsp;&nbsp;差|&nbsp;&nbsp;整数、浮動小数点数、複素数|
|*(2項)|&nbsp;&nbsp;積|&nbsp;&nbsp;整数、浮動小数点数、複素数|
|/(2項)|&nbsp;&nbsp;商(商は0の方向へ切り捨て/切り上げされる)|&nbsp;&nbsp;整数、浮動小数点数、複素数|
|%(2項)|&nbsp;&nbsp;剰余|&nbsp;&nbsp;整数|
|&(2項)|&nbsp;&nbsp;論理積(ビット演算)|&nbsp;&nbsp;整数|
|\|(2項)|&nbsp;&nbsp;論理和(ビット演算)|&nbsp;&nbsp;整数|
|^(2項)|&nbsp;&nbsp;排他的論理和(ビット演算)|&nbsp;&nbsp;整数|
|&^(2項)|&nbsp;&nbsp;ビットクリア(`a &^ b == a & (^b)`)|&nbsp;&nbsp;整数|
|<<(2項)|&nbsp;&nbsp;左シフト(ビット演算)|&nbsp;&nbsp;整数|
|>>(2項)|&nbsp;&nbsp;右シフト(ビット演算)|&nbsp;&nbsp;整数|
|==|等しい|比較可能な型|
|!=|等しくない|比較可能な型|
|<|未満|順序が有る型|
|<=|以下|順序が有る型|
|>|超|順序が有る型|
|>=|以上|順序が有る型|
|&&|and(論理値)|論理値|
|\|\||or(論理値)|論理値|
|!|not(論理値)|論理値|
<br/>

比較可能な型
: 論理値、整数値、浮動小数点数、複素数、文字列

順序が有る型
: 整数値、浮動小数点数、文字列

**NOTE**
`a = a [演算子] b`という形の2項演算子は`a [演算子]= b`という短縮形を持つ。

