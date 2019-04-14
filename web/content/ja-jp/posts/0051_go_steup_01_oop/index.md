---
title: "Go言語ステップアップ(オブジェクト指向プログラミング)"
date:    2019-04-13T19:00:00+09:00
lastmod: 2019-04-13T19:00:00+09:00
draft: false
tags: ["Go", "Golang"]
categories: ["Notes"]
authors:
- otaon
---

# 目的
Go言語でオブジェクト指向プログラミングを行う方法を残す。

# 参考文献
- [POSTD - Goはオブジェクト指向言語だろうか？](https://postd.cc/is-go-object-oriented/)
- [Go言語でのダックタイピング](https://blog.mmmcorp.co.jp/blog/2018/10/26/go-duck-typing/)
- [Qiita - オブジェクト指向言語としてGolangをやろうとするとハマること](https://qiita.com/shibukawa/items/16acb36e94cfe3b02aa1)
- [Qiita - Go言語のInterfaceの考え方、Accept interfaces,return structs](https://qiita.com/weloan/items/de3b1bcabd329ec61709)

# オブジェクト指向プログラミング原則(OOP原則)の定義
何を以ってオブジェクト指向というのかは諸説ある。  
例えば有名なものは下記(3大原則「カプセル化・継承・多態性」を含む。wikipediaから引用)。

1. **カプセル化**（encapsulation）
1. **継承**（inheritance）
1. **多態性**（polymorphism）
  1. アドホック多態性（ad hoc polymorphism）
     1. 関数オーバーロード（function overloading）
     1. 演算子オーバーロード（operator overloading）
  1. パラメータ多態性（parameter polymorphism）
     1. ジェネリック関数（generic function）
     1. ジェネリックプログラミング（generic programming）
  1. サブタイプ多態性（subtyping）
     1. 仮想関数（virtual function）
     1. 動的ディスパッチ（dynamic dispatch）
     1. ダブルディスパッチ（double dispatch）
     1. 多重ディスパッチ（multiple dispatch）
1. メッセージパッシング（message passing）

# GoにおけるOOP原則の定義
本稿では独断で、下記が表現できるプログラミングスタイルのことと定義する。  
実際、下記を表現していればオブジェクト指向プログラミングだと言って差し支えないと思う(思いたい)。

1. カプセル化
1. 部分型(is-aの関係を表現できる)
  1. 継承
  1. ポリモーフィズム
1. オブジェクトを集約・コンポジットできる(has-aの関係を表現できる)
  1. 集約
  1. コンポジション

# Goで各OOP原則を実践する
ここからは、実際にGoでOOP原則を表現していく。

## カプセル化

## 部分型(is-a)

### 継承

### ポリモーフィズム

#### ダックタイピング

## 集約・コンポジション(has-a)

### 集約

### コンポジション
