---
title: "オブジェクト指向原則集(SOLID、他)"
date:    2019-05-05T00:00:00+09:00
lastmod: 2019-05-05T00:00:00+09:00
draft: false
toc: true
tags: ["オブジェクト指向", "OOP", "SOLID", "SRP", "OCP", "LSP", "ISP", "DIP"]
categories: ["Notes"]
authors:
- otaon
---

# 目的
プログラミングや設計において守るべき「SOLID原則」についての学習記録を残す。

# 参考文献
- SOLIDについて
  - [POSTD - 開発者が知っておくべきSOLIDの原則](https://postd.cc/solid-principles-every-developer-should-know/)
    - 原文 [Bits and Pieces - SOLID Principles every Developer Should Know](https://blog.bitsrc.io/solid-principles-every-developer-should-know-b3bfa96bb688)
  - [Qiita - オブジェクト指向の法則集](https://qiita.com/kenjihiranabe/items/9eddc70e279861992274)
  - [Wikipedia - 開放/閉鎖原則](https://ja.wikipedia.org/wiki/%E9%96%8B%E6%94%BE/%E9%96%89%E9%8E%96%E5%8E%9F%E5%89%87)
  - [Wikipedia - リスコフの置換原則](https://ja.wikipedia.org/wiki/%E3%83%AA%E3%82%B9%E3%82%B3%E3%83%95%E3%81%AE%E7%BD%AE%E6%8F%9B%E5%8E%9F%E5%89%87)
  - [Wikipedia - 依存性逆転の原則](https://ja.wikipedia.org/wiki/%E4%BE%9D%E5%AD%98%E6%80%A7%E9%80%86%E8%BB%A2%E3%81%AE%E5%8E%9F%E5%89%87)
  - [SlideShare - 20160526 依存関係逆転の原則](https://www.slideshare.net/ShintaroKurosawa/20160526-62511723)
  - [The Interface Segregation Principle (ISP)　インターフェース分離原則](http://etc9.hatenablog.com/entry/20090925/1253883259)
- Graphvizについて(本稿の図作成に使用)
  - [いろは二〇八 - Graphvizの基本 - 02](https://iroha208.com/categories/graphviz/02/)
  - [Qiita - Graphvizとdot言語でグラフを描く方法のまとめ](https://qiita.com/rubytomato@github/items/51779135bc4b77c8c20d)
  - [Graphviz - Node, Edge and Graph Attributes](https://www.graphviz.org/doc/info/attrs.html)

# SOLID原則について
SOLID原則とは、プログラミングや設計において守るべき主要な原則5つをまとめたもの。

|&nbsp;頭文字|&nbsp;略称|&nbsp;英名|&nbsp;和名|
|---|---|---|---|
| &nbsp;**S** | &nbsp;SRP | &nbsp;Single Responsibility Principle     | &nbsp;単一責任の原則 |
| &nbsp;**O** | &nbsp;OCP | &nbsp;The Open-Closed Principle           | &nbsp;開放／閉鎖原則 |
| &nbsp;**L** | &nbsp;LSP | &nbsp;The Liskov Substitution Principle   | &nbsp;リスコフの置換原則 |
| &nbsp;**I** | &nbsp;ISP | &nbsp;The Interface Segregation Principle | &nbsp;インターフェース分離原則 |
| &nbsp;**D** | &nbsp;DIP | &nbsp;The Dependency Inversion Principle  | &nbsp;依存性逆転の原理 |

## **S** : Single Responsibility Principle (SRP) : 単一責任の原則

## **O** : The Open-Closed Principle (OCP) : 開放／閉鎖原則
OCPは、プログラムや設計の拡張・修正に対する下記の原則のこと。

- 拡張に対して開いているべき (開放原則(open))
  - 機能拡張はコード修正によってではなく、コードの追加によって実現せよ
  - これが守られている⇒既存コード(共通ライブラリ等)を変更せずに機能拡張できる
  - これが守られていない⇒既存コード(共通ライブラリ等)を変更する必要がある
- 修正に対して閉じているべき (閉鎖原則(closed))
  - コードに修正が必要になった時、不要に影響範囲が漏れ出ることを避けよ
  - これが守られている⇒修正コードのみテストすれば良い(共通ライブラリ等はテストせずに済む)
  - これが守られていない⇒修正コード(修正の入った共通ライブラリ等もテストする必要がある)

上記の原則を満たすコードは、機能拡張の際に既存のコードに影響しないと考えて良い。(トートロジーじみているが。)  

開放/閉鎖原則の詳細については「[オブジェクト指向原則集(SOLID、他):開放／閉鎖原則について]({{<relref "posts/0056_solid_OCP/index.md" >}})」を参照。

## **L** : The Liskov Substitution Principle (LSP) : リスコフの置換原則
> `T` 型のオブジェクト`x`に関して真となる属性を`q(x)`とする。  
> このとき`S`が`T` の派生型であれば、`S`型のオブジェクト`y` について`q(y)`が真となる。

`S`が`T`の派生型であれば、プログラム内で`T`型のオブジェクトが使われている箇所は全て`S`型のオブジェクトで置換可能であるべき。

リスコフの置換原則を契約プログラミングに適用すると、契約と継承の相互作用に次のような制約をもたらす

- 派生型は、事前条件を強めることはできない。つまり、上位の型よりも強い事前条件を持つ派生型を作ることはできない。
  - サブクラスは、スーパークラスが呼び出せる状況なら必ず呼び出せること。
- 派生型は、事後条件を弱めることはできない。つまり、上位の型よりも弱い事後条件を持つ派生型を作ることはできない。
  - サブクラスは、スーパークラスが達成することは必ず達成すること。

リスコフの置換原則の詳細については「[オブジェクト指向原則集(SOLID、他):リスコフの置換原則について]({{<relref "posts/0057_solid_LSP/index.md" >}})」を参照。

## **I** : The Interface Segregation Principle (ISP) : インターフェース分離原則
- クライアントは自分が使うインタフェースだけを依存すべきである
- 単一のインタフェースより複数のインタフェースを使うべきである
- インタフェースをクライアントごとに分離すべきである

### メインルーチンから依存性を注入する
`User`クラスから実際に`User Repository`クラスを使うにはどうしたら良いか。  
最も簡単な方法は、これらのクラスの更に外側にある`Main`クラスからオブジェクトを注入することである。  
`Main`クラスがアプリケーション全体を統括する責務を持つならば、`LogicDLL`パッケージおよび`DataAccessDLL`パッケージに依存していても問題ない。

{{<figure src="DI4.png" alt="DI4" width="300" align="aligncenter">}}

## **D** : The Dependency Inversion Principle (DIP) : 依存性逆転の原理
この原則は、下記のことを表す。

- 上位レベルのモジュールは、下位レベルのモジュールに依存すべきではない
- 上位レベルのモジュール、下位レベルのモジュール、ともに抽象（abstractions)に依存すべき
- 抽象は、詳細に依存してはならない
- 詳細は、抽象に依存すべき

依存性逆転の原理の詳細については「[オブジェクト指向原則集(SOLID、他):依存性逆転の原理について]({{<relref "posts/0058_solid_DIP/index.md" >}})」を参照。

----

## The Reuse/Release Equivalence Principle (REP)

## The Common Reuse Principle (CRP)

## The Common Closure Principle (CCP)

## The Acyclic Dependencies Principle (ADP)

## The Stable Dependencies Principle (SDP)

## The Stable Abstractions Principle (SAP)


# プログラミングスタイル
## The Law of Demeter（デメテルの法則）
	


