---
title: "オブジェクト指向原則集(SOLID、他):リスコフの置換原則について"
date:    2019-05-05T03:00:00+09:00
lastmod: 2019-05-05T03:00:00+09:00
draft: false
toc: true
tags: ["オブジェクト指向", "OOP", "SOLID", "LSP"]
categories: ["Notes"]
authors:
- otaon
---

# 目的
プログラミングや設計において守るべき「SOLID原則」のうち、「リスコフの置換原則」についての学習記録を残す。

# 参考文献
「[オブジェクト指向原則集(SOLID、他)]({{<relref "posts/0055_solid/index.md" >}})」を参照。

# SOLID原則について
SOLID原則とは、プログラミングや設計において守るべき主要な原則5つをまとめたもの。

|&nbsp;頭文字|&nbsp;略称|&nbsp;英名|&nbsp;和名|
|---|---|---|---|
| &nbsp;**S** | &nbsp;SRP | &nbsp;Single Responsibility Principle     | &nbsp;単一責任の原則 |
| &nbsp;**O** | &nbsp;OCP | &nbsp;The Open-Closed Principle           | &nbsp;開放／閉鎖原則 |
| &nbsp;**L** | &nbsp;LSP | &nbsp;The Liskov Substitution Principle   | &nbsp;リスコフの置換原則 |
| &nbsp;**I** | &nbsp;ISP | &nbsp;The Interface Segregation Principle | &nbsp;インターフェース分離原則 |
| &nbsp;**D** | &nbsp;DIP | &nbsp;The Dependency Inversion Principle  | &nbsp;依存性逆転の原理 |

SOLID全体の説明については「[オブジェクト指向原則集(SOLID、他)]({{<relref "posts/0055_solid/index.md" >}})」を参照。

## **L** : The Liskov Substitution Principle (LSP) : リスコフの置換原則
> `T` 型のオブジェクト`x`に関して真となる属性を`q(x)`とする。  
> このとき`S`が`T` の派生型であれば、`S`型のオブジェクト`y` について`q(y)`が真となる。

`S`が`T`の派生型であれば、プログラム内で`T`型のオブジェクトが使われている箇所は全て`S`型のオブジェクトで置換可能であるべき。

リスコフの置換原則を契約プログラミングに適用すると、契約と継承の相互作用に次のような制約をもたらす

- 派生型は、事前条件を強めることはできない。つまり、上位の型よりも強い事前条件を持つ派生型を作ることはできない。
  - サブクラスは、スーパークラスが呼び出せる状況なら必ず呼び出せること。
- 派生型は、事後条件を弱めることはできない。つまり、上位の型よりも弱い事後条件を持つ派生型を作ることはできない。
  - サブクラスは、スーパークラスが達成することは必ず達成すること。

### 実装例
車クラス と F1クラス を用意する。  
これらのクラスを利用するドライバクラスを用意する。  

```cs
class Driver
{
	var car = new Car();
	var f1 = new F1();

	var crt = CreateCurrentLocation();
	var dst = CreateRandomDestination();

	// 事前条件チェック
	if (dst == null) dst = DefaultDestination;
	if (crt == null) crt = Home;

	// サブルーチン呼び出し
	var message = car.SetDestination(crt, dst);

	// 事前条件を満たしているのでメッセージの妥当性が保証されている
	car.Read(message);


	var dst = CreateRandomDestination();

	// 事前条件チェック
	if (dst == null) dst = DefaultDestination;

	// サブルーチン呼び出し
	var message = f1.SetDestination(crt, dst);

	// 事前条件を満たしているのでメッセージの妥当性が保証されている
	f1.Read(message);
}

class Car
{
	public Message SetDestination(string currentPlace, string destination)
	{
		// 事前条件
		if (currentPlace == null) throw new NullCurrentPlaceException();
		if (destination == null) throw new NullDestinationException();

		// 不変条件
		InvariantTest();

		this.Destination = destination;
		ChangeDriveMode(Mode.Ready);
		var message = new Message(this.Mode.Message);

		// 事後条件
		if (!this.Destination.IsValid) throw new InvalidDestinationException();
		if (!this.Mode.IsReady) throw new NotPreparedException();
		if (message == null) throw new NullMessageException();

		// 不変条件
		InvariantTest();

		return message;
	}

	// クラス不変条件の判定
	private void InvariantTest()
	{
		if (!this.CurrentPlace.IsValidLocation) throw new InvalidLocationException();
		if (!this.Destination.IsValidLocation) throw new InvalidLocationException();
		if (this.CurrentPlace == this.Destination) throw new InvalidLocationException();
	}
}

class F1 : Car
{
	public Message SetDestination(string currentPlace, string destination)
	{
		// 事前条件 派生型では事前条件を弱めることのみできる
		if (destination == null) throw new NullDestinationException();

		// 不変条件
		InvariantTest();

		this.Destination = destination;
		ChangeDriveMode(Mode.Ready);
		var message = new Message(this.Mode.Message);

		// 事後条件 派生型では事後条件を強化のみできる
		if (!this.Destination.IsValid) throw new InvalidDestinationException();
		if (!this.Mode.IsReady) throw new NotPreparedException();
		if (message == null) throw new NullMessageException();
		if (!this.IsIgnited) throw new NotIgnitedException();

		// 不変条件
		InvariantTest();

		return message;
	}
}
```

### Programming By Contract 契約プログラミング
プログラムコードの中にプログラムが満たすべき仕様についての記述を盛り込む事で設計の安全性を高める技法。  
コードを呼ぶ側が事前条件と不変条件を満たす義務を負うことで、呼ばれたコードはその条件が恒真であるとの前提を利益として得る。  
引き換えに、呼ばれたコードは事後条件と不変条件を義務として負い、呼ぶ側の利益としてこれを保証する。

- 事前条件 (precondition)
  - サブルーチンの開始時に、これを呼ぶ側で保証すべき性質。例：引数チェック。
- 事後条件 (postcondition)
  - サブルーチンが、終了時に保証すべき性質。例：戻り値チェック。
- 不変条件 (invariant)
  - クラスなどのオブジェクトがその外部に公開しているすべての操作の開始時と終了時に保証されるべき、オブジェクト毎に共通した性質。

