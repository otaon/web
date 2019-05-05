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

### 開放/閉鎖原則の実例
下記図のように、ライブラリ`Common Library`とそれを使用するコード`User (A|B|C|D)`について考える。  

{{<figure src="Library and Users.svg" alt="Library and Users" width="400" align="aligncenter">}}

ライブラリの基本仕様
: 指定ディレクトリ内のファイルオブジェクトを探索し、その名前(つまりファイル名)の一覧を返す。  
メソッドは下記の通り。
  
```cs
namespace Library
{
	class Library
	{
		public List<string> GetFileNames(string path)
		{
			return new List<string>(Directory.GetFiles(path, "*"));
		}
	}
}

namespace User
{
	class User
	{
		public Main()
		{
			var lib = new Library();
			var strings = lib.GetFileNames("/path/to/directory/");
		}
	}
}
```

最初、各Userは`List<string> GetFileNames(string path)`を利用している。  
しかし、あるときUser Aが**ファイル名を全て大文字にした上で一覧を返してほしい**と要望してきたとする(機能拡張)。
このとき、修正方法には大きく２つの選択肢がある。

- 既存の`GetFileNames`メソッド内で基本仕様と大文字仕様の両方に対応する。
- 大文字変換部分を、別の箇所(別メソッド、別クラス、User A自身)に任せ、既存の`GetFileNames`メソッドは変更しない。

前者の対応方針は誤り。なぜならば、今回と同様の要望(機能拡張)が発生した場合、その都度ライブラリの既存メソッドを修正する必要がある(**開放原則**への違反)。
こうなると、既存メソッドはどんどん肥大化していき、複雑度も増していく。  
さらに、修正したメソッドの使用者(=他User)に対して、それらが全くコードを変更していなくても再テストを要求することになる(**閉鎖原則**への違反)。  
したがって後者の対応方針を取るのが正解となる。

よく見かけるのは、**ストラテジーパターン**、または、**デコレータ**を使用した方法。  
つまり、下記のように、実現したいロジックをライブラリのクラスメンバやメソッド引数に渡す方法。  

例えば、ライブラリとして機能を提供したいなら、はじめのうちは任意のロジックを引数などで渡せるようにした汎用的な関数を用意しておき、大勢の利用者がいる事が分かった時点で特殊化すれば良いと思う。

#### 基本クラスのメソッドにオプション引数を使って、予め拡張性を持たせる方法
デリゲートを渡すためのオプション引数を用意する方法。  
関数を第1級オブジェクトとして使用できる言語で実現可能。  
Library自体に変更が入らないため、Libraryの回帰テストが不要になる。

- 変更箇所・・・User。
- 大文字変換のロジック・・・User側にある。

```cs
namespace Library
{
	class Library
	{
		public List<string> GetFileNames(string path, Func<string, string> func = null)
		{
			var filenames = new List<string>(Directory.GetFiles(path, "*"));
			return (func != null) ? filenames : filenames.Select(func).ToList();
		}
	}
}
```

```cs
namespace User
{
	class User
	{
		public Main()
		{
			var lib = new Library();
			var strings = lib.GetFileNames("/path/to/directory/", (s) => s.ToUpper());
		}
	}
}
```

#### 基本クラスのコンストラクタを使って、予め拡張性を持たせる方法
コンストラクタにデリゲートを渡す方法。  
関数を第1級オブジェクトとして使用できる言語で実現可能。  
`GetFileNames`を同じ設定で使いまわしたいなら、こちらのほうが使いやすい。  
Library自体に変更が入らないため、Libraryの回帰テストが不要になる。

- 変更箇所・・・User。
- 大文字変換のロジック・・・User側にある。

```cs
namespace Library
{
	class Library
	{
		private Func<string, string> Function = null;

		public Library(Func<string, string> function = null)
		{
			Function = function;
		}

		public List<string> GetFileNames(string path)
		{
			var filenames = new List<string>(Directory.GetFiles(path, "*"));
			return (Function != null) ? filenames : filenames.Select(Function).ToList();
		}
	}
}
```

```cs
namespace User
{
	class User
	{
		public Main()
		{
			var lib = new Library((s) => s.ToUpper());
			var strings = lib.GetFileNames("/path/to/directory/");
		}
	}
}
```

#### 基本クラスを継承したサブクラスとコンストラクタを使って、拡張性を持たせる方法
実行したいロジックを実装したサブクラス作成する方法。  
ライブラリの既存コードは変更不要だが、ライブラリ側で新規クラスを作成する必要がある。  
関数を第1級オブジェクトとして使用できない言語でも実現可能。  
Library側は既存クラスに変更が入らないため、既存クラスの回帰テストが不要になる。

- 変更箇所・・・Library(クラス追加のみ)、User(呼び出し変更のみ)。
- 大文字変換のロジック・・・Library側にある。

```cs
namespace Library
{
	class Library
	{
		virtual public List<string> GetFileNames(string path)
		{
			return new List<string>(Directory.GetFiles(path, "*"));
		}
	}

	class UpperCaseLibrary : Library
	{
		override public List<string> GetFileNames(string path)
		{
			base.GetFileNames().Select((s) => s.ToUpper()).ToList();
		}
	}
}
```

```cs
namespace User
{
	class User
	{
		public Main()
		{
			var lib = new UpperCaseLibrary();
			var strings = lib.GetFileNames("/path/to/directory/");
		}
	}
}
```

#### ストラテジーパターンを使って、拡張性を持たせる方法
ストラテジーパターンを用いる方法。  
ライブラリの既存コードは変更不要だが、ライブラリ側で新規クラスを作成する必要がある。  
関数を第1級オブジェクトとして使用できない言語でも実現可能。  
Library側は既存クラスに変更が入らないため、既存クラスの回帰テストが不要になる。

- 変更箇所・・・Library(クラス追加のみ)、User(呼び出し変更のみ)。
- 大文字変換のロジック・・・Library側にある。

[Wikipedia - Strategy パターン](https://ja.wikipedia.org/wiki/Strategy_%E3%83%91%E3%82%BF%E3%83%BC%E3%83%B3)

```cs
namespace Library
{
	class Container
	{
		private Library Lib;

		public Container(Library library)
		{
			Lib = library;
		}

		public List<string> Execute(string path)
		{
			// Libの動的型で定義されたGetFileNames()を動的ディスパッチにより呼び出す
			return Lib.GetFileNames(path);
		}
	}

	class Library
	{
		virtual public List<string> GetFileNames(string path)
		{
			return new List<string>(Directory.GetFiles(path, "*"));
		}
	}

	class UpperCaseLibrary : Library
	{
		override public List<string> GetFileNames(string path)
		{
			return base.GetFileNames(path).Select((s) => s.ToUpper()).ToList();
		}
	}
}
```

```cs
namespace User
{
	class User
	{
		public Main()
		{
			var container = new Container(new UpperCaseLibrary());
			var strings = container.Execute("/path/to/directory/");
		}
	}
}
```

#### 拡張メソッドを使って、拡張性を持たせる方法
C#などでは、既存クラスに外からあたかもメソッドを追加したかのように見せかける機能がある。  
実質的にやっていることは、User側で`GetFileNames`メソッドのwrapperメソッドを実装しているに過ぎない。

- 変更箇所・・・User。
- 大文字変換のロジック・・・User側にある。

```cs
namespace Library
{
	class Library
	{
		public List<string> GetFileNames(string path)
		{
			var filenames = new List<string>(Directory.GetFiles(path, "*"));
		}
	}
}
```

```cs
namespace User
{
	class User
	{
		public Main()
		{
			var lib = new Library();
			var strings = lib.GetUpperCaseFileNames("/path/to/directory/");
		}
	}

	class LibraryExtensions
	{
		// 拡張メソッド
		public static List<string> GetUpperCaseFileNames(this Library lib, string path)
		{
			return lib.GetFileNames(path).Select((s) => s.ToUpper()).ToList();
		}
	}
}
```

### メイヤーの開放/閉鎖原則
1988年に、バートランド・メイヤーが『オブジェクト指向ソフトウェアの構築(Object Oriented Software Construction)』の中で「開放/閉鎖原則」という語を生み出したと一般に称される。

> - コードが完成したら、エラーを修正するには、クラスの**実装**だけを変更すべき。  
> - 新しい機能を拡張するには、別のクラスを作るべき。

この原則を守るコードでは、オリジナルのクラスから派生クラスを継承させることによってコードの再利用が可能となる。  
この派生クラスは、オリジナルのクラスと同じインタフェースを持っていても持っていなくとも良い。  
(確か、契約的プログラミングへのサポートが強いオブジェクト指向言語Eiffelでは派生型で基本型に有るインターフェースを削除できたはず(要検証)。)

メイヤーの定義においては、(インターフェースではなくクラスの)実装の継承をしても良い。  
実装は継承によって再利用可能であるが、インタフェース仕様は必ずしもそうではない。  
既存の実装は修正に対して閉じており、また新しいクラスは既存のインタフェースを引き継ぐ必要はない。

### ポリモーフィックな開放/閉鎖原則
1990年代、開放/閉鎖原則とは、一般的に抽象インタフェースの利用を指すように意味が変わっていった。
その定義では、実装はもはや固定ではなく、複数の実装が存在可能となり、互いにポリモーフィックに入れ替えることができる。


## **L** : The Liskov Substitution Principle (LSP) : リスコフの置換原則
> `T` 型のオブジェクト`x`に関して真となる属性を`q(x)`とする。  
> このとき`S`が`T` の派生型であれば、`S`型のオブジェクト`y` について`q(y)`が真となる。

`S`が`T`の派生型であれば、プログラム内で`T`型のオブジェクトが使われている箇所は全て`S`型のオブジェクトで置換可能であるべき。

リスコフの置換原則を契約プログラミングに適用すると、契約と継承の相互作用に次のような制約をもたらす

- 派生型は、事前条件を強めることはできない。つまり、上位の型よりも強い事前条件を持つ派生型を作ることはできない。
  - サブクラスは、スーパークラスが動く状況なら必ず動かなければならない。
- 派生型は、事後条件を弱めることはできない。つまり、上位の型よりも弱い事後条件を持つ派生型を作ることはできない。
  - サブクラスは、スーパークラスが達成することは必ず達成しなければならない。

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

## **I** : The Interface Segregation Principle (ISP) : インターフェース分離原則
- クライアントは自分が使うインタフェースだけを依存すべきである
- 単一のインタフェースより複数のインタフェースを使うべきである
- インタフェースをクライアントごとに分離すべきである

### メインルーチンから依存性を注入する
`User`クラスから実際に`User Repository`クラスを使うにはどうしたら良いか。  
最も簡単な方法は、これらのクラスの更に外側にある`Main`クラスからオブジェクトを注入することである。  
`Main`クラスがアプリケーション全体を統括する責務を持つならば、`LogicDLL`パッケージおよび`DataAccessDLL`パッケージに依存していても問題ない。

#### イメージ

{{<figure src="DI4.png" alt="DI4" width="300" align="aligncenter">}}


## **D** : The Dependency Inversion Principle (DIP) : 依存性逆転の原理
この原則は、下記のことを表す。
- 上位レベルのモジュールは、下位レベルのモジュールに依存すべきではない
- 上位レベルのモジュール、下位レベルのモジュール、ともに抽象（abstractions)に依存すべき
- 抽象は、詳細に依存してはならない
- 詳細は、抽象に依存すべき

例えば、下記のような依存関係があるとする。

{{<figure src="NOT-DI1.png" alt="NOT-DI1" width="150" align="aligncenter">}}

下位レベルのモジュールの再利用性を高めるためには、上記を、下記のような依存関係に逆転させたい。
ロジック層とデータアクセス層との間だけにこの原則を適用したコード例を下記に示す。

{{<figure src="NOT-DI2.png" alt="NOT-DI2" width="150" align="aligncenter">}}

### サンプルプログラム
- ユーザの誕生日から現在の年齢を計算する
- ユーザの情報は永続化して保存可能

```cs
public void Main()
{
	var user = new User (Date Time.Now);
	Console.WriteLine("You are " + user.Age.ToString() + "years old.");
	user.Save(user);
}
```

```cs
public class User
{
	public DateTime Birthday { set; }

	public int Age { get => GetAge(); }

	private int GetAge()
	{
		return 17;
	}

	public void Save()
	{
		var repository = new UserRepository();
		repository.Save(this);
	}
}
```

```cs
public class UserRepository
{
	public void Save(User user)
	{
		// Insert Database
	}
}
```

### 上位モジュールと下位モジュールを抽象に依存させる
上位モジュール(Logic DLL)と下位モジュール(Data Access DLL)を抽象に依存させる。  
しかし、単純に抽象に依存させると、`User`クラスが`UserRepository`インターフェースに依存してしまっている。

#### コード

```cs
public interface IUserRepository
{
	void Save(User user);
}
```

```cs
public class User
{
	private DateTime _birthday;

	// コンストラクタ
	public User(DateTime birthday)
	{
		_birthday = birthday;
	}

	public int Age => GetAge();

	private int GetAge()
	{
		return 17;
	}

	public Save()
	{
		// まだ、UserRepositoryの実体に依存してしまっている
		IUserRepository repository = new UserRepository();

		repository.Save(this);
	}
}
```

#### イメージ

{{<figure src="DI1.png" alt="DI1" width="200" align="aligncenter">}}

### 依存性の注入
依存関係を、クラスやソースコードの外側から注入することで、`User`クラスを`UserRepository`インターフェースに依存させなくできる。

```cs
public class User
{
	// フィールドで依存先のインターフェースを持つ
	private IUserRepository _repository;
	private DateTime _birthday;

	// コンストラクタ
	// コンストラクタでIUserRepositoryの実装を受け取る
	public User(DateTime birthday, IUserRepository repository)
	{
		_birthday = birthday;
		_repository = repository;
	}

	public int Age => GetAge();

	private int GetAge()
	{
		return 17;
	}

	public Save()
	{
		// コンストラクタで受け取ったインスタンスを使う
		// IUserRepository型である限り、実体が何なのかは無視できる
		_repository.Save(this);
	}
}
```

#### イメージ

{{<figure src="DI2.png" alt="DI2" width="200" align="aligncenter">}}

### 下位モジュールから上位モジュールに依存する
パッケージの持ち方を変える事により、「下位モジュールから上位モジュールに依存する」が実現できる。

#### イメージ

{{<figure src="DI3.png" alt="DI3" width="200" align="aligncenter">}}


----

## The Reuse/Release Equivalence Principle (REP)

## The Common Reuse Principle (CRP)

## The Common Closure Principle (CCP)

## The Acyclic Dependencies Principle (ADP)

## The Stable Dependencies Principle (SDP)

## The Stable Abstractions Principle (SAP)


# プログラミングスタイル
## The Law of Demeter（デメテルの法則）
	


