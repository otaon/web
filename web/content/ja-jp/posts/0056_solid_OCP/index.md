---
title: "オブジェクト指向原則集(SOLID、他):開放／閉鎖原則について"
date:    2019-05-05T02:00:00+09:00
lastmod: 2019-05-05T02:00:00+09:00
draft: false
toc: true
tags: ["programming", "オブジェクト指向", "OOP", "SOLID", "OCP"]
categories: ["programming"]
authors:
- otaon
---

# 目的
プログラミングや設計において守るべき「SOLID原則」のうち、「開放／閉鎖原則」についての学習記録を残す。

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
