---
title: "オブジェクト指向原則集(SOLID、他):依存性逆転の原理について"
date:    2019-05-05T05:00:00+09:00
lastmod: 2019-05-05T05:00:00+09:00
draft: false
toc: true
tags: ["programming", "オブジェクト指向", "OOP", "SOLID", "DIP"]
categories: ["programming"]
authors:
- otaon
---

# 目的
プログラミングや設計において守るべき「SOLID原則」についての学習記録を残す。

# 参考文献
SOLID全体の説明については「[オブジェクト指向原則集(SOLID、他)]({{<relref "posts/0055_solid/index.md" >}})」を参照。

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

### 0. サンプルプログラム
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

### 1. 上位モジュールと下位モジュールを抽象に依存させる
上位モジュール(Logic DLL)と下位モジュール(Data Access DLL)を抽象に依存させる。  
しかし、単純に抽象に依存させると、`User`クラスが`UserRepository`インターフェースに依存してしまっている。

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

{{<figure src="DI1.png" alt="DI1" width="200" align="aligncenter">}}

### 2. 依存性の注入
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

{{<figure src="DI2.png" alt="DI2" width="200" align="aligncenter">}}

### 3. 下位モジュールから上位モジュールに依存する
パッケージの持ち方を変える事により、「下位モジュールから上位モジュールに依存する」が実現できる。

{{<figure src="DI3.png" alt="DI3" width="200" align="aligncenter">}}


### 4. メインルーチンから依存性を注入する
`User`クラスから実際に`User Repository`クラスを使うにはどうしたら良いか。  
最も簡単な方法は、これらのクラスの更に外側にある`Main`クラスから**オブジェクト**を注入すること。  
`Main`クラスがアプリケーション全体を統括する責務を持つならば、`LogicDLL`パッケージおよび`DataAccessDLL`パッケージに依存していても問題ない。

{{<figure src="DI4.png" alt="DI4" width="300" align="aligncenter">}}

依存性の注入と言われているが、実際の処理を見て分かる通り、大抵のオブジェクト指向の文脈においては**オブジェクトの注入**と呼んだ方が分かりやすいと思う。