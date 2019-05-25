---
title: "Go言語ステップアップ(オブジェクト指向プログラミング)"
date:    2019-05-01T00:00:00+09:00
lastmod: 2019-05-01T00:00:00+09:00
draft: false
toc: true
tags: ["Go", "Golang", "OOP", "オブジェクト指向プログラミング", "ポリモーフィズム", "多態性"]
categories: ["Go"]
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

1. **カプセル化**(encapsulation)
1. **継承**(inheritance)
1. **多態性**(polymorphism)
  1. アドホック多態性(ad hoc polymorphism)
     1. 関数オーバーロード(function overloading)
     1. 演算子オーバーロード(operator overloading)
  1. パラメータ多態性(parameter polymorphism)
     1. ジェネリック関数(generic function)
     1. ジェネリックプログラミング(generic programming)
  1. サブタイプ多態性(subtyping)
     1. 仮想関数(virtual function)
     1. 動的ディスパッチ(dynamic dispatch)
     1. ダブルディスパッチ(double dispatch)
     1. 多重ディスパッチ(multiple dispatch)
1. メッセージパッシング(message passing)

# Go言語におけるOOP原則の定義
本稿では独断で、下記が表現できるプログラミングスタイルのことと定義する。  
実際、下記を表現していればオブジェクト指向プログラミングだと言って差し支えないと思う(思いたい)。

1. カプセル化
1. オブジェクトを集約・コンポジットできる(has-aの関係を表現できる)
  1. 集約
  1. コンポジション
1. ポリモーフィズム
  1. 部分型(is-aの関係を表現できる)

# Go言語で各OOP原則を実践する
ここからは、実際にGo言語でOOP原則を表現していく。

## カプセル化
カプセル化は、構造体とメソッドによって実現できる。  
下記のコードでは、`Human`のフィールドに名前(`Name`)、年齢(`Age`)、郵便番号(`ZipCode`)を持ち、さらに、メソッドに自己紹介(`Introduce`)を持つ。

```go
// 人間
type Human struct {
	Name string
	Age int
	ZipCode string
}

// Humanのメソッド
// 自己紹介する
func (h *Human) Introduce() {
	fmt.Printf("Hi, I'm %s. I'm %d years old.\n", h.Name, h.Age)
}

func main() {
	h := Human {
		Name: "John",
		Age: 3,
		ZipCode: "1234-5678",
	}
	h.Introduce()	// Humanの持つIntroduce()を呼ぶ
}
// => "Hi, I'm John. I'm 3 years old."
```

## 集約・コンポジション(has-a)
オブジェクトがオブジェクトを内包する(持つ)のも、構造体によって実現できる。  

### コンポジション
コンポジションは、１つの「部分」インスタンスが高々１つの「全体」インスタンスに持たれる。  
多重度をあえて記載すると、下図のとおり。

{{<figure src="composite.svg" alt="composite" width="400">}}

上記「カプセル化」のコードの`Human`に、ペットとして`Animal`型を所有させると、下記のとおり「コンポジション」を実現できる。

```go
package main

import (
	"fmt"
)

// 人間
type Human struct {
	Name string		// 名前
	Age int			// 年齢
	ZipCode string	// 郵便番号
	Pets []Animal	// ペット(コンポジション)
}

// Humanのメソッド
// 自己紹介する
// NOTE: レシーバは、型が(p *Human)型なので必ず参照渡しになる
func (p *Human) Introduce() {
	fmt.Printf("Hi, I'm %s. I'm %d years old.\n", p.Name, p.Age)
	p.Age += 1
}

// 動物
type Animal struct {
	Name string	// 名前
	Age int		// 年齢
	Kind string	// 種類
}

func main() {
	p := Human { Name: "John", Age: 30, ZipCode: "1234-5678" }
	p.Pets = []Animal {
		Animal { Name: "Robert", Age:50, Kind: "Dog" },
		Animal { Name: "Tama", Age:10, Kind: "Chicken" },
		Animal { Name: "Taro", Age:14, Kind: "Panda" },
	}

	// pが参照渡しとなることに注意
	// Introduce()にはpを参照渡しすることに注意
	p.Introduce()	// 1年目
	p.Introduce()	// 2年目

	// Humanが持っているペット達を紹介
	fmt.Println("I have pets")
	for _, pet := range p.Pets {
		fmt.Printf("  %s(%d), a %s,\n", pet.Name, pet.Age, pet.Kind)
	}
	fmt.Println("that's all.")

	// そういえば今日はRobertの誕生日
	p.Pets[0].Age += 1
	fmt.Printf("%s \"Hey, I turned %d today.\"\n", p.Pets[0].Name, p.Pets[0].Age)
}
```


```bash
Hi, I'm John. I'm 30 years old.
Hi, I'm John. I'm 31 years old.
I have pets
  Robert(50), a Dog,
  Tama(10), a Chicken,
  Taro(14), a Panda,
that's all.
Robert "Hey, I turned 51 today."
```

上記のコンポジションの方法を用いて、擬似的な継承を実現できる。  
継承まがいのコードを雑に書いても良いなら、下記コードのように構造体に対して無名フィールドのコンポジションを使えば良い。

```go
package main

import (
	"fmt"
)

// 人間
type Human struct {
	Name string		// 名前
	Age int			// 年齢
	ZipCode string	// 郵便番号
}

// Humanのメソッド
// 自己紹介する
func (p *Human) Introduce() {
	fmt.Printf("Hi, I'm %s. I'm %d years old.\n", p.Name, p.Age)
}

// 国民
type People struct {
	Human				// 国民は人間(擬似的な継承)
	Nationality string	// 国籍
	Income int			// 収入
}

// 国民のメソッド
// 納税する
func (p *People) PayTax() int {
	if p.Age <= 20 {
		return 0
	} else {
		return int(float64(p.Income) * 0.5)
	}
}

func main() {
	p1 := People {
		Human: Human { Name: "John", Age: 30, ZipCode: "1234-5678" },
		Nationality: "Japan",
		Income: 100,
	}
	p1.Introduce()
	fmt.Printf("It's too hard for me to pay %d for Tax >_<\n", p1.PayTax())

	fmt.Println()

	p2 := People {
		Human: Human { Name: "Bob", Age: 3, ZipCode: "1234-1234" },
		Nationality: "Spain",
		Income: 10000,
	}
	p2.Introduce()
	fmt.Printf("It's too hard for me to pay %d for Tax >_<\n", p2.PayTax())
}
```

```bash
Hi, I'm John. I'm 30 years old.
It's too hard for me to pay 50 for Tax >_<
Hi, I'm Bob. I'm 3 years old.
It's too hard for me to pay 0 for Tax >_<
```

### 集約
集約は、複数の「全体」インスタンスが１つの「部分」インスタンスを共有する可能性がある。  
多重度をあえて記載すると、下図のとおり。

{{<figure src="aggregate.svg" alt="aggregate" width="400">}}

上記「コンポジション」のコードの`Human`に、ペットとして`Animal`のポインタ型を所有させると、下記のとおり「集約」を実現できる。

```go
package main

import (
	"fmt"
)

// 人間
type Human struct {
	Name string		// 名前
	Age int			// 年齢
	ZipCode string	// 郵便番号
	Pets []*Animal	// ペット(集約)
}

// Humanのメソッド
// 自己紹介する
// NOTE: レシーバは、型が(p *Human)型なので必ず参照渡しになる
func (p *Human) Introduce() {
	fmt.Printf("Hi, I'm %s. I'm %d years old.\n", p.Name, p.Age)
	p.Age += 1
}

// 動物
type Animal struct {
	Name string	// 名前
	Age int		// 年齢
	Kind string	// 種類
}

func main() {
	p1 := Human { Name: "John", Age: 30, ZipCode: "1234-5678" }
	p2 := Human { Name: "Bob", Age: 100, ZipCode: "1111-1111" }

	Pets := []*Animal {
		&Animal { Name: "Robert", Age:50, Kind: "Dog" },
		&Animal { Name: "Tama", Age:10, Kind: "Chicken" },
		&Animal { Name: "Taro", Age:14, Kind: "Panda" },
	}

	p1.Pets = Pets

	// Introduce()にはp1を参照渡しすることに注意
	p1.Introduce()	// 1年目
	p1.Introduce()	// 2年目

	// Humanが持っているペット達を紹介
	fmt.Println(p1.Name, ": I have pets")
	for _, pet := range p1.Pets {
		fmt.Printf("  %s(%d), a %s,\n", pet.Name, pet.Age, pet.Kind)
	}
	fmt.Println("that's all.")

	// そういえば今日はRobertの誕生日
	p1.Pets[0].Age += 1
	fmt.Printf("%s \"Hey, I turned %d today.\"\n", p1.Pets[0].Name, p1.Pets[0].Age)

	// p1のペットをp2にも所有させる(複数の親が同一インスタンスを所有=集約)
	p2.Pets = Pets

	// Introduce()にはp2を参照渡しすることに注意
	p2.Introduce()	// 1年目
	p2.Introduce()	// 2年目

	// Humanが持っているペット達を紹介
	fmt.Println(p2.Name, ": I have pets")
	for _, pet := range p2.Pets {
		fmt.Printf("  %s(%d), a %s,\n", pet.Name, pet.Age, pet.Kind)
	}
	fmt.Println("that's all.")

	// そういえば今日はRobertの誕生日
	p2.Pets[0].Age += 1
	fmt.Printf("%s \"Hey, I turned %d today.\"\n", p2.Pets[0].Name, p2.Pets[0].Age)
}
```

```bash
Hi, I'm John. I'm 30 years old.
Hi, I'm John. I'm 31 years old.
John : I have pets
  Robert(50), a Dog,
  Tama(10), a Chicken,
  Taro(14), a Panda,
that's all.
Robert "Hey, I turned 51 today."
Hi, I'm Bob. I'm 100 years old.
Hi, I'm Bob. I'm 101 years old.
Bob : I have pets
  Robert(51), a Dog,
  Tama(10), a Chicken,
  Taro(14), a Panda,
that's all.
Robert "Hey, I turned 52 today."
```

## ポリモーフィズム
ポリモーフィズムには、アドホック多相、パラメータ多相、部分型付け等が存在する。  

### アドホック多相(Ad hoc polymorphism)
ある関数が、異なる型の引数に対してそれぞれ異なる実装を持つ性質。  
多重定義(オーバーロード)として多くのプログラミング言語でサポートされる。

オーバーロード
: 同一名(シンボル)を持つ関数あるいはメソッドおよび同一の演算子記号について複数定義し、利用時にプログラムの文脈に応じて選択する。  
例えば異なるシグネチャで同一名の関数を定義する。

#### 例: C#によるオーバーロード

```cs
class TypeA
{
	// 2乗する(int型version)
	public int Square(int i)
	{
		return i * i;
	}

	// 2乗する(float型version)
	public int Square(double f)
	{
		return (int)(f * f);
	}
}

void Main()
{
	var obj = new TypeA();
	Console.WriteLine(obj.Square(3));	// 9
	Console.WriteLine(obj.Square(2.5));	// 6
}
```

### パラメータ多相(Parametric polymorphism)
コードが特定の型を指定せずに書かれることで、さまざまな型に対して透過的に使用できる性質。  
ジェネリクスとして多くのプログラミング言語でサポートされる。

#### 例: C#によるジェネリクス

```cs
class TypeA
{
	// IComparableを満たす任意の型に対して最大値を返す
	public T Max<T>(T a, T b) where T : IComparable
	{
		return a.CompareTo(b) > 0 ? a : b;
	}
}

void Main()
{
	var obj = new TypeA();
	Console.WriteLine(obj.Max<int>(1, 2));	// 2
}
```

### 部分型付け(Subtyping)
部分型付けとも。共通の上位型をもつ複数の型を、1つの名前で扱う性質。  
これにより、**部分型(is-a)**を実現できる。  
大抵のオブジェクト指向言語では、サブクラス化(継承)によって部分型多相を提供する。

#### 例: C#による継承とオーバーライド

```cs
class Type
{
	virtual public string Name()
	{
		return "Type";
	}
}

class Subtype : Type
{
	override public string Name()
	{
		return "Subtype";
	}
}

void Main()
{
	Type obj;
	obj = new Subtype();
	Console.WriteLine(obj.Name());	// "Subtype"
}
```

### Go言語における部分型(is-a)
Go言語には継承の機能がない。したがって、継承ではなく、インターフェース実装によって部分型を実現する必要がある。  
Go言語には`interface{}`型があるため、これを使用する。

#### Go言語によるインターフェース実装

- Humanインターフェース

```go
type Human interface {
	talk() string	// 話せる
}
```

- Person: Humanインターフェースを満たす

```go
type Person struct {}
func (*Person) talk() string {
	return "I am a person."
}
```

- Male: Humanインターフェースを満たす

```go
type Male struct {}
// MaleでPersonのtalk()をオーバーライド
func (*Male) talk() string {
	return "I am a male."
}
```

- Female: Humanインターフェースを満たす

```go
type Female struct {}
// FemaleでPersonのtalk()をオーバーライド
func (*Female) talk() string {
	return "I am a female."
}
```

- NewGender: Humanインターフェースを満たす

```go
// NewGender.talk()でNewGender.Person.talk()を呼び出せる
type NewGender struct{
	Person
}
```

- NewNewGender: Humanインターフェースを満たさない

```go
// talk()を実装していない
type NewNewGender struct{}
```

- NotGender: Humanインターフェースを満たさない

```go
// 異なるシグネチャのtalk()を実装している
type NotGender struct{}
func (*NotGender) talk() (string, string) {
	return "I am not a gender.", "not error"
}
```

- NotGender: Humanインターフェースを満たすが**talk()を呼び出すとPanic発生する**

```go
type MaybePerson struct {
	// MaybePerson.talk()というアクセスが可能となる
	// ただし、MaybePersonにはtalk()が実装されていないため、呼び出し時にPanic発生する
	Human
}
```

- 上記構造体・インターフェースの動作確認コード
  - 構造体ごとに型アサーションしつつ`talk()`を呼び出す

```go
package main

import (
	"fmt"
)

func main() {
	fmt.Println("--- Person ---")
	var person interface{} = new(Person)
	if value, ok := person.(Human); ok {
		fmt.Println(" ", value.talk())
	} else {
		fmt.Printf("  Person: %T\n", value)
	}

	fmt.Println("--- Male ---")
	var male interface{} = new(Male)
	if value, ok := male.(Human); ok {
		fmt.Println(" ", value.talk())
	} else {
		fmt.Printf("  Male: %T\n", value)
	}

	fmt.Println("--- Female ---")
	var female interface{} = new(Female)
	if value, ok := female.(Human); ok {
		fmt.Println(" ", value.talk())
	} else {
		fmt.Printf("  Female: %T\n", value)
	}

	fmt.Println("--- NewGender ---")
	var newgen interface{} = new(NewGender)
	if value, ok := newgen.(Human); ok {
		fmt.Println(" ", value.talk())
	} else {
		fmt.Printf("  NewGender: %T\n", value)
	}

	fmt.Println("--- NewNewGender ---")
	var newnewgen interface{} = new(NewNewGender)
	if value, ok := newnewgen.(Human); ok {
		fmt.Println(" ", value.talk())
	} else {
		fmt.Printf("  NewNewGender: %T\n", value)
	}

	fmt.Println("--- NotGender ---")
	var notgen interface{} = new(NotGender)
	if value, ok := notgen.(Human); ok {
		fmt.Println(" ", value.talk())
	} else {
		fmt.Printf("  NotGender: %T\n", value)
	}

	// !! this calling causes Panic !!
	fmt.Println("--- MaybePerson ---")
	var maybe interface{} = new(MaybePerson)
	if value, ok := maybe.(Human); ok {
		fmt.Println(" ", value.talk())
	} else {
		fmt.Printf("  MaybePerson: %T\n", value)
	}
}
```

- 実行結果

```bash
> go run .\test.go
--- Person ---
  I am a person.
--- Male ---
  I am a male.
--- Female ---
  I am a female.
--- NewGender ---
  I am a person.
--- NewNewGender ---
  NewNewGender: <nil>
--- NotGender ---
  NotGender: <nil>
--- MaybePerson ---
panic: runtime error: invalid memory address or nil pointer dereference
[signal 0xc0000005 code=0x0 addr=0x18 pc=0x493bc9]

goroutine 1 [running]:
main.(*MaybePerson).talk(0xc000050200, 0x4ac700, 0xc000050200)
        <autogenerated>:1 +0x39
main.main()
        C:/Users/username/test.go:112 +0x82e
exit status 2
```

#### Go言語とダックタイピングの関係
動的型付け言語におけるオブジェクトの型の考え方の一つで、振る舞い(メソッドの有無)によって型を特定する。
ダックタイピングの説明は、Wikipediaの概要が分かりやすい。

- [Wikipedia - ダック・タイピング](https://ja.wikipedia.org/wiki/%E3%83%80%E3%83%83%E3%82%AF%E3%83%BB%E3%82%BF%E3%82%A4%E3%83%94%E3%83%B3%E3%82%B0)

> ダック・タイピング(duck typing)とは、Smalltalk、Perl、Python、Rubyなどのいくつかの動的型付けオブジェクト指向プログラミング言語に特徴的な型付けの作法のことである。  
> それらの言語ではオブジェクト(変数の値)に何ができるかはオブジェクトそのものが決定する。  
> これによりポリモーフィズム(多態性)を実現することができる。  
> つまり、静的型付け言語であるJavaやC#の概念で例えると、**オブジェクトがあるインタフェースのすべてのメソッドを持っているならば、たとえそのクラスがそのインタフェースを宣言的に実装していなくとも、オブジェクトはそのインタフェースを実行時に実装しているとみなせる**。

Go言語では、とある`interface{}`型で要求するメソッドを全て実装していれば、例えばJavaにおける`implements`のようにキーワードで明示しなくとも、その`interface{}`型を実装したことになる。  
すなわち、これがGo言語におけるダックタイピング。

##### C#の`dynamic`型によるダックタイピング
Go以外の静的言語でも、実行時の型情報を自己参照することによってダックタイピングは可能。  
下記は、C#の`dynamic`型(内部的にはリフレクション機能)を用いたダックタイピングの例(Wikipediaから引用)。

```cs
class Duck
{
	public string Sound()
	{
		return "quack";
	}
}

class Cat
{
	public string Sound()
	{
		return "myaa";
	}
}

public class DuckTypingTest
{
	static void Test(dynamic obj)
	{
		// objがSound()メソッドを持つかは実行時に調べられる
		Console.WriteLine(obj.Sound());
	}

	public static void Main()
	{
		Test(new Duck());
		Test(new Cat());
	}
}
```

----

## Go言語で継承っぽい事をやる方法
Go言語には継承がないと書いたが、継承らしいことを実現できなくはないらしい。

 {{< tweet 670415696656883713 >}}

内容が凄いコードだったので、下記にコメントを補いつつ示す。

```go
package main

// すくなくとも基本型が実装すべきインターフェース
type Worker interface {
	Do()
}

// 基本型
type Base struct {
	// 派生型をコンポジションするための匿名メンバ
	Worker
}

// 基本型のメソッド
func (b *Base) Do() {
	if b.Worker != nil {
		// bの内包する派生型がWorkerを満たす(Do()がある)場合、
		// 派生型の実装を呼び出す
		b.Worker.Do()
	} else {
		// 基本型のメソッドの実装
		println("Base.Do")
	}
}

// 派生型
type Derived struct {
	// 基本型をコンポジションするための匿名メンバ
	// これがあると、Do()が有るか検査される時にBaseの方も確認されるから、
	// 常にWorkerを満たすとみなされる
	Base
}

/* このコメントアウトを外せば Derived.Do が呼ばれる(オーバーライド)
func (d *Derived) Do() {
	println("Derived.Do")
}
*/

// 基本型を生成し、さらに派生型を内包して返す
func NewBase(w Worker) Worker {
	return &Base{w}
}

func main() {
	d := NewBase(new(Derived))
	d.Do()
}
```

・・・ツイートで既に非推奨だと言われている通り、上記のコードは複雑すぎて実用性は無いと思う。  
基本型から派生型の`Do()`をダックタイピングしたい(C#のdynamic型の様に)ならこの方法になると思うが、オーバーライドするだけなら下記のコードで事足りるはず。

```go
package main

import (
	"fmt"
)

type Worker interface {
	Do()
}

type Base struct {}

func (b *Base) Do() {
	fmt.Println("Base.Do")
}

type Derived struct {
	Worker
}

/*
func (d *Derived) Do() {
	fmt.Println("Derived.Do")
}
*/

func NewDerived() Worker {
	return &Derived { Worker: new(Base) }
}

func main() {
	b := new(Base)
	fmt.Printf("%T\n", b)
	b.Do()
	
	d := NewDerived()
	fmt.Printf("%T\n", d)
	d.Do()
}
```

- 実行結果: コメントアウトした状態

```bash
*main.Base
Base.Do
*main.Derived
Base.Do
```

- 実行結果: コメントアウトを外した状態

```bash
*main.Base
Base.Do
*main.Derived
Derived.Do
```