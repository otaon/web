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

# Go言語におけるOOP原則の定義
本稿では独断で、下記が表現できるプログラミングスタイルのことと定義する。  
実際、下記を表現していればオブジェクト指向プログラミングだと言って差し支えないと思う(思いたい)。

1. カプセル化
1. オブジェクトを集約・コンポジットできる(has-aの関係を表現できる)
  1. 集約
  1. コンポジション
1. 部分型(is-aの関係を表現できる)
  1. 継承
  1. ポリモーフィズム


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

// =>
// Hi, I'm John. I'm 30 years old.
// Hi, I'm John. I'm 31 years old.
// I have pets
//   Robert(50), a Dog,
//   Tama(10), a Chicken,
//   Taro(14), a Panda,
// that's all.
// Robert "Hey, I turned 51 today."
//
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
// =>
// Hi, I'm John. I'm 30 years old.
// It's too hard for me to pay 50 for Tax >_<
//
// Hi, I'm Bob. I'm 3 years old.
// It's too hard for me to pay 0 for Tax >_<
//
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
// =>
// Hi, I'm John. I'm 30 years old.
// Hi, I'm John. I'm 31 years old.
// John : I have pets
//   Robert(50), a Dog,
//   Tama(10), a Chicken,
//   Taro(14), a Panda,
// that's all.
// Robert "Hey, I turned 51 today."
// Hi, I'm Bob. I'm 100 years old.
// Hi, I'm Bob. I'm 101 years old.
// Bob : I have pets
//   Robert(51), a Dog,
//   Tama(10), a Chicken,
//   Taro(14), a Panda,
// that's all.
// Robert "Hey, I turned 52 today."
```

## 部分型(is-a)
部分型は、オブジェクト指向におけるサブタイプとして知られている。  
ここで、Go言語における部分型の扱いを理解するために、継承・ポリモーフィズムについて考える。

### 継承
Go言語には継承はない。つまり、親クラスがあって、子クラスがその親クラスの属性と振る舞いを使用する、という機構はサポートされていない。  
ただし、トリッキーな方法で継承らしきものは実現できる(本稿末を参照)。

### ポリモーフィズム
ポリモーフィズムには、アドホック多相(オーバーロード)、パラメータ多相(ジェネリクス)、部分型付け等、複数種類が存在する。  
このうち、**部分片付け**は、メソッド呼び出しの際に、そのレシーバの型において実装されたメソッドを実行することを意味する。


```go
// Human型
type Human interface {
	// 話せる
	talk() string
	// 聞ける
	listen(string) string
}

type Person struct {
	// Humanインターフェースを実装したことになり、Person.talk()というアクセスが可能となる
	// ただし、Personにtalk()およびlisten()を実装しなければ、呼び出し時にパニック発生する。
	Human
}

type Male struct {
	Person
}

func (*Male) talk() string {

}

func (*Male) listen() string {

}

type Female struct {
	Person
}

func (*Female) talk() string {

}

func (*Female) listen() string {

}

```


#### ダックタイピング
動的型付け言語におけるオブジェクトの型の考え方の一つで、振る舞い(メソッドの有無)によって型を規定する。  
wikipediaの概要が分かりやすい。

- [wikipedia - ダック・タイピング](https://ja.wikipedia.org/wiki/%E3%83%80%E3%83%83%E3%82%AF%E3%83%BB%E3%82%BF%E3%82%A4%E3%83%94%E3%83%B3%E3%82%B0)

> ダック・タイピング（duck typing）とは、Smalltalk、Perl、Python、Rubyなどのいくつかの動的型付けオブジェクト指向プログラミング言語に特徴的な型付けの作法のことである。  
> それらの言語ではオブジェクト（変数の値）に何ができるかはオブジェクトそのものが決定する。  
> これによりポリモーフィズム（多態性）を実現することができる。  
> つまり、静的型付け言語であるJavaやC#の概念で例えると、オブジェクトがあるインタフェースのすべてのメソッドを持っているならば、たとえそのクラスがそのインタフェースを宣言的に実装していなくとも、オブジェクトはそのインタフェースを実行時に実装しているとみなせる。

Go言語では、インターフェースによってダックタイピングできるように設計されている。  
このインターフェースが要求したメソッドをすべて実装してさえれば、インターフェースを実装していることを明示しなくとも、その型は当該インターフェースであるとみなされる。
これが、振る舞い(メソッドの有無)によって型が規定されるという意味。


## golang で継承っぽい事をやる方法

 {{< tweet 670415696656883713 >}}

```go

package main

type Worker interface {
	Do()
}

type Base struct {
	Worker
}

func (b *Base) Do() {
	if b.Worker != nil {
		b.Worker.Do()
	} else {
		println("Base.Do")
	}
}

type Derived struct {
	Base
}

/* このコメントアウトを外せば Derived.Do が呼ばれる
func (d *Derived) Do() {
	println("Derived.Do")
}
*/

func NewBase(w Worker) Worker {
	return &Base{w}
}

func main() {
	d := NewBase(new(Derived))
	d.Do()
}
```