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
1. オブジェクトを集約・コンポジットできる(has-aの関係を表現できる)
  1. 集約
  1. コンポジション
1. 部分型(is-aの関係を表現できる)
  1. 継承
  1. ポリモーフィズム


# Goで各OOP原則を実践する
ここからは、実際にGoでOOP原則を表現していく。

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
上記のコードの`Human`に、ペットとして`Animal`型を所有させると、下記コードになる。

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
	Pets []Animal	// ペット
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
	p.Pets = []Animal{
		Animal { Name: "Robert", Age:50, Kind: "Dog"},
		Animal { Name: "Tama", Age:10, Kind: "Chicken"},
		Animal { Name: "Taro", Age:14, Kind: "Panda"},
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
	Human				// 国民は人間
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

### コンポジション

## 部分型(is-a)

### 継承

### ポリモーフィズム

#### ダックタイピング


