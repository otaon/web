---
title: "Go言語入門(構造体・メソッド)"
date:    2019-04-13T18:00:00+09:00
lastmod: 2019-04-13T18:00:00+09:00
draft: false
tags: ["Go", "Golang"]
categories: ["Notes"]
authors:
- otaon
---

# 目的
Go言語の基本構文を残す。

# 参考文献
- [スターティング Go言語](https://www.shoeisha.co.jp/book/detail/9784798142418)

# ポインタ・構造体・インターフェース
## 構造体
Goにおける構造体は、C言語と同様に、データと手続きを一体化するためのデータ構造のこと。  
Goでは、構造体を、Javaなどにおけるクラスやオブジェクトのような目的で使用することになる。  
ちなみに、後述する「インターフェース」もGoの構造体に柔軟性を与えるために使用される。

### `type`で型定義する
構造体では、予約語`type`を使用して、任意の既存の型に対してエイリアス(別名)を定義する。そこで、まずは`type`による型定義の使用方法について説明する。
書式は「`type 定義する型 既存の型`」となる。

```go
type MyInt int

// MyInt型の変数n1を定義(初期値5)
var n1 MyInt = 5

// 変数n2に、int型のリテラル7をMyInt型に型変換して代入
n2 := MyInt(7)

fmt.Println(n1)	// => 5
fmt.Println(n2)	// => 7

// typeによる様々なエイリアスの例
// type(...)の形で、まとめて型名を定義できる
type (
	IntPair [2]int
	Strings []string
	AreaMap map[string][2]float64
	IntsChannel chan []int
)

pair := IntPair{1, 2}	// [2]int{1, 2}
strs := Strings{"Apple", "Banana", "Cherry"}	// []string
amap := AreaMap{"Tokyo", {35.6789, 123.456}}	// map[string][2]float64
ich := map(IntsChannel)	// chan []int

// 関数型にエイリアスを定義する事もできる
// 型名で明確な意味付けをする
type Callback func(i int) int

func Sum(ints []int, callback Callback) int {
	var sum int
	// 引数intsの各要素を足す => 15
	for _, i := range ints {
		sum += i
	}
	// int型の引数を取ってint型の戻り値返すコールバック関数をコール
	// callback(15) => 15 * 2 => 30
	return callback(sum)
}

func main() {
	n := Sum(
		[]int{1, 2, 3, 4, 5},
		func(i int) int {
			return i * 2
		},
	)
	// n == 30
}
```

#### エイリアス型の互換性
型の互換性については注意が必要。  
例えば、下記の通りに定義した型`T0`と型`T1`はいずれも`int`型へのエイリアス。  
このとき、型`T0`と型`T1`には互換性がない。つまり、同じ`int`型のエイリアスだが、全く別の型として認識される。

```go
type T0 int
type T1 int

t0 := T0(5)		// t0 == 5
i0 := T1(t0)	// i0 == 5

t1 := T1(8)		// t1 == 8
i2 := int(t1)	// i1 == 8

t0 = t1	// コンパイルエラー
```

### 構造体の定義
構造体を使用する前には、前述の通り`type`と組み合わせて新しい型を定義することになる。  
構造体は「`struct { フィールドの定義 }`」によって囲われた範囲で定義する。  
構造体定義と型定義の順番は、必ず「`struct`で定義された構造体に対して、`type`を使って新しい型名を与える」である必要がある。

`struct`の中には、任意の型を持つ「フィールド(field)」を任意の数だけ並べることができる。  
次の例に示す`Point`型では、`int`型の変数`X`と変数`Y`をフィールドに持つ。

```go
type Point struct {
	X int
	Y int
}

// 異なる定義方法
type Point struct {
	X, Y int
}
```

構造体のフィールドにアクセスするには、C言語と同様に`.`を用いる。

```go
var pt Point
pt.X	// 0
pt.Y	// 0

// 構造体のフィールドへ値を代入する
pt.X = 10
pt.Y = 8

// 構造体のフィールドを参照する
pt.X	// 10
pt.Y	// 8
```

### 複合リテラル
構造体型に書くフィールドの初期値を指定子つつ構造体を生成するための方法として「複合リテラル」が用意されている。
`{}`で囲んだ中に各フィールドの初期値を列挙して、それを構造体の変数に代入できる。初期値を並べる順番は、フィールドが定義された順序と同じ。

```go
pt := Point {1, 2}
pt.X	// 1
pt.Y	// 2
```

構造体を各フィールドの初期値を上記のように指定して生成する書き方は、フィールドの定義順序を意識せざるを得ないため、使い勝手が悪い。  
そこで、他言語のラベル付き引数のように「`フィールド: 値`」と記述することで、順序を気にせず初期値を与えることができる。  
また、任意の一部のフィールドのみ初期化することも可能となる。この場合、未初期化状態のフィールドは、その型の初期値が入る。

```go
pt := Point{X: 1, Y: 2}
pt.X	// 1
pt.Y	// 2

pt = Point{Y: 28}
pt.X	// 0
pt.Y	// 28
```

### フィールド定義の詳細な仕様について
構造体のフィールド名のルールは、変数や関数など他のGoの識別子と同様。  
つまり、UTF-8エンコーディングであれば、日本語フィールド名も使用できる。  
しかし、Goの慣例としては、**フィールド名は英大文字で始まりの英数字を用いるべき**。

```go
type Person struct {
	ID uint
	name string
	部署 string
}

p := Person{ID: 17, name: "ヤマメくん", 部署: "四万十川"}
```

Goにおいては、フィールド名の省略ができる。  
フィールド名を省略した場合は「`フィールド名=型名`」という定義であるとみなされる。  
基本型に対しては、このような仕様は大して利点は無いが、構造体に別の構造体を埋め込む場合(後述)には有効な機能となる。

```go
type T struct {
	int
	float64
	string
}

t := T{1, 3.14, "文字列"}
t.int		// 1
t.float64	// 3.14
t.string	// "文字列"
```

Goの構造体には、「無名フィールド(blank field)」を定義できる。フィールド名「`_`」を与えると、そのフィールドは無名フィールドになる。  
無名フィールドには名前のとおりフィールド名が存在しないため、参照や代入はできない。また、複合リテラルを使ったフィールド値の初期化もできない。  
無名フィールドは、構造体のメモリ領域のアライメント調整に使用できるらしい。

```go
type T struct {
	N uint	// 32bit確保
	_ int16	// 16bit確保
	S []string	// []string型の分のメモリ領域確保
}

t := T {
	N: 12,
	S: []string{"A", "B", "C"},
}
fmt.Println(t)	// "{12 0 [A B C]}"
```

### 構造体を含む構造体(構造体を入れ子にする)
構造体のフィールドに構造体を持たせるには、2種類の方法がある。  
それぞれの方法は使用目的が異なるため、仕組みを正しく理解するべき。

- フィールド名をつけて構造体を埋め込む
- フィールド名を省略して構造体を持たせる

#### フィールド名をつけて構造体を埋め込む
次の例では、`Feed`型の構造体を定義して、`Animal`型の構造体のフィールドに埋め込む定義を行っている。  
`Animal`型に埋め込む`Feed`型のフィールド名を、型と同じ`Feed`にしているが、これは問題無い。むしろ、実際のGoの構造体の定義ではよく見られる。  
このように、フィールド名を構造体名と同じにしておけば、複合リテラルによる初期化の際などに混乱せずに済む。

```go
type Feed struct {
	Name string
	Amount uint
}

type Animal struct {
	Name string
	Feed Feed	// Feed型の埋め込み
}

a := Animal {
	Name: "Monkey",
	Feed: Feed {
		Name: "Banana",
		Amount: 10,
	},
}

a.Name			// "Monkey"
a.Feed.Name		// "Banana"
a.Feed.Amount	// 10

a.Feed.Amount = 15
a.Feef.Amount	// 15
```

#### フィールド名を省略して構造体を埋め込む
次の例では前述とは違い、`Animal`型に埋め込む`Feed`型のフィールド名を省略して定義している。  
このとき、`Feed`が持つ各フィールドにアクセスする方法が前述と異なり、あたかも`Animal`が持つフィールドのようにアクセスできる。  

構造体内の構造体にも同名のフィールドが定義されている場合(この場合`Name`が同名)、そのフィールドは外側の構造体の側のものを指す。
したがって、構造体内の構造体のフィールドにアクセスするには、前述のように`Animal.Feed.Name`として、構造体名を明示して指定する必要がある。

```go
type Feed struct {
	Name string
	Amount uint
}

type Animal struct {
	Name string
	Feed	// フィールド名を省略
}

a := Animal {
	Name: "Monkey",
	Feed: Feed {
		Name: "Banana",
		Amount: 10,
	},
}
a.Amount	// 10
a.Amount = 15
a.Amount	// 15
a.Feed.Amount	// 15 (== a.Amount)
```

**Note**  
この、構造体を入れ子にする方法を単純に使用しても、部分型が表現できないため、オブジェクト指向における**継承**は表現できない。  
これは、例えば、下記の場合を考えることによって分かる。  
派生クラスの構造体`SubClass`の中に、基本クラスの構造体`BaseClass`をフィールド名を省略して定義したとする。このとき、確かに`SubClass.FieldName`とすることで、`BaseClass.FieldName`にアクセスできる。  
しかし、この例において構造体`SubClass`は`BaseClass`の部分型ではないため、`BassClass`の`SubClass`への代入はコンパイルエラーとなってしまう。

```go
type BassClass struct {
	Name string
	Kind string
}

type SubClass struct {
	Name string
}

sub := BassClass {
	Name: "John",
	BassClass: BassClass {
		Name: "Ape",
		Kind: "Mammals",
	},
}

sub.Name			// "John"
sub.BassClass.Name	// "Ape"
sub.Kind			// "Mammals"


var sub1 SubClass
var bass1 BaseClass
sub1 = bass1	// コンパイルエラー
```

##### 暗黙的なフィールド定義の注意点
###### 定義されるフィールド名の決まり
暗黙的にフィールド定義する際は、ポインタ型を埋め込む場合に注意する必要がある。  
ポインタ型の修飾子やパッケージのプリフィックス部分を埋め込む場合の暗黙的なフィールド名は、それらの修飾を取り除いたものになる。

- `T1`...T1型。
- `*T2`...T2型。フィールド名から`*`が取り除かれる。
- `packageFoo.T3`...別パッケージのT3型。フィールド名から`packageFoo.`が取り除かれる。
- `*packageFoo.T4`...別パッケージのT4型のポインタ型。フィールド名から`*packageFoo`が取り除かれる。

```go
struct {
	T1				// フィールド名は「T1」
	*T2				// フィールド名は「T2」	// ポインタのデリファレンスは無視
	packageFoo.T3	// フィールド名は「T3」	// パッケージのプリフィックスは無視
	*packageFoo.T4	// フィールド名は「T4」	//  T2とT3の複合パターン
}
```

##### 再帰的な定義は禁止
構造体のフィールドに、自身の方を含むような再帰的な定義、または、相互再帰(循環する定義)は、コンパイルエラーとなる。

```go
type T struct {
	T	// コンパイルエラー
}

type T0 struct {
	T1
}

type T1 struct {
	T0	// T0はこの構造体T1を持つためコンパイルエラー
}
```

### 無名の構造体型
`struct { フィールド定義 }`という、構造体型そのものを型として利用できる。  
このように定義した構造体と、`type`によって定義された構造体とには互換性がある。

```go
func showStruct(s struct{X, Y int}) {
	fmt.Println(s)
}

// 無名の構造体型
s:= struct{X, Y int}{X: 1, Y: 2}
showStruct(s)	// => "{1 2}"

type Point struct {
	X, Y int
}

p := Point{X: 3, Y: 8}
showStruct(p)	// => "{3 8}"
```

### 構造体とポインタ
構造体のポインタ型を定義したら、通常は`*p.FieldName`として参照できる。  
また、アスタリスクを省略して`p.FieldName`としても参照できる。

構造体を関数へ参照渡しするためには、構造体型へのポインタを使用する。

```go
type Point struct {
	X, Y int
}

// 値渡しなのでswapできない
func swap(p Point) {
	// フィールドX, Yの値を入れ替える
	x, y := p.Y, p.X
	p.X = x
	p.Y = y
}

p := Point{X: 1, Y: 2}
swap(p)	// 値渡し
p.X	// 1
p.Y	// 2

// 参照渡し
func swap(p *Point) {
	x, y := p.Y, p.X
	p.X = x
	p.Y = y
}

p := &Point{X: 1, Y: 2}	// Pointのポインタ型を定義
swap(p)	// 参照渡し
p.X	// 2
p.Y	// 1
```

### `new()`関数で指定子た型のポインタ型を生成する
指定した型のポインタ型を生成するために、組み込み関数`new()`が使用できる。

```go
type Person struct {
	Id int
	Name string
	Area string
}

// 変数pは*Person型
p := new(Person)

// *p.FieldName は p.FieldNameとも記述できる
p.Id	// 0
p.Name	// ""
p.Area	// ""
```

