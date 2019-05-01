---
title: "Go言語入門(インターフェース)"
date:    2019-04-13T19:00:00+09:00
lastmod: 2019-04-13T19:00:00+09:00
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
## インターフェース
ここでは`interface{}`型の本質と、その使用方法について述べる。

### インターフェースとは
インターフェースは型の一種で、任意の型が「どのようなメソッドを実装するべきか」を規定するための枠組み。
すなわち、一般的なプログラミング言語におけるインターフェースと同じ目的で使用される。

Goの型システムには階層構造がない。つまり、オブジェクト指向におけるスーパークラスとサブクラスの関係がない。
また、Goでは型から部分型への型変換ができない。  
しかし、インターフェースを使用することで、これらの問題を解決できる。

### インターフェース`error`
Goの組み込み型`error`はインターフェースとして定義されている。
インターフェースは、予約語`interface`を使って、`interface{メソッドのシグネチャの列挙}`という形式で定義する。  
メソッドのシグネチャの列挙では、、**型が実装すべきメソッドのシグネチャ**を任意の数だけ列挙する。

`error`型では、文字列を返すメソッド`Error`のみが定義されている。

```go
// errorインターフェースの構造体
type error interface {
	Error() string
}

// error型を使用した関数
func DoSomething() (int, error) {
	// ....省略....
}

// error型に共通する使用方法
_, err := DoSomething()
if err != nil {
	fmt.Println(err.Error())
}
```

Goでは、実体としてパッケージや型に応じて独自のエラー型が定義されているが、これらは`error`インターフェースによって隠蔽されている。  
このように`error`型の処理の仕方が統一されているおかげで、`error`型の使用者側は迷わずに使用できる。

次に、`error`型の実装例を確認する。  
コードを見て分かるとおり、この書き方では、独自定義型`MyError`が`error`の部分型かどうかの情報は、どこにもないことに注意すること。
つまり、ある型`foo`において、インターフェースが要求するインターフェースのいくつかが実装されていたとしても、`foo`がインターフェースを実装しているとは言い切れない。ましてや、`foo`はあるインターフェースの部分型とは言えない。

```go
// エラーを表す独自定義の型
type MyError struct {
	Message string
	ErrCode int
}

// errorインターフェースが要求するError()メソッドを実装
func (e *MyError) Error() string {
	return e.Message
}

func RaiseError() error {
	// MyError型をerror型として返す
	return MyError{Message: "エラーが発生しました", ErrorCode: 1234}
}

// 戻り値の実体はMyError型だが、error型として返ってきた
err := RaiseError()
// error型なのでErrorを使用できる
err.Error()
err.ErrCode	// error型なので見れない
```

`error`型の変数に対して、その実体の`MyError`独自のフィールドを参照することはできない。  
参照したい場合は、**型アサーション**を用いる。

```go
// 型アサーションで実体の型にキャストする
e, ok := err.(MyError)
if ok {
	e.ErrCode	// 1234
}
```

### インターフェースのメリット
最も有効な使用方法としては、`error`インターフェースのように**異なる型に共通の性質を付与する**使い方がある。

```go
// 文字列化することを示すインターフェース
type Stringify interface {
	ToString() string
}

// 構造体Person型
type Person struct {
	Name string
	Age int
}

func (p *Person) ToString() string {
	return fmt.Sprintf("%s(%d)", p.Name, p.Age)
}

// 構造体Car型
type Car struct {
	Number string
	Model string
}

func (c *Car) ToString() string {
	return fmt.Sprintf("[%s] %s", c.Number, c.Model)
}

// 異なる型を、共通のインターフェースにまとめる
vs := []Stringify {
	&Person {Nam: "Taro", Age: 21},
	&Car {Number: "XXX-01234", Model: "PX512"},
}
for _, v := range vs {
	fmt.Println(v.ToString())
}
// =>
// Taro(21)
// [XXX-0123] PX512
```

上記コードで`Person`と`Car`には共通の`ToString()`メソッドが存在する。  
これにより、下記部分で`Person`と`Car`は`Stringify`インターフェース型としてまとめられる。
したがって、`Stringify`インターフェース型のスライスに両者を代入できる。

```go
// 異なる型を、共通のインターフェースにまとめる
vs := []Stringify {
	&Person {Nam: "Taro", Age: 21},
	&Car {Number: "XXX-01234", Model: "PX512"},
}
```


### `fmt.Stringer`
`fmt`パッケージに定義されている`Stringer`型は、非常に単純なインターフェースで、文字列を返すメソッド`String`が定義されている。

```go
type Stringer interface {
	String() string
}
```

次のコードで定義されている`T`型のポインタを関数`fmt.Println()`にわたすと、`&{10 Taro}`のような文字列が出力される。

```go
type T struct {
	Id int
	Name string
}

t := &T{Id: 10, Name: "Taro"}
fmt.Println(t)	// => "&{10 Taro}"
```

`fmt`パッケージの関数は、様々なデータ型を`interface{}`型として受け取り、それを読みやすい形式に変換する。
`fmt.Stringer`インターフェースを使用すると、任意の型の文字列表現をカスタマイズできる。

下記コードのように、任意の型に対するレシーバとして、`String()`メソッドを定義すると、`fmt.Println()`内部において、その定義した`String()`メソッドが使用される。
すなわち、`fmt`パッケージは、`fmt.Stringer`インターフェースが要求するメソッドを実装した型であれば、メソッド`String`が返す文字列を出力などに使用する。

```go
func (t *T) String() string {
	return fmt.Sprintf("<<%d, %s", t.Id, t.Name)
}

t := &T{Id: 10, Name: "Taro"}
fmt.Println(t)	// => "<<10, Taro>>"
```

### インターフェースが定義するメソッドのアクセシビリティ
インターフェースに定義されたメソッドは、構造体型などと同様に、メソッドの名の先頭が大文字だとパッケージ外から参照可能となり、小文字だとパッケージ外から参照不可となる。  
インターフェースのメソッドを外部に隠蔽することで得られるメリットは通常無いため、原則的には外部から参照可能なメソッドのみを定義すべき。

```go
package foo

type I interface {
	Method1() string
	Method2()
}

type T struct {}

func (t *T) Method1() string {
	return "Method1()"
}

func (t *T) method2() string {
	return "method2()"
}

func NewI() I {
	return &T{}
}
```

```go
package main

t := foo.NewI()
t.Method1()	// OK
t.method2()	// コンパイルエラー
```

### インターフェースを含むインターフェースを定義する
下記のように、インターフェースを含むインターフェースを定義できる。  
メソッド名を重複させるとエラーとなるため、注意が必要。

```go
type I0 interface {
	Method0() int
}

type I1 interface {
	I0	// インターフェースI0を含む
	Method1() int
}

type I2 interface {
	I1	// インターフェースI1を含む
	Method2() int
}
```

### `interface{}`の本質とは
前の章で説明した`interface{}`は、Goの全ての方と互換性のある特殊な型。  
そして、`interface{}`とは、**からのインターフェース**を表す型。  
すなわち、実装すべきメソッドを1つも要求していないインターフェースを表す型。

構造体を定義する場合、`struct`による型定義は、必ずしも`type`によるエイリアス定義を必要とせず無名のままで使用できる。
`interface`も同様に、エイリアス定義を必要とせずに、型定義として使用できる。

次の例では、あらかじめ`type`を使用したインターフェースの型定義をせずに、関数`ShowId`の引数の型に直接`interface{GetId() int}`という型定義を行っている。  
構造体`T`型はインターフェース`interface {GetId() int}`が要求するメソッド`GetId() int`を実装しているため、関数`ShowId()`の引数として渡すことができる。

```go
// 構造体T型
type T struct{Id int}

// 構造体T型は、インターフェースinterface {GetId() int}が要求するメソッドGetId() intを実装する
func (t *T) GetId() int {
	return t.Id
}

func ShowId(id interface {GetId() int}) {
	// 引数idは、 GetId() int というメソッドを持つ型
	fmt.Println(id.GetId())
}

t := &T{Id: 19}
ShowId(t)	// => "19"
```
