---
title: "Go言語入門(スコープ)"
date:    2019-04-13T14:00:00+09:00
lastmod: 2019-04-13T14:00:00+09:00
draft: false
toc: true
tags: ["Go", "Golang"]
categories: ["Go"]
authors:
- otaon
---

# 目的
Go言語の基本構文を残す。

# 参考文献
- [スターティング Go言語](https://www.shoeisha.co.jp/book/detail/9784798142418)

# 基本構文
## スコープ
Goにおいて、定義済み識別子以外の構成要素はパッケージに属する。

### パッケージのスコープ
- **識別子の1文字目が大文字**の定数、変数、関数は、他のパッケージから参照できる。
- **識別子の1文字目が小文字**の定数、変数、関数は、自分のパッケージからしか参照できない。
- **識別子の1文字目が大文字小文字の区別のない文字(日本語など)**の定数、変数、関数は、自分のパッケージからしか参照できない。

```go
package foo

// 定数
const (
	A = 1		// 先頭大文字・・・他のパッケージから参照できる
	abc = "abc"	// 先頭小文字・・・他のパッケージから参照できない
)

// パッケージ変数
var (
	m = 256	// 先頭小文字・・・他のパッケージから参照できない
	N = 512	// 先頭大文字・・・他のパッケージから参照できる
)

// public関数
func DoSomething() {
	// ...
}

// private関数
func doSomething() {
	// ...
}
```

```go
package bar

import (
	"foo"
)

foo.A	// 1
foo.abc	// コンパイルエラー

foo.N	// 512
foo.m	// コンパイルエラー
```

### いろいろな`import`方法
インポートする際に、別名を指定できる。  
下記のコードでは、パッケージ`fmt`を`f`という別名で**上書き**している。  
したがって、下記の`main`パッケージでは、パッケージ`fmt`を`fmt`という名前で参照できない。

```go
package main

import (
	f "fmt"	// パッケージ"fmt"を、名前fとしてインポートする
)

func main() {
	f.Println("Hello, World!")	// パッケージ名をfとして参照できる
}
```

パッケージ名を省略するためには、別名に`.`を指定する。

```go
package main

import (
	"fmt"
	. "math"	// mathパッケージを、パッケージ名無しでインポート
)

func main() {
	fmt.Println(Pi)	// Piをmathパッケージ指定なしで参照できる
}
```

パッケージ名を省略した場合、そのパッケージが持つ識別子と名前衝突の危険がある。

```go
package main

import (
	. "math"	// mathパッケージを、パッケージ名無しでインポート
)

const (
	Pi	// math.Piと名前衝突するためコンパイルエラー
)
```

### `import`宣言のスコープはファイル単位
Goでは１つのパッケージを複数ファイルに分割して定義できる。しかし、`import`宣言は各々のファイル内でのみ有効。

- `main1.go`

```go
package main

import (
	f "fmt"
)

func main() {
	foo()
}
```

- `main2.go`

```go
package main

func foo() {
	fmt.Println("Hello, World!")	// fmtが解決できないためコンパイルエラー
}
```

### 関数のスコープ
Goにおいて、関数はスコープを形成する。すなわち、関数内で定義した変数や定数は、その関数内でのみ有効。  
また、関数の引数名と戻り値名も関数と同名の変数などを関数内部で定義(シャドーイング)することは不可能。

C言語のように`{}`を用いることで、スコープを定義することができる。

```go
func foo(a int) (b string) {
	{
		var a int
		const b = "string"
	}
	return b	// "" 戻り値の初期値
}
```

