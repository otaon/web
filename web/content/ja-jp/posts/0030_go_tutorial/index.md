---
title: "Go言語入門(環境構築・プログラム作成・パッケージ作成・実行・テスト実行)"
date:    2019-03-30T22:00:00+09:00
lastmod: 2019-03-30T22:00:00+09:00
draft: false
tags: ["Go", "Golang"]
categories: ["Notes"]
authors:
- otaon
---

# 目的
Go言語の学習記録として、基本的な文法などを残す。

# 参考文献
- [スターティング Go言語](https://www.shoeisha.co.jp/book/detail/9784798142418)

# 環境構築
## Goをインストールする
GoはWindows, Mac, Linuxで使用できる。  
今回はWindowsでのインストール方法を記載する。他の２つでのインストールはパッケージ管理システムを使えばすぐできる。

### Chocolateyを使ってGoをインストールする
最近ではWindowsにもまともなパッケージ管理システムがある。Chocolateyというものだ。  
パッケージの登録基準が甘いとか風の噂で聞いたが、とにかくデファクトスタンダードになりつつある。  
まずはこれをインストールする。

- [Chocolatey](https://chocolatey.org/)

インストールしたら、管理者権限でpowershellまたはコマンドプロンプトを起動する。  
下記のコマンドを実行して、Go言語をインストールする。

```cmd
choco install golang
```

既にインストール済みで、アップデートしたい場合は下記コマンドを実行する。

```cmd
choco upgrade golang
```

### 環境変数を設定する
chocolateyでのGo言語インストール後、下記のとおりに環境変数を設定する。  
各々の環境変数の役割を理解し、必要なら変更すること。

- `GOROOT = C:\Go`・・・Goのbinが格納されているフォルダの場所。
- `GOPATH = C:\work\go`・・・外部のライブラリが格納されているフォルダの場所。

もちろん、`Path`にはGOROOTと同じフォルダへのパスを通しておくこと。

- `Path = C:\Go\bin;....` (または`%GOROOT%\bin`)

**NOTE**  
自分自身のプロジェクトと、外部パッケージを混在させたくない場合、下記の通り設定すると良い。

- `GOPATH = C:\work\go;C:\Go`
  - `go get`コマンドによって外部パッケージが`C:\work\go`にダウンロードされる。
  - 自分自身のプロジェクト`C:\Go`が`go get`管理下になる。
- `Path = C:\work\go\bin;C:\Go\bin;....`
  - 外部パッケージと自分自身のプロジェクト両方の`bin`へのパスが通る。

### Go言語のコマンドが使えることを確認する
コマンドプロンプト等で下記コマンドを実行してバージョンが表示されれば、Go言語が使用可能となっていることが分かる。

```sh
$ go version
go version go1.11.1 windows/amd64
```

# Goプログラム作成・実行→パッケージ管理→テスト実行
## `Hello, World!`
何はともあれ、最初にHello, World!を実行する。

```go
package main

import (
	"fmt"
)

func main() {
	fmt.Println("Hello, World!")
}
```

これをファイル`hello.go`として保存し、下記コマンドを実行する。

```sh
$ go run hello.go
Hello, World!
```

このように、Goプログラムのファイルパスを渡して`go run`を実行するだけで、Goプログラムのビルドと実行を一緒に行ってくれる。

```sh
$ go run [ファイルパス+]
```

### パッケージ(`package`)
ここから、`Hello, World!`のコードについて説明する。

```go
package main
```

Goでは、**プログラムの全ての要素**が何らかのパッケージ(`package`)に属す必要がある。  
`package main`は、「このファイルが`main`パッケージに属す」という宣言だ。

**NOTE**  
Goでは、<u>**1つのファイルは必ず1つのパッケージにのみ属す**</u>という決まりがある。  
複数の`package`宣言があると、コンパイルエラーが発生する。

### インポート(`import`)
```go
import (
	"fmt"
)
```

`import`宣言で、ファイル内のプログラムで使用するパッケージを指定する。  
必要なパッケージが無ければ`import`宣言は省略可能。  
上記では文字列の入出力に使用する`fmt`パッケージをインポートしている。

**NOTE**  
指定したパッケージは、通常は環境変数`GOPATH`に指定したディレクトリ内のパッケージから探索される。  
<u>`"./animals"`のように相対パスで記述することで、この`import`が書かれているファイルから相対パス先のディレクトリを指定できる。</u>


**NOTE**  
使用していないパッケージを`import`すると、コンパイルエラーが発生する。  
<u>参照されていないパッケージを強制的にプログラム内に組み込む</u>必要がある場合、`_`を補うとコンパイルエラーを発生させない。

```go
package main

import (
	_ "fmt"	// _をつけると参照されないパッケージを取り込める
)

func main() {
	// fmtを使用しない
}
```

### エントリポイント`main()`
```go
func main() {
	fmt.Println("Hello, World!")
}
```

Goプログラムのエントリポイントは、`main`パッケージの`main()`関数だと定められている。

## プログラムのビルド
### ビルドコマンド`go build`
Goファイルを実行ファイル形式にコンパイルする。コンパイルすると、実行ファイルがGoプログラムと同じフォルダに生成される。

```sh
$ go build -o hello.exe hello.go
```

ディレクトリを指定したい場合はwindowsであっても`/`を区切り文字として使用する。

```sh
$ go build -o hello.exe hello/hello.go
```

Goファイルを指定しないと、ディレクトリ内の全てのGoファイルを対象にビルドする。  
また、`-o`で出力ファイル名を指定しないと、`ディレクトリ名.exe`という実行ファイルを出力する。

```sh
$ pwd
/c/go-tutorial/hello/
$ go build # hello.exeが生成される
```

### ビルド&実行コマンド`go run`
ビルドと実行を同時に行うには`go run`コマンドを使う。  

```sh
$ go run hello.go
```

**NOTE**  
`go run`コマンドを使うときは、ビルド対象ファイルを全て指定する必要がある。

## パッケージ構成
複数のGoファイルでプログラムを構成する。  
下記のように`zoo`というプログラムの構成を想定する。

```sh
zoo/	# zooアプリケーション
|- animals/	# animalsパッケージのためのディレクトリ
|  |- elephant.go
|  |- monkey.go
|  `- rabbit.go
|- main.go	# mainパッケージ
`- app.go	# mainパッケージ
```

`zoo/animals/elephant.go`

```go
package animals

func ElephantFeed() string {
        return "Grass"
}
```

`zoo/animals/monkey.go`

```go
package animals

func MonkeyFeed() string {
        return "Banana"
}
```

`zoo/animals/rabbit.go`

```go
package animals

func RabbitFeed() string {
        return "Carrot"
}
```

上記の`zoo/animals/*.go`は、下記のように一つのファイルにまとめた場合と同等。

```go
package animals

func ElephantFeed() string {
        return "Grass"
}

func MonkeyFeed() string {
        return "Banana"
}

func RabbitFeed() string {
        return "Carrot"
}
```

`zoo/main.go`

```go
package main

import(
        "fmt"
        "./animals"
)

func main() {
        // 同一パッケージmain内の関数AppNameの呼び出しにはパッケージ指定が不要
        fmt.Println(AppName())

        fmt.Println(animals.ElephantFeed())
        fmt.Println(animals.MonkeyFeed())
        fmt.Println(animals.RabbitFeed())
}
```

`zoo/app.go`

```go
package main

func AppName() string {
        return "Zoo Application"
}
```


上記ファイルをビルドするには、下記コマンドを実行する。

```sh
$ pwd
/c/go-tutorial/zoo/
$ go build
```

**NOTE**  
1つのディレクトリには、1つのパッケージ定義のみ存在可能。  
つまり、`zoo/`ディレクトリには`main`パッケージのGoファイルのみ存在可能。  
異なるパッケージは、異なるフォルダに分けて格納する必要がある。

## パッケージをテストする
Goにはパッケージのテストを行うための機能が標準で備わっている。  
<u>Goでは、ファイル名が`_test.go`で終わっているファイルをテストコードと認識する。</u>  
<u>また、1つのパッケージに対して、複数のテストコードを作成可能。</u>

### テストコードを作成する
上記で作成した`animals`パッケージをテストする`animals_test.go`ファイルの作成手順を説明する。

まず、テストに必要な標準パッケージ`testing`をインポートする。  
先頭に`Test`がつく関数がテスト関数となる。  
テスト関数内で、`t.Errorf(...)`と記述する。これが実行された場合、関数はテスト失敗のエラーを発生させる。

`animals_test.go`

```go
package animals

import(
        "testing"
)

func TestElephantFeed(t *testing.T) {
        expect := "Grass"
        actual := ElephantFeed()

        if expect != actual {
                t.Errorf("%s != %s", expect, actual)
        }
}

func TestMonkeyFeed(t *testing.T) {
        expect := "Banana"
        actual := MonkeyFeed()

        if expect != actual {
                t.Errorf("%s != %s", expect, actual)
        }
}

func TestRabbitFeed(t *testing.T) {
        expect := "Carrot"
        actual := RabbitFeed()

        if expect != actual {
                t.Errorf("%s != %s", expect, actual)
        }
}
```

### テストコードを実行する
`go test`コマンドに、テスト対象パッケージのディレクトリを指定し、テスト実行する。  
ディレクトリを指定しなければ、カレントディレクトリのパッケージを対象としてテスト実行する。

```sh
$ pwd
/c/go-tutorial/zoo/
$ go test ./animals
ok      /c/go-tutorial/zoo/animals   0.127s
```

`-v`オプションを付けることで、より詳細なテスト結果を出力できる。

```sh
$ go test -v ./animals
=== RUN   TestElephantFeed
--- PASS: TestElephantFeed (0.00s)
=== RUN   TestMonkeyFeed
--- PASS: TestMonkeyFeed (0.00s)
=== RUN   TestRabbitFeed
--- PASS: TestRabbitFeed (0.00s)
PASS
ok      /c/go-tutorial/zoo/animals   0.124s
```
