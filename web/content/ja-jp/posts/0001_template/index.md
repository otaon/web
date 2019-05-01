---
title: "テンプレート"
date: 2019-03-01T16:01:23+09:00
lastmod: 2019-03-01T16:01:23+09:00
draft: false
tags: ["template"]
categories: ["Notes"]
resources:
- name: header
  src: 'header.jpg'
authors:
- otaon
---

記事作成用テンプレート。

<!--more-->

# ヘッダ1
## ヘッダ1-1

段落1

段落2  
非常に長い文章は、禁則事項を考慮しつつ折り返される。  
テスト。テスト。テスト。テスト。テスト。テスト。テスト。テスト。テスト。テスト。テスト。テスト。テスト。テスト。  

### ヘッダ1-1-1
#### ヘッダ1-1-1-1
##### ヘッダ1-1-1-1-1
###### ヘッダ1-1-1-1-1-1
####### ヘッダは6段階まで

## コードブロック
先頭にスペースを4個入れることでコードブロックを表現できる。

    text code blocks
    text code blocks
    text code blocks
    text code blocks

## ハイライト付きコードブロック

\`\`\`language name  
code  
\`\`\`

```python
print('hello world')
```

```common lisp
(defun foo (m n)
  (if (eq m n)
      (prin1 "Hello world")
	  (prin1 "Hello universe"))
  (prin1 "Hello world"))
```

```go
package main

import "fmt"

func main() {
    fmt.Printf("Hello World\n")
}
```

## シェル用ハイライト

\{\{< shhighlight bash "hl_lines=2 4" >}}  
\# test  
echo test  
\# just a test  
echo hello world  
\{\{< /shhighlight >}}

{{< shhighlight bash "hl_lines=2 4" >}}
# test
echo test
# just a test
echo hello world
{{< /shhighlight >}}

## 引用


\> を行頭につけると引用文になる。

> This is the first pragraph.
>
> This is the second paragraph.

## リスト
* Red
* Green
* Blue

1. Red
2. Green
3. Blue
    - A
    - B
    - C
      1. D
      2. E
      3. F


## タスクリスト
- [ ] a task list item
- [ ] list syntax ~~required~~
- [ ] normal **formatting**
- [ ] incomplete
- [x] completed

## 定義リスト

```
定義用語1
: 用語の説明1

定義用語2
: 用語の説明2
```

定義用語1
: 用語の説明1

定義用語2
: 用語の説明2

## 書式付きテキスト
| Name              | Markdown              | HTML tag             |
| ----------------- | --------------------- | -------------------- |
| *Emphasis*        | \*Emphasis\*          | `<em></em>`          |
| **Strong**        | \*\*Strong\*\*        | `<strong></strong>`  |
| `code`            | \`code\`              | `<code></code>`      |
| ~~Strikethrough~~ | \~\~Strikethrough\~\~ | `<del></del>`        |
| __Underline__     | \_\_underline\_\_     | `<u></u>`            |
| <kbd>Key</kbd>    | \<kbd\>Key\</kbd\>    | `<kbd></kbd>`        |

## Tables

| A     | B     | C     |
| ----- | ----- | ----- |
| a     | b     | c     |
| d     | e     | f     |

## 脚注
脚注は下記の通り記載できる。\[^脚注1]

\[^脚注1]: 脚注の内容を記載する。

脚注の実例[^footnote1].

[^footnote1]: ここに脚注の内容が表示される。


## 水平線

\-\-\-\-

----

\*\*\*\*

******

## リンク

リンクをインライン表示  
\[an example]("URLキャプション")

ここ [GitHub](http://github.com/otaon"筆者GitHubページ") をクリック。

## Images

- 自動的にページ幅に合わせて表示  
\![Globe]\(https://upload.wikimedia.org/wikipedia/commons/thumb/6/67/Octicons-globe.svg/120px-Octicons-globe.svg.png)

![Globe](https://upload.wikimedia.org/wikipedia/commons/thumb/6/67/Octicons-globe.svg/120px-Octicons-globe.svg.png)

- 幅を指定して表示  
\{\{\<figure src="/web/images/globe.svg" alt="Globe" align="aligncenter" width="100" caption="**Globe**">}}  
{{<figure src="/web/images/globe.svg" alt="Globe" align="aligncenter" width="100" caption="**Globe**">}}

## Twiiter
ツイッターの投稿IDを指定することで、そのツイートを表示できる。  
\{\{< tweet 877500564405444608 >}}

{{< tweet 877500564405444608 >}}


## Google Map
Google MapのIDを指定して、Google Mapをインライン表示できる。  
\{\{< googlemaps id="17_6iCOL6LkRjIFGPKmXBxjsvbBc" height="400">}}

{{< googlemaps id="17_6iCOL6LkRjIFGPKmXBxjsvbBc" height="400">}}


