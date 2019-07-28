---
title: "mermaid.jsをHugoで使えるようにする"
date:    2019-03-08T00:00:00+09:00
lastmod: 2019-03-08T00:00:00+09:00
draft: false
toc: true
tags: ["Hugo", "mermaid.js"]
categories: ["Hugo"]
authors:
- otaon
---

## 目的
Hugoで`mermaid.js`を使用できるようにする設定手順を示す。  
`mermaid.js`は`mermaid`のjavascript版で、これを使えばサイトで表示するダイアグラム等を簡単に記述できるようになる。

- `mermaid`説明サイト: [mermaid - GitBook](https://mermaidjs.github.io/)
- 参考にしたページ: [Hugoにmermaidを組み込んでみた](https://qiita.com/_takeuchi_/items/35c52fd85884a83c154d)

## mermaidをUNPKGから取得する
今回はmermaid.jsの本体のみ必要なので、UNPKGから`mermaid.js`をダウンロードする。  
ちなみにUNPKGとは、npmなどのコンテンツをダウンロードすることができるCDNのひとつ。  
サイトのデータ量を少しでも削減したい場合は、ダウンロードせずに、直接CDNのURLを指定すること。

- [最新版のmermaid.jsが置いてあるURL](https://unpkg.com/mermaid/dist/)
  - `mermaid.js`

今回は気が向いたときにコードを読み書きするかも知れないので通常版をダウンロードした。  
ミニマイズ済みのもので良ければ`mermaid.min.js`をダウンロードすれば良い。

ダウンロードしたら`themes/<テーマ名>/static/asset`にmermaidフォルダを作成してコピーする。  
本サイトの場合、下記に格納した。[^ディレクトリ構成]  
`<git-top>/web/web/themes/hugo-theme-den/static/asset/mermaid/mermaid.js`

[^ディレクトリ構成]:本サイトのディレクトリ構成は[ここ]({{<ref "/posts/0003_how_to_build_web_page_with_Hugo/index.md">}})を参照すること。


## `mermaid.js`用の`div`に置換するためショートコードを作成する

`themes/theme/layouts/shortcodes`に`mermaid.html`を作成して下記を記述する。  
これで`mermaid`クラス内のコンテンツが、`align`設定を考慮しつつ表示されるようになる。
本サイトの場合`<git-top>/web/web/themes/hugo-theme-den/layouts/shortcodes/mermaid.js`に置いてある。

```html
<div class="mermaid" align="{{ if .Get "align" }}{{ .Get "align" }}{{ else }}center{{ end }}">
    {{ safeHTML .Inner }}
</div>
```

## 全ページで`mermaid.js`が読み込まれるように設定する
下記の設定を、scriptタグが書かれている箇所に追記しておく。

```html
<script src="{{"asset/mermaid/mermaid.js" | relURL}}"></script>
```

**追記対象ファイル**

- `themes/theme/layouts/partials`
  - `header.html` or `footer.html` or `scripts.html`

`scripts.html`に`jquery`や`bootstrap`などの読み込みも行っているため、ここに記述するのが良さそう。

## 記事内で`mermaid.js`を使用する
記事内で下記のように記述すると`mermaid.js`がレンダリングしてくれる。

- 記事に記載する`mermaid.js`用コード[^サンプルコード]

[^サンプルコード]:https://mermaidjs.github.io/

`{`の前のエスケープは実際には無い。

```
\{\{\<mermaid align="center">}}
sequenceDiagram
    participant Alice
    participant Bob
    Alice->John: Hello John, how are you?
    loop Healthcheck
        John->John: Fight against hypochondria
    end
    Note right of John: Rational thoughts <br/>prevail...
    John-->Alice: Great!
    John->Bob: How about you?
    Bob-->John: Jolly good!
\{\{\</mermaid>}}
```

- `mermaid.js`がレンダリングした結果

{{<mermaid align="center">}}
sequenceDiagram
    participant Alice
    participant Bob
    Alice->John: Hello John, how are you?
    loop Healthcheck
        John->John: Fight against hypochondria
    end
    Note right of John: Rational thoughts <br/>prevail...
    John-->Alice: Great!
    John->Bob: How about you?
    Bob-->John: Jolly good!
{{</mermaid>}}
