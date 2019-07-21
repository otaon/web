---
title: "p5.jsをHugoの記事に埋め込む方法"
date:    2019-07-20T16:16:45+09:00
lastmod: 2019-07-20T16:16:45+09:00
draft: false
toc: true
tags: ["p5.js", "Hugo"]
categories: ["Demos"]
authors:
- otaon
---

# 実例
{{< p5js sketch="sketch.js">}}

div id=canvas↓↓  
<div id="canvas"></div>  
/div id=canvas↑↑

# 方法
## 1. p5.jsのライブラリを`static/`ディレクトリに配置する
`static/`に配置したものは、ビルド時にそのままの構成で`public/`下に配置される。  
そこで、webサイト上のどこでもp5.jsを使用するために、この`static/`にp5.js用のプログラムを配置する。

p5.jsを使用するために、[p5.js | download](https://p5js.jp/download/)から`p5.js`か`p5.min.js`をダウンロードする。  
ミニマイズしてあるものを使いたければ`p5.min.js`を使用すること。  
ここからは`p5.min.js`を例にする。

次に`web/static/asset/p5/p5.min.js`に、ダウンロードした`p5.min.js`を配置する。  
`asset/`は、`static/`下のフォルダが乱雑にならないように用意しただけ。  
`mermaid`は無関係。

```sh
web/web/static/asset$ tree
.
|-- mermaid
|   `-- mermaid.js
`-- p5
    `-- p5.min.js

2 directories, 2 files
```

## 2. p5.js用のshortcodeを作成する
記事のディレクトリ内に`p5.min.js`を配置していれば、`<script>`タグを以下のように記述することで`p5.min.js`が読み込まれ、スケッチ`sketch.js`が実行、表示される。

```
<script src="p5.min.js"></script>
<script src="sketch.js"></script>
```

`p5.min.js`は上述の通り`static/`下に格納したため、そのパスを指定する必要がある。  
しかし、ローカルで確認する(`hugo serve`)ときと実際にサーバにデプロイしたときのURLが異なる場合、URLを直に書くとどちらかの場合に正しく実行されない。  
そこで、Webサイトのエントリポイントを表す`{{ .Site.BaseURL }}`を使用して、両方の場合に合わせてURLを変化させる必要がある。  
ただし、`{{ .Site.BaseURL }}`を記事内に直接書いても展開されないため、p5.js用のショートコード(`p5js.html`)を用意して、そのショートコード内で展開させる。

**`web\web\themes\hugo-theme-den\layouts\shortcodes\p5js.html`**  

```html
<div class="p5js-canvas">
	<script src="{{ .Site.BaseURL }}asset/p5/p5.min.js"></script>
	<script src="{{ .Get "sketch" }}"></script>
</div>
```

`class="p5js-canvas"`
: クラス名は適当。他の設定などに被らなければ何でも良い。

`<script src="{{ .Site.BaseURL }}asset/p5/p5.min.js"></script>`
: これが記事内で展開されることで、`static/`に格納した`p5.min.js`が読み込まれる。

`<script src="{{ .Get "sketch" }}"></script>`
: ショートコードの`sketch`属性に実行したいスケッチを指定することで、それが記事内に展開される。  

## 3. 表示させたいスケッチを任意のフォルダに配置する
実行したいスケッチを任意のフォルダに配置する。  
前述のとおり、ローカルで確認する(`hugo serve`)ときと実際にサーバにデプロイしたときのURLが異なる場合についての考慮は面倒くさいので、スケッチは記事と同じディレクトリに格納し、そのファイル名をショートコードの`sketch`属性に記載した方が良い。

## 4. 記事にshortcodeを記載する
記事内に表示したいスケッチ`sketch.js`を記事と同じディレクトリに格納したとする。  
このとき、記事に下記の通りに記載すると、本記事の実例の通り表示される。

(下記の`\`は本当は記載しない)
```html
\{\{< p5js sketch="sketch.js">}}
<div id="canvas"></div>
```

`\{\{< p5js sketch="sketch.js">}}`
: このショートコードは上述の`p5js.html`に従って`<script>`などに展開される。

`<div id="canvas"></div>`
: スケッチが表示される位置を指定する。  
スケッチの`setup()`関数に下記のように記載すると、「スケッチ表示箇所の親要素のidは`canvas`」と指定される。  
このとき、`id="canvas"`の下にスケッチが表示される。

```javascript
function setup() {
	let canvas = createCanvas(500, 300);
	canvas.parent('canvas');
}
```
