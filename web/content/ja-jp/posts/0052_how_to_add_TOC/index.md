---
title: "HugoにTOCを追加する"
date:    2019-05-01T22:00:00+09:00
lastmod: 2019-05-01T22:00:00+09:00
draft: false
toc: true
tags: ["TOC", "Hugo"]
categories: ["Hugo"]
authors:
- otaon
---

# 目的
HugoにTOC(目次)を追加する方法を残す。

# 参考文献
- [わからないから調べるブログ - Hugoに開閉式の目次を実装する方法](https://akisnote.com/2018/11/table-of-contents/)
- [Qiita - HUGOで作れるCMSっぽいパーツ:関連記事・目次・JSON-LDなど](https://qiita.com/y_hokkey/items/f9d8b66b3770a82d4c1c)

# 設定ファイルと設定箇所の一覧
上記の参考文献を見つつ、下記のとおり設定すれば、記事の頭にTOCが挿入されるはず。

## 記事の基本情報のデフォルト値を変更

- `web\archetypes\default.md` 変更

```diff
  title: "{{ replace .Name "-" " " | title }}"
  date: {{ .Date }}
  draft: true
+ toc: true
  ---
```

- `web\themes\hugo-theme-den\archetypes\default.md` 変更

```diff
  +++
+ toc = true
  +++
```

## TOCを記事に追加するように設定を変更

- `web\themes\hugo-theme-den\layouts\posts\single.html` 変更

```diff
{{ define "main" }}
<div class="container content">
  <article>
+ {{ if eq .Params.toc true }}
+   {{ partial "toc.html" . }}
+ {{ end }}
  {{ .Content }}
  </article>
```

- `web\themes\hugo-theme-den\layouts\partials\toc.html` 新規追加

```diff
+ <section class="js-toc">
+   <div class="toc">
+   {{ .TableOfContents }}
+   </div>
+ </section>
```

## TOC用のcssクラス`toc`のデザイン設定

- `web\themes\hugo-theme-den\static\css\den.css` 変更

```diff
  @import url('https://fonts.googleapis.com/css?family=Open+Sans:300,300i,400,400i,600,600i,700');
  
  @import url("syntax.css");
+ @import url("style.css");
  
  /* Responsive */
  @media (min-width: 768px) {
```

- `web\themes\hugo-theme-den\static\css\style.css` 新規作成

```diff
+ .toc ul {
+     list-style-type:none;
+ }
+ .toc ul ul {
+     list-style-type:disc;
+ }
+ .toc:before {
+     content: "TOC";
+     font-size: 120%;
+     font-weight: bold;
+     padding-left: 3em;
+ }
+ .toc  {
+     background: #f8f8f8;
+     border-radius: 5px;
+     padding: 1em;
+ }
```

- `web\themes\hugo-theme-den\static\js\script.js` 新規作成(これは必要なのか不明)

```diff
+ var $tocPlace = $('#js-toc-place');
+ if ($tocPlace.length === 1) {
+     $('.js-toc').appendTo($tocPlace);
+ }
```

## 記事にTOCを追加するための設定方法

- `web\content\ja-jp\posts\0051_go_steup_01_oop\index.md` 変更

```diff
  date:    2019-05-01T00:00:00+09:00
  lastmod: 2019-05-01T00:00:00+09:00
  draft: false
+ toc: true
  tags: ["Go", "Golang", "OOP", "オブジェクト指向プログラミング", "ポリモーフィズム", "多態性"]
  categories: ["Notes"]
  authors:
```