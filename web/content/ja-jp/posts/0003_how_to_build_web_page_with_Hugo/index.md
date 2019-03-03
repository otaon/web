---
title: "GitHub + Hugo + Travis CI で静的サイトを GitHub Pages に公開する"
date:    2019-03-03T16:23:58+09:00
lastmod: 2019-03-04T16:00:00+09:00
draft: false
tags: ["template"]
categories: ["Notes"]
authors:
- otaon
---

# はじめに
自前でサーバを構築することなく、全て外部サービスを使用してブログ的な静的サイトを楽に作りたい場合、この記事が役に立つと思う。  
このWebサイトも、この記事と全く同じ方法で作成している。  

`html`, `css`諸々を書いて、サーバを立てて、ドメインを取って…という事を全くせずにWebサイトが構築できる。

<!--more-->

----

# 前提条件
- GitHub、および、Gitの基本的な使い方が分かる。
- Markdown記法、および、極初歩的な`html`, `css`が扱える。
- CIツールを使用したことがある。TravisCIの使用経験は問わない。

# 事前に知っておくべきこと
## GitHub Pagesとは
[GitHub Pages](https://pages.github.com/) とは、GitHubによる静的サイトのホスティングサービス。  
GitHubアカウントがあれば、誰でも独自のWebサイトが構築できる。  
デフォルトでは`<アカウント名>.github.io`にページを公開できる。

## Hugoとは
[Hugo](https://gohugo.io/)とは、Go言語で書かれた、比較的新しい静的サイトジェネレータ。  
これを使うと、Markdown記法で記事を書くだけで、`html`, `css`ファイルを生成できる。

## TravisCIとは
TravisCIとは、ドイツのTravis CI社によって運営されている継続的インテグレーション(CI)サービス。  
オープンソースプロジェクトなら`travis-ci.org`で無料で、プライベートプロジェクトであれば`travis-ci.com`で有料で利用できる。  
ただし、GitHubアカウントでログインする場合は、無料だが`travis-ci.com`での利用になる。

![travisCI-personal-page](travisCI-personal-page.PNG)

----

# Webサイトの記事デプロイ手順
この記事のとおりにサービスの設定を行えば、下記の手順で記事をデプロイできるようになる。

1. GitHubリポジトリに、記事用のブランチを作成する。
1. 記事を書いてcommit, pushする。
1. ブランチをmasterにマージする。
1. TravisCIが、masterのheadが進んだことを検知し、自動ビルドとデプロイを開始する。
1. 記事が公開される。

----

# サイト構築手順
ほとんどの手順で以下サイト様を参考にした。  
[ぽよメモ - Hugo + Travis CI + Github pagesで独自ドメイン+HTTPSなWebページを公開する](https://poyo.hatenablog.jp/entry/2018/06/08/145255)

備忘録のため、また、一部構成を変えた部分があったため、以下に全手順を残す。

1. Webサイトの記事を置くリポジトリを作成する
1. `Hugo`をインストールする
1. リポジトリで`Hugo`のプロジェクト初期設定をする。
1. `Hugo`のテーマを作るor公式サイトで選ぶ


# まとめ
