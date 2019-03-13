---
title: "Gitの使い方"
date:    2019-03-13T01:00:00+09:00
lastmod: 2019-03-13T01:00:00+09:00
draft: false
tags: ["Git"]
categories: ["Notes"]
authors:
- otaon
---

# 目的
Gitを端末上で使用するため、操作とそのコマンドを記す。

# 操作の種類一覧
1. 情報更新/情報表示
1. リポジトリ操作
1. ブランチ操作
1. ファイル操作
1. 差分(diff)確認


----

# 情報更新/情報表示
## リモートリポジトリの情報をローカルリポジトリに反映する(フェッチ)
**NOTE:** `fetch`が実際にやっているのは、リモート追跡ブランチの作成。

```bash
# シンタックス
$ git fetch [リポジトリ略称 ブランチ名]

# 例
$ git fetch origin how-to-use-git
From github.com:otaon/web
 * branch            how-to-use-git -> FETCH_HEAD
```

- `[ブランチ名]` 作成対象となるリモート追跡ブランチ。省略すると全てのリモート追跡ブランチを作成する。

## ローカルブランチ&リモートブランチを一覧表示
```bash
# シンタックス
$ git branch [-a]

# 例
$ git branch -a
  how-to-make-web-page-with-hugo
* how-to-use-git
  how-to-use-mermaid-in-hugo
  master
  migrate-from-gist-to-githubio
  remotes/origin/gh-pages
  remotes/origin/how-to-make-web-page-with-hugo
  remotes/origin/how-to-use-git
  remotes/origin/how-to-use-mermaid-in-hugo
  remotes/origin/master
  remotes/origin/migrate-from-gist-to-githubio
```

- `[-a]` ローカルとリモートの全てのリポジトリ情報。省略した場合はローカルのみ。

## ローカルリポジトリ状態を表示
現在いるブランチ、そのupstream、ファイルのステージング状況を表示する。

```bash
# シンタックス
$ git status

# 例
$ git status
On branch how-to-use-git
Your branch is up to date with 'origin/how-to-use-git'.

Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

        modified:   content/ja-jp/posts/0027_how_to_use_git/index.md

no changes added to commit (use "git add" and/or "git commit -a")
```

## コミットログを表示
```bash
# シンタックス
$ git log [--pretty=short] [--graph] [-数字] [-p|-u|--patch 対象ファイルパス*] [--decorate]

# 例
$ git log -2 --graph
* commit d37e53c8f12bfba66c1bd1accce89ca6bcbc33a0 (HEAD -> how-to-use-git, origin/how-to-use-git)
| Author: otaon <******@github.com>
| Date:   Tue Mar 12 03:24:19 2019 +0900
|
|     feature: add figure of `merge`
|
* commit de2baba4ddb3f7469aba02f88190a790523312cd
| Author: otaon <******@github.com>
| Date:   Mon Mar 11 02:28:11 2019 +0900
|
|     feature: edit an article
```

- `[--pretty=short]` 表示メッセージを短くする。
- `[--graph]` ブランチをグラフ表示する。
- `[-数字]` 指定した通じの数だけログを表示する。
- `[-p|-u|--patch 対象ファイルパス*]` 指定ファイルの差分をパッチ形式で表示する。
- `[--decorate]` 現在のHEAD、ブランチ名、タグ名を表示する。

----

# リポジトリ操作
## リポジトリを新規作成する
```bash
# -*- ローカルリポジトリを作成する -*-
# シンタックス
$ git init [ディレクトリ]
# 例
$ git init web

# -*- リモートリポジトリを作成する -*-
# シンタックス
$ git init --bare --shared [ディレクトリ]
# 例
$ git init --bare --shared web
```

- `[ディレクトリ]` 指定したディレクトリに`.git`というサブディレクトリが作成される。省略時はカレントディレクトリ。
- `--bare` 最小限のリポジトリを作成する。もし環境変数`GIT_DIR`が設定されていなければ、現在のワーキング・ディレクトリが設定される。[^git環境変数]
- `--shared` Gitリポジトリが複数のユーザで共有されることを指定する。このオプションは、同じグループに属するユーザがそのリポジトリにpushすることを許可する。

[^git環境変数]:`.git`フォルダの場所を指す環境変数。指定されていない場合、Gitはディレクトリツリーを`~`または`/`にたどり着くまで上っていき、各ディレクトリで`.git`ディレクトリを探す。[10.8_Gitの内側-環境変数](https://git-scm.com/book/ja/v2/Git%E3%81%AE%E5%86%85%E5%81%B4-%E7%92%B0%E5%A2%83%E5%A4%89%E6%95%B0)

## リモートリポジトリからローカルリポジトリをクローンする
既にリモートリポジトリが存在する時、それを元にローカルリポジトリをクローンする。

```bash
# シンタックス
$ git clone リモートリポジトリ名 [ディレクトリ]
# 例
$ git clone git@github.com:otaon/web.git web
```

- `[ディレクトリ]` 指定したディレクトリを作成してリモートリポジトリ内のデータを置く。  
  省略した場合はリモートリポジトリ名のディレクトリを作成してリモートリポジトリ内のデータを置く。

## リモートリポジトリをローカルリポジトリに関連付ける
既にローカルリポジトリとリモートリポジトリが存在する時、それをリモートリポジトリに関連付ける。

```bash
# シンタックス
$ git remote add リモートリポジトリの略称 リモートリポジトリのURL
# 例
$ git remote add origin git@github.com:otaon/web.git
```

## 現在のローカルリポジトリのブランチをpush
```bash
# シンタックス
$ git push [-u|--set-upstream] リモートリポジトリの略称 ブランチ名
# 例
$ git push origin how-to-use-git
```

- `[-u|--set-upstream]` ローカルリポジトリ/現在ブランチの**upstream**をリモートリポジトリの略称/ブランチ名に設定する。  
  こうすると、次回からは`git push`で**upstream**にpushできるようになる。

## リモートリポジトリのブランチをpull
```bash
# シンタックス
$ git pull リモートリポジトリの略称 ブランチ名
# 例
$ git pull origin how-to-use-git
```

----

# ブランチ操作
## ローカルブランチを作成する
現在のコミットに、ローカルブランチを作成する。  
リモート追跡ブランチが作成されている場合は、それを元にローカルブランチを作成する。

```bash
# ローカルブランチを作成
$ git branch ブランチ名
# ローカルブランチをチェックアウトする(ローカルブランチは自動的に作成される)
$ git checkout how-to-use-git
```

## ローカルブランチを作成する/切り替える
```bash
# シンタックス
$ git checkout [-b] [-f] ブランチ名
# 例
$ git checkout master # masterをチェックアウト
$ git checkout -b how-to-use-git # hot-to-use-gitブランチを作成してチェックアウト
```

- `[-b]` このオプションは、`git branch ブランチ名; git checkout ブランチ名`のショートハンドとして動作する。
- `[-f]` 作業ブランチやステージに変更があった場合でも、それを強制的に破棄してチェックアウトする。

### リモートブランチを扱いたい場合
リモートブランチは直接チェックアウトできない。  
そこで、リモートブランチに対応する**リモート追跡ブランチ**を作成し、それからローカルブランチをチェックアウトする。  
リモート追跡ブランチが存在する場合、そのブランチ名をチェックアウトすると、下記が自動的に行われる。

- ローカルブランチは自動的に作成される。
- 作成されたローカルブランチの**upstream**ブランチに、リモートブランチが自動的に設定される。

```bash
# 例
## 特定のリモート追跡ブランチを作成
$ git fetch origin how-to-use-git
## ローカルブランチをチェックアウトする(ローカルブランチは自動的に作成される)
$ git checkout how-to-use-git
```

## 現在のブランチに、指定コミットの指定ファイルを展開する
```bash
# シンタックス
$ git checkout [コミットSHA] ファイルパス

# 例
$ git checkout afpj73z index.html
```

- `[コミットSHA]` 展開対象のファイルがあるコミット。省略時は現在のindexのコミットを指す。

## 指定したブランチを現在のブランチにマージ
```bash
# シンタックス
$ git merge [--no-ff] ブランチ名

# 例
## 現在masterにいるとして、how-to-use-gitブランチをmasterにマージ
$ git merge --no-ff how-to-use-git
```

**NOTE** indexが変化するのはカレントブランチであり、引数で指定したindexは一切変化しない事を覚えておくこと。  
つまり、`merge`実行時のカレントブランチは常に「変更を取り込む側」となる。

- `master`ブランチにいるときに`git merge --no-ff `
  - featureブランチでの開発が完了してmasterブランチにマージしたい場合などに用いる。
  - {{<mermaid align="center">}}
graph LR;
	ee[master] -.-> c
	style ee fill:#f9f,stroke:#333,stroke-width:4px, stroke-dasharray: 5, 5
	g>HEAD]
	style g fill:#9f9,stroke:#333,stroke-width:4px
	g -.-> ee
	f[master] --> d((d))
	style f fill:#f9f,stroke:#333,stroke-width:4px

	g --> f
	a((a)) --> b((b))
	b --> c((c))
	c --> d((d))
	style a fill:#f99
	style b fill:#f99
	style c fill:#f99
	style d fill:#f99

	b --> ba((ba))
	ba --> bb((bb))
	bb ==>|git merge --no-ff how-to-use-git| d
	i[how-to-use-git] --> bb
	style i fill:#f9f,stroke:#333,stroke-width:4px
{{</mermaid>}}

- `how-to-use-git`ブランチにいるときに`git merge [--ff] master`
  - featureブランチを作成したまま放置していたらmasterが進んでしまった場合などに用いる。
  - {{<mermaid align="center">}}
graph LR;
	g>HEAD]
	style g fill:#9f9,stroke:#333,stroke-width:4px
	g -.-> d[how-to-use-git]
	style d fill:#f9f,stroke:#333,stroke-width:4px, stroke-dasharray: 5, 5
	d ==>|git merge master| f
	g --> f[how-to-use-git]
	style f fill:#f9f,stroke:#333,stroke-width:4px

	e[master] --> c
	style e fill:#f9f,stroke:#333,stroke-width:4px

	a((a)) --> b((b))
	b((b)) --> c((c))
	style a fill:#f99
	style b fill:#f99
	style c fill:#f99

	d -.-> b
	f --> c
{{</mermaid>}}

## コミット履歴を改竄する(rebase)
### ブランチの開始地点を変更する
ブランチ元にコミットが発生した時、それに追従するために、ブランチの開始地点を、ブランチ元の新しいHEADに変更する。

```bash
# シンタックス
$ git rebase [ブランチ元] [現在のブランチ]

# 例
$ git rebase master how-to-use-git
```

{{<mermaid align="center">}}
graph LR;
	e[master] --> c
	style e fill:#f9f,stroke:#333,stroke-width:4px
	a((a)) ==> b((b))
	b((b)) ==> c((c))
	style a fill:#f99
	style b fill:#f99
	style c fill:#f99
	b --> x((x))
	x --> y((y))
	style x stroke-dasharray: 5, 5
	style y stroke-dasharray: 5, 5

	c --> x2((x'))
	x2 --> y2((y'))

	e2[how-to-use-git] -.-> y
	style e2 fill:#f9f,stroke:#333,stroke-width:4px, stroke-dasharray: 5, 5

	x ==>|git rebase master| x2
	y ==>|git rebase master| y2

	e3[how-to-use-git] --> y2
	style e3 fill:#f9f,stroke:#333,stroke-width:4px
	z3>HEAD]  -.-> e2
	z3 --> e3
	style z3 fill:#9f9,stroke:#333,stroke-width:4px
{{</mermaid>}}

### 複数のコミットを一つにまとめる
複数のコミットをまとめた、新しいコミットを作る。(つまりSHAが変わることに注意)

```bash
# シンタックス
$ git rebase -i 改竄対象の直前のコミット
# 例
$ git rebase -i HEAD~2

# エディタで、HEADを含めて2つまでのコミット履歴をpickからfixupに編集する
### 編集前 ###(上の方が古い)
# pick 7a34294 first commit
# pick 6fba227 second commit

### 編集後 ###(2つ目のコミットをfixupに変更)
# pick 7a34294 first commit
# fixup 6fba227 second commit
```

{{<mermaid align="center">}}
graph LR;
	a((a)) -.-> b((b))
	style b stroke-dasharray: 5, 5
	b((b)) -.-> c((c))
	a --> d((b + c))

	z>HEAD]  -.-> e
	style z fill:#9f9,stroke:#333,stroke-width:4px
	e[master] -.-> c
	style c stroke-dasharray: 5, 5
	style e fill:#f9f,stroke:#333,stroke-width:4px, stroke-dasharray: 5, 5

	b ==>|git rebase| d
	c ==>|git rebase| d
	e2[master] --> d
	style e2 fill:#f9f,stroke:#333,stroke-width:4px
	z --> e2
{{</mermaid>}}

----

# ファイル操作
## ファイルをステージングする
```bash
# シンタックス
$ git add [--all|ファイル]
# 例
$ git pull index.html
```

- `--all` 変更が加えられたファイルと未追跡だったファイルをaddする

## ステージング済みのファイルをリポジトリへコミット
```bash
# シンタックス
$ git commit [-m "コミットメッセージ"]
# 例
$ git commit -m "feature: edit index.html"
```

## 直前のコミットメッセージを修正
```bash
# シンタックス
$ git commit --amend # -> エディタでメッセージを修正
# 例
$ git commit --amend # -> エディタでメッセージを修正
```

## 変更をリセット
```bash
# シンタックス
$ git reset [--soft|--mixed|--hard] [HEAD|HEAD^|SHA|ブランチ名]
# 例
git reset --mixed HEAD # addを取り消す
git reset --hard ORIG_HEAD # git resetを取り消す
```

- `--soft` HEADの位置のみリセットする。(`commit`のみ取り消し)
- `--mixed|指定なし` HEADの位置とindexをリセットする。(`add`と`commit`を取り消し)
- `--hard` HEADの位置とindexとワークツリー内容をリセットする。(ワークツリーの編集内容と`add`と`commit`を取り消し)

----

# 差分確認
## 様々な差分(diff)を確認
```bash
# シンタックス
$ git diff
  [HEAD
  |HEAD^
  |HEAD..リモート名/ブランチ名
  |リモート名/ブランチ名..HEAD
  |--cached
  |変更前のSHA..変更後のSHA
  |確認したいコミットのSHA^..確認したいコミットのSHA
  |ブランチA..ブランチB]
  [-- 対象ファイルパス]
  [その他オプション]
# 例
$ git diff -U5 how-to-use-git..origin/how-to-use-git -- index.html
```

**`A..B`と書いた場合、左が古く、右が新しいとみなされる**

- 比較対象の状態を指定する方法
  - `指定なし` index(ステージ領域) → 現在のワークツリー
  - `HEAD` 最新コミット → 現在のワークツリー
  - `HEAD^|HEAD^..HEAD` 一つ前のコミット → 最新コミット
  - `HEAD..リモートリポジトリ名/ブランチ名` 最新コミット →指定リモートリポジトリ/指定ブランチ
  - `リモート名/ブランチ名..HEAD` 指定リモートリポジトリ/指定ブランチ →最新コミット
  - `--cached|--staged` 最新コミット →index(ステージ領域)
  - `SHA1..SHA2` 指定コミット(SHA1) → 指定コミット(SHA2)
  - `SHA1^..SHA1` 指定コミット(SHA1)の一つ前 → 指定コミット(SHA1)
  - `ブランチ名A..ブランチ名B` ブランチA → ブランチB
- 比較対象のファイルを指定する方法
  - `-- 対象ファイルパス+` 指定ファイルパスのみを対象とする
     - `git diff -- ファイルパスA ファイルパスB` 指定ファイルパスAと、指定ファイルパスB
- 表示形式を指定する方法
  - `--stat` 変更点ではなく、ファイル毎の変更種別と変更量のみ表示する。
  - `--name-only` ファイル名のみ表示する。
  - `git add -n .; git diff --name-only` git add をdry-runして追加されたファイルも対象として、ファイル名のみ表示する。
  - `-U0` `-U10` 変更行の前後0行or10行を表示する。
  - `--color-words` 単語に色を付ける。
  - `--compaction-heuristic` 上方向への差分比較も実施した上で差分表示する。環境によっては使用不可の模様。
