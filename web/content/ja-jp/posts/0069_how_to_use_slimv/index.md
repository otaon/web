---
title: "roswellとvimとslimvでlisp環境を作る"
date:    2019-07-28T21:00:00+09:00
lastmod: 2019-07-28T21:00:00+09:00
draft: false
toc: true
tags: ["lisp", "vim", "slimv", "roswell"]
categories: ["実践-Common-Lisp"]
authors:
- otaon
---

# この記事について
Vimでlispのコーディングと実行を行うための環境を作成する手順を示す。

# 参考文献
以下以外にも適宜リンクを記載する。

- [Common Lispのライブラリ事情](http://takoeight0821.hatenablog.jp/entry/2017/01/29/141315)
- [Lispのパッケージ管理入門．Quicklisp，ASDF，Roswellの違いなど](http://diary.wshito.com/comp/lisp/lisp-pm/)
- [require, ASDF, quicklispを正しく使う](http://keens.github.io/blog/2014/11/30/quicklisp/)
- [独学Common Lisp 第24章「システムの構築（ASDF）」](http://lisp.satoshiweb.net/2018/01/asdf.html)

# 環境の構成
構成は以下の通り。

- Windows
- Vim
- Slimv
- Roswell

# 各構成要素の説明
## Vim
好きなエディタ。説明略。

## Slimv
EmacsのSlimeのVim版。  
VimとSwankサーバとを通信させる。

- [GitHub - kovisoft/slimv](https://github.com/kovisoft/slimv)
- [Slimv Tutorial - Part One](https://kovisoft.bitbucket.io/tutorial.html)
- [slimvショートカット一覧]({{<ref "posts/0025_slimv_shortcuts/index.md">}})

## Roswell(require, ASDF, Quicklispとの違い)
[Roswell](https://github.com/roswell/roswell)とは、Common Lispの実行環境を構築するためのユーティリティ。

> Roswell is a Lisp implementation installer/manager, launcher, and much more!

Roswellの内部ではquicklispを使っており、そのquicklispの内部ではASDFを使っている。
下記にこれらについて他サイトの説明を引用しながら軽く説明する。

## provide, require(非推奨)
> ANSI Common Lispに存在するライブラリ管理システムです。  
> `(require :hoge)`でファイルを読み込み、そのファイルで`(provide :hoge)`されていれば、2回目以降の`(require :hoge)`では何も行われません。  
> 
> これは無駄なロードを防ぐための仕組みです。  
> しかし、REQUIREがどこのファイルを読みにいくかは処理系の実装に依存しています。   
> 処理系ポータブルなコードを書くには非常に不便です。

すでに使うべきではない機能。大体のプロジェクトではASDFを使用している。

## ASDF : Another System Definition Facility
> ASDFは，ローカルに存在するライブラリを，依存関係に基づいてロードします．  
> 実際には，ロードする前にコンパイルまで行ってくれます．  
> 依存関係を定義するファイルは.asdファイルで，通常プロジェクトトップに置かれます．
> 
> Lisp標準のrequire関数で外部ファイルをロードする際，ほとんどの処理系がASDFを用いてファイルをロードするため，ASDF自体はLisp標準規格に含まれませんが，処理系と共に予めインストールされていることが多いです．

ASDFは、Common Lispのソースコードをsystemとしてまとめ、ビルドし、ロードするための「モジュール管理プログラム」。

## Quicklisp
> Roswellは何かと言うと，Quicklispのようなインストーラです．  
> Quicklispとの違いは，ライブラリだけでなくLisp処理系のインストールもできる点です．  
> RoswellでインストールされたLisp処理系を使うと，Roswellでインストールしたライブラリを何の設定もなしにrequireでロードできます．  
> したがって，RoswellさえあればQuicklispは必要ありません．  
> また，Roswellでインストールした処理系とライブラリは，単一のディレクトリ以下に配置されるので，既存の開発環境と共存できます．

Quicklispは依存しているライブラリをウェブ上のリポジトリからダウンロード・インストールしてくれるツール。  
RoswellをインストールしたらQuicklispもインストールされる。  
RoswellがQuicklispを隠蔽する感じだと思う。

# 環境構築方法
## roswellを導入する

[Common Lispとリアル・ワールドを繋ぐ「Roswell」の紹介](http://blog.8arrow.org/entry/2015/06/11/101511)に導入手順の分かりやすい説明がある。  
それを参考にした導入手順を以下に示す。

### 1. roswell自体をインストールする
基本的にはroswellの[gitwiki](https://github.com/roswell/roswell/wiki)を参照しながら導入できる。

#### Macの場合
Homebrewを使用すれば簡単に導入できる。  
Roswellをインストールすると、バイナリ版SBCLとQuicklispがインストールされる。

```bash
$ brew install roswell
# ターミナルを開き直す
$ ros setup
$ ls -a ~/.roswell
.              archives       env            impls          lisp           src
..             config         helper.el      lib            local-projects tmp
```

#### Windowsの場合
下記から、OSのアーキテクチャに合うバイナリをインストールする。

[GitHub - Roswell Installation Windows](https://github.com/roswell/roswell/wiki/Installation#windows)

- Roswell-x86_64.zip
- Roswell-i686.zip

zipファイルを展開すると`ros.exe`などが入っている。  
このフォルダを扱いやすいところに置いて、パスを通しておく。

#### インストール結果の確認
Roswellでインストールしたlisp実行環境を確認するには下記コマンドを実行。

```bash
$ ros list installed
Installed implementations:

Installed versions of clisp:
clisp/2.49

Installed versions of sbcl-bin:
sbcl-bin/1.4.4
```

#### Roswellの使い方
インストールされた処理系、バージョンは切替ができ、rosで起動されるCommon Lispを変更できる。

```bash
# デフォルトで使用する処理系を確認する(ハイフン--は誤記ではない)
$ ros run -- --version
SBCL 1.2.12

# デフォルトで使用する処理系を変更する
$ ros use sbcl/1.1.8

# デフォルトで使用する処理系が変更された
$ ros run -- --version
SBCL 1.1.8
```

### 2. roswellでslimeをインストールする
slimvで使用するため、インストールする。

```bash
$ ros install slime
```

## Slimvを導入する
### 1. SlimvをVimにインストールする
Vimプラグイン管理用のプラグイン`Vundle`では、以下の通りインストールできる。

```vimrc
" 1. compatibleを無効化する
if &compatible
	set nocompatible
endif

" 2. filetype設定を一時無効化する
filetype off

" Vundleインストール時に指定したディレクトリをセット
let s:Vundle_dir = expand('~/.vim/bundle/Vundle.vim')

" ランタイムパスにVundleを追加する
if &runtimepath !~# '/Vundle.vim'
	" Vundleが存在していない場合はgithubからcloneする
	if !isdirectory(s:Vundle_dir)
		execute '!git clone https://github.com/VundleVim/Vundle.vim.git' s:Vundle_dir
	endif
	execute 'set runtimepath^=' . fnamemodify(s:Vundle_dir, ':p')
endif

set rtp+=$HOME/.vim/bundle/Vundle.vim/
call vundle#begin('$HOME/.vim/bundle/')

" Slimvをインストール
Plugin 'otaon/slimv'

call vundle#end()
filetype plugin indent on
syntax enable
```

### 2. Slimv用の設定を.vimrcに記載する
.vimrcを設定する。（WindowsとMacの場合なので、Linuxなどの場合はおそらく適宜修正が必要。）  
自分の場合はプラグインごとに設定ファイルを分けているので、その設定ファイル`08slimv.vim`を別に示す。

- `08slimv.vim`

```vimrc
" 開き括弧を入力すると自動的に閉じ括弧を挿入する(デフォルト1)
let g:paredit_mode=1
" 括弧内で改行すると追加でもう一つ改行を挿入する。サブフォーム入力支援用。(デフォルト1)
let g:paredit_electric_return=0

" REPLの表示位置
"   0: no split
"   1: horizontal split above (default)
"   2: horizontal split below
"   3: vertical split left
"   4: vertical split right
let g:slimv_repl_split=4
let g:slimv_repl_name='REPL'
" 評価する画面でCtrl-Returnをすることで評価されるようにする
let g:slimv_repl_simple_eval=0

if has("win32")
	let g:slimv_lisp='ros run'
	let g:slimv_impl='sbcl'
elseif has("mac")
	let g:slimv_lisp='/usr/local/bin/ros run'
	let g:slimv_impl='sbcl'
endif
	"let g:slimv_preferred='clisp'

" ====================================================================
" swank起動コマンド設定

if has("unix")
	"let g:slimv_swank_cmd = '! xterm -e ros --load ~/.roswell/lisp/slime/$DATE/start-swank.lisp &'
	"" swankを直接起動
	"let g:slimv_swank_cmd = "!ros -e '(ql:quickload :swank) (swank:create-server :port 4005 :dont-close t)' wait &"

elseif has("win32")
	let g:slimv_swank_cmd='!start-process powershell.exe '.$HOME.'\\vimfiles\\myconfig\config_plugins\\run-swank.ps1'

elseif has("mac")
	" (同じターミナルの新規タブでslimvが用意したswank起動)
	let g:slimv_swank_cmd = '!osascript $HOME/.vim/myconfig/config_plugins/08slimv-roswell.scpt'
endif

" ====================================================================

" swankサーバのポート設定
let g:swank_port = 4005

" 括弧をカラフルに表示する
let g:lisp_rainbow=1

" asdf用のファイルタイプをlispに設定する
autocmd BufNewFile,BufRead *.asd set filetype=lisp

" 現在のファイルだけを対象としてタグファイルを作成
function! s:generate_lisp_tags()
  let g:slimv_ctags='ctags -a -f '.$HOME.'/.tags/lisp.tags '.expand('%:p').' --language-force=Lisp'
  call SlimvGenerateTags()
endfunction
command! -nargs=0 GenerateLispTags call <SID>generate_lisp_tags()

" 再帰的にタグファイルを作成
function! s:generate_lisp_tags_recursive()
  let g:slimv_ctags='ctags -a -f '.$HOME.'/.tags/lisp.tags -R '.expand('%:p:h').' --language-force=Lisp'
  call SlimvGenerateTags()
endfunction
command! -nargs=0 GenerateLispTagsRecursive call <SID>generate_lisp_tags_recursive()
```

### 3. タグファイル用のフォルダを作成する
タグファイルの格納場所として、`$HOME`直下に`.tags`ディレクトリを作成する。

**補足**  
brew などでインストールした素のclispを使用する場合は、（多分）`slimv_swank_cmd`を設定する必要はない。  
roswellを使用する場合はswankサーバをroswellのものにするため、`slimv_swank_cmd`の設定が必要となる。

### 4. swankの起動スクリプトを作成する
MacやWindowsにおいて非同期処理でswankサーバを起動しておくには、.vimrcに設定したとおり、swankサーバ起動用のスクリプトを記述する必要がある。

#### Macの場合のswankサーバ起動スクリプト
Mac用のスクリプトを以下に示す。  
roswellにインストールしたswankのバージョンの部分`2018.03.28`は、ダウンロードしたものに合わせて適宜修正すること。

- `08slimv-roswell.scpt`

```osascript
#!/usr/bin/env osascript

------------------------------------------------------------
-- 参考サイト
-- https://maku77.github.io/mac/applescript/terminal.html
------------------------------------------------------------

------------------------------------------------------------
-- roswell実行スクリプトの説明
------------------------------------------------------------
-- -- roswellが用意したswank起動スクリプトを使用する
-- "ros run --load $HOME/.roswell/lisp/slime/2018.03.28/start-swank.lisp"
-- -- slimvが用意したswank起動スクリプトを使用する
-- "ros run --load $HOME/.vim/bundle/slimv/slime/start-swank.lisp"

------------------------------------------------------------
-- スクリプトのエントリポイント(newWindow/newTabのいずれかを呼ぶこと)
------------------------------------------------------------
on run
	set cmd to "ros run --load $HOME/.roswell/lisp/slime/2018.03.28/start-swank.lisp"
    my newTab(cmd)
end run

------------------------------------------------------------
-- 新規ウィンドウでスクリプト実行
------------------------------------------------------------
-- 新しいタブを開いてコマンドを実行する
on newTab(command)
    -- Open a new tab and wait a little
    tell application "System Events"
        keystroke "t" using command down
        delay 0.5
    end tell

    -- Run the command in the new tab
    tell application "Terminal"
        do script command in front window
    end tell
end newTab

------------------------------------------------------------
-- 新規タブでスクリプト実行
------------------------------------------------------------
on newWindow(command)
	tell application "Terminal"
		do script command
	end tell
end newWindow
```

#### Windowsの場合のswankサーバ起動スクリプト
powershell用のスクリプトを以下に示す。  
なお、このスクリプトではサーバで待ち受けるポート番号を、.vimrcで設定したものと同じに設定する必要がある。
以下の場合は`4005`番に設定してある。

- `run-swank.ps1`

```ps1
ros -e '(ql:quickload :swank) (swank:create-server :port 4005 :dont-close t)' wait
```

Vimからではなく、swankサーバを独立して起動させておきたい場合、ショートカットを作っておくと便利。

`C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe`のショートカットを作成して、「リンク先」に以下を記述する。  
(スクリプトを`C:\myscripts\run-swank.ps1`に置いてある場合。)

```ps1
C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe -noexit C:\myscripts\run-swank.ps1
```