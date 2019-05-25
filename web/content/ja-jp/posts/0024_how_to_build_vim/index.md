---
title: "Vimをビルドする"
date:    2019-03-07T02:00:00+09:00
lastmod: 2019-03-07T02:00:00+09:00
draft: false
toc: true
tags: ["build", "vim"]
categories: ["vim"]
authors:
- otaon
---

#### gitからclone
```bash
$ git clone https://github.com/vim/vim.git ~/git/vim
```

#### コンフィグ
`prefix`にvimのインストール先を設定する。

```bash
$ cd ~/git/vim 
$ ./configure \
--prefix=$HOME/.local \
--with-features=huge \
--enable-multibyte \
--enable-rubyinterp \
--enable-pythoninterp \
--enable-python3interp \
--enable-perlinterp \
--enable-fontset \
--enable-luainterp \
--enable-fail-if-missing
```

#### make install
```bash
$ make install
```

#### PATHを通す
場合によっては最初からPATHが通っている。

**$HOME/.profile***

```bash
# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi
```
