---
title: "Common Lisp ディレクトリパスを取得/設定する"
date:    2019-03-07T00:00:00+09:00
lastmod: 2019-03-07T00:00:00+09:00
draft: false
tags: ["lisp"]
categories: ["Notes"]
authors:
- otaon
---

# 本記事の目的
Lisp処理系起動時のディレクトリパス/カレントディレクトリパスを取得/設定する

# 参考サイト
[処理系を起動したディレクトリのパスネームを返す](https://lisphub.jp/common-lisp/cookbook/index.cgi?%E5%87%A6%E7%90%86%E7%B3%BB%E3%82%92%E8%B5%B7%E5%8B%95%E3%81%97%E3%81%9F%E3%83%87%E3%82%A3%E3%83%AC%E3%82%AF%E3%83%88%E3%83%AA%E3%81%AE%E3%83%91%E3%82%B9%E3%83%8D%E3%83%BC%E3%83%A0%E3%82%92%E8%BF%94%E3%81%99)

## 処理系を起動したディレクトリのパスネームを返す
truenameを使う。

### 対応環境: `SBCL`, `CLISP`, `CMUCL`

```lisp
(truename "./")  ;=> #P"処理系を起動したディレクトリ"
```
### 実行例
```lisp
(truename "./") ;=> #P"/users/username/"
```

## 各処理系でのカレントディレクトリの取得と設定
### 対応環境: `SBCL`

```lisp
; ディレクトリパスを取得する
(sb-posix:getcwd) ;=>#P"カレントディレクトリ"

; ディレクトリパスを設定する
(sb-posix:chdir #P"設定したいディレクトリ")
```
### 対応環境: `CLISP`
```lisp
; ディレクトリパスを取得する
(ext:default-directory) ;=>#P"カレントディレクトリ"

; ディレクトリパスを設定する
(ext:cd #P"設定したいディレクトリ")
```
### 対応環境: `CMUCL`
```lisp
; ディレクトリパスを取得する
(extensions:default-directory) ;=>#P"カレントディレクトリ"

; ディレクトリパスを設定する
(setf (extensions:default-directory) #P"設定したいディレクトリ")
```

カレントディレクトリの設定後に `(truename "./")` が返す値が変化しているかは処理系により異なる。
また、処理系によっては `*default-pathname-defaults*` に起動時のディレクトリが入っている。
CLISPだと `*default-pathname-defaults* ;=> #P""` のとおり空白だった。

SBCLだと、 `*default-pathname-defaults*` を書き換えないと`load`関数で参照する先が変わらない。
