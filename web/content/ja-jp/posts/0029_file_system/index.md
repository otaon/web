---
title: "ファイルシステムの概要"
date:    2019-03-24T00:00:00+09:00
lastmod: 2019-03-24T00:00:00+09:00
draft: false
toc: true
tags: ["file system"]
categories: ["Notes"]
authors:
- otaon
---

# 目的
極単純なファイルシステムを自作できる程度の知識を、初歩的な事柄をまとめる。

# 参考文献
- [ファイルシステム](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=2ahUKEwix1PLmnKDhAhUKO3AKHShZBOMQFjAAegQIBBAC&url=http%3A%2F%2Fresearch.nii.ac.jp%2F~ichiro%2Flecture%2Fos2001%2Fnotes%2FOS2001-file.pdf&usg=AOvVaw15ZC6zFCuZreyMzU2s0TAA)


# ファイルシステムとは
> ファイルシステムは、コンピュータのリソースを操作するための、オペレーティングシステム (OS) が持つ機能の一つ。  
> ファイルとは、主に補助記憶装置に格納されたデータを指すが、デバイスやプロセス、カーネル内の情報といったものもファイルとして提供するファイルシステムもある。  
> より正確に定義すれば、ファイルシステムは抽象データ型の集まりであり、ストレージ、階層構造、データの操作/アクセス/検索のために実装されたものである。

[Wikipedia - ファイルシステム](https://ja.wikipedia.org/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E3%82%B7%E3%82%B9%E3%83%86%E3%83%A0)

**NOTE:** ファイルシステムを操作するためには、OSが提供する機能を利用することになる。

# ディスクファイルシステムとは
> 「ディスクファイルシステム」は、直接的か間接的かに関わらずコンピュータシステムに接続された補助記憶装置、特にハードディスク上にファイルを格納するためのものである。
> ディスクファイルシステムとしては、FAT、NTFS、HFS、ext2、ext3、ext4、WAFL（英語版）、ISO 9660、ODS-5（英語版）、UDF、HPFS、JFS、UFS、VTOC、XFSなどがある。
> ディスクファイルシステムの一部はジャーナルファイルシステムまたはバージョニングファイルシステムでもある。 

[Wikipedia - ファイルシステム](https://ja.wikipedia.org/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E3%82%B7%E3%82%B9%E3%83%86%E3%83%A0)

# ファイルシステムへの要求
ファイルシステム、特にディスクファイルシステムへの要求

- メモリと比較して低速な読み書き速度に対応できる
- 記憶装置の容量を無駄無く利用できる
- 信頼性・耐障害性が高い
- 記憶装置の容量拡大に容易に対応できる
- 記憶装置の物理的な差異を吸収する論理インターフェースを提供できる

# ファイルシステムの構成要素
ファイルシステムの典型例をクラス図風に示す。[^PlantUML Previewer]

{{<figure src="disk-structure.png" alt="ファイルシステムの構成要素" align="aligncenter" width="700" caption="ファイルシステムの構成要素">}}

[^PlantUML Previewer]: 右記サイトでPlantUMLを使って作成した。(http://sujoyu.github.io/plantuml-previewer/)

## ブロック
多くのOSにおいては、ディスクを、(トラック)セクタを要素とする1次元配列として管理する。

多くのＯＳがディスクをセクタを要素とする１次元配列として管理

- セクタの指定方法
- シリンダ番号、ヘッド番号（ディスク番号）、セクタ番号の３項組
- セクタサイズ
  - システムに依存（代表的サイズ:512B）
- ブロック
  - ＯＳがハードディスクを読み書きする際のデータ単位
  - ブロックサイズ
     - ＯＳに依存。512B、1024B、2048B、4096B、8192Bなど
- **注意** ブロックサイズが大きいと
  - ○ まとめて入出力処理が可能なため高速化
  - × ファイルサイズがブロックサイズより小さいと無駄が多い

{{<figure src="Disk-structure2.svg" alt="ディスクセクタ" align="aligncenter" width="200" caption="A トラック<br/>B 幾何学的セクタ<br/>C (トラック)セクタ<br/>D クラスタ">}}

## ボリューム
- 記憶媒体のことを、ＯＳではボリュームと呼ぶ
  - 磁気ディスクや光ディスク、磁気テープなどの記憶する媒体

- ボリュームの構成
  - 初期プログラムローダ(IPL)
     - システム起動時に自動的に読み込まれるプログラムのこと
- ボリュームの管理情報
  - ボリュームの名前、領域割り当てサイズなどのボリューム管理に必要な情報のこと
- ファイル
  - ファイル管理情報とファイル内容自体のこと

- UNIXのボリューム構成例

{{<figure src="blocks.svg" alt="UNIXのボリューム構成例" align="aligncenter" width="200" caption="UNIXのボリューム構成例">}}

{{<figure src="typical_UNIX_filesystem.png" alt="典型的なUNIXファイルシステム(Wikipedia)" align="aligncenter" width="300" caption="典型的なUNIXファイルシステム(Wikipedia)">}}

# ファイルへのアクセス方法
## ファイルのアクセス（読み出し・書き込み）の方法
- 順アクセス
  - ファイルの先頭から末尾に向かってアクセス（`read`、`write`を使用)
- 直接アクセス
  - ファイル上の任意の位置をアクセス（`seek`、`read`、`write`を使用)
- 検索付き直接アクセス
  - キー値からアクセス対象となるレコードを取得・アクセス

# ファイルの操作
OSが提供するシステムコール、または、APIによってファイルを操作する。

{{<figure src="file-io-api.svg" alt="ファイルの操作" align="aligncenter" width="500" caption="ファイルの操作">}}

`OPEN` 
: ファイルアクセスに先立ち、ディスク上のファイルの位置を調べる。また、バッファ領域を含むファイル入出力に必要な情報領域(テーブル)を作成する。

`CLOSE`
: ファイル入出力にテーブルの領域を解放する。

`READ`
: ファイルからメモリにデータを読み出す

`WRITE`
: メモリからファイルにデータを書き込む

`SEEK`
: ファイルのアクセス位置を変更する

## `open()` ファイルをオープンする

| `open()`の情報   | ファイルをオープンする |
|:-------|------|
| **ヘッダ** | `fcntl.h`, `sys/types.h`, `sys/stat.h` |
| **書式**   | `int open(const char *path, int mode, [mode_t creat_mode])` |
| **引数**   | |
| `path`     | ファイル名 |
| `mode`     | オープンするファイルに許可を与えるモード |
| `creat_mode` | `mode`に`O_CREAT`指定時に設定できる、オープンするファイルのモード（省略可能）。 |
|            | `mode`で設定できる値は、下記※１の3つのいずれか1つと、<br/>※２の任意の値を「\|(論理和)」で結んだものとなる。 |
| **※1**    | |
| `O_RDONLY` | 読み込み可 |
| `O_WRONLY` | 書き込み可 |
| `O_RDWR`   | 読み書き可 |
| **※2**    | |
| `O_APPEND` | ファイルポインタをファイルの最後に移動する |
| `O_CREAT`  | ファイルを生成する |
| `O_TRUNC`  | ファイルが存在する場合、サイズを0にする |
| `O_BINARY` | バイナリモード |
| `O_TEXT`   | テキストモード  |
|            | modeにO_CREATを指定した場合、第3引数で※３の`creat_mode`を指定できる。 |
|            | |
| **※3**    | |
| `S_IWRITE` | 読み書き可 |
| `S_IREAD`  | 読み込み可 |
| `S_IREAD \| S_IWRITE` | 読み書き可 |
|            | |
| **戻り値** | |
| 成功       | ファイルデスクリプタ（そのファイルを一意に識別できる数値） |
| 失敗       | `-1` |
|            | |
| **解説**   | ファイルをオープンし、ファイルデスクリプタを返却する低水準入出力関数。 |

## `write()` ファイルデスクリプタへ書き込む

| `write()`の情報   | ファイルデスクリプタへ書き込む |
|:-------|------|
| **ヘッダ** | `unistd.h` |
| **書式** | `int write(int fd, void *buf, unsigned int byte)` |
| **引数** | |
| `fd` | ファイルデスクリプタ |
| `buf` | ファイルに書き込むデータが格納されている領域のポインタ |
| `byte` | ファイルへ書き込むバイト数 |
| **戻り値** | |
| 成功 | 書き込んだバイト数 |
| 失敗 | `-1` |
| **解説** | ファイルデスクリプタ(`fd`)へ`buf`に格納されているbyteバイト分のデータを書き込む。 |

## `read()` ファイルデスクリプタから読み込む

| `read()`の情報   | ファイルデスクリプタから読み込む |
|:-------|------|
| **ヘッダ** | `unistd.h` |
| **書式** | `ssize_t read(int fd, void *buf, size_t byte)` |
| **引数** | |
| `fd` | ファイルデスクリプタ |
| `buf` | 読み込んだデータを保存する領域のポインタ |
| `byte` | ファイルから読み込むバイト数 |
| **戻り値** | |
| **成功** | 読み込んだバイト数（ファイルの終端に達した場合は0）。 |
| **失敗** | `-1` |
| **解説** | ファイルデスクリプタ(`fd`)から、`byte`分のデータを読み込み、bufへ格納する。 |

## `close()` ファイルをクローズする

| `close()`の情報   | ファイルをクローズする |
|:-------|------|
| **ヘッダ** | `unistd.h` |
| **書式** | `int close(int fd)` |
| **引数** | |
| `fd` | ファイルデスクリプタ |
| **戻り値** | |
| 成功 | `0` |
| 失敗 | `-1` |
| **解説** | ファイルをクローズする低水準入出力関数。 |

## `fseek()` ファイルポインタの位置を設定する

| `fseek()`の情報   | ファイルポインタの位置を設定する |
|:-------|------|
| **ヘッダ** | `stdio.h` |
| **書式** | `int fseek(FILE *fp, long offset, int origin);` |
| **引数**  | |
| `fp` | ファイルポインタ |
| `offset` | `origin`で指定した位置からファイルポインタを移動するバイト数 |
| `origin` | ファイルポインタの初期設定値 |
| | origenで指定できる値は※1の通り。|
| **※1**    | |
| `SEEK_CUR` | 現在のファイルポインタの位置 |
| `SEEK_END` | ファイルの終端 |
| `SEEK_SET` | ファイルの先頭 |
| **戻り値** | |
| 成功 | `0` |
| 失敗 | `-1` |
| **解説** | 現在のファイルポインタを、`origin`で指定した位置から、`offset`バイト分移動した位置に設定する。<br/>`offset`には負の値を設定することが可能で、負の値を入力するとファイルポインタは前に移動する。 |

<br/>

参考サイト: [C言語例文集](http://www.geocities.jp/p_lan_c/index.html)

# ファイルの管理情報
ファイルを管理するための代表的な情報を下記に示す。

- 名前
- 属性
- 物理的位置（ブロック）
- 大きさ（サイズ）
- 保護情報（アクセス権）
- 参照時間、生成時間

**NOTE**  
ファイルの管理情報とファイル本体を結ぶ部分をディレクトリと呼ぶ。  
ディレクトリに管理情報そのものを保持、または、ディレクトリに管理情報のある領域（ブロック）のポインタを保持する。

# ディレクトリ構造
1階層のみのファイルシステムだと同一ファイル名を扱えない。  
そこで、多階層のディレクトリ構造を導入する。

- 1階層のみのファイルシステム・・・同一の名前空間となり、名前が衝突する。

{{<mermaid align="center">}}
graph TD;
	root[root];
	root-->a((file a));
	root-->b((file b));
	root-->c((file c));
	root-->d((file d));
{{</mermaid>}}

- 多階層のファイルシステム・・・異なるディレクトリでは異なる名前空間となる。

{{<mermaid align="center">}}
graph TD;
	root[root];
	root-->a((file a));
	root-->b((file b));
	root-->dir1[dir 1];
	root-->dir2[dir 2];
	dir1-->c((file a));
	dir1-->d((file b));
	dir2-->dir2_1[dir 2-1];
	dir2_1-->e((file a));
	dir2_1-->f((file b));
	dir2-->dir2_2[dir 2-2];
	dir2_2-->g((file a));
	dir2_2-->h((file b));
{{</mermaid>}}

# ファイルの実装
- 磁気テープの場合は、先頭から末尾まで、順番にヘッドを走査するしかない。  
- 磁気ディスクの場合は、ランダムアクセス可能。→記憶媒体へのファイル配置方法は多様。
  1. 連続方式
  1. 連結リスト方式
  1. 索引方式

**NOTE**  
OSは、ブロック(セクタサイズの整数倍)単位で読み書きする。

# ファイルの割り当て方式
## 連続割り当て方式(contiguous)
ファイルをディスク上に連続して格納する。

- :white_check_mark: 必要ブロック数と開始ブロック番号だけでファイルの格納場所を特定できる。
- :white_check_mark: 連続ブロックのため、シーケンシャルアクセス、ランダムアクセス共に高速。
- :x: ファイルサイズが固定になる。ファイルサイズ変更するには、保存し直す必要がある。
- :x: 断片化（フラグメンテーション）により無駄な領域が発生しやすい。

{{<figure src="contiguous-allocation.jpg" alt="連続割り当て方式" align="aligncenter" width="300" caption="連続割り当て方式">}}

## 連結リスト割り当て方式(linked-list)
ファイルを構成するブロックを先頭から順にリンクで連結する。

- :white_check_mark: ファイルサイズ（必要ブロック数）を変更が容易
- :white_check_mark: 断片化による無駄な領域が発生しない
- :x: リンク不良によりファイルアクセスが不可能になる  
   - 一つでもブロックが壊れると、それ以降のブロックも読めなくなる
- :x: ランダムアクセスの効率が悪い

{{<figure src="linked-allocation.jpg" alt="連結リスト割り当て方式" align="aligncenter" width="300" caption="連結リスト割り当て方式">}}

## リスト検索表割り当て方式(FAT:file-allocation-table)
ファイルを構成するブロックを先頭から順にリンクで連結する。  
リンク情報を取り出して、検索（FAT）として**メモリ**上に展開する。

- :white_check_mark: シーケンシャルアクセスが高速
- :x: 検索情報（リンク表）が大量のメモリを消費する

{{<figure src="Fat32_structure.png" alt="リスト検索表割り当て方式" align="aligncenter" width="400" caption="リスト検索表割り当て方式">}}

```c
typedef struct DirEntry_t {
    Byte    name[8];            /* file name */
    Byte    extension[3];       /* file name extension */
    Byte    attribute;          /* file attribute
                                     bit 4    directory flag
                                     bit 3    volume flag
                                     bit 2    hidden flag
                                     bit 1    system flag
                                     bit 0    read only flag */
    Byte    reserved;           /* use NT or same OS */
    Byte    createTimeMs;       /* VFAT で使用するファイル作成時刻の10ミリ秒 (0 ～ 199) */
    Byte    createTime[2];      /* VFAT で使用するファイル作成時間 */
    Byte    createDate[2];      /* VFAT で使用するファイル作成日付 */
    Byte    accessDate[2];      /* VFAT で使用するファイル・アクセス日付 */
    Byte    clusterHighWord[2]; /* クラスタ番号の上位(FAT32用)16 bits(First cluster,MSB) */
    Byte    updateTime[2];
    Byte    updateDate[2];
    Byte    cluster[2];         /* start cluster number(First cluster,LSB) */
    Byte    fileSize[4];        /* file size in bytes (directory is always zero) */
}   DirEntry;
```

[FAT FS フォーマットの実装についての覚え書き](http://www.geocities.co.jp/SiliconValley-PaloAlto/2038/fat.html)

## 索引割り当て方式(index)
ファイル構成するブロック番号を表形式で索引ブロック(index block)内に格納する。

- :white_check_mark: 断片化による無駄な領域が発生しない
- :small_red_triangle: ファイルサイズ（必要ブロック数）の変更が比較的容易
  - ただし、ファイルサイズが増加して索引ブロックに収まらないときは索引ブロックの追加が必要
- :small_red_triangle_down: 最少２回のディスクアクセスが必要

{{<figure src="index-allocation.jpg" alt="索引割り当て方式" align="aligncenter" width="400" caption="索引割り当て方式">}}

## iノード方式(i-node)
索引割り当て方式では大容量用ファイルは一つの索引ブロックでは収まらない。  
→「索引ブロックの索引ブロック」（間接ブロック）を導入する。

- 索引ブロック(i-node block)・・・間接ブロック、または、データブロックへの参照を持つ
- 間接ブロック・・・1段、2段、3段の間接ブロックが存在する(下図は1段と2段)
  - **ext2**では、iノード構造体が持つデータブロック参照用の配列は15個。
     - そのうち12個を「直接参照」、残りの3つをそれぞれ「一段間接参照」「二段間接参照」「三段間接参照」用に使用することで、複数のデータブロックを必要とする大きめのファイルを効率的に扱うようにしている。
  - **ext4**では、extent機能というものを使用できる。この場合、ここで述べたような間接ブロックは使用しない。(下記slideshareの37ページあたり参照)

{{<figure src="inode.png" alt="iノード方式" align="aligncenter" width="400" caption="iノード方式">}}

- [ブロックアルゴリズムとB-Treeアルゴリズム (2/3)](https://www.atmarkit.co.jp/ait/articles/0306/24/news002_2.html)
- [slideshare - ファイルシステム](https://www.slideshare.net/YoshihiroYunomae/f-36905134)
