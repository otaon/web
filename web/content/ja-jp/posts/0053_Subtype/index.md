---
title: "部分型付け・派生型・リスコフの置換原則・仮想関数テーブル"
date:    2019-05-01T23:00:00+09:00
lastmod: 2019-05-01T23:00:00+09:00
draft: false
toc: true
tags: ["部分型付け", "派生型", "リスコフの置換原則", "仮想関数テーブル"]
categories: ["Notes"]
authors:
- otaon
---

# 目的
部分型付け・派生型・リスコフの置換原則・仮想関数テーブルについての学習記録を残す。

# 参考文献
本文中に記す。

# ポリモーフィズム(部分型付け)
とりあえずWikipediaの記事から、言葉を少し補って引用する。

[Wikipedia - ポリモーフィズム#部分型付け](https://ja.wikipedia.org/wiki/%E3%83%9D%E3%83%AA%E3%83%A2%E3%83%BC%E3%83%95%E3%82%A3%E3%82%BA%E3%83%A0#%E9%83%A8%E5%88%86%E5%9E%8B%E4%BB%98%E3%81%91)

> いくつかのプログラミング言語では、特定の多相性の状況において使用できる型の範囲を制限するために**部分型付け (部分型多相とも呼ばれる)**を採用している。  
> そういった言語で部分型付けを使用すると、ある型`Type`のオブジェクトを受け取る関数は、`Type`の部分型である型`Subtype`のオブジェクトを渡された場合でも正しく動作する (**リスコフの置換原則**)。  
> この型の関係性はしばしば`Subtype<:Type`と表記される。一般的に部分型多相は動的に解決される (後述)。

**部分型付け**は、基本型が使われている箇所を完全に置き換え可能な部分型を定義すること。  
つまり、基本型(例:`Base`クラス)の箇所を派生型(例:`Derived`クラス)に置き換えられるように派生型を定義すること。  
C言語の場合、`char`型の箇所を`int`型に置き換えても成り立つ(今はエッジケースは無視する)ため、`int`型は`char`型の派生型だと言える。

> オブジェクト指向言語は**サブクラス化 (継承)**によって部分型多相を提供する。  
> 典型的な実装では、各クラスはそれぞれ**仮想関数テーブル(`vtable`)**と呼ばれる関数のテーブルを持ち、各オブジェクトは自らのクラスのvtableへのポインタを持つ。  
> 多相なメソッドを呼出すときには、このvtableを参照する。

オブジェクト指向言語において、仮想関数テーブル(`vtable`)

> 多くのオブジェクト指向言語では、**仮想関数の呼出しに1番目の引数 (`this`オブジェクト)の vtable だけを参照する単一ディスパッチ**を採用している。  
> つまりその他の引数の実行時の型は仮想関数の呼び出しに全く無関係である。  
> 一方でCommon Lisp Object Systemなどでは、**メソッドの呼出しが全ての引数に対して多相的となる多重ディスパッチ**を採用している。

[ 数理科学的バグ撲滅方法論のすすめ - 第4回　関数型言語とオブジェクト指向，およびOCamlの"O"について](https://tech.nikkeibp.co.jp/it/article/COLUMN/20061107/252787/)

> OCamlは，クラスや継承・インタフェースなどを宣言しなくても，オブジェクトやメソッドの型を自動で推論する。  
> 部分型関係（いわゆるis-a関係の一種）も，実際のオブジェクトの型にしたがい，あらかじめ宣言をしなくても成立する。  
> これを構造的部分型（structural subtyping）という。  
> 逆に，JavaやC++のように，あらかじめ宣言しなければ成立しない部分型関係を名前的部分型（nominal subtyping）と呼ぶ。

すなわち、

# 仮想関数テーブル
サブクラス化(継承)したプログラムにおいてメソッドを呼び出す方法として、最も典型的な方法とされる**仮想関数テーブル**とは何か?要点だけまとめると下記の通り。  

- 継承関係にあるクラス其々が自身のメソッドを登録するための関数ポインタのテーブルを確保する。  
  これが仮想関数テーブル。
- インスタンス化の際、同一シグニチャの関数は仮想関数テーブルの同一エントリに登録される。
- あるインスタンスからメソッドを呼び出すと、そのインスタンスが持つ仮想関数テーブルに登録された関数ポインタが参照する関数を呼び出す(=ディスパッチする)。

仮想関数テーブルの動作を理解するために、下記サイトの説明とサンプルコードを読んだ。  
[++C++; - [雑記] 仮想関数テーブル](https://ufcpp.net/study/csharp/oo_vftable.html)  
ここのコードを少々修正し、コメントを補いつつ下記に示す。

このコードでは、基本クラス`Shape`と、派生クラス`Rectangle`, `Circle`を定義し、それぞれのクラスが面積と周囲長を求めるメソッドを実装する。

- `Test.c`: 各クラスのインスタンス化とメソッド呼び出し

```cpp
#include "ShapeC.h"

#include <stdio.h>
#include <stdlib.h>

void print(Shape* s) {
	printf("%s\n%f\n%f\n\n",
		(char*)s->vftable[0],	// クラス名(型情報)
		((TypeGetArea)s->vftable[VF_GetArea])(s),	// GetArea()呼び出し
		((TypeGetPerimeter)s->vftable[VF_GetPerimeter])(s));	// GetPerimeter()呼び出し
}

int main() {
	Shape* s;

	// Shapeクラスの動作確認
	//s = (Shape*)malloc(sizeof(Shape));
	//Shape_Constructor(s);
	//Shape_Destructor(s);
	//free(s);

	// Rectangleクラスの動作確認
	s = (Shape*)malloc(sizeof(Rectangle));
	Rectangle_Constructor((Rectangle*)s, 2, 3);
	print(s);
	Rectangle_Destructor((Rectangle*)s);
	free(s);

	// Circleクラスの動作確認
	s = (Shape*)malloc(sizeof(Circle));
	Circle_Constructor((Circle*)s, 1.41421356);
	print(s);
	Circle_Destructor((Circle*)s);
	free(s);
}
```

- `Shape.h`: 各クラスの定義(メンバ変数と、メソッド定義に必要な準備)

```cpp
#pragma once

//----------------------------------------------------------------
// class Shape に相当
typedef struct TagShape {
	// 仮想関数テーブル
	void** vftable;
} Shape;

#define VF_GetArea 1	// 仮想関数テーブルでGetArea()が格納されているオフセット
#define VF_GetPerimeter 2	// 仮想関数テーブルでGetPerimeter()が格納されているオフセット

// 関数ポインタ用にtypedef
//// TypeGetArea
typedef double (*TypeGetArea)(Shape* this);
//// TypeGetPerimeter
typedef double (*TypeGetPerimeter)(Shape* this);

// Shape用仮想関数テーブル
extern void* ShapeVftable[];

// -*- プロトタイプ宣言 -*-
// コンストラクタ・デストラクタ
void Shape_Constructor(Shape* this);
void Shape_Destructor(Shape* this);

//----------------------------------------------------------------
// class Rectangle に相当
typedef struct TagRectangle {
	// 基本型を内包させる
	Shape base;

	// Rectangle特有のメンバ変数
	double width;
	double height;
} Rectangle;

// Rectangle用仮想関数テーブル
extern void* RectangleVftable[];

// -*- プロトタイプ宣言 -*-
// コンストラクタ・デストラクタ
void Rectangle_Constructor(Rectangle* this, double w, double h);
void Rectangle_Destructor(Rectangle* this);
// メソッド群
double Rectangle_GetArea(Rectangle* this);
double Rectangle_GetPerimeter(Rectangle* this);

//----------------------------------------------------------------
// class Circle に相当
typedef struct TagCircle {
	// 基本型を内包させる
	Shape base;

	// Circle特有のメンバ変数
	double radius;
} Circle;

// Circle用仮想関数テーブル
extern void* CircleVftable[];

// -*- プロトタイプ宣言 -*-
// コンストラクタ・デストラクタ
void Circle_Constructor(Circle* this, double r);
void Circle_Destructor(Circle* this);
// メソッド群
double Circle_GetArea(Circle* this);
double Circle_GetPerimeter(Circle* this);
```

- `Shape.c`: 各クラスの定義(メソッド定義)

```cpp
#include "ShapeC.h"
#include <stddef.h>

//----------------------------------------------------------------
// class Shape に相当

// コンストラクタ
void Shape_Constructor(Shape* this) {
	// Shapeのフィールドに仮想関数テーブルを登録
	this->vftable = ShapeVftable;
}

// デストラクタ
void Shape_Destructor(Shape* this) {}

// Shapeクラスの仮想関数テーブル
void* ShapeVftable[] = {
	"class Shape",	// クラス名(型情報)
	NULL,			// ShapeにおいてGetArea()は未定義なのでnull
	NULL			// ShapeにおいてGetPerimeter()は未定義なのでnull
};

//----------------------------------------------------------------
// class Rectangle に相当

// コンストラクタ
void Rectangle_Constructor(Rectangle* this, double w, double h) {
	// 基本クラスのコンストラクタを呼び出す
	Shape_Constructor(&this->base);

	// Rectangleのフィールドに仮想関数テーブルを登録(上書き)
	this->base.vftable = RectangleVftable;

	// Rectangleのメンバ変数を初期化
	this->width = w;
	this->height = h;
}

// デストラクタ
void Rectangle_Destructor(Rectangle* this) {
	// 基本クラスのデストラクタを呼び出す
	Shape_Destructor(&this->base);
}

// メソッド群

double Rectangle_GetArea(Rectangle* this) {
	return this->width * this->height;
}

double Rectangle_GetPerimeter(Rectangle* this) {
	return 2 * (this->width + this->height);
}

// Rectangleクラスの仮想関数テーブル
void* RectangleVftable[] = {
	"class Rectangle",		// クラス名(型情報)
	Rectangle_GetArea,		// Rectangleにおいて定義したGetArea()
	Rectangle_GetPerimeter	// Rectangleにおいて定義したGetPerimeter()
};


//----------------------------------------------------------------
// class Circle に相当

// コンストラクタ
void Circle_Constructor(Circle* this, double r) {
	// 基本クラスのコンストラクタを呼び出す
	Shape_Constructor(&this->base);

	// Circleのフィールドに仮想関数テーブルを登録(上書き)
	this->base.vftable = CircleVftable;

	// Circleのメンバ変数を初期化
	this->radius = r;
}

// デストラクタ
void Circle_Destructor(Circle* this) {
	// 基本クラスのデストラクタを呼び出す
	Shape_Destructor(&this->base);
}

// メソッド群

double Circle_GetArea(Circle* this) {
	return 3.14159265358979 * this->radius * this->radius;
}

double Circle_GetPerimeter(Circle* this) {
	return 2 * 3.14159265358979 * this->radius;
}

// Circleクラスの仮想関数テーブル
void* CircleVftable[] = {
	"class Circle",		// クラス名(型情報)
	Circle_GetArea,		// Circleにおいて定義したGetArea()
	Circle_GetPerimeter	// Circleにおいて定義したGetPerimeter()
};
```

- 実行結果

```bash
$ ./test.exe
class Rectangle
6.000000
10.000000

class Circle
6.283185
8.885766
```