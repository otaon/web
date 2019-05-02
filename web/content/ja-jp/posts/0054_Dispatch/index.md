---
title: "ディスパッチ・仮想関数テーブルについて学ぶ"
date:    2019-05-02T00:00:00+09:00
lastmod: 2019-05-02T00:00:00+09:00
draft: false
toc: true
tags: ["ディスパッチ", "多重ディスパッチ", "仮想関数テーブル"]
categories: ["Notes"]
authors:
- otaon
---

# 目的
プログラミングにおいて頻出する「ディスパッチ」という概念についての学習記録を残す。

# 参考文献
本文中に記す。

# ディスパッチとは
ディスパッチとは、多重定義されたメソッド等に対して、そこで呼び出されるべき定義を1つに特定して実行すること。  
ここで、多重定義は、特定されるべきメソッドをクラス毎に定義するものではなく、複数クラスに渡って定義することを指す。  
詳細は下記「単一ディスパッチ」「多重ディスパッチ」の章を参照。

[Wikipedia - 多重ディスパッチ](https://ja.wikipedia.org/wiki/%E5%A4%9A%E9%87%8D%E3%83%87%E3%82%A3%E3%82%B9%E3%83%91%E3%83%83%E3%83%81)

## 単一ディスパッチ
単一ディスパッチ(Single dispatch)とは、ディスパッチする際に、1個の引数(レシーバのみ)が関与して特殊化が行われるもの。  

### 単一ディスパッチでのプログラム例
宇宙船や小惑星といったオブジェクトが出てくるゲームを想定する。  
2つのオブジェクトが衝突する場合、何と何が衝突するかによってプログラムは様々な反応をすると想定する。

Java のように単一ディスパッチのみ行う言語では、コードは次のようになる（ただし、Visitorパターンをこれに活用することも可能）。

```java
// "instanceof" オペレータを使って、実行時のデータ型比較をする
class Asteroid extends Thing {
    public void collide_with(Thing other) {
        if (other instanceof Asteroid) {
            // 小惑星と小惑星の衝突を処理
        }
        else if (other instanceof Spaceship) {
            // 小惑星と宇宙船の衝突を処理
        }
    }
}

class Spaceship extends Thing {
    public void collide_with(Thing other) {
        if (other instanceof Asteroid) {
            // 宇宙船と小惑星の衝突を処理
        }
        else if (other instanceof Spaceship) {
            // 宇宙船と宇宙船の衝突を処理
        }
    }
}
```

**注意**  
上記の例で引数の型を検査するのにオーバーロードを用いずに`instanceof`を使っているのは、オーバーロードを用いると、静的型に対して型の検査してしまうため。
したがって、もし下記の`Asteroid`と`Spaceship`が継承関係にあったら、下記のコードは想定どおりに動かない。

例えば、下記のオーバーロードを用いたコードでは静的型を検査するため、インスタンスが実際にどのクラスなのかを見ずにメソッドが特定される。

```java
// オーバーロードを用いて、静的にデータ型比較をする
class Asteroid extends Thing {
    public void collide_with(Asteroid other) {
        // 小惑星と小惑星の衝突を処理
    }
    public void collide_with(Spaceship other) {
        // 小惑星と宇宙船の衝突を処理
    }
}

class Spaceship extends Thing {
    public void collide_with(Asteroid other) {
        // 宇宙船と小惑星の衝突を処理
    }
    public void collide_with(Spaceship other) {
        // 宇宙船と宇宙船の衝突を処理
    }
}
```

## 多重ディスパッチ
多重ディスパッチ(Multiple dispatch)またはマルチメソッド(Multimethods)とは、ディスパッチする際に、2個以上の引数(レシーバ含む)が関与して特定（特殊化）がおこなわれるもの。

### 多重ディスパッチでのプログラム例
単一ディスパッチと同様に、宇宙船や小惑星といったオブジェクトが出てくるゲームを想定する。  
2つのオブジェクトが衝突する場合、何と何が衝突するかによってプログラムは様々な反応をすると想定する。

Common Lispのように多重ディスパッチをする言語では、コードは次のようになる。  
CLOSでは、メソッドはクラス内ではなく、独立して定義される。このとき、レシーバも引数にする。  
したがって、CLOSにおいてメソッドをディスパッチする際は、全ての引数の動的型を検査する。

```cl
(defmethod collide-with ((x asteroid) (y asteroid))
  ;; 小惑星が小惑星に衝突する場合を処理
  ...)
(defmethod collide-with ((x asteroid) (y spaceship))
  ;; 小惑星が宇宙船に衝突する場合を処理
  ...)
(defmethod collide-with ((x spaceship) (y asteroid))
  ;; 宇宙船が小惑星に衝突する場合を処理
  ...)
(defmethod collide-with ((x spaceship) (y spaceship))
  ;; 宇宙船が宇宙船に衝突する場合を処理
  ...)
```

# 仮想関数テーブルとは
## ポリモーフィズム(部分型付け)と仮想関数テーブルの関連性
ポリモーフィズム(部分型付け)とは、基本型が使われている箇所を完全に置き換え可能な部分型を定義すること。
そして、ポリモーフィック(多相)なメソッドを呼び出す際、呼び出し対象のメソッドを特定するために**仮想関数テーブル**を使用する。  
ポリモーフィズムの詳細については「[ポリモーフィズム・部分型付け・派生型]({{<relref "posts/0053_Subtype/index.md" >}})」を参照。

## 仮想関数テーブルの役割と仕組み
**仮想関数テーブル(vtable)**とは、クラスごとに定義された、クラスメソッドorオブジェクトメソッドの関数ポインタのテーブルのこと。
各オブジェクトは自らのクラスのvtableへのポインタを持つ。

仮想関数テーブルの仕組みの概要は下記の通り。  

- 各々のインスタンスが、自身のメソッドを登録するための関数ポインタのテーブル(=**仮想関数テーブル**)を確保する。  
  - クラスは継承関係にあってもなくても構わない。
- インスタンス化の際、同一シグニチャの関数は自身の仮想関数テーブルの同一箇所に登録される。
  - 派生型の仮想関数テーブルでは、派生型用の関数ポインタで基本型用に登録された関数ポインタを**上書き登録**する。
- あるインスタンスからメソッドを呼び出すと、そのインスタンスが持つ仮想関数テーブルに登録された関数ポインタが参照する関数を呼び出す(=ディスパッチする)。

ディスパッチには、先述したとおり大別して2種類ある(単一ディスパッチ、多重ディスパッチ)。

[Wikipedia - ポリモーフィズム#部分型付け](https://ja.wikipedia.org/wiki/%E3%83%9D%E3%83%AA%E3%83%A2%E3%83%BC%E3%83%95%E3%82%A3%E3%82%BA%E3%83%A0#%E9%83%A8%E5%88%86%E5%9E%8B%E4%BB%98%E3%81%91)

> - 多くのオブジェクト指向言語では、**仮想関数の呼び出しに1番目の引数 (`this`オブジェクト)の vtable だけを参照する単一ディスパッチ**を採用している。  
> - つまりその他の引数の実行時の型は仮想関数の呼び出しに全く無関係である。  
> - 一方でCommon Lisp Object System(CLOS)などでは、**メソッドの呼び出しが全ての引数に対して多相的となる多重ディスパッチ**を採用している。

### 仮想関数テーブルの実装例(単一ディスパッチ用)
この章では、上記の仮想関数テーブルについての説明を、実装例によって理解する。  
ここでは、Javaなどで採用されている「単一ディスパッチ」における仮想関数テーブルの実装を例示する。

まず、仮想関数テーブルの動作を理解するために、下記サイトの説明とサンプルコードを使用した。
ここで掲載されているコードを少々修正し、コメントを補いつつ下記に引用する。  
[++C++; - [雑記] 仮想関数テーブル](https://ufcpp.net/study/csharp/oo_vftable.html)  


このコードでは、下記を実施している。

- 基本クラス`Shape`と、派生クラス`Rectangle`, `Circle`を定義する。
- 各々のクラスで面積と周囲長を求めるメソッドを実装する。

#### `Test.c`: 各クラスのインスタンス化とメソッド呼び出し
処理の説明はコード中のコメントに残した。本文では要点のみ説明する。

このプログラムにおけるクラスのインスタンス化は、下記の通り行う。  
これにより、派生型のインスタンスを、基本型の変数に代入したことになる。

```cpp
SuperClassType variable = (SuperClassType*)malloc(sizeof(DerivedClassType));
```

また、各インスタンスのメソッド呼び出しは下記の通り行う。  
`define`部分は、仮想関数テーブルにおいて呼び出し対象のメソッドが格納されているエントリのオフセットを表す。  
`typedef`部分は、呼び出し対象のメソッドの型を表す。
メソッド呼び出し部分では、仮想関数テーブルから呼び出し対象のメソッドを取り出し、適切な型キャストをした上で呼び出している。

```cpp
#define VF_MethodType1 1

typedef int (*MethodType1)(SuperClassType* this);

i = ((MethodType1)s->vftable[VF_MethodType1])(variable);
```

##### Test.c

```cpp
#include "ShapeC.h"

#include <stdio.h>
#include <stdlib.h>

void print(Shape* s) {
    printf("%s\n%f\n%f\n\n",
        (char*)s->vftable[0],    // クラス名(型情報)
        ((TypeGetArea)s->vftable[VF_GetArea])(s),    // GetArea()呼び出し
        ((TypeGetPerimeter)s->vftable[VF_GetPerimeter])(s));    // GetPerimeter()呼び出し
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

#### `Shape.h`: 各クラスの定義(メンバ変数と、メソッド定義に必要な準備)

##### Shape.h

```cpp
#pragma once

//----------------------------------------------------------------
// class Shape に相当
typedef struct TagShape {
    // 仮想関数テーブル
    void** vftable;
} Shape;

// 仮想関数テーブルでGetArea()が格納されているオフセット
#define VF_GetArea 1
// 仮想関数テーブルでGetPerimeter()が格納されているオフセット
#define VF_GetPerimeter 2

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

#### `Shape.c`: 各クラスの定義(メソッド定義)
クラス定義では、メンバ変数、コンストラクタ、メソッド、仮想関数テーブルを定義する。

このプログラムでは、メンバ変数は、それが属すクラスのコンストラクタにおいて初期化される。

```cpp
// Rectangleのメンバ変数を初期化
this->width = w;
this->height = h;
```

また、メソッドを定義したら、その関数ポインタを仮想関数テーブルの適切なエントリに登録する。  
この際、派生クラスの場合は、内包する基本クラスの仮想関数テーブル(`this->base.vftable`)を使用する。

```cpp
// Rectangleクラスの仮想関数テーブル
void* RectangleVftable[] = {
    "class Rectangle",        // クラス名(型情報)
    Rectangle_GetArea,        // Rectangleにおいて定義したGetArea()
    Rectangle_GetPerimeter    // Rectangleにおいて定義したGetPerimeter()
};

/* コンストラクタ内 */
// Rectangleのフィールドに仮想関数テーブルを登録(上書き)
this->base.vftable = RectangleVftable;
```

##### Shape.c
###### `#include`部分

```cpp
#include "ShapeC.h"
#include <stddef.h>
```

###### Shapeクラス定義部分

```cpp
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
    "class Shape",    // クラス名(型情報)
    NULL,             // ShapeにおいてGetArea()は未定義なのでnull
    NULL              // ShapeにおいてGetPerimeter()は未定義なのでnull
};
```

###### Rectangleクラス定義部分

```cpp
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
    "class Rectangle",        // クラス名(型情報)
    Rectangle_GetArea,        // Rectangleにおいて定義したGetArea()
    Rectangle_GetPerimeter    // Rectangleにおいて定義したGetPerimeter()
};
```

###### Circleクラス定義部分

```cpp
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
    "class Circle",        // クラス名(型情報)
    Circle_GetArea,        // Circleにおいて定義したGetArea()
    Circle_GetPerimeter    // Circleにおいて定義したGetPerimeter()
};
```

#### 実行結果
実行結果は下記の通り。インスタンスの型によって呼び出されるメソッドが異なることが分かる。

```bash
$ ./test.exe
class Rectangle
6.000000
10.000000

class Circle
6.283185
8.885766
```