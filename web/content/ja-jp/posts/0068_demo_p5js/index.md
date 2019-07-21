---
title: "p5.js Demo01 Rotating lines"
date:    2019-07-20T23:00:00+09:00
lastmod: 2019-07-20T23:00:00+09:00
draft: false
toc: true
tags: ["p5.js"]
categories: ["Demos"]
authors:
- otaon
---

# デモ
{{< p5js sketch="sketch.js">}}
<div id="canvas" style="text-align: center;"></div>  

# 概要
3色の線(=点の集まり)をY軸に対して回転させながら表示するだけの単純なプログラム。  
単純だけど、`blendMode(SCREEN);`を指定してあると光った感じになって綺麗。

もともとProcessingでP3Dを用いて3D表示させていたProgramをp5.jsに移植した。  
雑で変更ミニマムな移植なので修正したほうが良い(多分面倒だからしない)。

Processingのほとんどの関数がp5.jsにもあり、しかも使い方もほぼ同じなので、あまり難しいことをしなければ変更不要なのが嬉しい。


# コード
```javascript
var colors = [];

var AMOUNT = 100;
var points = [];

function setup()
{
	let canvas = createCanvas(500, 500, WEBGL);
	canvas.parent('canvas');

	blendMode(SCREEN);
	colorMode(HSB, 1);
	smooth();

	background(0);
	strokeWeight(3);

	colors = [
		color(0.95, 1, 1),	// 赤
		color(0.54, 1, 1),	// 青
		color(0.16, 1, 1)	// 黄
	];

	for (var i = 0; i < AMOUNT; i++) {
		points[i] = [];
		points[i][0] = random(-150, 150);
		points[i][1] = random(-150, 150);
		points[i][2] = random(-150, 150);
	}
}

function draw()
{
	circle(0, 0, 0);
	background(0);
	//translate(width / 2, height / 2);	// (0, 0) is the center in the default value in WEBGL
	rotateY(frameCount / 20.0);

	for (var rot_count = 0; rot_count < 50; rot_count++) {
		rotateY(0.005);
		var i = 0;
		stroke(colors[0]);
		for (; i < AMOUNT / 3; i++) {
			point(points[i][0], points[i][1], points[i][2]);
		}

		stroke(colors[1]);
		for (; i < 2 * AMOUNT / 3; i++) {
			point(points[i][0], points[i][1], points[i][2]);
		}

		stroke(colors[2]);
		for (; i < AMOUNT; i++) {
			point(points[i][0], points[i][1], points[i][2]);
		}
	}
}
```

## 注意点
Processingでは`(x, y) = (0, 0)`が左上の座標を表すため、`translate(width / 2, height / 2);`が必要。  
しかし、p5.jsのWEBGLでは(おそらく)すでに`(x, y) = (0, 0)`がキャンバスの中心を指しているため不要。
