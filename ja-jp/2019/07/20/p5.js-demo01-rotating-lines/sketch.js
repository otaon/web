var colors = [];

var AMOUNT = 100;
var points = [];

function setup()
{
	let canvas = createCanvas(500, 500, WEBGL);
	canvas.parent('canvas');

	blendMode(SCREEN);
	colorMode(HSB, 1);

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
