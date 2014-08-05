'use strict';

function Color(r, g, b, a) {

  this.red = r;
  this.green = g;
  this.blue = b;
  this.alpha = a;

  this.setR = function (r) {
    this.red = r;
  };

  this.setG = function (g) {
    this.green = g;
  };

  this.setB = function (b) {
    this.blue = b;
  };

  this.setA = function (a) {
    this.alpha = a;
  };

  this.toRGBColorString = function () {
    var color = [this.red, this.green, this.blue];
    return 'rgb(' + color.join(',')  +  ')';
  }

  this.toRGBAColorString = function () {
    var color = [this.red, this.green, this.blue, this.alpha];
    return 'rgba(' + color.join(',')  +  ')';
  }

  this.fromRGBColorString = function (rgb) {
    var color = rgb.slice(4, rgb.length - 1).split(',');
    this.red = parseInt(color[0], 10);
    this.green = parseInt(color[1], 10);
    this.blue = parseInt(color[2], 10);
  }

  this.fromRGBAColorString = function (rgba) {
    var color = rgba.slice(5, rgb.length - 1).split(',');
    this.red = parseInt(color[0], 10);
    this.green = parseInt(color[1], 10);
    this.blue = parseInt(color[2], 10);
    this.alpha = parseInt(color[3], 10);
  }


  this.getColor = function () {
    return {
      red: this.red,
      green: this.green,
      blue: this.blue,
      alpha: this.alpha
    }
  }

  this.setColor = function (color) {
    this.red = color.red;
    this.green = color.green;
    this.blue = color.blue;
    this.alpha = color.alpha;
  }
}