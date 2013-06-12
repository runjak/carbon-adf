/**
  Defines basic methods that aid working with
  a paper from RaphaelJs.
  All methods may take this.paper as given.
*/
PaperView = Backbone.View.extend({
  initialize: function(){
    this.paper = null;
  }
, mkPaper: function(p){
    this.paper = Raphael(p, 65536, 65536);
  }
, drawGrid: function(){
    var w = this.paper.width;
    var h = this.paper.height;
    var grid = '';
    for(var i = 10; i < w; i += 20)
      grid += 'M'+i+' 0L'+i+' '+h;
    for(var i = 10; i < h; i += 20)
      grid += 'M0 '+i+'L'+w+' '+i;
    this.paper.path(grid+'Z').attr({stroke: '#03c', 'stroke-opacity': 0.5});
    return this;
  }
, centerViewBox: function(w, h){
    var pw = this.paper.width;
    var ph = this.paper.height;
    var x  = (pw - w)/2;
    var y  = (ph - h)/2;
    this.paper.setViewBox(x, y, pw, ph);
    return this;
  }
, getViewBox: function(){
    var vb = this.paper._viewBox;
    return {x: vb[0], y: vb[1], w: vb[2], h: vb[3]};
  }
, getContainerSize: function(){
    var c = $(this.paper.canvas).parent();
    return {w: c.width(), h: c.height()};
  }
, pan: function(dx, dy){
    var vb = this.getViewBox();
    var x  = vb.x + dx;
    var y  = vb.y + dy;
    var c  = this.getContainerSize();
    x = _.max([0, _.min([vb.w - c.w, x])]);
    y = _.max([0, _.min([vb.h - c.h, y])]);
    this.paper.setViewBox(x, y, vb.w, vb.h);
    return this;
  }
, getZoom: function(){
    var w  = this.paper.width;
    var h  = this.paper.height;
    var vb = this.getViewBox();
    return {zx: w/vb.w, zy: h/vb.h};
  }
, setZoom: function(zx, zy){
    var w  = this.paper.width  / zx;
    var h  = this.paper.height / zy;
    var vb = this.getViewBox();
    this.paper.setViewBox(vb.x, vb.y, w, h);
    return this;
  }
, deltaZoom: function(dzx, dzy){
    var z  = this.getZoom();
    var zx = _.min([2,_.max([.1, z.zx + dzx])]);
    var zy = _.min([2,_.max([.1, z.zy + dzy])]);
    this.setZoom(zx, zy);
    return this;
  }
});
