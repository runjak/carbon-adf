/**
  Defines basic methods that aid working with
  a paper from RaphaelJs.
  All methods may take this.paper as given.
*/
PaperView = Backbone.View.extend({
//Setup
  initialize: function(){
    this.paper = null;
  }
, mkPaper: function(p){
    this.paper = ButtonSetFactory(Raphael(p, 1000, 1000));
  }
//Data:
, getContainer: function(){
    return $(this.paper.canvas).parent();
  }
, getContainerSize: function(){
    var c = this.getContainer();
    return {w: c.width(), h: c.height()};
  }
, getViewBox: function(){
    var vb = this.paper._viewBox;
    if(vb === null || typeof(vb) === 'undefined')
      return {x: 0, y: 0
             ,w: this.paper.width
             ,h: this.paper.height
             ,z: this.paper._vbSize};
    return {x: vb[0], y: vb[1]
           ,w: vb[2], h: vb[3]
           ,z: this.paper._vbSize};
  }
//Perspective:
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
, setZoom: function(z){
    var w  = this.paper.width  * z;
    var h  = this.paper.height * z;
    var vb = this.getViewBox();
    this.paper.setViewBox(vb.x, vb.y, w, h);
    return this;
  }
, deltaZoom: function(dz){
    var z = this.getViewBox().z;
        z = _.min([2,_.max([.1, z + dz])]);
    this.setZoom(z);
    return this;
  }
, resetPanZoom: function(){
    var c  = this.getContainerSize();
    var pw = this.paper.width;
    var ph = this.paper.height;
    var x  = (pw - c.w)/2;
    var y  = (ph - c.h)/2;
    this.paper.setViewBox(x, y, pw, ph);
    return this;
  }
, mouseToPaper: function(p){
    var o  = this.getContainer().offset();
    var vb = this.getViewBox();
    return {
      x: (p.x - o.left) * vb.z + vb.x
    , y: (p.y - o.top ) * vb.z + vb.y
    };
  }
//Drawing:
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
});
