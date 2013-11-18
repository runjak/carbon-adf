/**
  This file extends the Springy.js Vector with some functions.
*/
Springy.Vector.prototype.middleFromBBox = function(b){
  return new Springy.Vector(
    b.x + 0.5*b.width
  , b.y + 0.5*b.height
  );
};

Springy.Vector.prototype.dot = function(v){
  return this.x*v.x+this.y*v.y;
};

/**
  Produces an orthogonal vector
*/
Springy.Vector.prototype.orth = function(){
  return new Springy.Vector(this.y, -this.x);
};

/**
  Edges are simple objects in the form of
  {p1: Springy.Vector, p2: Springy.Vector}.
  This function returns Springy.Vector || null.
  The algorithm is adapted from
  http://wiki.processing.org/w/Line-Line_intersection
*/
Springy.Vector.prototype.edgeIntersection = function(e1, e2){
  var p1 = e1.p1
    , p2 = e1.p2
    , p3 = e2.p1
    , p4 = e2.p2;
  var x1 = p1.x, x2 = p2.x, x3 = p3.x, x4 = p4.x;
  var y1 = p1.y, y2 = p2.y, y3 = p3.y, y4 = p4.y;
  var bx = x2-x1;
  var by = y2-y1;
  var dx = x4-x3;
  var dy = y4-y3;
  var b_dot_d_perp = bx*dy - by*dx;
  if(b_dot_d_perp === 0) return null;
  var cx = x3-x1;
  var cy = y3-y1;
  var t = (cx*dy - cy*dx)/b_dot_d_perp;
  if(t < 0 || t > 1) return null;
  return new Springy.Vector(x1 + t*bx, y1 + t*by);
};

/**
  Calculates a line between two given BBoxes,
  where the line has the form
  {p1: Springy.Vector, p2: Springy.Vector}.
*/
paper.lineBoxToBox = function(b1, b2){
  var mm = { p1: this.middleFromBBox(b1)
           , p2: this.middleFromBBox(b2)}; 
  var e1 = this.edgesFromBBox(b1);
  var e2 = this.edgesFromBBox(b2);
  var ps1 = _.map(e1, function(e){return this.edgeIntersection(mm, e);});
  var ps2 = _.map(e2, function(e){return this.edgeIntersection(mm, e);});
  var boxCheck = function(b){return function(p){
    if(p === null)       return false;
    if(p.x < (b.x-0.1))  return false;
    if(p.y < (b.y-0.1))  return false;
    if(p.x > (b.x2+0.1)) return false;
    if(p.y > (b.y2+0.1)) return false;
    return true;
  };};
  ps1 = _.filter(ps1, boxCheck(b1));
  ps2 = _.filter(ps2, boxCheck(b2));
  var closer = function(x){return function(p,m){
    var a = paper.sqLength(paper.vSubtract(x,p));
    var b = paper.sqLength(paper.vSubtract(x,m));
    if(a < b) return p;
    return m;
  }};
  return {
    p1: _.reduce(ps1, closer(mm.p2), {x: b1.x, y: b1.y})
  , p2: _.reduce(ps2, closer(mm.p1), {x: b2.x, y: b2.y})
  };
};

/**
  Projects this vector onto v
  FIXME chk, if this is used.
*/
Springy.Vector.prototype.project = function(v){
  var n = v.normalise()
    , s = this.dot(v);
  return n.multiply(s);
};
