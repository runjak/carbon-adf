Calculator = function(paper){
  paper.middleFromBBox = function(b){
    return {x: b.x + 0.5*b.width, y: b.y + 0.5*b.height};
  };
  paper.edgesFromBBox = function(b){
    return [
      {p1: {x: b.x , y: b.y }, p2: {x: b.x2, y: b.y }}
    , {p1: {x: b.x , y: b.y }, p2: {x: b.x , y: b.y2}}
    , {p1: {x: b.x2, y: b.y }, p2: {x: b.x2, y: b.y2}}
    , {p1: {x: b.x , y: b.y2}, p2: {x: b.x2, y: b.y2}}
    ];
  };
  paper.vAdd = function(v1, v2){
    return {x: v1.x+v2.x, y: v1.y+v2.y};
  };
  paper.vSubtract = function(v1, v2){
    return {x: v1.x-v2.x, y: v1.y-v2.y};
  };
  paper.scProduct = function(s, v){
    return {x: s*v.x, y: s*v.y};
  };
  paper.dotProduct = function(a, b){
    return a.x*b.x+a.y*b.y;
  };
  paper.sqLength = function(v){
    return Math.pow(v.x, 2) + Math.pow(v.y, 2);
  };
  paper.unit = function(v){
    var u = Math.sqrt(paper.sqLength(v));
    return {x: v.x/u, y: v.y/u};
  };
  paper.vOrth = function(v){
    return paper.unit({x: v.y, y: -v.x});
  };
  //http://wiki.processing.org/w/Line-Line_intersection
  paper.edgeIntersection = function(e1, e2){
    var p1 = e1.p1;
    var p2 = e1.p2;
    var p3 = e2.p1;
    var p4 = e2.p2;
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
    return {
      x: x1 + t*bx
    , y: y1 + t*by
    };
  };
  paper.lineBoxToBox = function(b1, b2){
    var mm = { p1: paper.middleFromBBox(b1)
             , p2: paper.middleFromBBox(b2)}; 
    var e1 = paper.edgesFromBBox(b1);
    var e2 = paper.edgesFromBBox(b2);
    var ps1 = _.map(e1, function(e){return paper.edgeIntersection(mm, e);});
    var ps2 = _.map(e2, function(e){return paper.edgeIntersection(mm, e);});
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
  /* Projects a onto b */
  paper.project = function(a, b){ // FIXME chk if this function is used
    b = paper.unit(b);
    var s = paper.dotProduct(a, b);
    return paper.scProduct(s, b);
  };
  return paper;
};
