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
    return Math.pow(v.x) + Math.pow(v.y);
  };
  paper.unit = function(v){
    var u = Math.sqrt(paper.sqLength(v));
    return {x: v.x/u, y: v.y/u};
  };
  //DO MAGIC HERE:
  //FIXME reimplement with new wisdom
  paper.edgeIntersection = function(e1, e2){
    var a  = e1.p1;
    var ab = paper.vSubtract(e1.p2, e1.p1);
    var c  = e2.p1;
    var cd = paper.vSubtract(e2.p2, e2.p1);
    var ac = paper.vSubtract(c, a);
    var cdab = paper.vSubtract(ab, cd);
    var t  = paper.dotProduct(ac, paper.scProduct(-1, cdab));
    if(t < 0 || t > 1) return null;
    return paper.vAdd(a, paper.scProduct(t, ab));
  };
  /* Projects a onto b */
  paper.project = function(a, b){ // FIXME chk if this function is used
    b = paper.unit(b);
    var s = paper.dotProduct(a, b);
    return paper.scProduct(s, b);
  };
  return paper;
};
/*
/ **
  Calculation:
  This is mainly to support PaperRelation.js,
  which needs to get the closest point on the edge 
  of one PaperArticle to another.
* /
, edgesFromBBox: 
, edgeSqLength: function(e){
    return Math.pow(e.x2 - e.x) + Math.pow(e.y2 - e.y);
  }
*/
