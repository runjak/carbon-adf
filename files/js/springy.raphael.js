/**
 * Code taken from the demo-raphael.html in the springy repo [1] by Jakob Runge 
 * This is mostly a copy&paste to get a stand alone version of the script,
 * but some minor adjustments are also included, to fit the job.
 * [1]: https://github.com/dhotson/springy
 */

/**
 * Originally grabbed from the official RaphaelJS Documentation
 * http://raphaeljs.com/graffle.html
 * Adopted (arrows) and commented by Philipp Strathausen http://blog.ameisenbar.de
 * Licenced under the MIT licence.
 */

/**
 * Usage:
 * connect two shapes
 * parameters:
 *    source shape [or connection for redrawing],
 *    target shape,
 *    style attrs to be given to the set that resembles the connection between both shapes
 * returns:
 *    connection { draw = function() }
 */
Raphael.fn.connection = function(obj1, obj2, style){
  var th = this, edge = {
    path: null
  , draw: function(){
      //Setup:
      var bb1 = obj1.getBBox()
        , bb2 = obj2.getBBox()
        , e  = Springy.Vector.lineBoxToBox(bb1,bb2)
        , st = e.p2.subtract(e.p1)
        , o  = st.orth().normalise(), o10 = o.multiply(10);
      //We want to build a path over 4 stations, a b c d, where cd gets an arrow added.
      var a = e.p1, b = a.add(st.divide(5).add(o10))
        , d = e.p2, c = d.add(st.divide(-5).add(o10));
      //Calculating the path…
      var p = Raphael.format("M{0} {1}L{2} {3}L{4} {5}L{6} {7}", a.x, a.y, b.x, b.y, c.x, c.y, d.x, d.y);
      //Adding the arrow tip
      var cd   = d.subtract(c)
        , part = cd.normalise().multiply(-5)
        , o1   = part.orth(), o2 = o1.multiply(-1)
        , end  = d.add(part)
        , p1   = end.add(o1), p2 = end.add(o2);
      p += Raphael.format("M{0} {1}L{2} {3}L{4} {5}Z", p1.x, p1.y, d.x, d.y, p2.x, p2.y);
      //Drawing:
      if(this.path){
        this.path.attr('path', p);
        window.p = this.path;
      }else{
        this.path = th.path(p).attr({'stroke-width': '2'}).attr(style);
      }
      //Done :)
      return this.path;
    }
  };
  edge.draw();
  return edge;
};

/***/
Raphael.el.place = function(v){
  this.transform(Raphael.format("T{0},{1}", v.x, v.y));
  return this.updateTVec();
};

/**
  Shift an element by a Springy.Vector.
*/
Raphael.el.shift = function(v){
  console.log('shift!');
  var t = this.tVec || new Springy.Vector(0,0)
    , p = t.add(v);
  this.transform(Raphael.format("t{0},{1}", p.x, p.y));
  return this.updateTVec();
};

/**
  Updates the TVec of an element.
*/
Raphael.el.updateTVec = function(){
  var bbox  = this.getBBox();
  this.tVec = new Springy.Vector(bbox.x, bbox.y);
  return this;
};

/***/
Raphael.el.showHover = function(){
  var t = this, glow = null, fill = null;
  return t.mouseover(function(){
    glow = t.glow();
    fill = t.attr('fill');
    t.attr('fill', '#4DB3D1');
  }).mouseout(function(){
    if(glow){
      glow.remove();
      glow = null;
    }
    if(fill){
      t.attr('fill', fill);
      fill = null;
    }
  });
};
