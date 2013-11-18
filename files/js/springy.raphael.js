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

Raphael.el.setOffset = function() {
  this.offsetx = this.attr('x');
  this.offsety = this.attr('y');
}

Raphael.fn.moveSet = function(set, x, y){
  set.forEach(function(item){
    item.attr({
      x: x + item.offsetx
    , y: y + item.offsety
    })
  });
}
