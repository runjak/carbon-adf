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
      var bb1 = obj1.getBBox()
        , bb2 = obj2.getBBox();
      //Calculating:
      var e  = Springy.Vector.lineBoxToBox(bb1,bb2);
      var st = e.p2.subtract(e.p1);
      var o  = st.normalise().orth();
      var l  = st.magnitude();
      st = e.p1.add(st.multiply((l-10)/l));
      //Drawing:
      var s = e.p1, t = e.p2;
      //Check if s, t are different:
      if(s.x == t.x && s.y == t.y)
        return;
      //Calculating the path…
      var p = Raphael.format("M{0} {1}L{2} {3}", s.x, s.y, t.x, t.y);
      _.each([o, o.multiply(-1)], function(r){
        var s = st.add(r.multiply(5));
        p += Raphael.format(",M{0} {1}L{2} {3}", s.x, s.y, t.x, t.y);
      });
      if(this.path){
        this.path.attr('path', p);
        window.p = this.path;
      }else{
        this.path = th.path(p).attr({'stroke-width': '2'}).attr(style);
      }
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
