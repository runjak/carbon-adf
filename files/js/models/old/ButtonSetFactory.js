ButtonSetFactory  = function(paper){
  paper.buttonSet = function(view){
    var s    = paper.set();
    var hide = s.hide;
    var show = s.show;
    s.hide = function(){this.visible = false; return hide.call(s);};
    s.show = function(){this.visible =  true; return show.call(s);};
    s.init = function(){
      _.each(this, function(btn){
        btn[0].attr('fill','#666');
        var glow = null;
        btn.mouseover(function(){
          if(glow) glow.remove();
          glow = btn[0].glow({color: '#fff'});
        }).mouseout(function(){
          if(glow) glow.remove();
          glow = null;
        });
      });
      return this;
    }
    s.place = function(){
      var p = view.back.attr(['x','y','height']);
      var offset = {x: p.x, y: p.y + p.height + 2};
      _.each(this, function(btn){
        var r = btn[0].attr(['x','y','width']);
        var i = btn[1].attr(['x','y']);
        i = {x: i.x - r.x, y: i.y - r.y};
        btn[0].attr({x: offset.x, y: offset.y});
        btn[1].attr({x: offset.x + i.x, y: offset.y + i.y});
        offset.x += r.width + 3;
      });
      return this;
    };
    return s;
  };
  return paper;
};
