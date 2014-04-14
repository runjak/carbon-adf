SingleResultViewGraph = Backbone.View.extend({
  initialize: function(){
    window.foo = this; // FIXME DEBUG
    this.setTree = {};
    this.paper = Raphael(this.$('.paper').get(0), 400, 400);
    this.style = {
      start: {x: 10, y: 10}
    , space: {x: 10, y: 10}
    };
  }
, setResult: function(r){
    if(this.model){
      this.model.results.off(null, null, this);
    }
    this.model = r;
    if(r){
      r.results.on('add remove reset', this.render, this);
    }
    this.render();
  }
, render: function(){
    console.log('SingleResultViewGraph.render()');
    this.paper.clear();
    if(!this.model) return;
    //Finding the roots of the result tree:
    var roots = {};
    this.model.results.each(function(r){
      if(r.parentCount() === 0)
        roots[r.get('id')] = r;
    });
    //Drawing the tree:
    var levels = [] // Elements on one level in the graph
      , nodes  = {} // rId -> Element
      , style  = this.style
      , pos    = _.clone(style.start)
      , maxX   = 0, maxY = 0;
    while(_.keys(roots).length > 0){
      var newRoots = {}
        , level    = [];
      //Building current level:
      _.each(_.values(roots), function(r){
        var text = r.showSet()
          , el   = this.paper.text(pos.x, pos.y, text).attr({
          fill: '#000000'
        , 'font-size': 16
        , 'stroke-width': 1
        });
        console.log(text+' at: '+JSON.stringify(pos));
        //Adding element to level and nodes:
        level.push(el);
        nodes[r.get('id')] = el;
        //Updating position:
        var width = 8*text.length;
        //var width = el.node.getSubStringLength(0, text.length);
        console.log('Width: ' + width);
        pos.x += width + style.space.x;
        if(pos.x > maxX) maxX = pos.x;
        //Adding children to newRoots:
        _.map(_.values(r.children), function(c){
          newRoots[c.get('id')] = c;
        });
      }, this);
      //Setting position for next level:
      pos.x  = style.start.x;
      pos.y += style.space.y;
      if(pos.y > maxY) maxY = pos.y;
      //Updating levels and roots:
      levels.push(level);
      roots = newRoots;
    }
    console.log(levels);
    //Adjusting size of the paper:
    console.log('Setting paperSize: '+JSON.stringify({width: maxX, height: maxY}));
    this.paper.setSize(maxX, maxY);
    this.paper.renderfix();
    this.paper.safari();
    //Spacing the nodes per layer:
    //Connecting nodes by lines:
  }
});
