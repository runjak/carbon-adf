SingleResultViewGraph = Backbone.View.extend({
  initialize: function(){
    window.foo   = this; // FIXME DEBUG
    this.setTree = {};
    this.$canvas = this.$('canvas');
    this.canvas  = this.$canvas.get(0);
    this.ctx     = this.canvas.getContext("2d");
    this.style   = {
      start: {x: 10, y: 10}
    , space: {x: 10, y: 26}
    , font:  "16px Verdana, sans-serif"
    };
    this.ctx.font = "16px Verdana, sans-serif";
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
    //We reuse DiscussionGraphView.resize for our own canvas:
    window.App.views.singleDiscussionView.discussionGraphView.resize.call(this);
    //Cleaning the canvas:
    this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
    //Don't draw if there's no model:
    if(!this.model) return;
    var levels  = this.model.levels
      , lWidths = []
      , style   = this.style;
    //Creating lWidth objects:
    _.each(levels, function(level){
      lWidths.push(_.map(level, function(r){
        var t = r.showSet()
          , w = this.ctx.measureText(t).width;
        return {text: t, width: w, result: r};
      }, this));
    }, this);
    //Calculating lWidthSums:
    var lWidthSums = {}; // Index -> WidthSum
    for(var i = 0; i < lWidths.length; i++){
      var ws    = lWidths[i]
        , space = (ws.length - 1) * style.space.x;
      lWidthSums[i] = _.reduce(_.pluck(ws, 'width'), function(sum, width){
        return sum + width;
      }, space);
    }
    //Finding the maximum width:
    var maxWidth = _.max(_.values(lWidthSums));
    //Drawing the levels:
    var pos    = _.clone(style.start)
      , rBoxes = {}; // ResultId -> BBox
    for(var i = 0; i < lWidths.length; i++){
      //Setup for the level:
      var ws    = lWidths[i]
        , space = style.space.x + (maxWidth - lWidthSums[i])/(ws.length - 1);
      //Drawing the ws:
      _.each(ws, function(w){
        this.ctx.fillText(w.text, pos.x, pos.y);
        rBoxes[w.result.get('id')] = {
          x:      pos.x
        , y:      pos.y
        , width:  w.width
        , height: 16
        , x2:     pos.x + w.width
        , y2:     pos.y - 8};
        pos.x += space + w.width;
      }, this);
      //End of the line:
      pos.x  = style.start.x;
      pos.y += style.space.y;
    }
    //Drawing parent/child relations:
    this.ctx.beginPath();
    var t = this;
    this.model.results.each(function(p){
      var pId  = p.get('id')
        , pBox = rBoxes[pId]
        , pX   = pBox.x + 0.5 * pBox.width;
      _.each(_.keys(p.children), function(cId){
        var cBox = rBoxes[cId];
        t.ctx.moveTo(pX, pBox.y + 3);
        t.ctx.lineTo(cBox.x + 0.5 * cBox.width, cBox.y2 - 3);
      });
    });
    this.ctx.stroke();
    //FIXME maybe use rBoxes for click events!
  }
});
