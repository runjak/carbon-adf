/**
  Renders a relation on a paper.
  model: Relation
  el:    Paper
*/
PaperRelation = Backbone.View.extend({
  initialize: function(){
    this.model.source.on('change:posX', this.render, this)
                     .on('change:posY', this.render, this);
    this.model.target.on('change:posX', this.render, this)
                     .on('change:posY', this.render, this);
    this.arrow = null; // Elements on the paper.
    this.render();
  }
, render: function(){
    //Cleaning:
    this.removeArrow();
    //Calculating:
    var s = this.model.source.getXY();
    var t = this.model.target.getXY();
    //Drawing:
    this.el.setStart();
    this.el.path('M'+s.x+','+s.y+'L'+t.x+','+t.y+'Z');
    this.el.circle(t.x, t.y ,5);
    this.arrow = this.el.setFinish();
  }
, remove: function(){
    this.model.source.off(null, null, this);
    this.model.target.off(null, null, this);
    this.removeArrow();
  }
, removeArrow: function(){
    if(!this.arrow) return;
    this.arrow.remove();
    this.arrow = null;
  }
});
