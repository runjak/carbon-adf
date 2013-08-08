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
    //Setup:
    var p = this.el;
    //Calculating:
    var e = this.el.lineBoxToBox(
      window.App.views.discussionArticles[this.model.source.get('id')].getBBox()
    , window.App.views.discussionArticles[this.model.target.get('id')].getBBox());
    var st = p.vSubtract(e.p2, e.p1);
    var o  = p.vOrth(st);
    var l  = Math.sqrt(p.sqLength(st));
    st = p.vAdd(e.p1, p.scProduct((l-10)/l, st));
    //Drawing:
    var s = e.p1;
    var t = e.p2;
    //Check if s, t are different:
    if(s.x == t.x && s.y == t.y)
      return;
    p.setStart();
    p.path('M'+s.x+' '+s.y+'L'+t.x+' '+t.y+'Z');
    _.each([o, p.scProduct(-1,o)], function(r){
      var s = p.vAdd(st,p.scProduct(5,r));
      p.path('M'+s.x+' '+s.y+'L'+t.x+' '+t.y+'Z');
    });
    this.setArrow(p.setFinish());
    this.arrow.attr({'stroke-width': '2'});
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
, setArrow: function(a){
    this.arrow = a;
    var r = this;
    a.mouseover(function(){r.showSelected();})
     .mouseout(function(){r.showUnselected();})
     .click(function(){r.click();});
  }
, showSelected: function(){
    if(!this.arrow) return;
    this.glow = this.arrow.glow({color: '#fff'});
  }
, showUnselected: function(){
    if(!this.glow) return;
    this.glow.remove();
    this.glow = null;
  }
, click: function(){
    window.App.views.singleDiscussionView.relationCreationModal.display(this.model);
  }
});
