/**
  Renders a single Article on a paper.
  model: Article
*/
PaperArticle = Backbone.View.extend({
  defaults: {// Paths from http://raphaeljs.com/icons/
    deletePath: "M24.778,21.419 19.276,15.917 24.777,10.415 21.949,7.585 16.447,13.087 10.945,7.585 8.117,10.415 13.618,15.917 8.116,21.419 10.946,24.248 16.447,18.746 21.948,24.248z"
  , attackPath: "M25.979,12.896,5.979,12.896,5.979,19.562,25.979,19.562z"
  , supportPath: "M25.979,12.896 19.312,12.896 19.312,6.229 12.647,6.229 12.647,12.896 5.979,12.896 5.979,19.562 12.647,19.562 12.647,26.229 19.312,26.229 19.312,19.562 25.979,19.562z"
  , customPath: "M3.25,6.469v19.062h25.5V6.469H3.25zM10.345,11.513l-4.331,1.926V12.44l3.124-1.288v-0.018L6.014,9.848v-1l4.331,1.927V11.513zM16.041,14.601h-5.05v-0.882h5.05V14.601z"
  }
, initialize: function(){
    //Binding event listeners:
    this.model.on('change:headline',    this.update, this);
    this.model.on('change:resultState', this.showResultState, this);
    this.position = new Springy.Vector(0, 0);
  }
  /**
    Builds the basic shape of the Article on a Paper.
  */
, shape: function(paper){
    var view = this;
    //Creating of shape elements:
    paper.setStart();
    this.back = paper.rect(5, 5, 30, 20, 5).attr({fill: Raphael.getColor()});
    this.elements = paper.set();
    this.text = paper.text(5, 5, this.model.get('headline'));
    this.buttons    = paper.set();
    this.btnDelete  = paper.path(this.defaults.deletePath).attr({fill: '#ff0000'}).showHover();
    this.btnAttack  = paper.path(this.defaults.attackPath).attr({fill: '#ff0000'}).showHover();
    this.btnSupport = paper.path(this.defaults.supportPath).attr({fill: '#00ff00'}).showHover();
    this.btnCustom  = paper.path(this.defaults.customPath).attr({fill: '#666666'}).showHover();
    this.buttons.push(this.btnDelete, this.btnAttack, this.btnSupport, this.btnCustom);
    this.btnDelete.click(function(){view.clickBtnDelete();});
    this.btnAttack.click(function(){view.clickBtnAttack();});
    this.btnSupport.click(function(){view.clickBtnSupport();});
    this.btnCustom.click(function(){view.clickBtnCustom();});
    this.elements.push(this.text, this.buttons);
    this.shape = paper.setFinish();
    // Arranging elements and returning the shape:
    return this.arrange().shape;
  }
  /**
    Update performs several actions to ensure, that it's shape stays up to date.
  */
, update: function(){
    if(!this.shape || !this.text) return;
    //Updating the headline:
    this.text.attr('text', this.model.get('headline'));
    //Making sure the text fits inside the rect:
    var tbbox = this.text.getBBox()
      , rbbox = this.back.getBBox();
    if(rbbox.width - 10 <= tbox.width)
      this.back.attr('width', tbox.width + 10);
    return this;
  }
  /**
    Places the whole shape at a certain location.
  */
, setPosition: function(p){
    //return this; // FIXME DEBUG
    _.each([this.back, this.text], function(e){
      e.attr({x: p.x, y: p.y});
    });
    this.position = p;
    return this.arrange();
  }
  /**
    Arranges the parts of the shape relative to each other.
  */
, arrange: function(){
    //Lining up buttons:
    var lastBBox = {x2: this.back.attr('x')};
    this.buttons.forEach(function(button){
      button.place(new Springy.Vector(lastBBox.x2 + 5, 0));
      lastBBox = button.getBBox();
    });
    //Pushing buttons under the text:
    var tbbox = this.text.getBBox()
      , down  = new Springy.Vector(0, tbbox.y2 + 5);
    this.buttons.forEach(function(button){
      var x = button.tVec.add(down);
      button.place(x);
    });
    //Individual fixes:
    //this.btnAttack.shift(new Springy.Vector(0, 0));
    //Stretching the rect:
    var ebbox = this.elements.getBBox()
      , attrs = {
          x: ebbox.x - 5
        , y: ebbox.y - 5
        , width:  ebbox.width  + 10
        , height: ebbox.height + 10
      };
    this.back.attr(attrs);
    return this;
  }
, showSelected: function(){
    if(!this.back.dragFrom){
      this.showUnselected();
      this.glow = this.back.glow({color: '#fff'});
    } return this;
  }
, showUnselected: function(){
    if(this.glow){
      this.glow.remove();
      this.glow = null;
    } return this;
  }
, showResultState: function(){
    switch(this.model.get('resultState')){
      case 'In':
        return this.showAccepted();
      break;
      case 'Out':
        return this.showRejected();
      break;
      case 'Udec':
      default:
        return this.showUnknown();
    }
  }
, showAccepted: function(){/*this.back.attr({fill: '#0f0'});*/ return this;}
, showRejected: function(){/*this.back.attr({fill: '#f00'});*/ return this;}
, showUnknown:  function(){/*this.back.attr({fill: '#666'});*/ return this;}
, dragStart: function(){
    this.showUnselected();
    this.back.attr('fill-opacity', .5);
    this.back.dragFrom = this.back.attr(['x','y']);
    this.text.hide();
    this.dragged = false;
  }
, drag: function(dx, dy){
    if(!this.dragged){
      this.dragged = true;
      this.buttons.hide();
    }
    var z = this.el._vbSize;
    var p = this.back.dragFrom;
    this.back.attr({
      x: p.x + dx * z
    , y: p.y + dy * z
    });
  }
, dragEnd: function(){
    this.back.dragFrom = null;
    this.back.attr('fill-opacity', 1);
    this.text.show();
    this.showSelected().update();
    if(!this.dragged){
      this.click();
    }else{ // Save new position
      var p = this.back.attr(['x','y']);
      this.model.setPosition({posX: p.x, posY: p.y});
    }
  }
, click: function(){
    if(this.discussion.setNewRelationEnd(this.model))
      return;
    if(this.buttons.visible){
      this.buttons.hide();
    }else this.buttons.show();
  }
, clickBtnDelete: function(){}  // FIXME IMPLEMENT
, clickBtnAttack: function(){}  // FIXME IMPLEMENT
, clickBtnSupport: function(){} // FIXME IMPLEMENT
, clickBtnCustom: function(){}  // FIXME IMPLEMENT
//, clickBtnDelete: function(){
//    if(!this.discussion) return;
//    this.discussion.removeArticle(this.model);
//  }
//, clickBtnAddRel: function(){
//    this.discussion.setNewRelationStart(this.model);
//    this.buttons.hide();
//  }
//, clickBtnCond: function(){
//    window.App.views.singleDiscussionView.articleConditionModal.display(this.model);
//    this.buttons.hide();
//  }
, remove: function(){
    if(this.model){
      this.model.off(null, null, this);
      var aid = this.model.get('id');
      window.App.views.discussionArticles[aid] = this;
    }
    this.set.remove();
    this.buttons.remove();
  }
, setDiscussion: function(d){
    this.discussion = d;
    return this;
  }
, getBBox: function(){
    return this.back.getBBox();
  }
});
