/**
  Renders a single Article on a paper.
  model: Article
*/
PaperArticle = Backbone.View.extend({
  initialize: function(){
    //Binding event listeners:
    this.model.on('change:headline',    this.update, this);
    this.model.on('change:resultState', this.showResultState, this);
    //Taking care that the paperArticle can be found:
    var aid = this.model.get('id');
    window.App.views.discussionArticles[aid] = this;
    //General content:
    var view  = this;
    this.el.setStart();
    this.back = this.el.rect(this.model.get('posX'), this.model.get('posY'), 100 , 20, 5).attr({fill: '#666'})
    var p     = this.back.attr(['x','y']);
    this.text = this.el.text(p.x + 40, (p.y+12.5)/2, this.model.get('headline'));
    this.set  = this.el.setFinish();
    this.set.mouseover(function(){view.showSelected();})
            .mouseout(function(){view.showUnselected();})
            .drag(this.drag, this.dragStart, this.dragEnd, this, this, this);
    // Building the buttons:
    this.buttons   = this.el.buttonSet(this);
    this.btnDelete = this.el.set(); // Button to delete the PaperArticle from the Discussion
    this.btnAddRel = this.el.set(); // Button to add a Relation to another Article
    this.btnCond   = this.el.set(); // Button to change the condition of the Article.
    this.btnDelete.push(
      this.el.rect(0,0,15,17,2)
    , this.el.image('files/img/ghw-trash.png',2,2,11,13)
    ).click(function(){view.clickBtnDelete();});
    this.btnAddRel.push(
      this.el.rect(17,0,15,17,2)
    , this.el.image('files/img/ghw-arrow-right.png',19,2,11,13)
    ).click(function(){view.clickBtnAddRel();});
    this.btnCond.push(
      this.el.rect(38,0,15,17,2)
    , this.el.image('files/img/ghw-cog.png',40,2,11,13)
    ).click(function(){view.clickBtnCond();});
    //Add relation link button here .)
    this.buttons.push(
      this.btnDelete
    , this.btnAddRel
    , this.btnCond
    ).init().place().hide();
    //Ensuring correct positioning:
    this.update();
  }
  /**
    Update needs to adjust all content and make sure, that the text is inside back and so on.
  */
, update: function(){
    //Update text content:
    this.text.attr('text', this.model.get('headline'));
    //Updating back size:
    var tBox = this.text.getBBox();
    this.back.attr({
      width:  tBox.width  + 10
    , height: tBox.height + 10
    });
    //Updating text position:
    var p = this.back.attr(['x','y']);
    this.text.attr({
      x: p.x + 5 + tBox.width/2
    , y: p.y + 5 + tBox.height/2
    });
    //Updating buttons:
    this.buttons.place();
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
, showAccepted: function(){this.back.attr({fill: '#0f0'}); return this;}
, showRejected: function(){this.back.attr({fill: '#f00'}); return this;}
, showUnknown:  function(){this.back.attr({fill: '#666'}); return this;}
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
, clickBtnDelete: function(){
    if(!this.discussion) return;
    this.discussion.removeArticle(this.model);
  }
, clickBtnAddRel: function(){
    this.discussion.setNewRelationStart(this.model);
    this.buttons.hide();
  }
, clickBtnCond: function(){
    window.App.views.singleDiscussionView.articleConditionModal.display(this.model);
    this.buttons.hide();
  }
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
