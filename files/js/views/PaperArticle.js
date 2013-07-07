/**
  Renders a single Article on a paper.
  model: Article
  el:    Paper
*/
PaperArticle = Backbone.View.extend({
  initialize: function(){
  //this.model.on('change', this.update, this);
    //General content:
    var view  = this;
    this.el.setStart();
    this.back = this.el.rect(this.model.get('posX'), this.model.get('posY'), 100 , 20, 5).attr({fill: '#666'})
    var p     = this.back.attr(['x','y']);
    this.text = this.el.text(p.x + 40, (p.y+12.5)/2, 'Hello World!'); //this.model.get('headline'));
    this.set  = this.el.setFinish();
    this.set.mouseover(function(){view.showSelected();})
            .mouseout(function(){view.showUnselected();})
            .drag(this.drag, this.dragStart, this.dragEnd, this, this, this);
    this.el.setStart();
    // Building the buttons:
    this.buttons = this.el.buttonSet(this);
    this.btnDelete = this.el.set(); // Button to delete the PaperArticle from the Discussion
    this.btnRelA   = this.el.set(); // Button to let a PaperArticle attack another
    this.btnRelD   = this.el.set(); // Button to let a PaperArticle defend another
    this.btnDelete.push(
      this.el.rect(0,0,15,17,2)
    , this.el.image('files/img/ghw-trash.png',2,2,11,13)
    ).click(function(){view.clickBtnDelete();});
    this.btnRelA.push(
      this.el.rect(0,0,15,17,2)
    , this.el.image('files/img/ghw-minus.png',3,6,9,4)
    ).click(function(){view.clickBtnRelA();});
    this.btnRelD.push(
      this.el.rect(0,0,15,17,2)
    , this.el.image('files/img/ghw-plus.png',2,2,11,12)
    ).click(function(){view.clickBtnRelD();});
    this.buttons.push(
      this.btnDelete
    , this.btnRelA
    , this.btnRelD
    ).init().place().hide();
  }
  /**
    Update needs to adjust all content and make sure, that the text is inside back and so on.
  */
, update: function(){
    //Update text content:
//  this.text.attr('text', this.model.get('headline'));
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
    if(this.buttons.visible){
      this.buttons.hide();
    }else this.buttons.show();
  }
, clickBtnDelete: function(){alert('clickBtnDelete');}
, clickBtnRelA: function(){alert('clickBtnRelA');}
, clickBtnRelD: function(){alert('clickBtnRelD');}
, remove: function(){
    if(this.model !== null && typeof(this.model) !== 'undefined'){
      this.model.off(null, null, this);
    }
    this.set.remove();
    this.buttons.remove();
  }
});
