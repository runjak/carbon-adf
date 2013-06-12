DiscussionGraphView = PaperView.extend({
  initialize: function(){
    var view = this;
    this.setModel(this.model);
    $(window).resize(function(){
      view.resize();
    });
    $(window).keyup(function(e){view.keyboard(e);});
  }
, events: {
    "click #DiscussionGraphViewZoomIn":    "zoomIn"
  , "click #DiscussionGraphViewZoomOut":   "zoomOut"
  , "click #DiscussionGraphViewMoveLeft":  "moveLeft"
  , "click #DiscussionGraphViewMoveRight": "moveRight"
  , "click #DiscussionGraphViewMoveUp":    "moveUp"
  , "click #DiscussionGraphViewMoveDown":  "moveDown"
  }
, render: function(){
    //Setup:
    var p = this.paper;
    var w = p.width;
    var h = p.height;
    p.clear();
    //Drawing:
    this.drawGrid();
    p.circle(32768,32768,42).attr({
      fill: '#0f0'
    , 'fill-opacity': 0.25});
    p.renderfix();
    p.safari();
  }
, resize: function(){
    var w = $(window).width()  - 50;
    var h = $(window).height() - 50;
    this.$el.width(w).height(h);
    h -= 60; //More elegance would be nice.
    this.$('.paper').css('max-width',  w+'px')
                    .css('max-height', h+'px');
    if(this.model !== null){
      if(this.paper === null){
        this.mkPaper(this.$('.paper').get(0));
        this.centerViewBox(w,h).render();
      }else{
        var p = this.model._previousAttributes;
        if(p.paperWidth !== w || p.paperHeight !== h){
          this.centerViewBox(w,h).render();
        }
      }
    }
  }
, setModel: function(m){
    if(this.model !== null && typeof(this.model) !== 'undefined')
      this.model.off(null, null, this);
    if(m === null || typeof(m) === 'undefined'){
      this.model = null;
      this.paper = null;
      this.$('.paper').html('');
    }else{
      this.model = m;
      this.model.on('change:paperWidth',  this.resize, this)
                .on('change:paperHeight', this.resize, this);
    }
    this.resize();
  }
, zoomIn:    function(){this.deltaZoom(.1,.1);}
, zoomOut:   function(){this.deltaZoom(-.1,-.1);}
, moveLeft:  function(){this.pan(100, 0);}
, moveRight: function(){this.pan(-100, 0);}
, moveUp:    function(){this.pan(0,100);}
, moveDown:  function(){this.pan(0,-100);}
, keyboard:  function(e){
    if(!this.$el.is('.fade.in'))
      return;
    switch(e.keyCode){
      case 81: // Q
        this.zoomIn();
      break;
      case 69: // E
        this.zoomOut();
      break;
      case 87: // W
        this.moveUp();
      break;
      case 65: // A
        this.moveLeft();
      break;
      case 83: // S
        this.moveDown();
      break;
      case 68: // D
        this.moveRight();
      break;
    }
  }
});
