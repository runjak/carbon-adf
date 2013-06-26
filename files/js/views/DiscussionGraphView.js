DiscussionGraphView = PaperView.extend({
  initialize: function(){
    this.dummyArticleFactory = new DummyArticleFactory();
    var view = this;
    this.setModel(this.model);
    $(window).resize(function(){
      view.resize();
    });
    $(window).keyup(function(e){view.keyboard(e);});
    this.$('.paper').click(function(e){
      view.mouse(e);});
    window.d = this; // FIXME DEBUG
  }
, events: {
    "click #DiscussionGraphViewCenter":    "resetPanZoom"
  , "click #DiscussionGraphViewZoomIn":    "zoomIn"
  , "click #DiscussionGraphViewZoomOut":   "zoomOut"
  , "click #DiscussionGraphViewMoveLeft":  "moveLeft"
  , "click #DiscussionGraphViewMoveRight": "moveRight"
  , "click #DiscussionGraphViewMoveUp":    "moveUp"
  , "click #DiscussionGraphViewMoveDown":  "moveDown"
  , "click #DiscussionGraphViewAddNode":   "addNode"
  }
, render: function(){
    //Setup:
    var view = this;
    var p = this.paper;
    var w = p.width;
    var h = p.height;
    //Cleaning:
    if(this.paperArticles)
      _.map(this.paperArticles, function(pa){pa.remove();});
    this.paperArticles = [];
    p.clear();
    //Drawing:
    this.drawGrid();
    if(this.model !== null && typeof(this.model) !== 'undefined'){
      //Placing Articles:
      this.model.articles.map(function(a){
        view.paperArticles.push(new PaperArticle({model: a, el: p}));
      });
      //Placing Relations:
      //FIXME implement
    }
    //Fixes:
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
        this.resetPanZoom().render();
      }else{
        var p = this.model._previousAttributes;
        if(p.paperWidth !== w || p.paperHeight !== h){
          this.resetPanZoom().render();
        }
      }
    }
  }
, articlesAdded:   function(a, collection, options){
    if(!this.paperArticles)
      this.paperArticles = [];
    this.paperArticles.push(new PaperArticle({model: a, el: this.paper}));
  }
, articlesRemoved: function(a, collection, options){
    var pAs = [];
    var aid = a.get('id');
    _.map(this.paperArticles, function(b){
      if(b.get('id') == aid){
        b.remove();
      }else pAs.push(b);
    });
    this.paperArticles = pAs;
  }
, setModel: function(m){
    if(this.model !== null && typeof(this.model) !== 'undefined'){
      this.model.off(null, null, this);
      this.model.articles.off(null, null, this);
    }
    if(m === null || typeof(m) === 'undefined'){
      this.model = null;
      this.paper = null;
      this.$('.paper').html('');
    }else{
      this.model = m;
      this.model.on('change:paperWidth',  this.resize, this)
                .on('change:paperHeight', this.resize, this);
      this.model.articles.on('reset',  this.render,          this)
                         .on('add',    this.articlesAdded,   this)
                         .on('remove', this.articlesRemoved, this);
      this.dummyArticleFactory.reset();
    }
    this.resize();
  }
, zoomIn:    function(){this.deltaZoom(-.1);}
, zoomOut:   function(){this.deltaZoom( .1);}
, moveLeft:  function(){this.pan( 100, 0);}
, moveRight: function(){this.pan(-100, 0);}
, moveUp:    function(){this.pan(0,  100);}
, moveDown:  function(){this.pan(0, -100);}
, keyboard:  function(e){
    if(!this.$el.is('.fade.in'))
      return;
    switch(e.keyCode){
      case 90: // Z
        this.resetPanZoom();
      break;
      case 187: // +
      case 81:  // Q
        this.zoomIn();
      break;
      case 189: // -
      case 69:  // E
        this.zoomOut();
      break;
      case 38: // ArrowUp
      case 87: // W
        this.moveUp();
      break;
      case 37: // ArrowLeft
      case 65: // A
        this.moveLeft();
      break;
      case 40: // ArrowDown
      case 83: // S
        this.moveDown();
      break;
      case 39: // ArrowRight
      case 68: // D
        this.moveRight();
      break;
      case 78: // N
        this.addNode();
      break;
      default:
      //console.log('Uncought keycode: '+e.keyCode);
    }
  }
, mouse: function(e){
    if(e.type === 'click'){
      var p = this.mouseToPaper({x: e.pageX, y: e.pageY});
      if(this.clickTask){
        var keep = this.clickTask(p);
        if(!keep) this.clickTask = null;
      }
    }
  }
, addNode: function(){
    var view = this;
    this.clickTask = function(p){
      view.dummyArticleFactory.nextDummy().done(function(a){
        a.set({posX: p.x, posY: p.y});
        view.model.articles.add(a);
      });
    };
  }
});
