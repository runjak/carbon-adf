DiscussionGraphView = PaperView.extend({
  initialize: function(){
    this.dummyArticleFactory       = new DummyArticleFactory();
    this.discussionGraphResultView = new DiscussionGraphResultView({el: this.$('#SingleDiscussionViewGraphResults')});
    this.setModel(this.model);
    var view = this;
    $(window).resize(function(){
      view.resize();
    });
    $('a[href="#SingleDiscussionViewGraph"]').on('shown', function(){
      view.resize();
    });
    this.useKeyboard = true;
    $(window).keyup(function(e){view.keyboard(e);});
    this.$('.paper').click(function(e){
      view.mouse(e);
    });
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
  , "click #DiscussionGraphViewHideRest":  "hideRest"
  }
, render: function(){
    //Setup:
    var view   = this
      , paper  = this.paper
      , layout = new Springy.Layout.ForceDirected(this.model.graph, paper.width, paper.height, 0.5);
    window.spLayout = layout;

    // calculate bounding box of graph layout.. with ease-in
    var currentBB = layout.getBoundingBox();
    var targetBB = {bottomleft: new Springy.Vector(-2, -2), topright: new Springy.Vector(2, 2)};
    // auto adjusting bounding box
    Springy.requestAnimationFrame(function adjust() {
      targetBB = layout.getBoundingBox();
      // current gets 20% closer to target every iteration
      currentBB = {
        bottomleft: currentBB.bottomleft.add( targetBB.bottomleft.subtract(currentBB.bottomleft)
          .divide(10)),
        topright: currentBB.topright.add( targetBB.topright.subtract(currentBB.topright)
          .divide(10))
      };
      //Keeps the wheels turning
      Springy.requestAnimationFrame(adjust);
    });

    // convert to/from screen coordinates
    toScreen = function(p) {
      var size = currentBB.topright.subtract(currentBB.bottomleft);
      var sx = p.subtract(currentBB.bottomleft).divide(size.x).x * paper.width;
      var sy = p.subtract(currentBB.bottomleft).divide(size.y).y * paper.height;
      //Keeping things positive:
      sx = _.max([sx,-sx]);
      sy = _.max([sy,-sy]);
      return new Springy.Vector(sx, sy);
    };

    var renderer = new Springy.Renderer(layout, function clear(){},
      function drawEdge(edge, p1, p2){
        if(!edge.connection){
          if(!edge.source.shape || !edge.target.shape)
            return;
          var s = edge.source.shape, t = edge.target.shape;
          edge.connection = paper.connection(s, t, {stroke: Raphael.getColor(), directed: true});
        }else edge.connection.draw();
      }, function drawNode(node, p){
        if(!node.shape){
          paper.setStart();
          paper.rect(0,0,60,30,5).attr({fill: Raphael.getColor()}).setOffset();
          node.shape = paper.setFinish();
        }
        var s = toScreen(p);
        paper.moveSet(node.shape, Math.floor(s.x), Math.floor(s.y));
      });
    renderer.start();
    window.renderer = renderer;
//  //Cleaning:
//  if(this.paperArticles)
//    _.each(this.paperArticles, function(pa){pa.remove();});
//  this.paperArticles = [];
//  if(this.paperRelations)
//    _.each(this.paperRelations, function(pr){pr.remove();});
//  this.paperRelations = [];
//  p.clear();
//  //Drawing:
//  this.drawGrid();
//  if(this.model !== null && typeof(this.model) !== 'undefined'){
//    //Placing Articles:
//    var discussion = this.model;
//    this.model.articles.each(function(a){
//      var pa = new PaperArticle({model: a, el: p});
//      view.paperArticles.push(pa.setDiscussion(discussion));
//    });
//    //Placing Relations:
//    this.model.relations.each(function(r){
//      view.paperRelations.push(new PaperRelation({model: r, el: p}));
//    });
//    //FIXME implement
//  }
//  //Fixes:
//  p.renderfix();
//  p.safari();
  }
, resize: function(){
    if(this.model){
      var h = $(window).height() - 5;
      h -= this.$('.paper').position().top;
      this.$('.paper').css('max-height', h+'px');
      if(this.paper === null){
        this.mkPaper(this.$('.paper').get(0));
      }
      this.resetPanZoom().render();
    }
  }
, articleAdded: function(a, collection, options){
    if(!this.paperArticles) this.paperArticles = [];
    if(typeof(a.get('posX')) === 'undefined' || typeof(a.get('posY')) === 'undefined')
      a.set({posX: 500, posY: 500});
    var pa = new PaperArticle({model: a, el: this.paper});
    this.paperArticles.push(pa.setDiscussion(this.model));
  }
, articleRemoved: function(a, collection, options){
    var pAs = [];
    var aid = a.get('id');
    _.map(this.paperArticles, function(b){
      if(b.model.get('id') === aid){
        b.remove();
      }else pAs.push(b);
    });
    this.paperArticles = pAs;
  }
, relationAdded: function(r, collection, options){
    if(!this.paperRelations) this.paperRelations = [];
    this.paperRelations.push(new PaperRelation({model: r, el: this.paper}));
  }
, relationRemoved: function(r, collection, options){
    var pRs = [];
    var rid = r.get('id');
    _.each(this.paperRelations, function(pr){
      if(pr.model.get('id') === rid){
        pr.remove();
      }else pRs.push(pr);
    });
    this.paperRelations = pRs;
  }
, setModel: function(m){
    this.discussionGraphResultView.setModel(m);
    if(this.model){
      this.model.off(null, null, this);
      this.model.articles.off(null, null, this);
      this.model.relations.off(null, null, this);
    }
    if(m === null || typeof(m) === 'undefined'){
      this.model = null;
      this.paper = null;
      this.$('.paper').html('');
    }else{
      this.model = m;
      this.model.on('change:paperWidth',  this.resize, this)
                .on('change:paperHeight', this.resize, this);
      this.model.articles.on('reset',  this.render,         this)
                         .on('add',    this.articleAdded,   this)
                         .on('remove', this.articleRemoved, this);
      this.model.relations.on('reset',  this.render,          this)
                          .on('add',    this.relationAdded,   this)
                          .on('remove', this.relationRemoved, this);
      this.dummyArticleFactory.reset(this.model.articles);
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
    if(!this.$el.is('.fade.in')) return;
    if(!this.useKeyboard) return;
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
      case 72: // H
        this.hideRest();
      break;
      case 82: // R
        e.shiftKey ? this.discussionGraphResultView.prev()
                   : this.discussionGraphResultView.next();
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
        view.model.addArticle(a);
      });
    };
  }
, hideRest: function(){
    var p = $('#SingleDiscussionView > h1'
          + ', #SingleDiscussionView > .creation'
          + ', #SingleDiscussionView > .deletion'
          + ', #SingleDiscussionView > .deadline'
          + ', #SingleDiscussionView > summary'
          + ', #SingleDiscussionView > ul');
    var i = $('#DiscussionGraphViewHideRest i');
    if(i.hasClass('icon-arrow-up')){
      p.hide();
      i.addClass('icon-arrow-down').removeClass('icon-arrow-up');
    }else{
      p.show();
      i.addClass('icon-arrow-up').removeClass('icon-arrow-down');
    }
    this.resize();
  }
, setUseKeyboard: function(use){
    this.useKeyboard = use;
  }
});
