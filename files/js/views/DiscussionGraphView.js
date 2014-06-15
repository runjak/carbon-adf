DiscussionGraphView = SpringyRenderer.extend({
  events: {
    "click #DiscussionGraphViewHideRest": "hideRest"
  , "click #DiscussionGraphViewToggleEngine": "toggleEngine"
  , "click #DiscussionGraphViewAddNode": "addDummyNode"
  , "click #DiscussionGraphViewRemoveNode": "removeNode"
  , "click #AddRelationAttack": "addRelationAttack"
  , "click #AddRelationDefend": "addRelationDefend"
  , "click #DiscussionGraphViewClearClick": "clearClick"
  }
, initialize: function(){
    var view = this;
    //Listining to keyboard events:
    this.useKeyboard = true;
    $(window).keyup(function(e){view.keyboard(e);});
    //Springy setup:
    this.idNodeSet = {};
    this.idEdgeSet = {};
    this.springySetup();
  }
, setModel: function(m){
    //FIXME uncomment this when it's time again.
    //this.discussionGraphResultView.setModel(m);
    if(this.model){
      this.model.discussion.arguments.off(null, null, this);
      this.model.discussion.relations.off(null, null, this);
      this.renderer.stop();
      window.App.dummyItemFactory.reset(null);
      //Cleaning old Nodes, Edges fall automatically:
      _.each(this.graph.nodes, function(n){
        this.removeNode(n);
      }, this.graph);
    }
    if(m === null || typeof(m) === 'undefined'){
      this.model = null;
    }else{
      this.model = m;
      this.model.discussion.arguments.on('reset add remove', this.updateNodes, this);
      this.model.discussion.relations.on('reset add remove', this.updateEdges, this);
      this.updateNodes().updateEdges().renderer.start();
      window.App.dummyItemFactory.reset(m);
    }
    // Initially setting the size whenever we get a new model
    this.resize();
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
//Switches the renderer between on/off
, toggleEngine: function(){
    var btn = this.$('#DiscussionGraphViewToggleEngine');
    btn.hasClass('btn-warning') ? this.renderer.stop() : this.renderer.start();
    btn.toggleClass('btn-warning').toggleClass('btn-info');
  }
//Adds a new dummy node to the Dicussion.
, addDummyNode: function(){
    var node = window.App.dummyItemFactory.nextItem();
    if(!node){
      var p = $.Deferred()
        , msg = "Couldn't generate a new dummy Node - are you logged in?";
      p.reject(msg);
      alert(msg);
      return p;
    }
    return this.model.addArgument(node);
  }
//Removing a node:
, removeNode: function(){
    if(!window.App.login.get('loggedIn')){
      alert('You must login in order to remove nodes.');
      console.log('DiscussionGraphView:removeNode() requires a login.');
      return;
    }
    var t = this;
    this.setClick(function(n){
      var item = n.data.item;
      var id = item.get('id');
      if(item.get('description') === null){
        console.log('Found an item with an empty description in DiscussionGraphView.removeNode()!');
        console.log(item);
      }
      var h = item.get('description').headline;
      console.log('removeNode('+id+') with headline = '+h);
      t.model.removeArgument(item).done(function(){
        var target = "discussion/"+t.model.get('id')+"/graph";
        window.App.router.navigate(target, {trigger: false});
      });
    }, 'Click nodes to remove them.');
  }
//Building new relations between existing nodes:
, addRelationAttack: function(){
    var msg = 'Add an attack relation by clicking two nodes.';
    return this.addRelation('RelationAttack', msg);
  }
, addRelationDefend: function(){
    var msg = 'Add a support relation by clicking two nodes.';
    return this.addRelation('RelationSupport', msg);
  }
, addRelation: function(type, msg){
    if(!window.App.login.get('loggedIn')){
      alert('You must login in order to add Relations.');
      console.log('DiscussionGraphView:addRelation() requires a login.');
      return;
    }
    var t = this, source = null, src = null; // src so that we can overwrite source with null later on.
    var valid = function(i){
      var ret = i.isProofStandardCustom();
      if(ret){
        var h = i.get('description').headline
          , msg = "The item '"+h+"' has a custom proofstandard; "
                + "It's condition must be edited to change it's relations.";
        alert(msg);
      };
      return !ret;
    };
    this.setClick(function(n){
      if(source === null){
        var s = n.data.item;
        if(valid(s)){
          src = source = s;
        }
      }else{
        var tgt = n.data.item;
        if(valid(tgt)){
          var sh = src.get('description').headline
            , th = tgt.get('description').headline;
          console.log('Adding relation: '+sh+" -"+type+"-> "+th);
          var relation = new Item({
            commitAuthor: window.App.login.get('id')
          , commitMessage: 'Relation added by user via GraphView.'
          , relation: {
              source:       src.get('id')
            , target:       tgt.get('id')
            , relationType: type
            }
          , description: {
              headline: 'Generic relation'
            , summary:  'Nothing much to say about generic relations.'
            }
          });
          relation.mySave().done(function(d){
            src.fetch();
            tgt.fetch();
            relation.set(d);
            t.model.discussion.relations.add(relation);
          }).fail(function(msg){
            console.log('Failed with DiscussionGraphView:addRelation(): ' + msg);
          });
        }
        source = null;
      }
    }, msg);
  }
//Setting a click handler for nodes
, setClick: function(f, msg){
    this.nodeSelected = f;
    var target = this.$('#DiscussionGraphClickAction');
    target.find('.message').html(msg);
    target.find('a').show();
  }
//Removes the node click handler.
, clearClick: function(){
    this.nodeSelected = null;
    var target = this.$('#DiscussionGraphClickAction');
    target.find('.message').html('');
    target.find('a').hide();
  }
//Listening for keybord inputs:
, keyboard:  function(e){
    return;//FIXME keyboard disabled for debugging reasons.
    if(!this.$el.is(':visible')) return;
    if(!this.useKeyboard) return;
    switch(e.keyCode){
      case 27: // ESC
        this.clearClick();
      break;
      case 65: // A
        this.addRelationAttack();
      break;
      case 68: // D
        this.addRelationDefend();
      break;
      case 78: // N
        this.addDummyNode();
      break;
      case 72: // H
        this.hideRest();
      break;
      case 82: // R
        this.toggleEngine();
// FIXME commented because of reasons.
//      e.shiftKey ? this.discussionGraphResultView.prev()
//                 : this.discussionGraphResultView.next();
      break;
      case 88: // X
        this.removeNode();
      break;
      default:
      //console.log('Uncought keycode: '+e.keyCode);
    }
  }
/*
  Not only does the resize function fix the canvas size,
  but it is also reused by the SingleResultViewGraph.
*/
, resize: function(){
    var c  = this.canvas
      , p  = this.$canvas.position()
      , ww = $(window).width()
      , wh = $(window).height();
    c.width  = ww - 2 * p.left;
    c.height = wh - p.top - 80;
  }
//Syncs the models nodes to the graph, returns the view.
, updateNodes: function(){
    var t = this
      , args = this.model.discussion.arguments
      , currentNodes = {}; // Map ItemId Bool
    //Building new nodes
    args.each(function(i){
      var iid = i.get('id');
      if(iid in t.idNodeSet){ // Noting nodes that are kept:
        currentNodes[iid] = true;
        return;
      }
      //Building the node options:
      var o = {
        item: i
      , label: i.get('description').headline
      }
      //Figuring out the nodes color:
      if(condition = i.get('condition')){
        if(ps = condition.proofStandard){
          o.color = t.colors[ps];
        }
        if(condition.formula === "c(v)"){
          o.color = t.colors.accepted;
        }else if(condition.formula === "c(f)"){
          o.color = t.colors.rejected;
        }
        o.formula = condition.formula;
      }
      //Adding a new node:
      var node = t.idNodeSet[iid] = new Springy.Node(iid, o);
      currentNodes[iid] = true;
      t.graph.addNode(node);
    });
    //Delete outdated nodes:
    _.each(t.graph.nodes, function(n){
      if(n.id in currentNodes)
        return;
      this.removeNode(n);
      delete t.idNodeSet[n.id];
    }, t.graph);
    return this;
  }
//Syncs the models edges to the graph, returns the view.
, updateEdges: function(){
    var rels = this.model.discussion.relations
      , currentEdges = {}; // Map ItemId Bool
    //Adding new edges:
    rels.each(function(r){
      var rid = r.get('id');
      currentEdges[rid] = r;
      if(rid in this.idEdgeSet)
        return;
      var d = r.get('relation')
        , s = d.source
        , t = d.target
        , source = this.idNodeSet[s]
        , target = this.idNodeSet[t];
      if(!source || !target)
        return;
      //Adding the edge:
      var edge = this.idEdgeSet[rid] = new Springy.Edge(rid, source, target, {item: r});
      this.graph.addEdge(edge);
    }, this);
    //Removing old edges:
    _.each(this.graph.edges, function(e){
      if(e.id in currentEdges)
        return;
      delete this.idEdgeSet[e.id];
      this.graph.removeEdge(e);
    }, this);
    //Updating options for edges:
    _.each(this.graph.edges, function(e){
      var r = e.data.item, o = {};
      if(rt = r.get('relation').relationType){
        o.color = this.colors[rt];
      }
      e.data = $.extend(e.data, o);
    }, this);
    return this;
  }
});
