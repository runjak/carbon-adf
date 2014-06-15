SingleResultViewGraph = Backbone.View.extend({
  initialize: function(){
    window.foo   = this; // FIXME DEBUG
    this.graph   = new Springy.Graph();
    this.$canvas = this.$('canvas');
    this.canvas  = this.$canvas.get(0);
    this.springy = this.$canvas.springy({graph: this.graph});
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
    //0.: We reuse DiscussionGraphView.resize for our own canvas:
    window.App.views.singleDiscussionView.discussionGraphView.resize.call(this);
    //1.: Cleaning the current graph
    this.cleanGraph();
    //2.: Inserting the current model into the graph
    if(!this.model) return;
    var lookup = this.model.getResultIdNameLookup()
      , data   = {nodes: [], edges: []}
      , rMap   = {}; // Label -> Result
    this.model.results.each(function(r){
      var label = r.showSet(lookup);
      rMap[label] = r;
      data.nodes.push(label);
      _.each(r.children, function(c){
        data.edges.push([c.showSet(lookup), label]);
      }, this);
    });
    this.graph.loadJSON(data);
    //Attaching results to nodes:
    _.each(this.graph.nodes, function(n){
      var r = rMap[n.id];
      if(!r) return;
      n.data.result = r;
    }, this);
    //3.: Inject click handlers into the graph
    //FIXME implement
  }
, cleanGraph: function(){
    _.each(this.graph.edges, function(e){
      this.removeEdge(e);
    }, this.graph);
    _.each(this.graph.nodes, function(n){
      this.removeNode(n);
    }, this.graph);
    this.graph.adjacency = {};
    this.graph.nodeSet   = {};
    this.graph.nodes     = [];
    this.graph.edges     = [];
    this.graph.notify.call(this.graph);
  }
});
