ArticleHistoryView = Sidebar.extend({
  initialize: function(){
    this.children = new ArticleCollection();
    this.parents  = new ArticleCollection();
    this.children.sortByCreation();
    this.parents.sortByCreation();
    this.setModel(this.model);
  }
, render: function(){
    if(this.target === null || typeof(this.target) === 'undefined')
      return;
    this.target.html('<li class="nav-header">History</li>');
    var mkItem = function(a){
      var aid = a.get('id');
      var headline = a.get('headline');
      var creation = a.stripFractionFromTime(a.get('creationTime'));
      return '<li><a href="#/article/'+aid+'">'+headline+' - '+creation+'</a></li>';
    };
    var view = this;
    this.children.reiterate(function(c){
      view.target.append(mkItem(c));
    });
    this.target.append('<li class="active">Current Article</li>');
    this.parents.reiterate(function(p){
      view.target.append(mkItem(p));
    });
  }
, setModel: function(m){
    if(this.model)
      this.model.off(null, null, this);
    this.model = m;
    if(m){
      this.model.on('change:children', this.render, this);
      this.model.on('change:parents',  this.render, this);
      this.updateCollections();
    }
  }
, updateCollections: function(){
    var c = _.map(this.model.get('children'), function(c){
      return new Article({id: c});
    });
    var p = _.map(this.model.get('parents'), function(p){
      return new Article({id: p});
    });
    this.children.reset(c);
    this.parents.reset(p);
    var view = this;
    $.when(this.children.fetchAll(), this.parents.fetchAll()).always(function(){
      view.render();
    });
  }
});
