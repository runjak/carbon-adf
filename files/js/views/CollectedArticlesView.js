CollectedArticlesView = Hideable.extend({
  initialize: function(){
    this.model.on('reset add remove', this.render, this);
  }
, render: function(){
    this.$el.html('<li class="nav-header">Collected articles</li>');
    var mkItem = function(a){
      var aid = a.get('id');
      var headline = a.get('description').headline;
      var creation = a.stripFractionFromTime(a.get('creationTime'));
      return '<li>'
           + '<a href="#/article/'+aid+'">'
           + headline+' - '+creation
           + '</a>'
           + '</li>';
    };
    var view = this;
    this.model.reiterate(function(a){
      view.$el.append(mkItem(a));
    });
  }
, toggle: function(){
    if(this.visible()){
      this.hide();
    }else
      this.show().render();
  }
});
