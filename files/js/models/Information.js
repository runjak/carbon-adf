Information = Backbone.Model.extend({
  urlRoot: '/information/'
, defaults: {}
, initialize: function(){}
, isContent: function(){ return (typeof this.get('media')) === 'string'; }
, isCollection: function(){} //FIXME implement
, isDiscussion: function(){} //FIXME implement
, create: function(callback){
    if(this.isContent()){
      var q = { title:       this.get('title')
              , description: this.get('description')
              , content:     this.get('media')};
      var t = this;
      $.post('/action/edit/create', q, function(d){ t.set(d); callback(); });
    }
  }
, _fetchRelations: function(relationEnd, callback){
    var url = 'relation/' + relationEnd  + '/' + this.get('id');
    $.get(url, function(data){
      var rs = new RelationCollection(data);
      callback(rs);
    });
  }
, fetchTargets: function(callback){
    this._fetchRelations('target', callback);
  }
, fetchSources: function(callback){
    this._fetchRelations('source', callback);
  }
, fetchRelations: function(callback){
    var options = {count: 2, callback: function(x){
                    x = $.extend(x[0], x[1]);
                    callback(x);
                  }};
    var cAnd = new CallbackAnd(options);
    this.fetchTargets(function(d){cAnd.call({targets: d});});
    this.fetchSources(function(d){cAnd.call({sources: d});});
  }
});
