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
, fetchRelations: function(relationEnd, callback){
    var url = this.urlRoot + relationEnd + this.get('id');
    $.get(url, function(data){
      var rs = new RelationColleciont(data);
      callback(rs);
    });
  }
, fetchTargets: function(callback){
    this.fetchRelations('target', callback);
  }
, fetchSources: function(callback){
    this.fetchRelations('source', callback);
  }
});
