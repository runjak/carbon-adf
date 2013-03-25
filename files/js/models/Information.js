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
, fetchRelations: function(callback){
    var t = this;
    var q = {informationId: this.get('id')}
    $.get('/relation', q, function(data){
      var rs = new RelationCollection(data);
      t.set({relations: rs});
      if(callback) callback(rs, t);
    });
  }
, getRelations: function(){return this.get('relations');}
});
