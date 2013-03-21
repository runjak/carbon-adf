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
});
var i = new Information({id: 28});
