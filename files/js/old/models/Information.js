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
, _fetchRelations: function(relationEnd){
    var url = 'relation/' + relationEnd  + '/' + this.get('id');
    var def = $.Deferred();
    $.get(url).done(function(d){
      def.resolve(new RelationCollection(d));
    }).fail(function(e){
      window.App.logger.log(e, true);
      def.resolve();
    });
    return def;
  }
, fetchTargets: function(){
    return this._fetchRelations('target');
  }
, fetchSources: function(){
    return this._fetchRelations('source');
  }
, fetchRelations: function(){
    var rDef = $.Deferred();
    var tDef = this.fetchTargets();
    var sDef = this.fetchSources();
    tDef.done(function(t){
      sDef.done(function(s){
        rDef.resolve({
          targets: t
        , sources: s
        });
      });
    });
    return rDef;
  }
});
