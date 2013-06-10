CollectedArticles = ArticleCollection.extend({
  initialize: function(){
    var l = this.loadLocal();
    if(l.length){
      var elems = _.map(l, function(aid){
        return new Article({id: aid});
        a.fetch();
        return a;
      });
      var fetches = _.map(elems, function(a){
        return a.fetch();
      });
      var coll = this;
      $.when.apply($, fetches).done(function(){
        coll.reset(elems);
      });
    }
    this.bind('reset add remove', this.saveLocal);
  }
, loadLocal: function(){
    var data = localStorage.collectedArticles;
    if(typeof(data) === 'undefined')
      data = '[]';
    return $.parseJSON(data);
  }
, saveLocal: function(){
    var data = this.map(function(a){return a.get('id');});
    localStorage.collectedArticles = JSON.stringify(data);
  }
});
