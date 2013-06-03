CollectedArticles = ArticleCollection.extend({
  initialize: function(){
    var l = this.loadLocal();
    if(l.length){
      var elems = _.map(l, function(aid){
        var a = new Article({id: aid});
        a.fetch();
        return a;
      });
      this.reset(elems);
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
