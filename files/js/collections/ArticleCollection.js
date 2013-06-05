ArticleCollection = ExtendedCollection.extend({
  model: Article
, sortByCreation: function(){
    this.comparator = function(a){
      return a.get('creationTime');
    };
  }
});
