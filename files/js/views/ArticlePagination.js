ArticlePagination = Pagination.extend({
  createCollection: function(){
    return new ArticleCollection();
  }
, itemsFromIds: function(aids){
    return _.map(aids, function(aid){
      return new Article({id: aid});
    });
  }
, createPager: function(){
    return new Pager({target: 'article/'});
  }
});
