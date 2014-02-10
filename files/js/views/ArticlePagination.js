ArticlePagination = Pagination.extend({
  createCollection: function(){
    return new ItemCollection();
  }
, itemsFromIds: function(aids){
    return _.map(aids, function(aid){
      return new Item({id: aid});
    });
  }
, createPager: function(){
    return new ItemPager({isArticle: true});
  }
});
