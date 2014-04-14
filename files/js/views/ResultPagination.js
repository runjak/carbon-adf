ResultPagination = Pagination.extend({
  createCollection: function(){
    return new ItemCollection();
  }
, itemsFromIds: function(rids){
    return _.map(rids, function(rid){
      return new Item({id: rid});
    });
  }
, createPager: function(){
    return new ItemPager({isDiscussion: true, isResult: true});
  }
});
