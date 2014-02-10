DiscussionPagination = Pagination.extend({
  createCollection: function(){
    return new ItemCollection();
  }
, itemsFromIds: function(dids){
    return _.map(dids, function(did){
      return new Item({id: did});
    });
  }
, createPager: function(){
    return new ItemPager({isDiscussion: true});
  }
});
