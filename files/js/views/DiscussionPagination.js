DiscussionPagination = Pagination.extend({
  createCollection: function(){
    return new DiscussionCollection();
  }
, itemsFromIds: function(dids){
    return _.map(dids, function(did){
      return new Discussion({id: did});
    });
  }
, createPager: function(){
    return new Pager({target: 'discussion/'});
  }
});
