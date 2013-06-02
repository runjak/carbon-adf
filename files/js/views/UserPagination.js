UserPagination = Pagination.extend({
  createCollection: function(){
    return new UserCollection();
  }
, itemsFromIds: function(uids){
    return _.map(uids, function(uid){
      return new User({id: uid});
    });
  }
, createPager: function(){
    return new Pager({target: 'user/'});
  }
});
