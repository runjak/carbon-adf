Pager = Backbone.Model.extend({
  defaults: {
    target: null
  , count:  0
  , limit: 30 
  , offset: 0
  , page:   []
  }
, fetchCount: function(){
    var pager = this;
    return $.get(this.get('target')).done(function(c){
      pager.set({count: c});
    });
  }
, fetchPage: function(p){
    var query = {
      offset: p * this.get('limit')
    , limit:  this.get('limit')
    };
    var pager = this;
    return $.get(this.get('target'), query).done(function(p){
      pager.set({page: p, offset: query.offset});
    });
  }
, getPages: function(){
    var count  = this.get('count');
    var limit  = this.get('limit');
    var offset = this.get('offset');
    var pages  = {
      prev: []
    , current: Math.floor(offset/limit)
    , next: []
    };
    var prevStart = Math.max(offset - 5 * limit, 0);
    var nextEnd   = Math.min(offset + 5 * limit, count);
    for(var i = prevStart; i < offset; i += limit)
      pages.prev.push(Math.floor(i/limit));
    for(var i = offset + limit; i < nextEnd; i += limit)
      pages.next.push(Math.floor(i/limit));
    return pages;
  }
});
