ItemPager = Pager.extend({
  defaults: $.extend(Pager.prototype.defaults, {
    isArticle: false
  , isDeleted: false
  , isDiscussion: false
  , isRelation: false
  , isResult: false
  , target: 'item/'
  })
, getQuery: function(){
    return {
      isArticle: this.get('isArticle')
    , isDeleted: this.get('isDeleted')
    , isDiscussion: this.get('isDiscussion')
    , isRelation: this.get('isRelation')
    , isResult: this.get('isResult')
    };
  }
, fetchCount: function(){
    var pager = this
      , t = this.get('target')
      , q = this.getQuery();
    return $.get(t, q).done(function(c){
      pager.set({count: c});
    });
  }
, fetchPage: function(p){
    var pager = this
      , t = this.get('target')
      , q = $.extend(this.getQuery(), {
          offset: p * this.get('limit')
        , limit: this.get('limit')
        });
    return $.get(t, q).done(function(p){
      pager.set({page: p, offset: q.offset});
    });
  }
});
