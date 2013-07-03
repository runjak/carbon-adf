Discussion = Item.extend({
  urlRoot: 'discussion/'
, defaults: {
    articles:  new ArticleCollection()
  , participants: new UserCollection()
  }
, initialize: function(){}
, create: function(){
    var q = {
      headline: this.get('headline')
    , description: this.get('description')
    };
    if(d = this.get('deadline'))
      q.deadline = d;
    var created = $.Deferred();
    var discussion = this;
    $.post(this.urlRoot, q).done(function(d){
      //Add articles to the discussion:
      var base = 'collection/' + d.collectionId + '/';
      discussion.articles.map(function(a){
        $.put(base + a.get('id'));
      });
      //Set discussion data and resolve:
      created.resolve(discussion.set(d));
    }).fail(function(f){
      created.reject(f);
    });
    return created;
  }
/* Custom load method because it's easier than to overwrite fetch.
   But fetch doesn't respect articles being a Collection and makes them an array.*/
, load: function(){
    var discussion = this;
    var ret = $.Deferred();
    this.fetch().done(function(d){
      var as = discussion.get('articles');
      var ac = new ArticleCollection();
      ac.fetchAndReset(_.map(as, function(a){
        return new Article(a);
      })).always(function(){
        discussion.set({articles: ac});
        ret.resolve(d);
      });
    }).fail(function(f){
      ret.reject(f);
    });
    return ret;
  }
, addArticle: function(a){
    if(!a) return;
    var discussion = this;
    return $.put(this.getCAUrl(a)).done(function(){
      discussion.get('articles').add(a);
    });
  }
, removeArticle: function(a){
    if(!a) return;
    var discussion = this;
    return $.delete(this.getCAUrl(a)).done(function(){
      discussion.get('articles').remove(a);
    });
  }
/* Produces the url necessary to add/remove an article to/from a collection. */
, getCAUrl: function(a){
    var cid = this.get('collectionId');
    var aid = a.get('id');
    return 'collection/'+cid+'/'+aid;
  }
});
