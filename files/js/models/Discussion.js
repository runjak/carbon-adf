Discussion = Item.extend({
  urlRoot: 'discussion/'
, defaults: {
    articles: new ArticleCollection()
  , participants: new UserCollection()
  }
, initialize: function(){}
, create: function(){
    var q = {
      headline: this.get('headline')
    , deadline: this.get('deadline')
    , description: this.get('description')
    };
    var created = $.Deferred();
    var discussion = this;
    $.post(this.urlRoot, q).done(function(d){
      //Add articles to the discussion:
      discussion.articles.map(function(a){
        //FIXME IMPLEMENT
      });
      //Set discussion data and resolve:
      created.resolve(discussion.set(d));
    }).fail(function(f){
      created.reject(f);
    });
    return created;
  }
});
