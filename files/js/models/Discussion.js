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
    , description: this.get('description')
    };
    if(d = this.get('deadline'))
      q.deadline = d;
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
