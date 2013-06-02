Article = Backbone.Model.extend({
  urlRoot: 'article/'
, defaults: {}
, initialize: function(){}
, create: function(headline, description, content){
    var article = this;
    var query   = {
      headline:    headline
    , description: description
    , content:     content
    };
    console.log(query);
    return $.post(this.urlRoot, query).done(function(d){
      article.set(d);
    });
  }
});
