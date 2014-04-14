$(function(){
  /*
    The app inspired by
    http://benhowdle.im/2013/03/24/patterns-for-managing-large-scale-backbone-applications/
  */
  /*Setting up marked.js:*/
  marked.setOptions({
    gfm:      true
  , pedantic: false
  , sanitize: true
  });
  //The singleton:
  window.App = {
    collectedArticles: new CollectedItems()
  , dummyItemFactory: new DummyItemFactory()
  , hideManager: new HideManager()
  , login: new Login()
  , router: new Router()
  , views:  {}
  };
  //Setting the views:
  window.App.views.loginState = new LoginState({
    model: window.App.login
  , el: $('#LoginState')
  });
  window.App.views.loginView = new LoginView({
    model: window.App.login
  , el: $('#LoginRegisterForm')
  });
  window.App.views.singleUserView = new SingleUserView({el: $('#SingleUserView')});
  window.App.views.userView = new UserView({el: $('#UserView')});
  window.App.views.articleView = new ArticleView({el: $('#ArticleView')});
  window.App.views.collectedArticlesView = new CollectedArticlesView({
    el: $('.CollectedArticlesView')
  , model: window.App.collectedArticles
  });
  window.App.views.singleArticleView = new SingleArticleView({el: $('#SingleArticleView')});
  window.App.views.createArticleView = new CreateArticleView({el: $('#CreateArticleView')});
  window.App.views.topMenuView = new TopMenuView({el: $('#TopMenu')});
  window.App.views.discussionView = new DiscussionView({el: $('#DiscussionView')});
  window.App.views.createDiscussionView = new CreateDiscussionView({el: $('#CreateDiscussionView')});
  window.App.views.singleDiscussionView = new SingleDiscussionView({el: $('#SingleDiscussionView')});
  window.App.views.resultView = new ResultView({el: $('#ResultView')});
  window.App.views.singleResultVlew = new SingleResultView({el: $('#SingleResultView')});
  //Starting the routing:
  Backbone.history.start();
  window.App.router.on('defaultRoute', function(actions){
    console.log('defaultRoute with args:\t'+JSON.stringify(actions));
  });
});
