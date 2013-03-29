$(function(){
  /*
    The app inspired by
    http://benhowdle.im/2013/03/24/patterns-for-managing-large-scale-backbone-applications/
  */
  window.App = {
    currentInformation:  new CurrentInformation()
  , logger: new Logger({el: $('#log')})
  , login: new Login()
  , router: new Router()
  , views: {}
  };
  //Inizializing the App:
  var app = window.App;
  app.logger.watch(app.login);
  app.views.mainView = new MainView({
    el: $('#view'), model: app.currentInformation});
  app.views.createView = new CreateView({
    el: $('#create'), model: app.currentInformation});
  app.views.loginView = new LoginView({
    el: $('#login'), model: app.login});
  app.views.profileView = new ProfileView({
    el: $('#profile'), model: app.login});
  app.views.userView = new UserView({el: $('#users')});
  app.views.adminView = new AdminView({
    el: $('#admin'), model: app.login});
  app.views.topmenuView = new TopmenuView({
    el: $('#main'), model: app.login});
//Router setup:
var router = app.router;
router.on('route:defaultRoute', function(actions){
  console.log('router with actions:\t' + actions);
});
router.on('route:mainView', function(inf){
  if(inf) app.currentInformation.set({id: inf});
});
router.on('route:loginView', function(){ app.views.loginView.render(); });
router.on('route:profileView', function(){ app.views.profileView.render(); });
router.on('route:userView', function(uid){
  if(uid){
    var u = new User({id: uid});
    u.fetch({success: function(u){
      app.views.userView.setSpecific(u);
    }});
  }
});
router.on('route:adminView', function(page){
  if(page)
    app.views.adminView.pager.setPage(page);
  app.views.adminView.render();
});
Backbone.history.start();
//Tooltips:
$('body').tooltip();
});
