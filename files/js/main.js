$(function(){
var router = new Router;
router.on('route:defaultRoute', function(actions){
  log('router with actions:\t' + actions);
});

Backbone.history.start();

var logger = new Logger({el: $('#log')});
var loginView = new LoginView({el: $('#login')});
var topMenuView = new TopMenuView({
    el: $('#topmenu')
  , 'logger': logger
  , children: [loginView]
  });
});
