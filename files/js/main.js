$(function(){
var router = new Router;
router.on('route:defaultRoute', function(actions){
  log('router with actions:\t' + actions);
});

Backbone.history.start();

var logger = new Logger({el: $('#log')});
console.log(logger);
var loginView = new LoginView({el: $('#login'), 'logger': logger});
console.log(loginView);
});
