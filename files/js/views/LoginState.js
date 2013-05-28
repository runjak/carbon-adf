LoginState = Backbone.View.extend({
  initialize: function(){
    this.model.on('change:loggedIn', this.onChange, this);
    this.render();
  }
, render: function(){
    var loggedIn = this.model.get('loggedIn');
    if(!loggedIn){
      this.$el.html('<a class="navbar-link" href="#/login">Login/Register</a>');
    }else{
      var msg = 'Logged in as <a href="#/user/' + this.model.get('id') + '">' + this.model.get('username') + '</a>';
      this.$el.html(msg);
    }
    window.App.router.watchClick(this.$el.find('a'));
  }
, onChange: function(){
    this.render();
    if(this.model.get('loggedIn')){
      window.App.router.navigate(this.$('a').attr('href'), {trigger: true});
    }else{
      window.App.router.navigate('#/login', {trigger: true});
    }
  }
});
