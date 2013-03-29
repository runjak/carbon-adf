UserView = Backbone.View.extend({
  initialize: function(){
    this.ucol = new UserCollection();
    this.listenTo(this.ucol, "reset", this.render);
    var t = this;
    this.pager = new Pager({
      el: this.$('.pager')
    , url: '/pages/user'
    , onSelection: function(){t.fetch();}
    });
    this.fetch();
    this.acc = t.$('#usersAccordion');
    this.acc.accordion();
  }
, render: function(){
    var t = this;
    this.acc.html('').accordion('destroy');
    this.ucol.each(function(user){
      t.acc.append(t.buildUserEntry(user));
    });
    t.acc.accordion();
  }
, fetch: function(){
    var q = { limit:  this.pager.getLimit()
            , offset: this.pager.getOffset() };
    var ucol = this.ucol;
    $.get('/user', q, function(data){
      ucol.reset(data);
    });
  }
, buildUserEntry: function(user){
    var title   = user.get('id')+": "+user.get('username');
    var content = JSON.stringify(user.attributes);
    return "<h3>"+title+"</h3>"+"<div>"+content+"</div>";
  }
, setSpecific: function(user){
    this.ucol.add(user, {at: 0});
    this.render();
  }
});
