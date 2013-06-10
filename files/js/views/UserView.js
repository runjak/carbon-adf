UserView = Hideable.extend({
  initialize: function(){
    this.HideTarget = this.$el.parent();
    this.pagination = new UserPagination({
      el: this.$('.pagination')
    });
    this.model = this.pagination.collection;
    this.model.on('reset add remove', this.render, this);
    var view = this;
    window.App.router.on('route:userView', function(){
      view.pagination.refresh();
      window.App.hideManager.render(view);
    });
  }
, render: function(){
    var rows = '';
    this.model.map(function(u){
      var uid      = u.get('id');
      var uname    = u.get('username');
      var creation = u.get('userCreation');
      var login    = u.get('lastLogin');
      var isAdmin  = u.get('isAdmin');
      isAdmin = isAdmin ? ' class="info"' : '';
      rows += '<tr'+isAdmin+'>'
            + '<td>'+uid+'</td>'
            + '<td><a href="#/user/'+uid+'">'+uname+'</a></td>'
            + '<td>'+creation+'</td>'
            + '<td>'+login+'</td></tr>';
    });
    this.$('tbody').html(rows);
    this.$('tbody a').each(function(){
      window.App.router.watchClick($(this));
    });
  }
});
