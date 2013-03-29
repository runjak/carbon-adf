AdminView = Backbone.View.extend({
  initialize: function(){
    this.listenTo(this.model, "change:loggedIn", this.render);
    var t = this;
    this.pager = new Pager({
      el: t.$('.pager')
    , url: '/pages/user'
    , onSelection: function(){
        window.App.router.navigate('#admin/'+t.pager.getPage(), {trigger: true});
      }
    });
  }
, render: function(){
    var t = this;
    t.pager.fetchPages();
    var q = { limit:  t.pager.getLimit()
            , offset: t.pager.getOffset()};
    $.get("/user", q, function(data){
      $('#AdminUserTable tbody', t.el).html('');
      $(data).each(function(i,e){
        var u = new User(e);
        window.App.logger.watch(u);
        var admin = u.get('isAdmin') ? ' checked="checked"' : '';
        var row = '<tr class="AdminUserEntry">'
                + '<td>'+u.get('id')+'</td>'
                + '<td>'+u.get('username')+'</td>'
                + '<td>'+u.get('karma')+'</td>'
                + '<td><input class="AdminUserEntryPassword" placeholder="Set new Password"></td>'
                + '<td><input class="AdminUserEntryIsAdmin" type="checkbox"'+admin+'></td>'
                + '<td><button class="AdminUserEntryUpdate" title="Update Data">'
                + '<span class="ui-icon ui-icon-disk"></span></button>'
                + '<button class="AdminUserEntryDelete" title="Delete User">'
                + '<span class="ui-icon ui-icon-trash"></span></button></td></tr>';
        $(row).appendTo('#AdminUserTable tbody', t.el).data('userData', u);
      });
    });
  }
, events: {
    "click .AdminUserEntryUpdate": "updateUser"
  , "click .AdminUserEntryDelete": "deleteUser"
  }
, updateUser: function(e){
    var row = this.rowFromEvent(e);
    var u = row.data('userData');
    var p = row.find('.AdminUserEntryPassword').val();
    var a = row.find('.AdminUserEntryIsAdmin').is(':checked');
    if(p !== '')
      u.setPassword(p);
    u.setAdmin(a);
  }
, deleteUser: function(e){
    var t = this;
    var row = t.rowFromEvent(e);
    row.data('userData').delete();
    row.remove();
  }
, rowFromEvent: function(e){ return $(e.currentTarget).closest('.AdminUserEntry'); }
});
