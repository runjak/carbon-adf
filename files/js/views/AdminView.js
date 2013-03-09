var AdminView = TopMenuChild.extend({
  initialize: function(){
    var t = this;
    this.pager = new Pager({
      el: $('.pager', t.el)
    , url: '/pages/user'
    , onSelection: function(){
        t.router.navigate('#admin/'+t.pager.getPage(), {trigger: true});
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
        var admin = e.isAdmin ? ' checked="checked"' : '';
        var row = '<tr class="AdminUserEntry">'
                + '<td>'+e.id+'</td>'
                + '<td>'+e.username+'</td>'
                + '<td><input class="AdminUserEntryPassword" placeholder="Set new Password"></td>'
                + '<td><input class="AdminUserEntryKarma" value="'+e.karma+'"></td>'
                + '<td><input class="AdminUserEntryIsAdmin" type="checkbox"'+admin+'></td>'
                + '<td><button class="AdminUserEntryUpdate" title="Update Data">'
                + '<span class="ui-icon ui-icon-disk"></span></button>'
                + '<button class="AdminUserEntryDelete" title="Delete User">'
                + '<span class="ui-icon ui-icon-trash"></span></button></td></tr>';
        $(row).appendTo('#AdminUserTable tbody', t.el).data('userData', e);
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
    var k = row.find('.AdminUserEntryKarma').val();
    var a = row.find('.AdminUserEntryIsAdmin').is(':checked');
    var logger = this.logger;
    if(p !== ''){
      var q = {username: u.username, password: p};
      $.post('/action/user/password', q, function(data){
        logger.logAction(data);
        row.find('.AdminUserEntryPassword').val('');
      });
    }
    if(k != u.karma){
      logger.log('Updating karma not implemented yet.');
    }
    if(a != u.isAdmin){
      var q = {username: u.username, admin: a};
      $.post('/action/user/admin', q, function(data){
        logger.logAction(data);
      });
    }
  }
, deleteUser: function(e){
    var t = this;
    var row = t.rowFromEvent(e);
    var q = { username: row.data('userData').username };
    $.post('/action/user/delete', q, function(data){
      t.logger.logAction(data);
      row.remove();
    });
  }
, rowFromEvent: function(e){ return $(e.currentTarget).closest('.AdminUserEntry'); }
});
