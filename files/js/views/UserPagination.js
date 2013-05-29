UserPagination = Backbone.View.extend({
  initialize: function(){
    this.model = new Pager({target: 'user/'});
    this.model.on('change:count', this.render, this);
    this.model.on('change:offset', this.render, this);
    this.model.on('change:page', this.updateUsers, this);
    this.users = new UserCollection();
    this.$el.addClass('pagination').html('<ul></ul>');
    this.model.fetchPage(0);
    this.model.fetchCount();
  }
, render: function(){
    //Rendering content:
    var pages = this.model.getPages();
    var lis   = '';
    _.each(pages.prev, function(i){
      var name = (i === _.first(pages.prev)) ? '«' : (i+1);
      lis += '<li><a data-page="'+i+'">'+name+'</a></li>';
    });
    lis += '<li class="active"><a>'
         + (pages.current + 1)
         + '</a></li>';
    _.each(pages.next, function(i){
      var name = (i === _.last(pages.next)) ? '»' : (i+1);
      lis += '<li><a data-page="'+i+'">'+name+'</a></li>';
    });
    if(pages.prev.length === 0 && pages.next.length === 0)
      lis = '';
    this.$('ul').html(lis);
    //Binding actions:
    var pager = this.model;
    this.$('a[data-page]').click(function(){
      var page = $(this).attr('data-page');
      pager.fetchPage(page); 
    });
  }
, updateUsers: function(){
    var uids = this.model.get('page');
    var users = [];
    var fetchAll = _.map(uids, function(uid){
      var u = new User({id: uid});
      users.push(u);
      return u.fetch();
    });
    var pagination = this;
    $.when.apply($, fetchAll).done(function(){
      pagination.users.set(users);
    });
  }
});
