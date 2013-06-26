DiscussionView = Hideable.extend({
  initialize: function(){
    this.HideTarget = this.$el.parent();
    this.pagination = new DiscussionPagination({
      el: this.$('.pagination')
    });
    this.model = this.pagination.collection;
    this.model.on('reset add remove', this.render, this);
    var view = this;
    window.App.router.on('route:discussionView', function(){
      view.pagination.refresh();
      window.App.hideManager.render(view);
    });
  }
, render: function(){
    var rows = '';
    this.model.map(function(d){
      var did  = d.get('id');
      var head = d.get('headline');
      var desc = d.get('description');
      var crea = d.get('creationTime');
      rows += '<tr>'
            + '<td>'+did+'</td>'
            + '<td><a href="#/discussion/'+did+'">'+head+'</a></td>'
            + '<td>'+desc+'</td>'
            + '<td>'+crea+'</td>'
            + '</tr>';
    });
    this.$('tbody').html(rows);
    this.$('tbody a').each(function(){
      window.App.router.watchClick($(this));
    });
  }
});
