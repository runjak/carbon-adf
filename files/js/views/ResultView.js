ResultView = Hideable.extend({
  initialize: function(){
    this.HideTarget = this.$el.parent();
    this.pagination = new ResultPagination({
      el: this.$('.pagination')
    });
    this.model = this.pagination.collection;
    this.model.on('reset add remove', this.render, this);
    var view = this;
    window.App.router.on('route:resultView', function(){
      view.pagination.refresh();
      window.App.hideManager.render(view);
    });
  }
, render: function(){
    var rows = '';
    this.model.map(function(r){
      var rid  = r.get('id');
      var desc = $.extend({headline: '', summary: ''}, r.get('description'));
      var head = desc.headline;
      var summ = desc.summary;
      var crea = r.get('creation');
      rows += '<tr>'
            + '<td>'+rid+'</td>'
            + '<td><a href="#/result/'+rid+'">'+head+'</a></td>'
            + '<td>'+summ+'</td>'
            + '<td>'+crea+'</td>'
            + '</tr>';
    });
    this.$('tbody').html(rows);
    this.$('tbody a').each(function(){
      window.App.router.watchClick($(this));
    });
  }
});
