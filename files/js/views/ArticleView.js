ArticleView = Hideable.extend({
  initialize: function(){
    this.HideTarget = this.$el.parent();
    this.pagination = new ArticlePagination({
      el: this.$('.pagination')
    });
    this.model = this.pagination.collection;
    this.model.on('reset add remove', this.render, this);
    var view = this;
    window.App.router.on('route:articleView', function(){
      view.pagination.refresh();
      window.App.hideManager.render(view);
    });
    this.$('#ArticleViewActionCollected').click(function(){
      window.App.views.collectedArticlesView.toggle();   
    });
  }
, render: function(){
    var rows = '';
    this.model.map(function(a){
      var aid  = a.get('id')
        , desc = $.extend({headline: 'No headline.', summary: ''}, a.get('description'))
        , head = desc.headline
        , summ = desc.summary
        , crea = a.get('creation')
        , collected = window.App.collectedArticles.elemById(aid);
      collected = (typeof(collected) === 'undefined') ? '' : ' class="success"';
      rows += '<tr'+collected+'>'
            + '<td>'+aid+'</td>'
            + '<td><a href="#/article/'+aid+'">'+head+'</a></td>'
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
