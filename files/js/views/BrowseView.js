BrowseView = Backbone.View.extend({
  initialize: function(){
    this.col = new InformationCollection();
    this.listenTo(this.col, "reset", this.render);
    var t = this;
    this.pager = new Pager({
      el: this.$('.pager')
    , url: '/pages/information'
    , onSelection: function(){t.fetch();}
    });
  }
, fetch: function(){
    var q = { limit:  this.pager.getLimit()
            , offset: this.pager.getOffset() };
    var col = this.col;
    $.get('/information', q, function(data){
      col.reset(data);
    });
  }
, render: function(){
    console.log('BrowseView:render()');
    var display = this.$('#BrowseDisplay').html('');
    this.col.each(function(i){
      var cells = [
        '<a href="#view/' + i.get('id') + '">' + i.get('title') + '</a>'
      , i.get('description')
      , '<a href="#users/' + i.get('author').id + '">' + i.get('author').username + '</a>'
      , i.get('creation')
      , (function(d){if(!d) return ""; return d})(i.get('deletion'))
      ];
      var content = _.reduce(cells, function(m, c){return m + "<td>" + c + "</td>";}, "");
      display.append("<tr>" + content + "</tr>");
    });
    display.find('a').click(function(){
      var target = $(this).attr('href');
      window.App.router.navigate(target, {trigger: true});
    });
  }
});
