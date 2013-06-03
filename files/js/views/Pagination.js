Pagination = Backbone.View.extend({
  initialize: function(){
    this.model = this.createPager();
    this.model.on('change:count', this.render, this);
    this.model.on('change:offset', this.render, this);
    this.model.on('change:page', this.performCollectionUpdate, this);
    this.collection = this.createCollection();
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
, setCollection: function(c){
    this.collection = c;
  }
, performCollectionUpdate: function(){
    var ids = this.model.get('page');
    var items = this.itemsFromIds(ids);
    var fetchAll = _.map(items, function(i){
      return i.fetch();
    });
    var pagination = this;
    $.when.apply($, fetchAll).done(function(){
      pagination.collection.set(items);
    });
  }
, itemsFromIds: function(ids){/*Overwrite this*/}
, createCollection: function(){/*Overwrite this*/}
, createPager: function(){/*Overwrite this*/}
});
