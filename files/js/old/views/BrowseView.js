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
      , (function(d){return d ? d : "";})(i.get('deletion'))
      , (function(i){
          return '<div data-iid="' + i + '">'
               + '<button class="collectInformation"><span class="ui-icon ui-icon-disk" '
               + 'title="Collect this Information"></span></button>'
               + '<button class="uncollectInformation"><span class="ui-icon ui-icon-close" '
               + 'title="Remove this Information from the pool of collected Informations"></span></button>'
               + '</div>';
        })(i.get('id'))
      ];
      var content = _.reduce(cells, function(m, c){return m + "<td>" + c + "</td>";}, "");
      display.append("<tr>" + content + "</tr>");
    });
    //Binding links:
    display.find('a').click(function(){
      var target = $(this).attr('href');
      window.App.router.navigate(target, {trigger: true});
    });
    //Binding buttons:
    this.col.each(function(i){
      var col = window.App.collectedInformations;
      var div = display.find('div[data-iid="'+i.get('id')+'"]');
      var cI = div.find('button.collectInformation');
      var uI = div.find('button.uncollectInformation');
      if(col.where({id: i.get('id')}).length){ cI.hide(); }else{ uI.hide(); }
      cI.click(function(){
        cI.hide(); col.add(i); uI.show();
      });
      uI.click(function(){
        uI.hide(); col.remove(i); cI.show();
      });
    });
  }
});
