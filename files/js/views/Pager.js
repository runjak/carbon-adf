Pager = Backbone.View.extend({
  initialize: function(){
    var o = this.options;
    this.limit       = o.limit ? o.limit : 30;
    this.url         = o.url;
    this.currentPage = o.currentPage ? o.currentPage : 0;
    this.pages       = [];
    this.onSelection = o.onSelection;
    this.fetchPages();
  }
, render: function(){
    var p = this;
    $(p.el).html('');
    if(p.pages.length <= 1)
      return;
    var c = 0;
    var elems = [p.currentPage];
    for(var i = p.currentPage - 1; i >= 0; i--){
      if(c>=5) break;
      elems.unshift(i);
      c++;
    }
    c = 0;
    for(var i = p.currentPage + 1; i < p.pages.length; i++){
      if(c>=5) break;
      elems.push(i);
      c++;
    }
    elems = $.map(elems, function(e,i){
      var cs = "ui-state-default";
      if(e === p.currentPage)
        cs += " current";
      if(i === 0)
        cs += " ui-corner-left";
      if(i === elems.length - 1)
        cs += " ui-corner-right";
      return "<li class='"+cs+"' data-page='"+e+"' "
           + "title='Goto page:"+(e+1)+"'>"+(e+1)+"</li>";
    });
    $(elems).each(function(i,e){
      $(e).appendTo(p.el).click(function(){
        if($(e).hasClass('current'))
          return;
        p.currentPage = parseInt($(e).attr('data-page'));
        p.render();
        p.onSelection();
      });
    });
  }
, fetchPages: function(){
    var t = this;
    $.get(t.url, {limit: t.limit}, function(data){
      t.pages = data.carry;
      t.render();
    });
  }
, getLimit: function(){ return this.limit; }
, getOffset: function(){
    if(this.pages.length > this.currentPage)
      return this.pages[this.currentPage];
    return 0;
  }
, getPage: function(){ return this.currentPage + 1; }
, setPage: function(p){
    if(p <= 0) return;
    this.currentPage = p - 1;
  }
});
