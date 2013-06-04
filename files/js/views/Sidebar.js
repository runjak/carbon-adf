Sidebar = Backbone.View.extend({
  initialize: function(){
    this.target  = null; //Use in render
    this.oldSpan = '';   //For this.hide()
    this.newSpan = '';   //For this.hide()
  }
, show: function(){
    var matches  = /span([^ ]+)/.exec(this.$el.attr('class'));
    var leftSpan = matches[1] - 3;
    this.oldSpan = matches[0];
    this.newSpan = 'span' + leftSpan;
    this.$el.addClass(this.newSpan)
            .removeClass(this.oldSpan)
            .after(this.sidebarHtml('sidebar span3 offset'+leftSpan));
    this.target = this.$el.parent().find('.sidebar ul.nav');
  }
, hide: function(){
    this.target.closest('.sidebar').remove();
    this.target = null; 
    this.$el.addClass(this.oldSpan).removeClass(this.newSpan);
  }
, sidebarHtml: function(classes){
    return '<div class="'+classes+'"><div class="well sidebar-nav"><ul class="nav nav-list"></ul></div></div>';
  }
});
