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
            .after('<div class="sidebar span3 offset'+leftSpan+'"></div>');
    this.target = this.$el.parent().find('.sidebar');
  }
, hide: function(){
    this.target.remove();
    this.target = null; 
    this.$el.addClass(this.oldSpan).removeClass(this.newSpan);
  }
});
