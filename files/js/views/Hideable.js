Hideable = Backbone.View.extend({
  show: function(){
    var t = (this.HideTarget !== undefined)
          ? this.HideTarget
          : this.$el;
    t.show();
  }
, hide: function(){
    var t = (this.HideTarget !== undefined)
          ? this.HideTarget
          : this.$el;
    t.hide();
  }
});
