Hideable = Backbone.View.extend({
  hideTarget: function(){
    if(this.HideTarget !== undefined)
      return this.HideTarget;
    return this.$el;
  }
, show: function(){
    this.hideTarget().show();
    return this;
  }
, hide: function(){
    this.hideTarget().hide();
    return this;
  }
, visible: function(){
    return this.hideTarget().is(':visible');
  }
});
