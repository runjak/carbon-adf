HideManager = Backbone.View.extend({
  initialize: function(){
    this.current = null;
  }
, render: function(hide){
    if(this.current !== null)
      this.current.hide();
    this.current = hide;
    if(hide !== null){
      if(hide.render)
        hide.render();
      hide.show();
    }
  }
});
