/**
  model is expected to be a Discussion.
*/
DiscussionResultView = Backbone.View.extend({
  initialize: function(){
    if(this.model)
      this.setModel(this.model);
  }
, render: function(){
    //FIXME implement
  }
, setModel: function(m){
    if(this.model){
      this.model.off(null, null, this);
    }
    this.model = m;
    if(this.model){
      //FIXME put binds here
    }
  }
});
