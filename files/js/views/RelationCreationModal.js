/**
  el: A modal for input of Data necessary to create a relation.
  model: A discussion
*/
RelationCreationModal = Backbone.View.extend({
  initialize: function(){
    this.setModel(this.model);
  }
, events: {
    'click #RelationCreationRemove': 'remove'
  , 'click #RelationCreationSave':   'save'
  , 'click #RelationCreationCreate': 'create'
  }
, setModel: function(m){
    if(this.model){
      this.model.off(null, null, this);
    }
    this.model = m;
    if(m){
      m.on('change:newRelationEnd', this.display, this);
    }
  }
, display: function(){
    if(!this.model.get('newRelationEnd')) return;
    console.log('RelationCreationModal.display()');
    this.source = this.model.get('newRelationStart');
    this.target = this.model.get('newRelationEnd');
    this.model.set({newRelationStart: null, newRelationEnd: null});
    //Check if relation already exists, and offer editing in that case.
    //FIXME WIP HERE
    if(true){
      this.$('#RelationCreationSave, #RelationCreationRemove').show();
      this.$('#RelationCreationCreate').hide();
    }else{
      this.$('#RelationCreationSave, #RelationCreationRemove').hide();
      this.$('#RelationCreationCreate').show();
    }
    //Displaying targets:
    this.$el.modal('toggle');
  }
, remove: function(){
    this.$el.modal('toggle');
  }
, save: function(){
    this.$el.modal('toggle');
  }
, create: function(){
    this.$el.modal('toggle');
  }
});
