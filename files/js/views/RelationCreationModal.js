/**
  el: A modal for input of Data necessary to create a relation.
  model: A discussion
*/
RelationCreationModal = Backbone.View.extend({
  initialize: function(){
    this.headline    = this.$('#RelationCreationHeadline');
    this.description = this.$('#RelationCreationDescription');
    this.description.autoResize();
    this.setModel(this.model);
    var modal = this;
    this.$('.modal-header > button').click(function(){
      modal.showModal(false);
    });
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
    this.source = this.model.get('newRelationStart');
    this.target = this.model.get('newRelationEnd');
    this.model.set({newRelationStart: null, newRelationEnd: null});
    //Check if relation already exists, and offer editing in that case.
    this.relation = this.model.get('relations')
      .relationFromTo(this.source, this.target);
    if(this.relation){
      this.$('#RelationCreationSave, #RelationCreationRemove').show();
      this.$('#RelationCreationCreate').hide();
      this.headline.val(this.relation.get('headline'));
      this.description.val(this.relation.get('description'));
    }else{
      this.$('#RelationCreationSave, #RelationCreationRemove').hide();
      this.$('#RelationCreationCreate').show();
      this.headline.val('');
      this.description.val('');
    }
    //Displaying targets:
    this.showModal(true);
  }
, remove: function(){
    alert('This will soon be implemented, maybe');
    //FIXME implement
    this.showModal(false);
  }
, save: function(){
    alert('This will soon be implemented, maybe');
    //FIXME implement
    this.showModal(false);
  }
, create: function(){
    var rel  = new Relation();
    var view = this;
    var h    = this.headline.val();
    var d    = this.description.val();
    rel.create(this.model, this.source, this.target, h, d).always(function(){
      view.showModal(false);
    });
  }
, showModal: function(show){
    window.App.views.singleDiscussionView.discussionGraphView.setUseKeyboard(!show);
    if(show) this.$el.modal('show');
    else     this.$el.modal('hide');
  }
});
