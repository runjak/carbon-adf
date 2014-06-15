/**
  model: is expected to be a Discussion.
  el: #SingleDiscussionViewGraphResults
  This view mostly lives as a child of the SingleDiscussionGraphView.
  It aids working with the results of a discussion when they're available.
*/
DiscussionGraphResultView = Backbone.View.extend({
  initialize: function(){
    window.foo = this; // FIXME DEBUG
    if(this.model)
      this.setModel(this.model);
    var view = this;
    this.$('#ResultSets').change(function(){view.renderResults();});
  }
, events: {
    'click #DiscussionGraphResultViewPrev': 'prev'
  , 'click #DiscussionGraphResultViewNext': 'next'
  }
, setModel: function(m){
    if(this.model){
      this.model.results.off(null, null, this);
    }
    this.model = m;
    if(m){
      this.model.results.on('reset add remove', this.render, this);
      this.$el.show();
    }else{
      this.$el.hide();
    }
    this.render();
  }
, render: function(){
    if(this.model && this.model.isEvaluated()){
      var options = ''
        , lookup  = this.model.getResultIdNameLookup();
      this.model.results.each(function(r){
        var selected = (options === '') ? ' selected="selected"' : ''
          , value    = ' data-rId="'+r.get('id')+'"'
          , label    = r.showSet(lookup);
        options += '<option'+selected+value+'>'+label+'</option>';
      });
      this.$('#ResultSets').html(options);
      this.$('#ResultSets option:first').prop('selected', true);
      this.renderResults();
      this.$el.show();
    }else{
      this.$('select > option').remove();
      this.$el.hide();
    }
  }
/**
  Makes sure the currently selected resultSet is rendered in the graph.
*/
, renderResults: function(){
    var rId = this.$('#ResultSets option:selected').attr('data-rId')
      , r   = this.model.results.elemById(rId);
    if(r) r.applyTo(this.model);
  }
, next: function(){
    var next = this.$('#ResultSets option:selected').next();
    if(next.length === 0){
      next = this.$('#ResultSets option:first');
    }
    next.prop('selected', true);
    this.renderResults();
  }
, prev: function(){
    var prev = this.$('#ResultSets option:selected').prev();
    if(prev.length === 0){
      prev = this.$('#ResultSets option:last');
    }
    prev.prop('selected', true);
    this.renderResults();
  }
});
