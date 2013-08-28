/**
  model: is expected to be a Discussion.
  el: #SingleDiscussionViewGraphResults
  This view mostly lives as a child of the SingleDiscussionGraphView.
  It aids working with the results of a discussion when they're available.
*/
DiscussionGraphResultView = Backbone.View.extend({
  initialize: function(){
    if(this.model)
      this.setModel(this.model);
    var view = this;
    this.$('#ResultSets').change(function(){view.renderResults();});
    this.$('#ResultSet').change(function(){view.resultSelected();});
  }
, events: {
    'click #DiscussionGraphResultViewPrev': 'prev'
  , 'click #DiscussionGraphResultViewNext': 'next'
  }
, render: function(){
    if(this.model && this.model.results.hasResults()){
      var results = this.model.results;
      //Rendering available sets:
      var options = '';
      _.each(results.ResultTypes, function(rType){
        var length = results[rType].length;
        if(length <= 0) return;
        var selected = (options === '') ? ' selected="selected"' : '';
        var data     = ' data-ResultType="' + rType + '"';
        var item     = rType + ' (' + length + ')';
        options += '<option' + selected + data + '>' + item + '</option>';
      });
      this.$('#ResultSets').html(options);
      this.renderResults();
      this.$el.show();
    }else{
      this.$('select > option').remove();
      this.$el.hide();
    }
  }
/**
  Shows the results that can be selected given the currently selected ResultSet.
*/
, renderResults: function(){
    var rType   = this.$('#ResultSets > option:selected')
                      .attr('data-ResultType');
    var results = this.model.results[rType];
    var select  = this.$('#ResultSet');
    if(results.length > 0){
      var options = '';
      results.each(function(r, i){
        var selected = (i === 0) ? ' selected="selected"' : '';
        var data     = ' data-index="'+i+'"';
        var inC      = r.getIn().length;
        var udecC    = r.getUdec().length;
        var outC     = r.getOut().length;
        var item     = (i+1)+': in('+inC+'),udec('+udecC+'),out('+outC+')';
        options += '<option'+selected+data+'>'+item+'</option>';
      });
      select.html(options).parent().show();
    }else{
      select.parent().hide();
      select.find('option').remove();
    }
    if(this.afterRenderResults){
       this.afterRenderResults();
       this.afterRenderResults = null;
    }
    this.resultSelected();
  }
, resultSelected: function(){
    var rType  = this.$('#ResultSets > option:selected').attr('data-ResultType');
    var rIx    = this.$('#ResultSet option:selected').attr('data-index');
    var result = this.model.results[rType].models[rIx];
    this.model.set({currentResult: result});
  }
, setModel: function(m){
    if(this.model){
      this.model.results.off(null, null, this);
    }
    this.model = m;
    if(m){
      this.model.results.on('change', this.render, this);
      this.$el.show();
    }else{
      this.$el.hide();
    }
    this.render();
  }
, next: function(){
    console.log('DiscussionGraphResultView.next();');
    var rIx = this.$('#ResultSet option:selected').next();
    if(rIx.length > 0){
      this.$('#ResultSet').val(rIx.val());
      this.resultSelected();
    }else{
      this.afterRenderResults = function(){
        this.$('#ResultSet').val(this.$('#ResultSet option:first').val());
      };
      var rType = this.$('#ResultSets > option:selected').next();
      if(rType.length === 0)
        rType = this.$('#ResultSets option:first');
      this.$('#ResultSets').val(rType.val());
      this.renderResults();
    }
  }
, prev: function(){
    console.log('DiscussionGraphResultView.prev();');
    var rIx = this.$('#ResultSet option:selected').prev();
    if(rIx.length > 0){
      this.$('#ResultSet').val(rIx.val());
      this.resultSelected();
    }else{
      this.afterRenderResults = function(){
        this.$('#ResultSet').val(this.$('#ResultSet option:last').val());
      };
      var rType = this.$('#ResultSets > option:selected').prev();
      if(rType.length === 0)
        rType = this.$('#ResultSets option:last');
      this.$('#ResultSets').val(rType.val());
      this.renderResults();
    }
  }
});
