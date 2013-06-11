/**
  I decided to ship my own extensions with backbone collections.
  For example Backbone.Collection.findWhere distinguishes between '5' and 5 in properties.
  While this is nice sometimes, it's certainly annoying if these types are mixed for some fields.
*/
ExtendedCollection = Backbone.Collection.extend({
  /**
    @param pred :: Element -> Bool
    @return [Element]
    Returns all elements of an ExtendedCollection that satisfy a given predicate.
  */
  elems: function(pred){
    return this.filter(function(e){return pred(e);});
  }
  /**
    @param pred :: Element -> Bool
    @return Maybe Element
    Returns the first element that satisfies a given predicate.
  */
, elem: function(pred){
    var es = this.elems(pred);
    if(es === [])
      return null;
    return es[0];
  }
  /**
    @param id
    @return Maybe Element
    Returns the first element that has the given id value.
  */
, elemById: function(id){
    return this.elem(function(e){
      return id == e.get('id');
    });
  }
  /**
    Performs a fetch on all elements and returnes a promise to be resolved when all are fetched.
  */
, fetchAll: function(){
    var fetches = this.map(function(x){return x.fetch();});
    return $.when.apply($, fetches);
  }
  /**
    @param f :: Element -> *
    Iterates a function f over each element of the ExtendedCollection in reverse order.
  */
, reiterate: function(f){ //Iterate in reverse order
    _.each(this.last(this.length).reverse(), f);
  }
  /**
    @param e :: Element
    @return Bool
    Inserts the given element into the collection if it's not already a member,
    and otherwise removes it from the collection.
    The Element is required to have an id field that will be used for matching.
    Returns true if the element was inserted.
  */
, toggleElem: function(e){
    var id = e.get('id');
    var duplicates = this.elems(function(x){
      return id == x.get('id');
    });
    if(duplicates.length === 0){
      this.add(e);
      return true;
    }
    this.remove(duplicates);
    return false;
  }
});
