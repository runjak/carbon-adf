function InformationCollection(){
  //Check if InformationCollection is working:
  this.working = (typeof(Storage)!=="undefined");
  /*
    Adds an iid to the localstorage collection.
    This allows users to later view Information as a bunch.
    @param iid String is expected to have the form of /.*IId \(Id (.*)\)$/
  */
  this.addInf = function(iid){
    //Checking the parameter:
    if(!iid){
      alert('Cannot collect iid:\t' + iid);
      return;
    }else if(! /.*IId \(Id (.*)\)$/.test(iid)){
      alert('Malformed iid:\t' + iid);
      return;
    }
    var iid = iid.match(/.*IId \(Id (.*)\)$/)[1];
    //Fetching localStorage:
    var iids = $.parseJSON(localStorage.iCol);
    if(!iids)
      iids = [];
    //Updating iids:
    iids = $.grep(iids, function(e, i){return e != iid;});
    iids.unshift(iid);
    //Saving the new iids:
    localStorage.iCol = "[" + iids + "]";
    alert('Information added to collection.');
  };
  /*
    Removes an iid from the localstorage collection.
    @param iid String followes the same notion as addInf
  */
  this.delInf = function(iid){
    alert('Implement InformationCollection.delInf(iid) in files/js/InformationCollection.js!');
  };
  /*
    Makes the users browser display the current collection
    by directing the window.location to '/information.html?items=[..]' where
    '..' are the comma seperated iids.
  */
  this.display = function(){
    var iids = localStorage.iCol;
    if(!iids){
      alert('Collect some Information first to view them together.');
      return;
    }
    document.location.href = "/information.html?items=" + iids;
  };
};

