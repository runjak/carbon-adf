function InformationCollection(){
  //Check if InformationCollection is working:
  this.working = (typeof(Storage)!=="undefined");
  //Regex to match iids:
  this.iidRegex = /.*IId \(Id (.*)\)$/;
  //Function to check valid iids:
  this.validIid = function(iid){
    if(!this.iidRegex.test(iid)){
      alert('Malformed iid:\t' + iid);
      return false;
    }
    return true;
  };
  //Function to fetch from localStorage:
  this.fetch = function(){
    var iids = $.parseJSON(localStorage.iCol);
    if(!iids)
      iids = [];
    return iids;
  };
  /*
    Adds an iid to the localstorage collection.
    This allows users to later view Information as a bunch.
    @param iid String is expected to have the form of /.*IId \(Id (.*)\)$/
  */
  this.addInf = function(iid){
    //Checking the parameter:
    if(!this.validIid(iid))
      return;
    var iid = iid.match(this.iidRegex)[1];
    //Fetching localStorage:
    var iids = this.fetch();
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
    //Checking the parameter:
    if(!this.validIid(iid))
      return;
    var iid= iid.match(this.iidRegex)[1];
    //Removing from localStorage:
    var iids = this.fetch();
    iids = $.grep(iids, function(e, i){return e != iid;});
    localStorage.iCol = "[" + iids + "]";
  };
  /*
    Makes the users browser display the current collection
    by directing the window.location to '/information.html?items=[..]' where
    '..' are the comma seperated iids.
  */
  this.display = function(){
    var iids = localStorage.iCol;
    if(!iids || iids == "[]"){
      alert('Collect some Information first to view them together.');
      return;
    }
    document.location.href = "/information.html?items=" + iids;
  };
};

