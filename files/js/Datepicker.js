function Datepicker(){
  var t = this;
  //Fetching the target:
  this.target = $('.datepicker').get(0);
  //Fetching the current Date input:
  this.getDate = function(){
    return $('input[name=date]', t.target).val();
  };
  //Fetching the current Time input:
  this.getTime = function(){
    return $('input[name=time]', t.target).val();
  };
  //Fetching the current input as Obj:
  this.val = function(){
    var date = t.getDate();
    var time = t.getTime();
    //Parsing the date:
    var dateRegex= /^(\d+)-(\d+)-(\d+)$/;
    if(!dateRegex.test(date)){
      alert('You need to pick a date.');
      return null;
    }
    date = date.match(dateRegex);
    //Parsing the time:
    var timeRegex = /^(\d{1,2}):(\d{1,2})$/;
    if(!timeRegex.test(time)){
      alert('Time must obey a format of hh:mm.');
      return null;
    }
    time = time.match(timeRegex);
    //Finish:
    return {
      year:   date[1]
    , month:  date[2]
    , day:    date[3]
    , hour:   time[1]
    , minute: time[2]
    };
  };
}
