/**
 * jQuery Crud plugin
 *
 * Copyright (c) 2013 Jakob Runge <mam09crm@studserv.uni-leipzig.de>
 * Licensed GPL:
 * http://www.gnu.org/licenses/gpl.html
 *
 * This plugin adds the put and delete methods to jQuery,
 * so that together with get and post the 4 crud operations
 * can be performed.
 */
jQuery.put = function(url, data){
  return jQuery.ajax(url, {type: 'PUT', data: data});
};
jQuery.delete = function(url, data){
  return jQuery.ajax(url, {type: 'DELETE', data: data});
};
