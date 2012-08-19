window.Helpers = {
  formJson : function (form) {
    var o = {};
    $("input, select", form).each(function (_,e) {
      e = $(e);
      var t = e.attr("type");
      var p = e.attr("name");
      if (!p) return;
      var v = e.val();
      v = t === "checkbox" ? v === "on" : v;
      if (p in o && o[p] instanceof Array) {
        o[p].push(v);
      } else if (p in o) {
        o[p] = [o[p], v];
      } else {
        o[p] = v;
      }
    });
    return o;
  }
};
