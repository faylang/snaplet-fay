/** @constructor
*/
var Application$SharedTypes = function(){
var True = true;
var False = false;

/*******************************************************************************
 * Thunks.
 */

// Force a thunk (if it is a thunk) until WHNF.
function _(thunkish,nocache){
  while (thunkish instanceof $) {
    thunkish = thunkish.force(nocache);
  }
  return thunkish;
}

// Apply a function to arguments (see method2 in Fay.hs).
function __(){
  var f = arguments[0];
  for (var i = 1, len = arguments.length; i < len; i++) {
    f = (f instanceof $? _(f) : f)(arguments[i]);
  }
  return f;
}

// Thunk object.
function $(value){
  this.forced = false;
  this.value = value;
}

// Force the thunk.
$.prototype.force = function(nocache) {
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};


function Fay$$seq(x) {
  return function(y) {
    _(x,false);
    return y;
  }
}

function Fay$$seq$36$uncurried(x,y) {
  _(x,false);
  return y;
}

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
  this.value = value;
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then(a){
  return function(b){
    return Fay$$bind(a)(function(_){
      return b;
    });
  };
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then$36$uncurried(a,b){
  return Fay$$bind$36$uncurried(a,function(_){ return b; });
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind(m){
  return function(f){
    return new $(function(){
      var monad = _(m,true);
      return _(f)(monad.value);
    });
  };
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind$36$uncurried(m,f){
    return new $(function(){
      var monad = _(m,true);
      return _(f)(monad.value);
    });
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$$_return(a){
  return new Fay$$Monad(a);
}

// Allow the programmer to access thunk forcing directly.
function Fay$$force(thunk){
  return function(type){
    return new $(function(){
      _(thunk,type);
      return new Fay$$Monad(Fay$$unit);
    })
  }
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$return$36$uncurried(a){
  return new Fay$$Monad(a);
}

// Unit: ().
var Fay$$unit = null;

/*******************************************************************************
 * Serialization.
 * Fay <-> JS. Should be bijective.
 */

// Serialize a Fay object to JS.
function Fay$$fayToJs(type,fayObj){
  var base = type[0];
  var args = type[1];
  var jsObj;
  switch(base){
    case "ptr": {
      jsObj = fayObj;
      break;
    }
    case "action": {
      // A nullary monadic action. Should become a nullary JS function.
      // Fay () -> function(){ return ... }
      jsObj = function(){
        return Fay$$fayToJs(args[0],_(fayObj,true).value);
      };
      break;
    }
    case "function": {
      // A proper function.
      jsObj = function(){
        var fayFunc = fayObj;
        var return_type = args[args.length-1];
        var len = args.length;
        // If some arguments.
        if (len > 1) {
          // Apply to all the arguments.
          fayFunc = _(fayFunc,true);
          // TODO: Perhaps we should throw an error when JS
          // passes more arguments than Haskell accepts.
          for (var i = 0, len = len; i < len - 1 && fayFunc instanceof Function; i++) {
            // Unserialize the JS values to Fay for the Fay callback.
            fayFunc = _(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);
          }
          // Finally, serialize the Fay return value back to JS.
          var return_base = return_type[0];
          var return_args = return_type[1];
          // If it's a monadic return value, get the value instead.
          if(return_base == "action") {
            return Fay$$fayToJs(return_args[0],fayFunc.value);
          }
          // Otherwise just serialize the value direct.
          else {
            return Fay$$fayToJs(return_type,fayFunc);
          }
        } else {
          throw new Error("Nullary function?");
        }
      };
      break;
    }
    case "string": {
      // Serialize Fay string to JavaScript string.
      var str = "";
      fayObj = _(fayObj);
      while(fayObj instanceof Fay$$Cons) {
        str += fayObj.car;
        fayObj = _(fayObj.cdr);
      }
      jsObj = str;
      break;
    }
    case "list": {
      // Serialize Fay list to JavaScript array.
      var arr = [];
      fayObj = _(fayObj);
      while(fayObj instanceof Fay$$Cons) {
        arr.push(Fay$$fayToJs(args[0],fayObj.car));
        fayObj = _(fayObj.cdr);
      }
      jsObj = arr;
      break;
    }
    case "tuple": {
      // Serialize Fay tuple to JavaScript array.
      var arr = [];
      fayObj = _(fayObj);
      var i = 0;
      while(fayObj instanceof Fay$$Cons) {
        arr.push(Fay$$fayToJs(args[i++],fayObj.car));
        fayObj = _(fayObj.cdr);
      }
      jsObj = arr;
      break;
    }
    case "defined": {
      fayObj = _(fayObj);
      if (fayObj instanceof $_Language$Fay$FFI$Undefined) {
        jsObj = undefined;
      } else {
        jsObj = Fay$$fayToJsUserDefined(args[0],fayObj["slot1"]);
      }
      break;
    }
    case "nullable": {
      fayObj = _(fayObj);
      if (fayObj instanceof $_Language$Fay$FFI$Null) {
        jsObj = null;
      } else {
        jsObj = Fay$$fayToJsUserDefined(args[0],fayObj["slot1"]);
      }
      break;
    }
    case "double": {
      // Serialize double, just force the argument. Doubles are unboxed.
      jsObj = _(fayObj);
      break;
    }
    case "int": {
      // Serialize int, just force the argument. Ints are unboxed.
      jsObj = _(fayObj);
      break;
    }
    case "bool": {
      // Bools are unboxed.
      jsObj = _(fayObj);
      break;
    }
    case "unknown":
      return fayObj;
    case "automatic":
    case "user": {
      if(fayObj instanceof $)
        fayObj = _(fayObj);
      jsObj = Fay$$fayToJsUserDefined(type,fayObj);
      break;
    }
    default: throw new Error("Unhandled Fay->JS translation type: " + base);
    }
    return jsObj;
}

// Unserialize an object from JS to Fay.
function Fay$$jsToFay(type,jsObj){
  var base = type[0];
  var args = type[1];
  var fayObj;
  switch(base){
    case "ptr": {
      fayObj = jsObj;
      break;
    }
    case "action": {
      // Unserialize a "monadic" JavaScript return value into a monadic value.
      fayObj = new Fay$$Monad(Fay$$jsToFay(args[0],jsObj));
      break;
    }
    case "string": {
      // Unserialize a JS string into Fay list (String).
      fayObj = Fay$$list(jsObj);
      break;
    }
    case "list": {
      // Unserialize a JS array into a Fay list ([a]).
      var serializedList = [];
      for (var i = 0, len = jsObj.length; i < len; i++) {
        // Unserialize each JS value into a Fay value, too.
        serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));
      }
      // Pop it all in a Fay list.
      fayObj = Fay$$list(serializedList);
      break;
    }
    case "tuple": {
      // Unserialize a JS array into a Fay tuple ((a,b,c,...)).
      var serializedTuple = [];
      for (var i = 0, len = jsObj.length; i < len; i++) {
        // Unserialize each JS value into a Fay value, too.
        serializedTuple.push(Fay$$jsToFay(args[i],jsObj[i]));
      }
      // Pop it all in a Fay list.
      fayObj = Fay$$list(serializedTuple);
      break;
    }
    case "defined": {
      if (jsObj === undefined) {
        fayObj = new $_Language$Fay$FFI$Undefined();
      } else {
        fayObj = new $_Language$Fay$FFI$Defined(Fay$$jsToFay(args[0],jsObj));
      }
      break;
    }
    case "nullable": {
      if (jsObj === null) {
        fayObj = new $_Language$Fay$FFI$Null();
      } else {
        fayObj = new $_Language$Fay$FFI$Nullable(Fay$$jsToFay(args[0],jsObj));
      }
      break;
    }
    case "double": {
      // Doubles are unboxed, so there's nothing to do.
      fayObj = jsObj;
      break;
    }
    case "int": {
      // Int are unboxed, so there's no forcing to do.
      // But we can do validation that the int has no decimal places.
      // E.g. Math.round(x)!=x? throw "NOT AN INTEGER, GET OUT!"
      fayObj = Math.round(jsObj);
      if(fayObj!==jsObj) throw "Argument " + jsObj + " is not an integer!";
      break;
    }
    case "bool": {
      // Bools are unboxed.
      fayObj = jsObj;
      break;
    }
    case "unknown":
      return jsObj;
    case "automatic":
    case "user": {
      if (jsObj && jsObj['instance']) {
        fayObj = Fay$$jsToFayUserDefined(type,jsObj);
      }
      else
        fayObj = jsObj;
      break;
    }
  default: throw new Error("Unhandled JS->Fay translation type: " + base);
  }
  return fayObj;
}

/*******************************************************************************
 * Lists.
 */

// Cons object.
function Fay$$Cons(car,cdr){
  this.car = car;
  this.cdr = cdr;
}

// Make a list.
function Fay$$list(xs){
  var out = null;
  for(var i=xs.length-1; i>=0;i--)
    out = new Fay$$Cons(xs[i],out);
  return out;
}

// Built-in list cons.
function Fay$$cons(x){
  return function(y){
    return new Fay$$Cons(x,y);
  };
}

// List index.
// `list' is already forced by the time it's passed to this function.
// `list' cannot be null and `index' cannot be out of bounds.
function Fay$$index(index,list){
  for(var i = 0; i < index; i++) {
    list = _(list.cdr);
  }
  return list.car;
}

// List length.
// `list' is already forced by the time it's passed to this function.
function Fay$$listLen(list,max){
  for(var i = 0; list !== null && i < max + 1; i++) {
    list = _(list.cdr);
  }
  return i == max;
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
  return function(y){
    return new $(function(){
      return _(x) * _(y);
    });
  };
}

function Fay$$mult$36$uncurried(x,y){

    return new $(function(){
      return _(x) * _(y);
    });

}

// Built-in +.
function Fay$$add(x){
  return function(y){
    return new $(function(){
      return _(x) + _(y);
    });
  };
}

// Built-in +.
function Fay$$add$36$uncurried(x,y){

    return new $(function(){
      return _(x) + _(y);
    });

}

// Built-in -.
function Fay$$sub(x){
  return function(y){
    return new $(function(){
      return _(x) - _(y);
    });
  };
}
// Built-in -.
function Fay$$sub$36$uncurried(x,y){

    return new $(function(){
      return _(x) - _(y);
    });

}

// Built-in /.
function Fay$$div(x){
  return function(y){
    return new $(function(){
      return _(x) / _(y);
    });
  };
}

// Built-in /.
function Fay$$div$36$uncurried(x,y){

    return new $(function(){
      return _(x) / _(y);
    });

}

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1, lit2) {
  // Simple case
  lit1 = _(lit1);
  lit2 = _(lit2);
  if (lit1 === lit2) {
    return true;
  }
  // General case
  if (lit1 instanceof Array) {
    if (lit1.length != lit2.length) return false;
    for (var len = lit1.length, i = 0; i < len; i++) {
      if (!Fay$$equal(lit1[i], lit2[i])) return false;
    }
    return true;
  } else if (lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons) {
    do {
      if (!Fay$$equal(lit1.car,lit2.car))
        return false;
      lit1 = _(lit1.cdr), lit2 = _(lit2.cdr);
      if (lit1 === null || lit2 === null)
        return lit1 === lit2;
    } while (true);
  } else if (typeof lit1 == 'object' && typeof lit2 == 'object' && lit1 && lit2 &&
             lit1.constructor === lit2.constructor) {
    for(var x in lit1) {
      if(!(lit1.hasOwnProperty(x) && lit2.hasOwnProperty(x) &&
           Fay$$equal(lit1[x],lit2[x])))
        return false;
    }
    return true;
  } else {
    return false;
  }
}

// Built-in ==.
function Fay$$eq(x){
  return function(y){
    return new $(function(){
      return Fay$$equal(x,y);
    });
  };
}

function Fay$$eq$36$uncurried(x,y){

    return new $(function(){
      return Fay$$equal(x,y);
    });

}

// Built-in /=.
function Fay$$neq(x){
  return function(y){
    return new $(function(){
      return !(Fay$$equal(x,y));
    });
  };
}

// Built-in /=.
function Fay$$neq$36$uncurried(x,y){

    return new $(function(){
      return !(Fay$$equal(x,y));
    });

}

// Built-in >.
function Fay$$gt(x){
  return function(y){
    return new $(function(){
      return _(x) > _(y);
    });
  };
}

// Built-in >.
function Fay$$gt$36$uncurried(x,y){

    return new $(function(){
      return _(x) > _(y);
    });

}

// Built-in <.
function Fay$$lt(x){
  return function(y){
    return new $(function(){
      return _(x) < _(y);
    });
  };
}


// Built-in <.
function Fay$$lt$36$uncurried(x,y){

    return new $(function(){
      return _(x) < _(y);
    });

}


// Built-in >=.
function Fay$$gte(x){
  return function(y){
    return new $(function(){
      return _(x) >= _(y);
    });
  };
}

// Built-in >=.
function Fay$$gte$36$uncurried(x,y){

    return new $(function(){
      return _(x) >= _(y);
    });

}

// Built-in <=.
function Fay$$lte(x){
  return function(y){
    return new $(function(){
      return _(x) <= _(y);
    });
  };
}

// Built-in <=.
function Fay$$lte$36$uncurried(x,y){

    return new $(function(){
      return _(x) <= _(y);
    });

}

// Built-in &&.
function Fay$$and(x){
  return function(y){
    return new $(function(){
      return _(x) && _(y);
    });
  };
}

// Built-in &&.
function Fay$$and$36$uncurried(x,y){

    return new $(function(){
      return _(x) && _(y);
    });
 ;
}

// Built-in ||.
function Fay$$or(x){
  return function(y){
    return new $(function(){
      return _(x) || _(y);
    });
  };
}

// Built-in ||.
function Fay$$or$36$uncurried(x,y){

    return new $(function(){
      return _(x) || _(y);
    });

}

/*******************************************************************************
 * Mutable references.
 */

// Make a new mutable reference.
function Fay$$Ref(x){
  this.value = x;
}

// Write to the ref.
function Fay$$writeRef(ref,x){
  ref.value = x;
}

// Get the value from the ref.
function Fay$$readRef(ref,x){
  return ref.value;
}

/*******************************************************************************
 * Dates.
 */
function Fay$$date(str){
  return window.Date.parse(str);
}

/*******************************************************************************
 * Application code.
 */

var $_Language$Fay$FFI$Nullable = function(slot1){this.slot1 = slot1;};var Language$Fay$FFI$Nullable = function(slot1){return new $(function(){return new $_Language$Fay$FFI$Nullable(slot1);});};var $_Language$Fay$FFI$Null = function(){};var Language$Fay$FFI$Null = new $(function(){return new $_Language$Fay$FFI$Null();});var $_Language$Fay$FFI$Defined = function(slot1){this.slot1 = slot1;};var Language$Fay$FFI$Defined = function(slot1){return new $(function(){return new $_Language$Fay$FFI$Defined(slot1);});};var $_Language$Fay$FFI$Undefined = function(){};var Language$Fay$FFI$Undefined = new $(function(){return new $_Language$Fay$FFI$Undefined();});var $_Language$Fay$FFI$Nullable = function(slot1){this.slot1 = slot1;};var Language$Fay$FFI$Nullable = function(slot1){return new $(function(){return new $_Language$Fay$FFI$Nullable(slot1);});};var $_Language$Fay$FFI$Null = function(){};var Language$Fay$FFI$Null = new $(function(){return new $_Language$Fay$FFI$Null();});var $_Language$Fay$FFI$Defined = function(slot1){this.slot1 = slot1;};var Language$Fay$FFI$Defined = function(slot1){return new $(function(){return new $_Language$Fay$FFI$Defined(slot1);});};var $_Language$Fay$FFI$Undefined = function(){};var Language$Fay$FFI$Undefined = new $(function(){return new $_Language$Fay$FFI$Undefined();});var $_Prelude$Just = function(slot1){this.slot1 = slot1;};var Prelude$Just = function(slot1){return new $(function(){return new $_Prelude$Just(slot1);});};var $_Prelude$Nothing = function(){};var Prelude$Nothing = new $(function(){return new $_Prelude$Nothing();});var $_Prelude$Left = function(slot1){this.slot1 = slot1;};var Prelude$Left = function(slot1){return new $(function(){return new $_Prelude$Left(slot1);});};var $_Prelude$Right = function(slot1){this.slot1 = slot1;};var Prelude$Right = function(slot1){return new $(function(){return new $_Prelude$Right(slot1);});};var Prelude$maybe = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) instanceof $_Prelude$Nothing) {var m = $p1;return m;}if (_($p3) instanceof $_Prelude$Just) {var x = _($p3).slot1;var f = $p2;return _(f)(x);}throw ["unhandled case in maybe",[$p1,$p2,$p3]];});};};};var $_Prelude$Ratio = function(slot1,slot2){this.slot1 = slot1;this.slot2 = slot2;};var Prelude$Ratio = function(slot1){return function(slot2){return new $(function(){return new $_Prelude$Ratio(slot1,slot2);});};};var Prelude$$62$$62$$61$ = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$bind(Fay$$fayToJs(["action",[["unknown"]]],$p1))(Fay$$fayToJs(["function",[["unknown"],["action",[["unknown"]]]]],$p2)));});};};var Prelude$$62$$62$ = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$then(Fay$$fayToJs(["action",[["unknown"]]],$p1))(Fay$$fayToJs(["action",[["unknown"]]],$p2)));});};};var Prelude$$_return = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$return(Fay$$fayToJs(["unknown"],$p1)));});};var Prelude$when = function($p1){return function($p2){return new $(function(){var m = $p2;var p = $p1;return _(p) ? _(_(Fay$$then)(m))(_(Fay$$$_return)(Fay$$unit)) : _(Fay$$$_return)(Fay$$unit);});};};var Prelude$forM_ = function($p1){return function($p2){return new $(function(){var m = $p2;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(_(Fay$$then)(_(m)(x)))(_(_(Prelude$forM_)(xs))(m));}if (_($p1) === null) {return _(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in forM_",[$p1,$p2]];});};};var Prelude$mapM_ = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var m = $p1;return _(_(Fay$$then)(_(m)(x)))(_(_(Prelude$mapM_)(m))(xs));}if (_($p2) === null) {return _(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in mapM_",[$p1,$p2]];});};};var Prelude$$61$$60$$60$ = function($p1){return function($p2){return new $(function(){var x = $p2;var f = $p1;return _(_(Fay$$bind)(x))(f);});};};var Prelude$sequence = function($p1){return new $(function(){var ms = $p1;return (function(){var k = function($p1){return function($p2){return new $(function(){var m$39$ = $p2;var m = $p1;return _(_(Fay$$bind)(m))(function($p1){var x = $p1;return _(_(Fay$$bind)(m$39$))(function($p1){var xs = $p1;return _(Fay$$$_return)(_(_(Fay$$cons)(x))(xs));});});});};};return _(_(_(Prelude$foldr)(k))(_(Fay$$$_return)(null)))(ms);})();});};var Prelude$sequence_ = function($p1){return new $(function(){if (_($p1) === null) {return _(Fay$$$_return)(Fay$$unit);}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var m = $tmp1.car;var ms = $tmp1.cdr;return _(_(Fay$$then)(m))(_(Prelude$sequence_)(ms));}throw ["unhandled case in sequence_",[$p1]];});};var $_Prelude$GT = function(){};var Prelude$GT = new $(function(){return new $_Prelude$GT();});var $_Prelude$LT = function(){};var Prelude$LT = new $(function(){return new $_Prelude$LT();});var $_Prelude$EQ = function(){};var Prelude$EQ = new $(function(){return new $_Prelude$EQ();});var Prelude$compare = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;return _(_(Fay$$gt)(_(x))(_(y))) ? Prelude$GT : _(_(Fay$$lt)(_(x))(_(y))) ? Prelude$LT : Prelude$EQ;});};};var Prelude$succ = function($p1){return new $(function(){var x = $p1;return _(Fay$$add)(_(x))(1);});};var Prelude$pred = function($p1){return new $(function(){var x = $p1;return _(Fay$$sub)(_(x))(1);});};var Prelude$enumFrom = function($p1){return new $(function(){var i = $p1;return _(_(Fay$$cons)(i))(_(Prelude$enumFrom)(_(Fay$$add)(_(i))(1)));});};var Prelude$enumFromTo = function($p1){return function($p2){return new $(function(){var n = $p2;var i = $p1;return _(_(Fay$$gt)(_(i))(_(n))) ? null : _(_(Fay$$cons)(i))(_(_(Prelude$enumFromTo)(_(Fay$$add)(_(i))(1)))(n));});};};var Prelude$enumFromBy = function($p1){return function($p2){return new $(function(){var by = $p2;var fr = $p1;return _(_(Fay$$cons)(fr))(_(_(Prelude$enumFromBy)(_(Fay$$add)(_(fr))(_(by))))(by));});};};var Prelude$enumFromThen = function($p1){return function($p2){return new $(function(){var th = $p2;var fr = $p1;return _(_(Prelude$enumFromBy)(fr))(_(Fay$$sub)(_(th))(_(fr)));});};};var Prelude$enumFromByTo = function($p1){return function($p2){return function($p3){return new $(function(){var to = $p3;var by = $p2;var fr = $p1;return (function(){var neg = function($p1){return new $(function(){var x = $p1;return _(_(Fay$$lt)(_(x))(_(to))) ? null : _(_(Fay$$cons)(x))(_(neg)(_(Fay$$add)(_(x))(_(by))));});};var pos = function($p1){return new $(function(){var x = $p1;return _(_(Fay$$gt)(_(x))(_(to))) ? null : _(_(Fay$$cons)(x))(_(pos)(_(Fay$$add)(_(x))(_(by))));});};return _(_(Fay$$lt)(_(by))(0)) ? _(neg)(fr) : _(pos)(fr);})();});};};};var Prelude$enumFromThenTo = function($p1){return function($p2){return function($p3){return new $(function(){var to = $p3;var th = $p2;var fr = $p1;return _(_(_(Prelude$enumFromByTo)(fr))(_(Fay$$sub)(_(th))(_(fr))))(to);});};};};var Prelude$fromIntegral = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Fay$$fayToJs(["int"],$p1));});};var Prelude$fromInteger = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Fay$$fayToJs(["int"],$p1));});};var Prelude$not = function($p1){return new $(function(){var p = $p1;return _(p) ? false : true;});};var Prelude$otherwise = true;var Prelude$show = function($p1){return new $(function(){return Fay$$jsToFay(["string"],JSON.stringify(Fay$$fayToJs(["automatic"],$p1)));});};var Prelude$error = function($p1){return new $(function(){return Fay$$jsToFay(["unknown"],(function() { throw Fay$$fayToJs(["string"],$p1) })());});};var Prelude$$_undefined = new $(function(){return _(Prelude$error)(Fay$$list("Prelude.undefined"));});var Prelude$either = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) instanceof $_Prelude$Left) {var a = _($p3).slot1;var f = $p1;return _(f)(a);}if (_($p3) instanceof $_Prelude$Right) {var b = _($p3).slot1;var g = $p2;return _(g)(b);}throw ["unhandled case in either",[$p1,$p2,$p3]];});};};};var Prelude$until = function($p1){return function($p2){return function($p3){return new $(function(){var x = $p3;var f = $p2;var p = $p1;return _(_(p)(x)) ? x : _(_(_(Prelude$until)(p))(f))(_(f)(x));});};};};var Prelude$$36$$33$ = function($p1){return function($p2){return new $(function(){var x = $p2;var f = $p1;return _(_(Fay$$seq)(x))(_(f)(x));});};};var Prelude$$_const = function($p1){return function($p2){return new $(function(){var a = $p1;return a;});};};var Prelude$id = function($p1){return new $(function(){var x = $p1;return x;});};var Prelude$$46$ = function($p1){return function($p2){return function($p3){return new $(function(){var x = $p3;var g = $p2;var f = $p1;return _(f)(_(g)(x));});};};};var Prelude$$36$ = function($p1){return function($p2){return new $(function(){var x = $p2;var f = $p1;return _(f)(x);});};};var Prelude$flip = function($p1){return function($p2){return function($p3){return new $(function(){var y = $p3;var x = $p2;var f = $p1;return _(_(f)(y))(x);});};};};var Prelude$curry = function($p1){return function($p2){return function($p3){return new $(function(){var y = $p3;var x = $p2;var f = $p1;return _(f)(Fay$$list([x,y]));});};};};var Prelude$uncurry = function($p1){return function($p2){return new $(function(){var p = $p2;var f = $p1;return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var x = Fay$$index(0,_($tmp1));var y = Fay$$index(1,_($tmp1));return _(_(f)(x))(y);}return (function(){ throw (["unhandled case",$tmp1]); })();})(p);});};};var Prelude$snd = function($p1){return new $(function(){if (Fay$$listLen(_($p1),2)) {var x = Fay$$index(1,_($p1));return x;}throw ["unhandled case in snd",[$p1]];});};var Prelude$fst = function($p1){return new $(function(){if (Fay$$listLen(_($p1),2)) {var x = Fay$$index(0,_($p1));return x;}throw ["unhandled case in fst",[$p1]];});};var Prelude$div = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;if (_(_(Fay$$and)(_(_(Fay$$gt)(_(x))(0)))(_(_(Fay$$lt)(_(y))(0))))) {return _(Fay$$sub)(_(_(_(Prelude$quot)(_(Fay$$sub)(_(x))(1)))(y)))(1);} else {if (_(_(Fay$$and)(_(_(Fay$$lt)(_(x))(0)))(_(_(Fay$$gt)(_(y))(0))))) {return _(Fay$$sub)(_(_(_(Prelude$quot)(_(Fay$$add)(_(x))(1)))(y)))(1);}}var y = $p2;var x = $p1;return _(_(Prelude$quot)(x))(y);});};};var Prelude$mod = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;if (_(_(Fay$$and)(_(_(Fay$$gt)(_(x))(0)))(_(_(Fay$$lt)(_(y))(0))))) {return _(Fay$$add)(_(_(Fay$$add)(_(_(_(Prelude$rem)(_(Fay$$sub)(_(x))(1)))(y)))(_(y))))(1);} else {if (_(_(Fay$$and)(_(_(Fay$$lt)(_(x))(0)))(_(_(Fay$$gt)(_(y))(0))))) {return _(Fay$$sub)(_(_(Fay$$add)(_(_(_(Prelude$rem)(_(Fay$$add)(_(x))(1)))(y)))(_(y))))(1);}}var y = $p2;var x = $p1;return _(_(Prelude$rem)(x))(y);});};};var Prelude$divMod = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;if (_(_(Fay$$and)(_(_(Fay$$gt)(_(x))(0)))(_(_(Fay$$lt)(_(y))(0))))) {return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var q = Fay$$index(0,_($tmp1));var r = Fay$$index(1,_($tmp1));return Fay$$list([_(Fay$$sub)(_(q))(1),_(Fay$$add)(_(_(Fay$$add)(_(r))(_(y))))(1)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(_(Prelude$quotRem)(_(Fay$$sub)(_(x))(1)))(y));} else {if (_(_(Fay$$and)(_(_(Fay$$lt)(_(x))(0)))(_(_(Fay$$gt)(_(y))(1))))) {return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var q = Fay$$index(0,_($tmp1));var r = Fay$$index(1,_($tmp1));return Fay$$list([_(Fay$$sub)(_(q))(1),_(Fay$$sub)(_(_(Fay$$add)(_(r))(_(y))))(1)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(_(Prelude$quotRem)(_(Fay$$add)(_(x))(1)))(y));}}var y = $p2;var x = $p1;return _(_(Prelude$quotRem)(x))(y);});};};var Prelude$min = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["unknown"],Math.min(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));});};};var Prelude$max = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["unknown"],Math.max(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));});};};var Prelude$recip = function($p1){return new $(function(){var x = $p1;return _(Fay$$div)(1)(_(x));});};var Prelude$negate = function($p1){return new $(function(){var x = $p1;return (-(_(x)));});};var Prelude$abs = function($p1){return new $(function(){var x = $p1;return _(_(Fay$$lt)(_(x))(0)) ? _(Prelude$negate)(x) : x;});};var Prelude$signum = function($p1){return new $(function(){var x = $p1;return _(_(Fay$$gt)(_(x))(0)) ? 1 : _(_(_(Fay$$eq)(x))(0)) ? 0 : (-(1));});};var Prelude$pi = new $(function(){return Fay$$jsToFay(["double"],Math.PI);});var Prelude$exp = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.exp(Fay$$fayToJs(["double"],$p1)));});};var Prelude$sqrt = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.sqrt(Fay$$fayToJs(["double"],$p1)));});};var Prelude$log = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.log(Fay$$fayToJs(["double"],$p1)));});};var Prelude$$42$$42$ = new $(function(){return Prelude$unsafePow;});var Prelude$$94$$94$ = new $(function(){return Prelude$unsafePow;});var Prelude$unsafePow = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["unknown"],Math.pow(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));});};};var Prelude$$94$ = function($p1){return function($p2){return new $(function(){var b = $p2;var a = $p1;if (_(_(Fay$$lt)(_(b))(0))) {return _(Prelude$error)(Fay$$list("(^): negative exponent"));} else {if (_(_(_(Fay$$eq)(b))(0))) {return 1;} else {if (_(_(Prelude$even)(b))) {return (function(){var x = new $(function(){return _(_(Prelude$$94$)(a))(_(_(Prelude$quot)(b))(2));});return _(Fay$$mult)(_(x))(_(x));})();}}}var b = $p2;var a = $p1;return _(Fay$$mult)(_(a))(_(_(_(Prelude$$94$)(a))(_(Fay$$sub)(_(b))(1))));});};};var Prelude$logBase = function($p1){return function($p2){return new $(function(){var x = $p2;var b = $p1;return _(Fay$$div)(_(_(Prelude$log)(x)))(_(_(Prelude$log)(b)));});};};var Prelude$sin = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.sin(Fay$$fayToJs(["double"],$p1)));});};var Prelude$tan = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.tan(Fay$$fayToJs(["double"],$p1)));});};var Prelude$cos = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.cos(Fay$$fayToJs(["double"],$p1)));});};var Prelude$asin = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.asin(Fay$$fayToJs(["double"],$p1)));});};var Prelude$atan = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.atan(Fay$$fayToJs(["double"],$p1)));});};var Prelude$acos = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.acos(Fay$$fayToJs(["double"],$p1)));});};var Prelude$sinh = function($p1){return new $(function(){var x = $p1;return _(Fay$$div)(_(_(Fay$$sub)(_(_(Prelude$exp)(x)))(_(_(Prelude$exp)((-(_(x))))))))(2);});};var Prelude$tanh = function($p1){return new $(function(){var x = $p1;return (function(){var a = new $(function(){return _(Prelude$exp)(x);});var b = new $(function(){return _(Prelude$exp)((-(_(x))));});return _(Fay$$div)(_(_(Fay$$sub)(_(a))(_(b))))(_(_(Fay$$add)(_(a))(_(b))));})();});};var Prelude$cosh = function($p1){return new $(function(){var x = $p1;return _(Fay$$div)(_(_(Fay$$add)(_(_(Prelude$exp)(x)))(_(_(Prelude$exp)((-(_(x))))))))(2);});};var Prelude$asinh = function($p1){return new $(function(){var x = $p1;return _(Prelude$log)(_(Fay$$add)(_(x))(_(_(Prelude$sqrt)(_(Fay$$add)(_(_(_(Prelude$$42$$42$)(x))(2)))(1)))));});};var Prelude$atanh = function($p1){return new $(function(){var x = $p1;return _(Fay$$div)(_(_(Prelude$log)(_(Fay$$div)(_(_(Fay$$add)(1)(_(x))))(_(_(Fay$$sub)(1)(_(x)))))))(2);});};var Prelude$acosh = function($p1){return new $(function(){var x = $p1;return _(Prelude$log)(_(Fay$$add)(_(x))(_(_(Prelude$sqrt)(_(Fay$$sub)(_(_(_(Prelude$$42$$42$)(x))(2)))(1)))));});};var Prelude$properFraction = function($p1){return new $(function(){var x = $p1;return (function(){var a = new $(function(){return _(Prelude$truncate)(x);});return Fay$$list([a,_(Fay$$sub)(_(x))(_(_(Prelude$fromIntegral)(a)))]);})();});};var Prelude$truncate = function($p1){return new $(function(){var x = $p1;return _(_(Fay$$lt)(_(x))(0)) ? _(Prelude$ceiling)(x) : _(Prelude$floor)(x);});};var Prelude$round = function($p1){return new $(function(){return Fay$$jsToFay(["int"],Math.round(Fay$$fayToJs(["double"],$p1)));});};var Prelude$ceiling = function($p1){return new $(function(){return Fay$$jsToFay(["int"],Math.ceil(Fay$$fayToJs(["double"],$p1)));});};var Prelude$floor = function($p1){return new $(function(){return Fay$$jsToFay(["int"],Math.floor(Fay$$fayToJs(["double"],$p1)));});};var Prelude$subtract = new $(function(){return _(Prelude$flip)(Fay$$sub);});var Prelude$even = function($p1){return new $(function(){var x = $p1;return _(_(Fay$$eq)(_(_(Prelude$rem)(x))(2)))(0);});};var Prelude$odd = function($p1){return new $(function(){var x = $p1;return _(Prelude$not)(_(Prelude$even)(x));});};var Prelude$gcd = function($p1){return function($p2){return new $(function(){var b = $p2;var a = $p1;return (function(){var go = function($p1){return function($p2){return new $(function(){if (_($p2) === 0) {var x = $p1;return x;}var y = $p2;var x = $p1;return _(_(go)(y))(_(_(Prelude$rem)(x))(y));});};};return _(_(go)(_(Prelude$abs)(a)))(_(Prelude$abs)(b));})();});};};var Prelude$quot = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;return _(_(_(Fay$$eq)(y))(0)) ? _(Prelude$error)(Fay$$list("Division by zero")) : _(_(Prelude$quot$39$)(x))(y);});};};var Prelude$quot$39$ = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["int"],~~(Fay$$fayToJs(["int"],$p1)/Fay$$fayToJs(["int"],$p2)));});};};var Prelude$quotRem = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;return Fay$$list([_(_(Prelude$quot)(x))(y),_(_(Prelude$rem)(x))(y)]);});};};var Prelude$rem = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;return _(_(_(Fay$$eq)(y))(0)) ? _(Prelude$error)(Fay$$list("Division by zero")) : _(_(Prelude$rem$39$)(x))(y);});};};var Prelude$rem$39$ = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["int"],Fay$$fayToJs(["int"],$p1) % Fay$$fayToJs(["int"],$p2));});};};var Prelude$lcm = function($p1){return function($p2){return new $(function(){if (_($p2) === 0) {return 0;}if (_($p1) === 0) {return 0;}var b = $p2;var a = $p1;return _(Prelude$abs)(_(Fay$$mult)(_(_(_(Prelude$quot)(a))(_(_(Prelude$gcd)(a))(b))))(_(b)));});};};var Prelude$find = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(_(p)(x)) ? _(Prelude$Just)(x) : _(_(Prelude$find)(p))(xs);}if (_($p2) === null) {return Prelude$Nothing;}throw ["unhandled case in find",[$p1,$p2]];});};};var Prelude$filter = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(_(p)(x)) ? _(_(Fay$$cons)(x))(_(_(Prelude$filter)(p))(xs)) : _(_(Prelude$filter)(p))(xs);}if (_($p2) === null) {return null;}throw ["unhandled case in filter",[$p1,$p2]];});};};var Prelude$$_null = function($p1){return new $(function(){if (_($p1) === null) {return true;}return false;});};var Prelude$map = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return _(_(Fay$$cons)(_(f)(x)))(_(_(Prelude$map)(f))(xs));}throw ["unhandled case in map",[$p1,$p2]];});};};var Prelude$nub = function($p1){return new $(function(){var ls = $p1;return _(_(Prelude$nub$39$)(ls))(null);});};var Prelude$nub$39$ = function($p1){return function($p2){return new $(function(){if (_($p1) === null) {return null;}var ls = $p2;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(_(_(Prelude$elem)(x))(ls)) ? _(_(Prelude$nub$39$)(xs))(ls) : _(_(Fay$$cons)(x))(_(_(Prelude$nub$39$)(xs))(_(_(Fay$$cons)(x))(ls)));}throw ["unhandled case in nub'",[$p1,$p2]];});};};var Prelude$elem = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var y = $tmp1.car;var ys = $tmp1.cdr;var x = $p1;return _(Fay$$or)(_(_(_(Fay$$eq)(x))(y)))(_(_(_(Prelude$elem)(x))(ys)));}if (_($p2) === null) {return false;}throw ["unhandled case in elem",[$p1,$p2]];});};};var Prelude$notElem = function($p1){return function($p2){return new $(function(){var ys = $p2;var x = $p1;return _(Prelude$not)(_(_(Prelude$elem)(x))(ys));});};};var Prelude$sort = new $(function(){return _(Prelude$sortBy)(Prelude$compare);});var Prelude$sortBy = function($p1){return new $(function(){var cmp = $p1;return _(_(Prelude$foldr)(_(Prelude$insertBy)(cmp)))(null);});};var Prelude$insertBy = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) === null) {var x = $p2;return Fay$$list([x]);}var ys = $p3;var x = $p2;var cmp = $p1;return (function($tmp1){if (_($tmp1) === null) {return Fay$$list([x]);}var $tmp2 = _($tmp1);if ($tmp2 instanceof Fay$$Cons) {var y = $tmp2.car;var ys$39$ = $tmp2.cdr;return (function($tmp2){if (_($tmp2) instanceof $_Prelude$GT) {return _(_(Fay$$cons)(y))(_(_(_(Prelude$insertBy)(cmp))(x))(ys$39$));}return _(_(Fay$$cons)(x))(ys);})(_(_(cmp)(x))(y));}return (function(){ throw (["unhandled case",$tmp1]); })();})(ys);});};};};var Prelude$conc = function($p1){return function($p2){return new $(function(){var ys = $p2;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(_(Fay$$cons)(x))(_(_(Prelude$conc)(xs))(ys));}var ys = $p2;if (_($p1) === null) {return ys;}throw ["unhandled case in conc",[$p1,$p2]];});};};var Prelude$concat = new $(function(){return _(_(Prelude$foldr)(Prelude$conc))(null);});var Prelude$concatMap = function($p1){return new $(function(){var f = $p1;return _(_(Prelude$foldr)(_(_(Prelude$$46$)(Prelude$$43$$43$))(f)))(null);});};var Prelude$foldr = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) === null) {var z = $p2;return z;}var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return _(_(f)(x))(_(_(_(Prelude$foldr)(f))(z))(xs));}throw ["unhandled case in foldr",[$p1,$p2,$p3]];});};};};var Prelude$foldr1 = function($p1){return function($p2){return new $(function(){if (Fay$$listLen(_($p2),1)) {var x = Fay$$index(0,_($p2));return x;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return _(_(f)(x))(_(_(Prelude$foldr1)(f))(xs));}if (_($p2) === null) {return _(Prelude$error)(Fay$$list("foldr1: empty list"));}throw ["unhandled case in foldr1",[$p1,$p2]];});};};var Prelude$foldl = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) === null) {var z = $p2;return z;}var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return _(_(_(Prelude$foldl)(f))(_(_(f)(z))(x)))(xs);}throw ["unhandled case in foldl",[$p1,$p2,$p3]];});};};};var Prelude$foldl1 = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return _(_(_(Prelude$foldl)(f))(x))(xs);}if (_($p2) === null) {return _(Prelude$error)(Fay$$list("foldl1: empty list"));}throw ["unhandled case in foldl1",[$p1,$p2]];});};};var Prelude$$43$$43$ = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;return _(_(Prelude$conc)(x))(y);});};};var Prelude$$33$$33$ = function($p1){return function($p2){return new $(function(){var b = $p2;var a = $p1;return (function(){var go = function($p1){return function($p2){return new $(function(){if (_($p1) === null) {return _(Prelude$error)(Fay$$list("(!!): index too large"));}if (_($p2) === 0) {var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;return h;}}var n = $p2;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return _(_(go)(t))(_(Fay$$sub)(_(n))(1));}throw ["unhandled case in go",[$p1,$p2]];});};};return _(_(Fay$$lt)(_(b))(0)) ? _(Prelude$error)(Fay$$list("(!!): negative index")) : _(_(go)(a))(b);})();});};};var Prelude$head = function($p1){return new $(function(){if (_($p1) === null) {return _(Prelude$error)(Fay$$list("head: empty list"));}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;return h;}throw ["unhandled case in head",[$p1]];});};var Prelude$tail = function($p1){return new $(function(){if (_($p1) === null) {return _(Prelude$error)(Fay$$list("tail: empty list"));}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return t;}throw ["unhandled case in tail",[$p1]];});};var Prelude$init = function($p1){return new $(function(){if (_($p1) === null) {return _(Prelude$error)(Fay$$list("init: empty list"));}if (Fay$$listLen(_($p1),1)) {var a = Fay$$index(0,_($p1));return Fay$$list([a]);}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;var t = $tmp1.cdr;return _(_(Fay$$cons)(h))(_(Prelude$init)(t));}throw ["unhandled case in init",[$p1]];});};var Prelude$last = function($p1){return new $(function(){if (_($p1) === null) {return _(Prelude$error)(Fay$$list("last: empty list"));}if (Fay$$listLen(_($p1),1)) {var a = Fay$$index(0,_($p1));return a;}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return _(Prelude$last)(t);}throw ["unhandled case in last",[$p1]];});};var Prelude$iterate = function($p1){return function($p2){return new $(function(){var x = $p2;var f = $p1;return _(_(Fay$$cons)(x))(_(_(Prelude$iterate)(f))(_(f)(x)));});};};var Prelude$repeat = function($p1){return new $(function(){var x = $p1;return _(_(Fay$$cons)(x))(_(Prelude$repeat)(x));});};var Prelude$replicate = function($p1){return function($p2){return new $(function(){if (_($p1) === 0) {return null;}var x = $p2;var n = $p1;return _(_(Fay$$lt)(_(n))(0)) ? _(Prelude$error)(Fay$$list("replicate: negative length")) : _(_(Fay$$cons)(x))(_(_(Prelude$replicate)(_(Fay$$sub)(_(n))(1)))(x));});};};var Prelude$cycle = function($p1){return new $(function(){if (_($p1) === null) {return _(Prelude$error)(Fay$$list("cycle: empty list"));}var xs = $p1;return (function(){var xs$39$ = new $(function(){return _(_(Prelude$$43$$43$)(xs))(xs$39$);});return xs$39$;})();});};var Prelude$take = function($p1){return function($p2){return new $(function(){if (_($p1) === 0) {return null;}if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return _(_(Fay$$lt)(_(n))(0)) ? _(Prelude$error)(Fay$$list("take: negative length")) : _(_(Fay$$cons)(x))(_(_(Prelude$take)(_(Fay$$sub)(_(n))(1)))(xs));}throw ["unhandled case in take",[$p1,$p2]];});};};var Prelude$drop = function($p1){return function($p2){return new $(function(){var xs = $p2;if (_($p1) === 0) {return xs;}if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;var n = $p1;return _(_(Fay$$lt)(_(n))(0)) ? _(Prelude$error)(Fay$$list("drop: negative length")) : _(_(Prelude$drop)(_(Fay$$sub)(_(n))(1)))(xs);}throw ["unhandled case in drop",[$p1,$p2]];});};};var Prelude$splitAt = function($p1){return function($p2){return new $(function(){var xs = $p2;if (_($p1) === 0) {return Fay$$list([null,xs]);}if (_($p2) === null) {return Fay$$list([null,null]);}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return _(_(Fay$$lt)(_(n))(0)) ? _(Prelude$error)(Fay$$list("splitAt: negative length")) : (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var a = Fay$$index(0,_($tmp1));var b = Fay$$index(1,_($tmp1));return Fay$$list([_(_(Fay$$cons)(x))(a),b]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(_(Prelude$splitAt)(_(Fay$$sub)(_(n))(1)))(xs));}throw ["unhandled case in splitAt",[$p1,$p2]];});};};var Prelude$takeWhile = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(_(p)(x)) ? _(_(Fay$$cons)(x))(_(_(Prelude$takeWhile)(p))(xs)) : null;}throw ["unhandled case in takeWhile",[$p1,$p2]];});};};var Prelude$dropWhile = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(_(p)(x)) ? _(_(Prelude$dropWhile)(p))(xs) : _(_(Fay$$cons)(x))(xs);}throw ["unhandled case in dropWhile",[$p1,$p2]];});};};var Prelude$span = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return Fay$$list([null,null]);}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(_(p)(x)) ? (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var a = Fay$$index(0,_($tmp1));var b = Fay$$index(1,_($tmp1));return Fay$$list([_(_(Fay$$cons)(x))(a),b]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(_(Prelude$span)(p))(xs)) : Fay$$list([null,_(_(Fay$$cons)(x))(xs)]);}throw ["unhandled case in span",[$p1,$p2]];});};};var Prelude$$_break = function($p1){return new $(function(){var p = $p1;return _(Prelude$span)(_(_(Prelude$$46$)(Prelude$not))(p));});};var Prelude$zipWith = function($p1){return function($p2){return function($p3){return new $(function(){var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return _(_(Fay$$cons)(_(_(f)(a))(b)))(_(_(_(Prelude$zipWith)(f))(as))(bs));}}return null;});};};};var Prelude$zipWith3 = function($p1){return function($p2){return function($p3){return function($p4){return new $(function(){var $tmp1 = _($p4);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return _(_(Fay$$cons)(_(_(_(f)(a))(b))(c)))(_(_(_(_(Prelude$zipWith3)(f))(as))(bs))(cs));}}}return null;});};};};};var Prelude$zip = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return _(_(Fay$$cons)(Fay$$list([a,b])))(_(_(Prelude$zip)(as))(bs));}}return null;});};};var Prelude$zip3 = function($p1){return function($p2){return function($p3){return new $(function(){var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return _(_(Fay$$cons)(Fay$$list([a,b,c])))(_(_(_(Prelude$zip3)(as))(bs))(cs));}}}return null;});};};};var Prelude$unzip = function($p1){return new $(function(){var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(_($tmp1.car),2)) {var x = Fay$$index(0,_($tmp1.car));var y = Fay$$index(1,_($tmp1.car));var ps = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var xs = Fay$$index(0,_($tmp1));var ys = Fay$$index(1,_($tmp1));return Fay$$list([_(_(Fay$$cons)(x))(xs),_(_(Fay$$cons)(y))(ys)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(Prelude$unzip)(ps));}}if (_($p1) === null) {return Fay$$list([null,null]);}throw ["unhandled case in unzip",[$p1]];});};var Prelude$unzip3 = function($p1){return new $(function(){var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(_($tmp1.car),3)) {var x = Fay$$index(0,_($tmp1.car));var y = Fay$$index(1,_($tmp1.car));var z = Fay$$index(2,_($tmp1.car));var ps = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(_($tmp1),3)) {var xs = Fay$$index(0,_($tmp1));var ys = Fay$$index(1,_($tmp1));var zs = Fay$$index(2,_($tmp1));return Fay$$list([_(_(Fay$$cons)(x))(xs),_(_(Fay$$cons)(y))(ys),_(_(Fay$$cons)(z))(zs)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(Prelude$unzip3)(ps));}}if (_($p1) === null) {return Fay$$list([null,null,null]);}throw ["unhandled case in unzip3",[$p1]];});};var Prelude$lines = function($p1){return new $(function(){if (_($p1) === null) {return null;}var s = $p1;return (function(){var isLineBreak = function($p1){return new $(function(){var c = $p1;return _(Fay$$or)(_(_(_(Fay$$eq)(c))("\r")))(_(_(_(Fay$$eq)(c))("\n")));});};return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var a = Fay$$index(0,_($tmp1));if (_(Fay$$index(1,_($tmp1))) === null) {return Fay$$list([a]);}var a = Fay$$index(0,_($tmp1));var $tmp2 = _(Fay$$index(1,_($tmp1)));if ($tmp2 instanceof Fay$$Cons) {var cs = $tmp2.cdr;return _(_(Fay$$cons)(a))(_(Prelude$lines)(cs));}}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(_(Prelude$$_break)(isLineBreak))(s));})();});};var Prelude$unlines = new $(function(){return _(Prelude$intercalate)(Fay$$list("\n"));});var Prelude$words = function($p1){return new $(function(){var str = $p1;return (function(){var words$39$ = function($p1){return new $(function(){if (_($p1) === null) {return null;}var s = $p1;return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var a = Fay$$index(0,_($tmp1));var b = Fay$$index(1,_($tmp1));return _(_(Fay$$cons)(a))(_(Prelude$words)(b));}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(_(Prelude$$_break)(isSpace))(s));});};var isSpace = function($p1){return new $(function(){var c = $p1;return _(_(Prelude$elem)(c))(Fay$$list(" \t\r\n\u000c\u000b"));});};return _(words$39$)(_(_(Prelude$dropWhile)(isSpace))(str));})();});};var Prelude$unwords = new $(function(){return _(Prelude$intercalate)(Fay$$list(" "));});var Prelude$and = function($p1){return new $(function(){if (_($p1) === null) {return true;}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(Fay$$and)(_(x))(_(_(Prelude$and)(xs)));}throw ["unhandled case in and",[$p1]];});};var Prelude$or = function($p1){return new $(function(){if (_($p1) === null) {return false;}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(Fay$$or)(_(x))(_(_(Prelude$or)(xs)));}throw ["unhandled case in or",[$p1]];});};var Prelude$any = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return false;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(Fay$$or)(_(_(p)(x)))(_(_(_(Prelude$any)(p))(xs)));}throw ["unhandled case in any",[$p1,$p2]];});};};var Prelude$all = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return true;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(Fay$$and)(_(_(p)(x)))(_(_(_(Prelude$all)(p))(xs)));}throw ["unhandled case in all",[$p1,$p2]];});};};var Prelude$intersperse = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return _(_(Fay$$cons)(x))(_(_(Prelude$prependToAll)(sep))(xs));}throw ["unhandled case in intersperse",[$p1,$p2]];});};};var Prelude$prependToAll = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return _(_(Fay$$cons)(sep))(_(_(Fay$$cons)(x))(_(_(Prelude$prependToAll)(sep))(xs)));}throw ["unhandled case in prependToAll",[$p1,$p2]];});};};var Prelude$intercalate = function($p1){return function($p2){return new $(function(){var xss = $p2;var xs = $p1;return _(Prelude$concat)(_(_(Prelude$intersperse)(xs))(xss));});};};var Prelude$maximum = function($p1){return new $(function(){if (_($p1) === null) {return _(Prelude$error)(Fay$$list("maximum: empty list"));}var xs = $p1;return _(_(Prelude$foldl1)(Prelude$max))(xs);});};var Prelude$minimum = function($p1){return new $(function(){if (_($p1) === null) {return _(Prelude$error)(Fay$$list("minimum: empty list"));}var xs = $p1;return _(_(Prelude$foldl1)(Prelude$min))(xs);});};var Prelude$product = function($p1){return new $(function(){if (_($p1) === null) {return _(Prelude$error)(Fay$$list("product: empty list"));}var xs = $p1;return _(_(_(Prelude$foldl)(Fay$$mult))(1))(xs);});};var Prelude$sum = function($p1){return new $(function(){if (_($p1) === null) {return _(Prelude$error)(Fay$$list("sum: empty list"));}var xs = $p1;return _(_(_(Prelude$foldl)(Fay$$add))(0))(xs);});};var Prelude$scanl = function($p1){return function($p2){return function($p3){return new $(function(){var l = $p3;var z = $p2;var f = $p1;return _(_(Fay$$cons)(z))((function($tmp1){if (_($tmp1) === null) {return null;}var $tmp2 = _($tmp1);if ($tmp2 instanceof Fay$$Cons) {var x = $tmp2.car;var xs = $tmp2.cdr;return _(_(_(Prelude$scanl)(f))(_(_(f)(z))(x)))(xs);}return (function(){ throw (["unhandled case",$tmp1]); })();})(l));});};};};var Prelude$scanl1 = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return _(_(_(Prelude$scanl)(f))(x))(xs);}throw ["unhandled case in scanl1",[$p1,$p2]];});};};var Prelude$scanr = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) === null) {var z = $p2;return Fay$$list([z]);}var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return (function($tmp1){var $tmp2 = _($tmp1);if ($tmp2 instanceof Fay$$Cons) {var h = $tmp2.car;var t = $tmp2.cdr;return _(_(Fay$$cons)(_(_(f)(x))(h)))(_(_(Fay$$cons)(h))(t));}return Prelude$$_undefined;})(_(_(_(Prelude$scanr)(f))(z))(xs));}throw ["unhandled case in scanr",[$p1,$p2,$p3]];});};};};var Prelude$scanr1 = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}if (Fay$$listLen(_($p2),1)) {var x = Fay$$index(0,_($p2));return Fay$$list([x]);}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return (function($tmp1){var $tmp2 = _($tmp1);if ($tmp2 instanceof Fay$$Cons) {var h = $tmp2.car;var t = $tmp2.cdr;return _(_(Fay$$cons)(_(_(f)(x))(h)))(_(_(Fay$$cons)(h))(t));}return Prelude$$_undefined;})(_(_(Prelude$scanr1)(f))(xs));}throw ["unhandled case in scanr1",[$p1,$p2]];});};};var Prelude$lookup = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {var _key = $p1;return Prelude$Nothing;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(_($tmp1.car),2)) {var x = Fay$$index(0,_($tmp1.car));var y = Fay$$index(1,_($tmp1.car));var xys = $tmp1.cdr;var key = $p1;return _(_(_(Fay$$eq)(key))(x)) ? _(Prelude$Just)(y) : _(_(Prelude$lookup)(key))(xys);}}throw ["unhandled case in lookup",[$p1,$p2]];});};};var Prelude$length = function($p1){return new $(function(){var xs = $p1;return _(_(Prelude$length$39$)(0))(xs);});};var Prelude$length$39$ = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;var acc = $p1;return _(_(Prelude$length$39$)(_(Fay$$add)(_(acc))(1)))(xs);}var acc = $p1;return acc;});};};var Prelude$reverse = function($p1){return new $(function(){var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(_(Prelude$$43$$43$)(_(Prelude$reverse)(xs)))(Fay$$list([x]));}if (_($p1) === null) {return null;}throw ["unhandled case in reverse",[$p1]];});};var Prelude$print = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs(["automatic"],$p1)));});};var Prelude$putStrLn = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs(["string"],$p1)));});};var $_Application$SharedTypes$Time = function(time){this.time = time;};var Application$SharedTypes$Time = function(time){return new $(function(){return new $_Application$SharedTypes$Time(time);});};var Application$SharedTypes$time = function(x){return new $(function(){return _(x).time;});};var Fay$$fayToJsUserDefined = function(type,obj){var _obj = _(obj);var argTypes = type[2];if (_obj instanceof $_Application$SharedTypes$Time) {var obj_ = {"instance": "Time"};var obj_time = Fay$$fayToJs(["string"],_(_obj.time));if (undefined !== obj_time) {obj_['time'] = obj_time;}return obj_;}if (_obj instanceof $_Prelude$EQ) {var obj_ = {"instance": "EQ"};return obj_;}if (_obj instanceof $_Prelude$LT) {var obj_ = {"instance": "LT"};return obj_;}if (_obj instanceof $_Prelude$GT) {var obj_ = {"instance": "GT"};return obj_;}if (_obj instanceof $_Prelude$Ratio) {var obj_ = {"instance": "Ratio"};var obj_slot1 = Fay$$fayToJs(["int"],_(_obj.slot1));if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}var obj_slot2 = Fay$$fayToJs(["int"],_(_obj.slot2));if (undefined !== obj_slot2) {obj_['slot2'] = obj_slot2;}return obj_;}if (_obj instanceof $_Prelude$Right) {var obj_ = {"instance": "Right"};var obj_slot1 = Fay$$fayToJs((type)[2] ? ((type)[2])[0] ? ((type)[2])[0] : ["unknown"] : ["unknown"],_(_obj.slot1));if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Prelude$Left) {var obj_ = {"instance": "Left"};var obj_slot1 = Fay$$fayToJs((type)[2] ? ((type)[2])[0] ? ((type)[2])[0] : ["unknown"] : ["unknown"],_(_obj.slot1));if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Prelude$Nothing) {var obj_ = {"instance": "Nothing"};return obj_;}if (_obj instanceof $_Prelude$Just) {var obj_ = {"instance": "Just"};var obj_slot1 = Fay$$fayToJs((type)[2] ? ((type)[2])[0] ? ((type)[2])[0] : ["unknown"] : ["unknown"],_(_obj.slot1));if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$FFI$Undefined) {var obj_ = {"instance": "Undefined"};return obj_;}if (_obj instanceof $_Language$Fay$FFI$Defined) {var obj_ = {"instance": "Defined"};var obj_slot1 = Fay$$fayToJs((type)[2] ? ((type)[2])[0] ? ((type)[2])[0] : ["unknown"] : ["unknown"],_(_obj.slot1));if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$FFI$Null) {var obj_ = {"instance": "Null"};return obj_;}if (_obj instanceof $_Language$Fay$FFI$Nullable) {var obj_ = {"instance": "Nullable"};var obj_slot1 = Fay$$fayToJs((type)[2] ? ((type)[2])[0] ? ((type)[2])[0] : ["unknown"] : ["unknown"],_(_obj.slot1));if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$FFI$Undefined) {var obj_ = {"instance": "Undefined"};return obj_;}if (_obj instanceof $_Language$Fay$FFI$Defined) {var obj_ = {"instance": "Defined"};var obj_slot1 = Fay$$fayToJs((type)[2] ? ((type)[2])[0] ? ((type)[2])[0] : ["unknown"] : ["unknown"],_(_obj.slot1));if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$FFI$Null) {var obj_ = {"instance": "Null"};return obj_;}if (_obj instanceof $_Language$Fay$FFI$Nullable) {var obj_ = {"instance": "Nullable"};var obj_slot1 = Fay$$fayToJs((type)[2] ? ((type)[2])[0] ? ((type)[2])[0] : ["unknown"] : ["unknown"],_(_obj.slot1));if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}return obj;};var Fay$$jsToFayUserDefined = function(type,obj){if (obj["instance"] === "Time") {return new $_Application$SharedTypes$Time(Fay$$jsToFay(["string"],obj["time"]));}if (obj["instance"] === "EQ") {return new $_Prelude$EQ();}if (obj["instance"] === "LT") {return new $_Prelude$LT();}if (obj["instance"] === "GT") {return new $_Prelude$GT();}if (obj["instance"] === "Ratio") {return new $_Prelude$Ratio(Fay$$jsToFay(["int"],obj["slot1"]),Fay$$jsToFay(["int"],obj["slot2"]));}if (obj["instance"] === "Right") {return new $_Prelude$Right(Fay$$jsToFay((type)[2] ? ((type)[2])[0] ? ((type)[2])[0] : ["unknown"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Left") {return new $_Prelude$Left(Fay$$jsToFay((type)[2] ? ((type)[2])[0] ? ((type)[2])[0] : ["unknown"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Nothing") {return new $_Prelude$Nothing();}if (obj["instance"] === "Just") {return new $_Prelude$Just(Fay$$jsToFay((type)[2] ? ((type)[2])[0] ? ((type)[2])[0] : ["unknown"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Undefined") {return new $_Language$Fay$FFI$Undefined();}if (obj["instance"] === "Defined") {return new $_Language$Fay$FFI$Defined(Fay$$jsToFay((type)[2] ? ((type)[2])[0] ? ((type)[2])[0] : ["unknown"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Null") {return new $_Language$Fay$FFI$Null();}if (obj["instance"] === "Nullable") {return new $_Language$Fay$FFI$Nullable(Fay$$jsToFay((type)[2] ? ((type)[2])[0] ? ((type)[2])[0] : ["unknown"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Undefined") {return new $_Language$Fay$FFI$Undefined();}if (obj["instance"] === "Defined") {return new $_Language$Fay$FFI$Defined(Fay$$jsToFay((type)[2] ? ((type)[2])[0] ? ((type)[2])[0] : ["unknown"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Null") {return new $_Language$Fay$FFI$Null();}if (obj["instance"] === "Nullable") {return new $_Language$Fay$FFI$Nullable(Fay$$jsToFay((type)[2] ? ((type)[2])[0] ? ((type)[2])[0] : ["unknown"] : ["unknown"],obj["slot1"]));}return obj;};
// Exports
this.Application$SharedTypes$Time = Application$SharedTypes$Time;
this.Application$SharedTypes$time = Application$SharedTypes$time;
this.Application$SharedTypes$Time = Application$SharedTypes$Time;

// Built-ins
this._ = _;
this.$           = $;
this.$fayToJs    = Fay$$fayToJs;
this.$jsToFay    = Fay$$jsToFay;

};
;
var main = new Application$SharedTypes();
main._(main.Application$SharedTypes$main);

