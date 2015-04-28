// @flow

// Generate a change handler that takes setter functions
// it'll look like this
// > update((c, v) => c.something = v)
// 

export type Save<T> = (current:T) => void;
export type Setter<T> = (current:T, value:any) => void;
export type ReactEventHandler = (e:any) => void;
export type Update<T> = (setter:Setter<T>, toValue?:Function) => ReactEventHandler;

export function makeUpdate<T>(current:T, save:Save<T>):Update<T> {
  return function(setter, toValue = value) {
    return function(e) {
      var value = toValue(e)

      // let the setter modify the current object
      setter(current, value)

      // notify them of an update
      save(current)
    }
  }
}

export function checked(e:any):Boolean {
  return e.target.checked
}

export function value(e:any):any {
  return e.target.value
}
