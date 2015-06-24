// @flow

import {Source} from './source'

export type FieldChange = {
  field: string;
  oldValue: string;
  newValue: string;
}

export function diff(from:Object, to:Object):Array<FieldChange> {
  // compare: name vs name, etc
  return Object.keys(from).filter(function(key) {
    return from[key] && from[key] != to[key]
  })
  .map(function(key) {
    return {
      field: key,
      oldValue: from[key],
      newValue: to[key]
    }
  })
}
