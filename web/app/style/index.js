// @flow
// styles!

import {assign} from 'lodash'

export var Colors = {

  highlight: "#F15D58",
  highlightTone: "#D05045",

  dark: "#333",
  darkTone: "#414042",

  offWhite: "#F8FBFB",
  white: "#FFF",
  light: "#DCDDCD",

  info: "#A0D3E8",
  paper: "#FFFBE6",
  paperLine: "#FFF5C2",

  code: '#F4F7FB',
  codeTone: '#D2DFF0',

  diffAdd: '#EAFFEA',
  diffRemove: '#FFECEC'
}

export var background = {
  backgroundColor: Colors.offWhite
}

// required for mobile safari
export var clickable = {
  cursor: 'pointer'
}

export var mobileInput = {
  fontSize: '1em'
}

export function displayIf(show:boolean, base:string = 'block'):{display:string} {
  return {
    display: (show) ? base : 'none'
  }
}

export function visibleIf(show:boolean):{visibility:string} {
  return {
    visibility: (show) ? 'visible' : 'hidden'
  }
}

export var CollapseBorder = {
  marginBottom: -1
}

export var CellBorder = 'solid 1px #DDD'

export var Cell = assign({}, CollapseBorder, {
  borderBottom: CellBorder,
  borderTop: CellBorder,
  padding: 10
})

