// @flow
// styles!

export var Colors = {

  highlight: "#F15D58",
  highlightTone: "#D05045",

  dark: "#333",
  darkTone: "#414042",

  offWhite: "#F8FBFB",
  white: "#FFF",
  light: "#DCDDCD"
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

export function displayIf(base:string, show:boolean):{display:string} {
  return {
    display: (show) ? base : 'none'
  }
}

export function visibleIf(show:boolean):{visibility:string} {
  return {
    visibility: (show) ? 'visible' : 'hidden'
  }
}
