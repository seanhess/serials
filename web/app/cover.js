// @flow

var React = require('react')
var {assign} = require('lodash')

import {Source, emptySource} from './model'

// but get the images at 2x resolution so they can be retina yo
// or just get the photos at that ratio
export var CoverSize = {
  Width: 150,
  Height: 240,
  Ratio: 1.6
}


export function coverStyle(url:string):Object {

  // otherwise it forgets about the cover. Wait until the image is ready
  if (!url) {
    return {

    }
  }

  return {
    background: 'url('+url+') no-repeat center center',
    backgroundSize: 'cover',
    width: CoverSize.Width,
    height: CoverSize.Height
  }
}

export class Cover extends React.Component {
  render() {
    var source:Source = this.props.source || emptySource()
    var showTitle:bool = source.imageMissingTitle

    var textStyle = assign(CoverTextStyle, display(showTitle))

    return <div style={assign(coverStyle(source.imageUrl), {position: 'relative'})}>
      <div style={textStyle}>
        {source.name}
      </div>
    </div>
  }
}

function display(value:bool) {
  return {
    display: (value) ? 'block' : 'none'
  }
}

// I could specify it in terms of percentages instead?
// that's a good idea.
// so do I want 2 or 3 across?
// definitely 3 :)

var CoverTextStyle = {
  padding: 10,
  color: 'white',
  textAlign: 'center',
  position: 'absolute',
  bottom: 0,
  fontSize: 18,
  backgroundColor: 'rgba(0, 0, 0, 0.5)',
  width: CoverSize.Width
}


