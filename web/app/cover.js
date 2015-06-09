// @flow

var React = require('react')
var {assign} = require('lodash')

import {Source, emptySource} from './model/source'
import {displayIf, Colors} from './style'

// but get the images at 2x resolution so they can be retina yo
// or just get the photos at that ratio
// 200x320 x2
// 150x240
// 100x160
// Blank Image - http://i.imgur.com/bMwt85W.jpg

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

export class CoverOverlay extends React.Component {
  render():React.Element {

    var style = assign(
      displayIf(this.props.show !== false),
      OverlayStyle,
      CoverTextStyle,
      this.props.style
    )

    return <div style={style}>
      {this.props.children}
    </div>
  }
}

export class Cover extends React.Component {
  render():React.Element {
    return <div style={assign(coverStyle(this.props.src), {position: 'relative'})}>
      {this.props.children}
    </div>
  }
}

export class SourceCover extends React.Component {
  render():React.Element {
    var source:Source = this.props.source || emptySource()
    var showTitle:bool = source.imageMissingTitle

    return <Cover src={source.imageUrl}>
      <CoverOverlay show={showTitle}>{source.name}</CoverOverlay>
    </Cover>
  }
}

// I could specify it in terms of percentages instead?
// that's a good idea.
// so do I want 2 or 3 across?
// definitely 3 :)

export var OverlayStyle = {
  padding: 10,
  color: Colors.light,
  textAlign: 'center',
  position: 'absolute',
  bottom: 0,
  fontSize: 18,
  backgroundColor: 'rgba(0, 0, 0, 0.5)',
  width: CoverSize.Width
}

export var CoverTextStyle = {
  fontSize: 18,
}


