// @flow

var React = require('react')
var {assign} = require('lodash')

// but get the images at 2x resolution so they can be retina yo
// or just get the photos at that ratio
export var CoverSize = {
  Width: 320,
  Height: 200,
  Ratio: 1.6
}


export function coverStyle(url:string):Object {
  return {
    background: 'url('+url+') no-repeat center center',
    backgroundSize: 'cover',
    width: '200px',
    height: '320px',
  }
}

export class Cover extends React.Component {
  render() {
    var source = this.props.source || {}
    return <div style={assign(coverStyle(source.imageUrl), {position: 'relative'})}>
      <div style={CoverTextStyle}>
        {source.name}
      </div>
    </div>
  }
}

var CoverTextStyle = {
  padding: 15,
  color: 'white',
  textAlign: 'center',
  position: 'absolute',
  bottom: 0,
  fontSize: 18,
  backgroundColor: 'rgba(0, 0, 0, 0.5)',
  width: 200
}


