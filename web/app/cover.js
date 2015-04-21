// @flow

var React = require('react')

// but get the images at 2x resolution so they can be retina yo
// or just get the photos at that ratio
export var CoverSize = {
  Width: 320,
  Height: 200,
  Ratio: 1.6
}

export class Cover extends React.Component {
  render() {

    return <span >
      <img />
    </span>
  }
}

export function coverStyle(url) {
  return {
    backgroundImage: 'url('+url+')',
    backgroundPosition: 'center center fixed',
    backgroundSize: 'cover',
    width: '200px',
    height: '320px'
  }
}


