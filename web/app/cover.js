// @flow

// but get the images at 2x resolution so they can be retina yo
// or just get the photos at that ratio
export var CoverSize = {
  Width: 320,
  Height: 200,
  Ratio: 1.6
}


export function coverStyle(url:string):Object {
  return {
    backgroundImage: 'url('+url+')',
    backgroundPosition: 'center center fixed',
    backgroundSize: 'cover',
    width: '200px',
    height: '320px'
  }
}


