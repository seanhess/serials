// @flow
import React from 'react'
import {Source} from '../model/source'
import {SourceCover} from '../cover'

export class SimpleGallery extends React.Component {

  render():React.Element {
    var sources:Array<Source> = this.props.sources || []
    var row = (s) => <GalleryCover source={s} key={s.id} />

    var style = {
      display: 'block',
      marginLeft: -15,
      marginRight: -15,
      textAlign: 'center'
    }

    return <div style={style}>
      {sources.map(row)}
    </div>
  }
}

export class GalleryCover extends React.Component {
  render():React.Element {
    var source = this.props.source
    var style = {
      margin: '0px 3px 0px 3px',
      display: 'inline-block'
    }

    var url = "#/books/"+source.id

    return <div style={style}>
      <a href={url}>
        <SourceCover source={source} />
      </a>
    </div>
  }
}
