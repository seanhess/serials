// @flow

import React from 'react'
import {Link} from 'react-router'
import {last, assign} from 'lodash'

import {SourceCover, Cover, CoverOverlay} from'../cover'
import {toDateString} from '../helpers'

import {SourceStatus, Status, emptySource, Source} from '../model/source'
import {Colors, displayIf} from '../style'

export class BookInfo extends React.Component {
  render():React.Element {
    var source = this.props.source
    var lastChapter = this.props.lastChapter

    return <div>
      <div style={{color: statusColor(source.status)}}>{source.status}</div>
      <div style={{color: '#888'}}>Updated {toDateString(lastChapter.added)}</div>
    </div>

  }
}

export class CoverColumns extends React.Component {
  render():React.Element {
    var left = this.props.children[0]
    var right = this.props.children[1]

    return <div>
      <div style={{marginTop: 10}}>
        <div style={{float: 'left', width: 160, marginBottom: 15}}>
          {left}
        </div>
        <div style={{marginLeft: 160}}>
          {right}
        </div>
      </div>
    </div>
  }
}

export class BookArt extends React.Component {
  render():React.Element {
    var source:Source = this.props.source || emptySource()
    var overlay = ""

    if (source.imageArtistUrl) {
      overlay = <CoverOverlay style={{padding: 4, paddingTop: 0}}>
        <a style={{color: 'white', fontSize: 12}} href={source.imageArtistUrl}>Art by {source.imageArtist}</a>
      </CoverOverlay>
    }

    return <Cover src={source.imageUrl} >
      {overlay}
    </Cover>
  }
}

export class BookTitle extends React.Component {
  render():React.Element {
    var source = this.props.source
    return <div>
      <h3 style={{display: 'inline-block', margin: 0}}>{source.name} </h3>
      <span> </span>
      <span style={authorLineStyle}>by {this.renderAuthor()}</span>
    </div>
  }

  renderAuthor():React.Element {
    var source = this.props.source
    if (source.authorUrl) {
      return <a href={source.authorUrl}>{source.author}</a>
    }
    else {
      return source.author
    }
  }
}

export function statusColor(status:SourceStatus):string {
  if (status === Status.Active) {
    return "#009800"
  }
  else if (status === Status.Complete) {
    //return "#207DE5"
    return Colors.dark
  }
  else {
    return "#888"
  }
}

var authorLineStyle = {
  fontSize: '1em',
  fontWeight: 'normal',
  margin: 0,
}
