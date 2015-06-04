// @flow

import React from 'react'
import {Link} from 'react-router'
import {last, assign} from 'lodash'

import {Cover} from'../cover'
import {toDateString} from '../helpers'

import {SourceStatus, Status} from '../model/source'
import {Colors, displayIf} from '../style'

export class BookInfo extends React.Component {
  render():React.Element {
    var source = this.props.source
    var lastChapter = this.props.lastChapter

    return <div>
      <div style={{color: statusColor(source.status)}}>{source.status}</div>
      <div style={{color: '#888'}}>Last Updated {toDateString(lastChapter.added)}</div>
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

          // <BookInfo source={this.props.source} lastChapter={lastChapter} />

export class BookArt extends React.Component {
  render():React.Element {
    var cover = <Cover source={this.props.source} />
    if (this.props.source.imageArtistUrl) {
      return <a href={this.props.source.imageArtistUrl}>{cover}</a>
    }
    else {
      return cover
    }
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
