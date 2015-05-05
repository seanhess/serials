// @flow

import React from 'react'
import Promise from 'bluebird'

import {RouteHandler} from 'react-router'
import {SourceModel, Source, emptySource, SourceStatus, Status} from '../model/source'
import {ChapterModel, showChapter, isLink} from '../model/chapter'
import {Cover} from'../cover'

import {toDateString} from '../helpers'
import {SomethingWrong} from './support'
import {last, groupBy, values} from 'lodash'

export class Book extends React.Component {

  static load(params) {
    return {
      source: SourceModel.find(params.id),
      chapters: ChapterModel.findBySource(params.id),
    }
  }

  render() {
    var source:Source = this.props.source || emptySource()
    var chapters = this.props.chapters || []
    var lastChapter = last(chapters) || {}
    var shown = chapters.filter(showChapter)

    // group them by arcs?
    var row = (c) => <Chapter chapter={c} key={c.id} />

    return <div>
      <h3> </h3>
      <div style={{marginTop: 10}}>
        <div style={{float: 'left', width: 160, marginBottom: 15}}>
          <Cover source={source} />
        </div>
        <div style={{marginLeft: 160}}>
          <h3>{source.name}</h3>
          <div>by {source.author}</div>
          <div style={{color: '#888'}}>Updated {toDateString(lastChapter.added)}</div>
          <div style={{color: statusColor(source.status)}}>{source.status}</div>
        </div>
      </div>

      <hr />

      <div style={{marginTop: 10}}>
        {shown.map(row)}
      </div>

      <hr />

      <SomethingWrong />
    </div>
  }
}

export function statusColor(status:SourceStatus):string {
  if (status === Status.Active) {
    return "#009800"
  }
  else if (status === Status.Complete) {
    return "#207DE5"
  }
  else {
    return "#888"
  }
}

export class Chapter extends React.Component {
  render() {
    var chapter:Chapter = this.props.chapter

    var content = ""
    if (isLink(chapter)) {
      content = this.renderLink(this.props.chapter)
    }
    else {
      content=this.renderTitle(this.props.chapter)
    }

    return <div>{content}</div>
  }

  renderLink(chapter:Chapter) {
    return <a href={chapter.content.linkURL}>{chapter.content.linkText}</a>
  }

  renderTitle(chapter) {
    return <h5 style={{marginTop: 15, marginBottom: 5}}>{chapter.content.titleText}</h5>
  }
}

