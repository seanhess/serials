// @flow

import React from 'react'
import Promise from 'bluebird'

import {RouteHandler} from 'react-router'
import {SourceModel, Source, emptySource} from '../model/source'
import {ChapterModel, showChapter} from '../model/chapter'
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
    var arcs = groupBy(shown, c => c.arc)

    // group them by arcs?
    //var row = (c) => <Chapter chapter={c} key={c.id} />

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
        </div>
      </div>

      <hr />

      <div style={{marginTop: 10}}>
        {values(arcs).map(function(cs) {
          return <Arc chapters={cs} key={cs[0].arc}/>
        })}
      </div>

      <hr />

      <SomethingWrong />
    </div>
  }
}

export class Chapter extends React.Component {
  render() {
    var chapter = this.props.chapter
    return <div>
      <a href={chapter.url}>{chapter.name}</a>
    </div>
  }
}

export class Arc extends React.Component {
  render() {
    var chapters = this.props.chapters
    return <div>
      <h5>{chapters[0].arc}</h5>
      <div>{chapters.map((c) => <Chapter chapter={c} key={c.id}/>)}</div>
      <br />
    </div>
  }
}
