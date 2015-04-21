// @flow

var React = require('react')
var Promise = require('bluebird')

var {RouteHandler} = require('react-router')
var {SourceModel, ChapterModel, showChapter} = require('../model')
var {Cover} = require('../cover')

export class Book extends React.Component {

  static load(params) {
    return Promise.join(
      SourceModel.find(params.id), 
      ChapterModel.findBySource(params.id),
      function(source, chapters) {
        return {source, chapters}
      }
    )
  }

  render() {
    var source = this.props.source || {}
    var chapters = this.props.chapters || []
    var shown = chapters.filter(showChapter)

    var row = (c) => <Chapter chapter={c} key={c.id} />

    return <div className="row small-12 columns">
      <h3> </h3>
      <Cover source={source} />

      <h3>Chapters</h3>
      <div>{shown.map(row)}</div>
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

