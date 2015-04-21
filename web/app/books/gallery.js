

// @flow

var React = require('react')

var {RouteHandler} = require('react-router')
var {SourceModel} = require('../model')
var {coverStyle} = require('../cover')

export class Gallery extends React.Component {

  static load() {
    return SourceModel.findAll()
    .then(function(sources) {
      return {sources: sources}
    })
  }

  render() {
    var sources = this.props.sources || []

    var row = (s) => <GalleryCover source={s} key={s.id} />

    var style = {
      display: 'flex',
      flexFlow: 'row wrap',
      justifyContent: 'flex-start'
    }

    return <div>
      <h2>Books</h2>
      <div style={style}>
        {sources.map(row)}
      </div>
    </div>
  }
}

export class GalleryCover extends React.Component {
  render() {
    var source = this.props.source
    var style = {
      margin: '0 5px',
    }

    var url = "#/books/"+source.id

    return <div style={style}>
      <a href={url}>
        <div style={coverStyle(source.imageUrl)} />
      </a>
    </div>
  }
}

