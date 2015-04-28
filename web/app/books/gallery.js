

// @flow

var React = require('react')

var {RouteHandler} = require('react-router')
var {SourceModel} = require('../model')
var {Cover} = require('../cover')

export class Gallery extends React.Component {

  static load() {
    return {sources: SourceModel.findAll()}
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
      margin: '0px 10px 10px 0px',
    }

    var url = "#/books/"+source.id

    return <div style={style}>
      <a href={url}>
        <Cover source={source} />
      </a>
    </div>
  }
}

