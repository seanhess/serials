

// @flow

var React = require('react')

var {RouteHandler} = require('react-router')
var {SourceModel} = require('../model')
var {Cover} = require('../cover')

import {SuggestBook} from './support'
import {Source} from '../model'
import {curry} from 'lodash'

export class Gallery extends React.Component {

  static load() {
    return {sources: SourceModel.findAll()}
  }

  constructor(props) {
    super(props)
    this.state = {search: ""}
  }

  search(e) {
    this.setState({search: e.target.value})
  }

  render() {
    var sources:Array<Source> = this.props.sources || []

    if (this.state.search) {
      var search = this.state.search.toLowerCase()
      sources = sources.filter(function(source) {
        return source.name.toLowerCase().match(search)
      })
    }

    var row = (s) => <GalleryCover source={s} key={s.id} />

    var style = {
      display: 'flex',
      flexFlow: 'row wrap',
      justifyContent: 'center',
      alignContent: 'center',
      marginLeft: -10,
      marginRight: -10
    }

    return <div style={{marginTop: 15}}>
      <div>
        <input type="text" value={this.state.search} placeholder="Find Book" onChange={this.search.bind(this)}/>
      </div>
      <div style={style}>
        {sources.map(row)}
      </div>
      <hr />
      <SuggestBook />
    </div>
  }
}

var matchTitle = curry(function(search, source) {
  return search
})

export class GalleryCover extends React.Component {
  render() {
    var source = this.props.source
    var style = {
      margin: '3px 3px 3px 3px',
    }

    var url = "#/books/"+source.id

    return <div style={style}>
      <a href={url}>
        <Cover source={source} />
      </a>
    </div>
  }
}
