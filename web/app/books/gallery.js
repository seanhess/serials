

// @flow

var React = require('react')

var {RouteHandler} = require('react-router')
var {SourceModel} = require('../model/source')

import {SourceCover} from '../cover'
import {SuggestBook} from './support'
import {Source, isNotHidden, isSearch} from '../model/source'
import {curry} from 'lodash'

export class Gallery extends React.Component {

  static load() {
    return {sources: SourceModel.findAll()}
  }

  constructor(props:any) {
    super(props)
    this.state = {search: ""}
  }

  search(e:any) {
    this.setState({search: e.target.value})
  }

  render():React.Element {
    var sources:Array<Source> = (this.props.sources || [])
                                .filter(isNotHidden)

    if (this.state.search) {
      sources = sources.filter(isSearch(this.state.search))
    }

    return <div style={{marginTop: 15}}>
      <div>
        <input type="text" value={this.state.search} placeholder="Search Books" onChange={this.search.bind(this)}/>
      </div>
      <SimpleGallery sources={sources} />
      <hr />
      <SuggestBook />
    </div>
  }
}

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
