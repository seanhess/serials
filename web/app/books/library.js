// @flow

import React from 'react'

import {RouteHandler} from 'react-router'
import {SourceModel, Source, isNotHidden, isSearch} from '../model/source'

import {SuggestBook} from './support'
import {SimpleGallery} from './gallery'
import {curry} from 'lodash'

export class Library extends React.Component {

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

