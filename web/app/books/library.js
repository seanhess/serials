// @flow

import React from 'react'

import {RouteHandler, Link} from 'react-router'
import {SourceModel, Source, isNotHidden, isSearch} from '../model/source'

import {SuggestBook} from './support'
import {SimpleGallery} from './gallery'
import {curry} from 'lodash'
import {displayIf} from '../style'
import {Routes} from '../router'

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

    var resultsContent = <SimpleGallery sources={sources} />

    if (sources.length === 0) {
      resultsContent = <div>Can't find what you're looking for? Submit a book by clicking below.</div>
    }

    var hasBooks = sources.length > 0

    return <div style={{marginTop: 15}}>
      <div>
        <input type="text" value={this.state.search} placeholder="Search Books" onChange={this.search.bind(this)}/>
      </div>

      <div>{resultsContent}</div>

      <hr />
      <AddBook />
    </div>
  }
}

export class AddBook extends React.Component {
  render():React.Element {
    return <div>
      <Link className="secondary button" to={Routes.source} params={{id: 'new'}}>
        <span className="fa fa-plus-circle"></span>
        <span> Submit a book</span>
      </Link>
    </div>
  }
}

