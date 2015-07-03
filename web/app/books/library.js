// @flow

// this page is going to turn into a "top" page, without showing ALL the results, just some.

import React from 'react'

import {RouteHandler, Link} from 'react-router'
import {SourceModel, Source, isNotHidden, isSearch, allTags} from '../model/source'
import {Users} from '../model/user'

import {SuggestBook} from './support'
import {SimpleGallery} from './gallery'
import {curry} from 'lodash'
import {displayIf} from '../style'
import {Routes, transitionTo} from '../router'
import {SubmitLink} from '../source/submit-link'
import {TagSelect} from '../source/tags'

export class Library extends React.Component {

  static load(params, query) {
    return {
      sources: SourceModel.findAll(query.tag),
      tags: allTags(),
      tag: query.tag
    }
  }

  constructor(props:any) {
    super(props)
    this.state = {search: ""}
  }

  search(e:any) {
    //window.location.hash.search = "globba=true"
    this.setState({search: e.target.value})

    // if they search by a tag... what returns? Do they just type "rational?" and it works?
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

    var hasBooks = (sources.length > 0)

    
    // I'd love to be able to clear the tag...

    return <div style={{marginTop: 15}}>

      <div style={{marginBottom: 10}}>
        <div style={displayIf(this.props.tag)}>
          <Link to={Routes.library}>
            <span className="fa fa-times-circle"/>
          </Link>
          <span> Tag: {this.props.tag}</span>
        </div>

        <div style={displayIf(!this.props.tag)}>
          <span>All Books</span>
        </div>
      </div>

      <div className="row">
        <div className="columns small-12">
          <input type="search" value={this.state.search} placeholder="Search by Title or Author" onChange={this.search.bind(this)}/>
        </div>
      </div>

      <div>
        {resultsContent}
      </div>

      <hr />

      <SubmitLink className="secondary button">
        <span className="fa fa-plus-circle"></span>
        <span> Submit a book</span>
      </SubmitLink>
    </div>
  }
}
