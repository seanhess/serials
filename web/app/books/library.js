// @flow

import React from 'react'
import {Link} from 'react-router'
import {userBooks} from '../model/subscription'
import {SimpleGallery} from './gallery'

export class Library extends React.Component {

  static load(params) {
    return {sources: userBooks(params.id)}
  }

  render() {
    var sources = this.props.sources
    return <div>
      <h3>My Books</h3>
      <SimpleGallery sources={sources} />
      <hr />
      <p><Link to="books">Discover new books</Link></p>
    </div>
  }
}
