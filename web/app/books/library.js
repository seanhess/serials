// @flow

import React from 'react'
import {Link} from 'react-router'
import {userBooks} from '../model/subscription'
import {notHidden} from '../model/source'
import {SimpleGallery} from './gallery'
import {Suggestion} from '../books/support'

export class Library extends React.Component {

  static load(params) {
    return {sources: userBooks(params.id)}
  }

  render():React.Element {
    var sources = (this.props.sources || [])

    return <div>
      <h3>My Books</h3>
      <SimpleGallery sources={sources} />
      <hr />
      <p><Link to="books">Discover new books</Link></p>
      <p><Suggestion /></p>
    </div>
  }
}
