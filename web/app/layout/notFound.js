// @flow
import React from 'react'
import {Link} from 'react-router'
import {MainContainer} from './main'
import {Routes, transitionTo} from '../router'

export class NotFound extends React.Component {

  render():React.Element {

    // hack to get root location to go to the right place
    if (window.location.hash.match(Routes.bookshelf)) {
      transitionTo(Routes.library)
    }

    return <div>
      <p style={{marginTop: 10}}>Page not found</p>
      <div><Link to={Routes.root}>Home</Link></div>
    </div>
  }
}
