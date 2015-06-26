// @flow

import React from 'react'
import {Link} from 'react-router'
import {Routes, transitionTo} from '../router'
import {Users} from '../model/user'

export class SubmitLink extends React.Component {
  onClick(e:Event) {
    if (!Users.isLoggedIn()) {
      transitionTo(Routes.login, {}, {to: Routes.source, id: 'new'})
      e.preventDefault()
    }
  }

  render():React.Element {
    return <Link className={this.props.className} style={this.props.style}
      onClick={this.onClick.bind(this)}
      to={Routes.source} params={{id: 'new'}}>
        {this.props.children}
    </Link>
  }
}
