
// @flow

import React from 'react'

import {RouteHandler, Link} from 'react-router'
import {assign} from 'lodash'
import {Users} from '../model/user'

export class MainContainer extends React.Component {
  render() {
    return <div className="row columns small-12">
      {this.props.children}
    </div>
  }
}

var TitleStyle = {
  color: 'white',
  fontSize: '18px',
  margin: 0,
}

var LinkStyle = {
  color: 'white',
  fontSize: '14px',
  margin: 14,
  display: 'inline-block'
}

var CenterText = {
  padding: 10,
  margin: 0,
}

var NavBar = {
  backgroundColor: '#333',
  height: 47,
  position: 'relative'
}

export class Header extends React.Component {

  logout() {
    Users.logout()
  }

  renderCurrentUser() {
    var currentUser = this.props.currentUser;

    if (currentUser) {
      return <div style={{display: 'inline-block'}}>
        <Link style={LinkStyle} to="library" params={{id: this.props.currentUser.id}}>My Books</Link>
        <p style={LinkStyle}>Hello, {this.props.currentUser.firstName}</p>
        <a style={LinkStyle} onClick={this.logout.bind(this)}>Logout</a>
      </div>
    }

    else {
      return <div style={{display: 'inline-block'}}>
        <Link style={LinkStyle} to='login'>Login</Link>
        <Link style={LinkStyle} to='signup'>Signup</Link>
      </div>
    }
  }

  render() {

    var isAdmin = false
    if (this.props.currentUser && this.props.currentUser.admin) {
      isAdmin = true
    }

    var adminStyle = assign({}, LinkStyle, {
      display: (isAdmin) ? 'inline-block' : 'none'
    })

    return <nav style={NavBar} role="navigation">
      <div style={{float: 'right'}}>
        <Link style={LinkStyle} to="about">About</Link>
        <Link style={adminStyle} to="sources">Admin</Link>
        {this.renderCurrentUser()}
      </div>
      <div style={CenterText}><Link to="books" style={TitleStyle}>Serials</Link></div>
    </nav>
  }
}

