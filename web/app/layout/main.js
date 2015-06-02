
// @flow

import React from 'react'

import {RouteHandler, Link} from 'react-router'
import {assign} from 'lodash'
import {Users} from '../model/user'
import {background, Colors} from '../style'

export class MainContainer extends React.Component {
  render():React.Element {
    return <div className="row columns small-12" style={background}>
      {this.props.children}
    </div>
  }
}

var TitleStyle = {
  color: Colors.white,
  fontSize: '18px',
  margin: 0,
}

var LinkStyle = {
  color: Colors.white,
  fontSize: '14px',
  margin: 14,
  display: 'inline-block'
}

var CenterText = {
  padding: 10,
  margin: 0,
}

var NavBar = {
  //backgroundColor: '#333',
  backgroundColor: Colors.dark,
  height: 47,
  position: 'relative'
}

export class Header extends React.Component {

  logout() {
    Users.logout().then(function() {
      window.location.hash = "/login"
    })
  }

  renderCurrentUser():React.Element {
    var currentUser = this.props.currentUser;

    if (currentUser) {
      return <div style={{display: 'inline-block'}}>
        <Link style={LinkStyle} to="library" params={{id: this.props.currentUser.id}}>My Books</Link>
        <a style={LinkStyle} onClick={this.logout.bind(this)}>Logout</a>
      </div>

        //<p style={LinkStyle}>Hello, {this.props.currentUser.firstName}</p>
    }

    else {
      return <div style={{display: 'inline-block'}}>
        <Link style={LinkStyle} to='login'>Login</Link>
      </div>
    }
  }

  render():React.Element {

    var isAdmin = false
    if (this.props.currentUser && this.props.currentUser.admin) {
      isAdmin = true
    }

    var adminStyle = assign({}, LinkStyle, {
      display: (isAdmin) ? 'inline-block' : 'none'
    })

    //<Link style={LinkStyle} to="about">About</Link>
    return <nav style={NavBar} role="navigation">
      <div style={{float: 'right'}}>
        <a style={LinkStyle} href="/">About</a>
        <Link style={adminStyle} to="admin">Admin</Link>
        {this.renderCurrentUser()}
      </div>
      <div style={CenterText}>
        <Link to="books" style={TitleStyle}>
          <img src="img/serials-icon-white.png" style={{height: 30, marginRight: 5}}/>
          <span style={{fontWeight: 'bold'}}>serials</span>
        </Link>
      </div>
    </nav>
  }
}

