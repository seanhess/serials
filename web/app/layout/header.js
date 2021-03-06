
// @flow

import React from 'react'

import {RouteHandler, Link} from 'react-router'
import {assign} from 'lodash'
import {Users} from '../model/user'
import {background, Colors, clickable} from '../style'
import {LinkStyle, TitleStyle, NavBar, CenterText} from './style'
import {AlertView} from '../alert'
import {Alerts} from '../model/alert'
import {Routes} from '../router'
import {OffCanvas} from './offcanvas'

//export class MainContainer extends React.Component {
  //render():React.Element {
    //return <div>
      //<OffCanvas>
        //hello
      //</OffCanvas>
    //</div>
  //}
//}

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
        <Link style={LinkStyle} to={Routes.bookshelf} params={{id: this.props.currentUser.id}}>My Books</Link>
        <a style={LinkStyle} onClick={this.logout.bind(this)}>Logout</a>
      </div>

        //<p style={LinkStyle}>Hello, {this.props.currentUser.firstName}</p>
    }

    else {
      return <div style={{display: 'inline-block'}}>
        <Link style={LinkStyle} to={Routes.login}>Login</Link>
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


    var signup = ""

    if (!this.props.currentUser) {
      signup = <a style={LinkStyle} href="/hello">Sign Up</a>
    }

    var linkTo = {to: Routes.root}
    if (this.props.currentUser) {
      linkTo = {to: Routes.bookshelf, params: {id: this.props.currentUser.id}}
    }

    return <nav style={NavBar} role="navigation">
      <div style={{float: 'right'}}>
        <Link to={Routes.about} style={LinkStyle}>About</Link>
        {signup}
        <Link style={adminStyle} to={Routes.admin}>Admin</Link>
        {this.renderCurrentUser()}
      </div>
      <div style={CenterText}>
        <Link {...linkTo} style={TitleStyle}>
          <img src="img/serials-icon-light.png" style={{height: 30, marginRight: 5}}/>
          <span style={{fontWeight: 'bold', color: Colors.light}}>Web Fiction</span>
        </Link>
      </div>
    </nav>
  }
}

export class SiteTitle extends React.Component {

  render() {
    var linkTo = {to: Routes.root}
    if (this.props.currentUser) {
      linkTo = {to: Routes.bookshelf, params: {id: this.props.currentUser.id}}
    }

    return <Link {...linkTo} style={TitleStyle}>
      <img src="img/serials-icon-light.png" style={{height: 26, marginRight: 5}}/>
      <span style={{fontWeight: 'bold', color: Colors.light}}>Web Fiction</span>
    </Link>
  }
}


