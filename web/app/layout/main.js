
// @flow

import React from 'react'

import {RouteHandler, Link} from 'react-router'
import {assign} from 'lodash'
import {Users} from '../model/user'
import {background, Colors, clickable, displayIf} from '../style'
import {AlertView} from '../alert'
import {Alerts} from '../model/alert'
import {Routes, transitionTo} from '../router'
import {Header, SiteTitle} from './header'
import {OffCanvas, MenuHeader, MenuLinkStyle} from './offcanvas'
import {SubmitLink} from '../source/submit-link'

export class Main extends React.Component {
  render():React.Element {
    return <MainContainer alert={this.props.alert} currentUser={this.props.currentUser}>
      <RouteHandler {...this.props}/>
    </MainContainer>
  }
}

export class MainContainer extends React.Component {
  render():React.Element {
    return <OffCanvas>

      <SiteTitle key="title" currentUser={this.props.currentUser} />

      <MainMenu key="menu" currentUser={this.props.currentUser}/>

      <div className="row">
        <div className="columns small-12" style={background}>
          {this.props.children}
        </div>
        <AlertView alert={this.props.alert}/>
      </div>
    </OffCanvas>
  }
}

//class MenuLink extends React.Component {
//}

var MenuIcon = {
  float: 'right',
  display: 'block',
  fontSize: 24,
  marginTop: 0
}

//class MenuIcon extends React.Component {
  //render():React.Element {
    //return <div className="right">
    //</div>
  //}
//}

export class MainMenu extends React.Component {

  render():React.Element {
    var user = this.props.currentUser
    var userContent = ""

    if (user) {
      userContent = <UserMenu currentUser={user} />
    }

    else {
      userContent = <AnonMenu />
    }

    return <ul className="off-canvas-list">
      {userContent}
      <div>
        <li><MenuHeader>Web Fiction</MenuHeader></li>

        <li><Link to={Routes.library} style={MenuLinkStyle}>
          <span style={MenuIcon} className="fa fa-book"></span>
          <span> Library</span>
        </Link></li>

        <li><SubmitLink style={MenuLinkStyle}>
          <span style={MenuIcon} className="fa fa-plus-circle"></span>
          <span> Submit a book</span>
        </SubmitLink></li>

        <li><Link to={Routes.about} style={MenuLinkStyle}>About</Link></li>
      </div>
    </ul>
  }
}

export class AnonMenu extends React.Component {
  render():React.Element {
    return <div>
      <li><MenuHeader>Account</MenuHeader></li>
      <li><Link to={Routes.login}>
        <span style={MenuIcon} className="fa fa-sign-in"></span>
        <span> Login</span>
      </Link></li>
    </div>
  }
}

export class UserMenu extends React.Component {

  logout() {
    Users.logout().then(function() {
      transitionTo("login")
    })
  }

  render():React.Element {
    var user = this.props.currentUser
    return <div>
      <li><MenuHeader>{user.firstName} {user.lastName}</MenuHeader></li>
      <li><Link to={Routes.bookshelf} params={user} style={MenuLinkStyle}>
        <span style={MenuIcon} className="fa fa-bookmark"></span>
        <span> My Bookshelf</span>
      </Link></li>

      <li style={displayIf(user.admin)}>
        <Link to={Routes.admin} style={MenuLinkStyle}>Admin</Link>
      </li>

      <li><a onClick={this.logout.bind(this)} style={MenuLinkStyle}>
        <span> Logout</span>
      </a></li>

    </div>
  }
}

// if logged in: you have user stuff there, like "Profile", "Logout", etc?
// or, why don't I JUST have the logged in button?

// WHat do I need in there:

// About
// Admin
// My Books
// Logout or Login
// Home
// My Proposals (etc)



