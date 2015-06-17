// @flow
import React from 'react'
import {Link} from 'react-router'
import {RouteHandler} from 'react-router'

import {FormSection} from '../comp'
import {Logo} from '../pages/about'
import {Users} from '../model/user'
import {Alerts} from '../model/alert'
import {AlertView} from '../alert'
import {makeUpdate} from '../data/update'

import {mobileInput} from '../style'
import {transitionTo} from '../router'

type LoginData = {email: string; password: string}

var emptyLogin = function():LoginData {
  return {
    email: '',
    password: ''
  }
}

export class LogoPage extends React.Component {
  render():React.Element {
    return <div>
      <div style={{padding: 25}} className="row small-12 columns">
        <div style={{textAlign: 'center'}}>
          <Logo />
        </div>
        {this.props.children}
      </div>
      <div style={{position: 'absolute', bottom: 0, right: 0, margin: 20}}><a href="https://github.com/seanhess/serials/blob/master/doc/user-agreement.md">User Agreement</a></div>
      <AlertView alert={this.props.alert}/>
    </div>
  }
}

export class Login extends React.Component {

  state: {login: LoginData};

  constructor(props:any) {
    super(props)
    this.state = {login: emptyLogin()}
  }

  componentWillMount() {
    if (Users.isLoggedIn()) {
      transitionTo("library", {id: Users.currentUserId()})
    }
  }

  onSubmit(e:any) {
    e.preventDefault()
    var login = this.state.login
    Users.login(login)
    .then((user) => {
      console.log("Logged in", user)
      if (user) {
        this.setState({login: emptyLogin()})
        // we don't need a message on login
        //Alerts.update("success", 'You have successfully logged in', true)

        if (this.props.query.to) {
          var params = this.props.query
          transitionTo(params.to, params)
        }
        else {
          transitionTo("library", {id: user.id})
        }
      }
    })
    .catch(function(err) {
      if (err.status == 401) {
        Alerts.update("error", 'Invalid email address or password')
      }
      else {
        console.error("ERROR", err)
        Alerts.oops()
      }
    })
  }

  render():React.Element {
    var login = this.state.login
    var update = makeUpdate(login, (v) => {
      this.setState({login: v})
    })

    return <LogoPage alert={this.props.alert}>
      <form onSubmit={this.onSubmit.bind(this)}>
        <label>Email</label>
        <input type="email"
          ref="email"
          name="email"
          value={login.email}
          onChange={update((s, v) => s.email = v)}
        />
        <label>Password</label>
        <input type="password"
          ref="password"
          name="password"
          value={login.password}
          onChange={update((s, v) => s.password = v)}
        />

        <button>Login</button>
      </form>

      <p>Don't have an account? <a href="/hello">Sign up for Early Access</a></p>
    </LogoPage>
  }
}

