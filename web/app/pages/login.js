// @flow
import React from 'react'
import {Link} from 'react-router'
import {RouteHandler} from 'react-router'

import {FormSection} from '../comp'
import {Users} from '../model/user'
import {Alerts} from '../model/alert'
import {AlertView} from '../alert'
import {makeUpdate} from '../data/update'

import {mobileInput} from '../style'
import {transitionTo} from '../router'

var emptyLogin = function() {
  return {
    email: '',
    password: ''
  }
}

export class LogoPage extends React.Component {
  render():React.Element {
    return <div>
      <div style={{padding: 25}} className="row small-12 columns">
        <div style={{textAlign: 'center', height: 200}}>
          <Link to="books"><img src="img/serials-logo-dark.png" style={{height: '100%'}}/></Link>
        </div>
        {this.props.children}
      </div>
      <AlertView alert={this.props.alert}/>
    </div>
  }
}

export class Login extends React.Component {

  constructor(props:any) {
    super(props)
    this.state = {login: emptyLogin()}
  }

  componentWillMount() {
    if (Users.isLoggedIn()) {
      window.location.hash = "/"
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

