// @flow
import React from 'react'
import {Link} from 'react-router'
import {RouteHandler} from 'react-router'

import {FormSection} from '../comp'
import {Users} from '../model/user'
import {Alerts} from '../model/alert'
import {AlertView} from '../alert'
import {makeUpdate} from '../data/update'

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
        Alerts.update({message: 'You have successfully logged in', type: 'success'}, true)
        this.setState({login: emptyLogin()})
        window.location.hash = "/"
      }
    })
    .catch(() => {
        Alerts.update({message: 'Invalid email address or password', type: 'error'})
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
        <input type="text"
          value={login.email}
          onChange={update((s, v) => s.email = v)}
        />
        <label>Password</label>
        <input type="password"
          value={login.password}
          onChange={update((s, v) => s.password = v)}
        />

        <button>Login</button>
      </form>
    </LogoPage>
  }
}

