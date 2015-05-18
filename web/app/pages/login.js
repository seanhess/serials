// @flow
import React from 'react'
import {RouteHandler} from 'react-router'

import {FormSection} from '../comp'
import {Users} from '../model/user'
import {makeUpdate} from '../data/update'

var emptyLogin = function() {
  return {
    email: '',
    password: ''
  }
}

export class LogoPage extends React.Component {
  render():React.Element {
    return <div style={{padding: 25}} className="row small-12 columns">
      <div style={{textAlign: 'center', height: 200}}>
        <img src="img/serials-logo-dark.png" style={{height: '100%'}}/>
      </div>
      {this.props.children}
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
      if (user) {
        this.setState({login: emptyLogin()})
        window.location.hash = "/"
      }
    })
  }

  render():React.Element {
    var login = this.state.login
    var update = makeUpdate(login, (v) => {
      this.setState({login: v})
    })

    return <LogoPage>
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

