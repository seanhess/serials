// @flow
import React from 'react'

import {FormSection} from '../comp'
import {Users} from '../model/user'
import {makeUpdate} from '../data/update'

var emptyLogin = function() {
  return {
    email: '',
    password: ''
  }
}

export class Login extends React.Component {

  constructor(props) {
    super(props)
    this.state = {login: emptyLogin()}
  }

  onSubmit(e) {
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

  render() {
    var login = this.state.login
    var update = makeUpdate(login, (v) => {
      this.setState({login: v})
    })

    return <div style={{padding: 25}}>
      <FormSection title="Serials Login">
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

          <button>Submit</button>
        </form>
      </FormSection>
    </div>
  }
}

