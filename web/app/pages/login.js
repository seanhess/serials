// @flow
import React from 'react'

import {FormSection} from '../comp'
import {UserModel} from '../model'
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

  onSubmit() {
    var login = this.state.login
    UserModel.login(login)
    .then((user) => {
      if (user) {
        this.props.setCurrentUser(user)
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

    return <div>
      <FormSection title="Login">
        <div>
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

          <button className="" onClick={this.onSubmit.bind(this)}>Submit</button>
        </div>
      </FormSection>
    </div>
  }
}

