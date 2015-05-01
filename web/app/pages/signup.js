// @flow
import React from 'react'

import {FormSection} from '../comp'
import {UserModel} from '../model'
import {makeUpdate} from '../data/update'

var emptySignup = function() {
  return {
    firstName: '',
    lastName: '',
    email: '',
    password: '',
    passwordConfirmation: ''
  }
}

export class Signup extends React.Component {

  constructor(props) {
    super(props)
    this.state = {signup: emptySignup()}
  }

  onSubmit() {
    var signup = this.state.signup
    UserModel.signup(signup)
    .then((user) => {
      if (user) {
        this.props.setCurrentUser(user)
        this.setState({signup: emptySignup()})
        window.location.hash = "/"
      }
    })
  }

  render() {
    var signup = this.state.signup
    var update = makeUpdate(signup, (v) => {
      this.setState({signup: v})
    })

    return <div>
      <FormSection title="Login">
        <div>
          <label>First Name</label>
          <input type="text"
            value={signup.firstName}
            onChange={update((s, v) => s.firstName = v)}
          />
          <label>Last Name</label>
          <input type="text"
            value={signup.lastName}
            onChange={update((s, v) => s.lastName = v)}
          />
          <label>Email</label>
          <input type="text"
            value={signup.email}
            onChange={update((s, v) => s.email = v)}
          />
          <label>Password</label>
          <input type="password"
            value={signup.password}
            onChange={update((s, v) => s.password = v)}
          />
          <label>Password Confirmation</label>
          <input type="password"
            value={signup.passwordConfirmation}
            onChange={update((s, v) => s.passwordConfirmation = v)}
          />

          <button className="" onClick={this.onSubmit.bind(this)}>Submit</button>
        </div>
      </FormSection>
    </div>
  }
}

