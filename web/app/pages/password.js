// @flow
import React from 'react'
import {Link} from 'react-router'
import {LogoPage} from './login'
import {makeUpdate} from '../data/update'
import {Alerts} from '../model/alert'
import {User, forgotPassword, resetPassword} from '../model/user'
import {transitionTo, Routes} from '../router'

export class ForgotPassword extends React.Component {

  constructor(props:any) {
    super(props)
    this.state = {email: "", submitted: false}
  }

  onSubmit(e:any) {
    e.preventDefault()
    var email = this.state.email
    this.setState({email: "", submitted: true})
    forgotPassword(email)
    .then(function() {
      // done...
    })
    return
  }

  render():React.Element {
    var content;

    if (this.state.submitted) {
      content = this.renderCheck()
    }
    else {
      content = this.renderForm()
    }

    return <LogoPage alert={this.props.alert}>
      {content}
    </LogoPage>
  }

  renderForm():React.Element {
    return <form onSubmit={this.onSubmit.bind(this)}>
      <label>Email</label>
      <input type="email"
        ref="email"
        name="email"
        value={this.state.email}
        onChange={e => this.setState({email: e.target.value})}
      />

      <button>Reset Password</button>
    </form>
  }

  renderCheck():React.Element {
    return <p>We've sent you an email with a link to reset your password</p>
  }
}

export class ResetPassword extends React.Component {

  state: {
    password: string;
    confirm: string;
  };

  constructor(props:any) {
    super(props)
    this.state = {password: "", confirm: ""}
  }

  onSubmit(e:any) {
    Alerts.clear()
    e.preventDefault()
    var password = this.state.password
    var confirm = this.state.confirm

    if (password !== confirm) {
      Alerts.update("error", "Passwords did not match")
      return
    }

    resetPassword(this.props.params.token, this.state.password)
    .then(() => {
      Alerts.update("success", "Password updated successfully", true)
      transitionTo(Routes.login)
    })
    // submit the reset password event
    // let them know
  }

  render():React.Element {
    return <LogoPage alert={this.props.alert}>
      <form onSubmit={this.onSubmit.bind(this)}>

        <label>Password</label>
        <input type="password"
          value={this.state.password}
          onChange={e => this.setState({password: e.target.value})}
        />
        <label>Password Confirmation</label>
        <input type="password"
          value={this.state.confirm}
          onChange={e => this.setState({confirm: e.target.value})}
        />

        <div className="row">
          <div className="columns small-12 medium-6">
            <button className="expand" onClick={this.onSubmit.bind(this)}>Reset Password</button>
          </div>
        </div>
      </form>
    </LogoPage>
  }
}
