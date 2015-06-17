// @flow
import React from 'react'

import {FormSection} from '../comp'
import {Users} from '../model/user'
import {makeUpdate} from '../data/update'
import {LogoPage} from './login'
import {Signup, signup, invitesFind, Invite, emptyInvite} from '../model/invite'
import {EmailLink} from '../books/support'
import {Alerts} from '../model/alert'
import {transitionTo} from '../router'

var emptySignup = function(invite):Signup {
  return {
    firstName: '',
    lastName: '',
    email: invite.email,
    code: invite.code,
    password: '',
    passwordConfirmation: ''
  }
}

type SignupProps = {
  alert: any;
  loaded: boolean;
  invite: Invite;
}

export class SignupPage extends React.Component {

  props: SignupProps;

  static load(params) {
    return {invite: invitesFind(params.code)}
  }

  onSignup(s:Signup) {
    signup(s)
    .then(() => Users.refresh())
    .then(function() {
      Alerts.update("success", 'Your account is created. Welcome!', true)
      transitionTo("library", {id: Users.currentUserId()})
    })
  }

  render():React.Element {
    var content = ""
    if (!this.props.loaded) {
      content = ""
    }
    else if (!this.props.invite) {
      content = <InvalidCode message="We couldn't find your beta code!"/>
    }
    else if (this.props.invite.signup) {
      content = <InvalidCode message="This beta code has already been used"/>
    }
    else {
      content = <SignupForm invite={this.props.invite} onSignup={this.onSignup.bind(this)}/>
    }

    //var signup = this.state.signup
    return <LogoPage alert={this.props.alert}>
      {content}
    </LogoPage>
  }
}

export class SignupForm extends React.Component {

  constructor(props:any) {
    super(props)
    this.state = {signup: null}
  }

  componentWillMount(props:any) {
    this.setState({signup: emptySignup(this.props.invite)})
  }

  onSubmit() {
    this.props.onSignup(this.state.signup)
  }

  render():React.Element {
    var signup = this.state.signup

    var update = makeUpdate(signup, (v) => {
      this.setState({signup: v})
    })

    return <div>

      <p style={{marginTop: 30}}>Welcome to Web Fiction! Enter your information to sign up for an account</p>

      <label>Email</label>
      <p>{signup.email}</p>

      <div className="row">
        <div className="small-12 medium-6 columns">
          <label>First Name</label>
          <input type="text"
            value={signup.firstName}
            onChange={update((s, v) => s.firstName = v)}
          />
        </div>
        <div className="small-12 medium-6 columns">
          <label>Last Name</label>
          <input type="text"
            value={signup.lastName}
            onChange={update((s, v) => s.lastName = v)}
          />
        </div>
      </div>

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

      <div className="row">
        <div className="columns small-12 medium-6">
          <button className="expand" onClick={this.onSubmit.bind(this)}>Create My Account</button>
        </div>
      </div>

      <div style={{marginBottom: 50}}/>

    </div>
  }
}

export class InvalidCode extends React.Component {
  render():React.Element {
    return <p style={{margin: 50}}><span>{this.props.message}</span>. Please contact support at <EmailLink /></p>
  }
}
