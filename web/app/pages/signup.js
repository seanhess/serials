// @flow
import React from 'react'

import {FormSection} from '../comp'
import {Users} from '../model/user'
import {makeUpdate} from '../data/update'
import {LogoPage} from './login'
import {invitesFind} from '../model/admin'
import {EmailLink} from '../books/support'

var emptySignup = function(email?:string) {
  return {
    firstName: '',
    lastName: '',
    email: email,
    password: '',
    passwordConfirmation: ''
  }
}

export class Signup extends React.Component {

  static load(params) {
    return {invite: invitesFind(params.code)}
  }

  render():React.Element {
    console.log("INVITE", this.props.invite, this.props.loaded)


    var content = ""
    if (!this.props.loaded) {
      content = ""
    }
    else if (!this.props.invite) {
      content = <InvalidCode />
    }
    else {
      content = <SignupForm email={this.props.invite.email}/>
    }

    //var signup = this.state.signup
    return <LogoPage>
      {content}
    </LogoPage>
  }
}

export class SignupForm extends React.Component {

  constructor(props:any) {
    super(props)
    this.state = {signup: emptySignup()}
  }

  componentWillMount(props:any) {
    this.setState({signup: emptySignup(this.props.email)})
  }

  onSubmit() {
    //var signup = this.state.signup
    //Users.signup(signup)
    //.then((user) => {
      //if (user) {
        //this.props.setCurrentUser(user)
        //this.setState({signup: emptySignup()})
        //window.location.hash = "/"
      //}
    //})
  }

  render():React.Element {
    var signup = this.state.signup

    var update = makeUpdate(signup, (v) => {
      this.setState({signup: v})
    })

    return <div>

      <p style={{marginTop: 30}}>Welcome to serials! Enter your information to sign up for an account</p>

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

      <div className="row">
        <div className="columns small-12 medium-6">
          <button className="expand" onClick={this.onSubmit.bind(this)}>Create My Account</button>
        </div>
      </div>
    </div>
  }
}

export class InvalidCode extends React.Component {
  render():React.Element {
    return <p style={{margin: 50}}>We couldn't find your beta code! Please contact support at <EmailLink /></p>
  }
}
