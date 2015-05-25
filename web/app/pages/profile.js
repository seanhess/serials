
// @flow
import React from 'react'

import {FormSection} from '../comp'
import {Users} from '../model/user'
import {makeUpdate} from '../data/update'

export class Profile extends React.Component {

  constructor(props:any) {
    super(props)
    this.state = {
      user: null,
      showPasswordChange: false
    }
  }

  componentWillMount() {
    if (!Users.isLoggedIn()) {
      window.location.hash = "/"
    }

    this.state = {user: this.props.currentUser}
  }

  onSubmit() {
    Users.update(this.state.user)
    .then((user) => this.setState({user: user}))
  }

  toggleChangePassword() {
    this.setState({showPasswordChange: !this.state.showPasswordChange})
  }

  render():React.Element {
    var user = this.state.user
    var update = makeUpdate(user, (v) => {
      this.setState({user: v})
    })
    var passwordChangeStyle = this.state.showPasswordChange ? {} : {display: 'none'}

    return <div>
      <h3 style={{marginTop: 30}}>Profile</h3>
      <div className="row">
        <div className="small-12 medium-6 columns">
          <label>First Name</label>
          <input type="text"
            value={user.firstName}
            onChange={update((s, v) => s.firstName = v)}
          />
        </div>
        <div className="small-12 medium-6 columns">
          <label>Last Name</label>
          <input type="text"
            value={user.lastName}
            onChange={update((s, v) => s.lastName = v)}
          />
        </div>
      </div>
      <label>Email</label>
      <input type="text"
        value={user.email}
        onChange={update((s, v) => s.email = v)}
      />
      <label>
        Current Password
        <span style={{fontSize: '10px'}}> (required to make changes)</span>
      </label>
      <input type="password"
        value={user.currentPassword}
        onChange={update((s, v) => s.currentPassword = v)}
      />
      <div className="row" style={{marginBottom: '20px'}}>
        <div className="columns small-12 medium-6">
          <a onClick={this.toggleChangePassword.bind(this)}>Change Password</a>
        </div>
      </div>
      <div style={passwordChangeStyle}>
        <label>Password</label>
        <input type="password"
          value={user.password}
          onChange={update((s, v) => s.password = v)}
        />
        <label>Password Confirmation</label>
        <input type="password"
          value={user.passwordConfirmation}
          onChange={update((s, v) => s.passwordConfirmation = v)}
        />
      </div>
      <div className="row">
        <div className="columns small-12 medium-6">
          <button className="expand" onClick={this.onSubmit.bind(this)}>Submit</button>
        </div>
      </div>
    </div>
  }
}

