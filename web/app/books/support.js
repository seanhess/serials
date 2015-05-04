// @flow
import React from 'react'
import _ from 'lodash'

var {BetaSignupModel} = require('../model/betaSignup')
var EMAIL = "serials@orbit.al"

import {makeUpdate} from '../data/update'

// TODO: should put this somewhere more common
var validateEmail = function(email) {
    var re = /^([\w-]+(?:\.[\w-]+)*)@((?:[\w-]+\.)*\w[\w-]{0,66})\.([a-z]{2,6}(?:\.[a-z]{2})?)$/i;
    return re.test(email);
}

export class BetaSignupView extends React.Component {

  constructor(props) {
    super(props)
    this.state = this.emptyState()
    this._timer = null
  }

  emptyState(obj = {}) {
    return _.assign({
      betaSignup: {email: ''},
      message: null,
      error: null
    }, obj)
  }

  onClick() {
    var betaSignup = this.state.betaSignup;
    if (validateEmail(betaSignup.email)) {
      BetaSignupModel.create(betaSignup)
      .then(() => {
        this.setState(this.emptyState({message: 'Thank you we will be emailing you'}))
        this.clearMessage({message: null})
      })
    }

    else {
      this.setState({error: 'Invalid Email Address'});
      this.clearMessage({error: null})
    }
  }

  clearMessage(message) {
    this._timer != null ? clearTimeout(this._timer) : null;
    this._timer = setTimeout(() => {this.setState(message)}, 5000)
  }

  renderMessage(message, color) {
    if (!message) return null
    return <div style={{color: color}}>{message}</div>
  }

  render() {
    var betaSignup = this.state.betaSignup
    var update = makeUpdate(betaSignup, (v) => {
      this.setState({betaSignup: v})
    })
    var message = this.renderMessage(this.state.message, '#008000')
    var error = this.renderMessage(this.state.error, '#ff0000')

    return <div>
      {error}
      {message}
      <div className='small-10 left'>
        <input type="text"
          placeholder='Give us your email for beta access'
          value={betaSignup.email}
          onChange={update((s, v) => s.email = v)}
        />
      </div>
      <div className='small-1 small-offset-1 right'>
        <button className='tiny' onClick={this.onClick.bind(this)}>Submit</button>
      </div>
    </div>
  }
}

export class SuggestBook extends React.Component {
  render() {
    return <div>
      <p>Can't find a book? Email us at <EmailLink/> and we will add it.</p>
      <BetaSignupView />
    </div>
  }
}

export class SomethingWrong extends React.Component {
  render() {
    return <div>
      <p>See something wrong? Email us at <EmailLink/> and we will fix it.</p>
      <BetaSignupView />
    </div>
  }
}

export class EmailLink extends React.Component {
  render() {
    return <a href={EMAIL}>{EMAIL}</a>
  }
}

