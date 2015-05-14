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

type State = {
  betaSignup?: {email: string};
  message?: ?string;
  error?: ?string;
}

export class BetaSignupView extends React.Component {

  _timer: ?number;

  constructor(props:any) {
    super(props)
    this.state = this.emptyState()
    this._timer = null
  }

  emptyState(obj:any = {}):State {
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

  clearMessage(message:State) {
    this._timer != null ? clearTimeout(this._timer) : null;
    this._timer = setTimeout(() => {this.setState(message)}, 5000)
  }

  renderMessage(message:string, color:string):React.Element {
    if (!message) return null
    return <div style={{color: color}}>{message}</div>
  }

  render():React.Element {
    var betaSignup = this.state.betaSignup
    var update = makeUpdate(betaSignup, (v) => {
      this.setState({betaSignup: v})
    })
    var message = this.renderMessage(this.state.message, '#008000')
    var error = this.renderMessage(this.state.error, '#ff0000')

    return <div className="row">
      {error}
      {message}
      <div className='small-12 medium-10 column'>
        <input type="text"
          placeholder='Give us your email for beta access'
          value={betaSignup.email}
          onChange={update((s, v) => s.email = v)}
        />
      </div>
      <div className='small-12 medium-2 column'>
        <button className='expand' onClick={this.onClick.bind(this)}>Submit</button>
      </div>
    </div>
  }
}

export class SuggestBook extends React.Component {
  render():React.Element {
    return <div>
      <p>Can't find a book? Email us at <EmailLink/> and we will add it.</p>
    </div>
  }
}

export class SomethingWrong extends React.Component {
  render():React.Element {
    return <div>
      <p>See something wrong? Email us at <EmailLink/> and we will fix it.</p>
    </div>
  }
}

export class EmailLink extends React.Component {
  render():React.Element {
    return <a href={EMAIL}>{EMAIL}</a>
  }
}

