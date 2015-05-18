// @flow
import React from 'react'
import _ from 'lodash'

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

