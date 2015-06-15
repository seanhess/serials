// @flow
import React from 'react'


import {makeUpdate} from '../data/update'
import {EMAIL} from '../model/settings'

// TODO: should put this somewhere more common
var validateEmail = function(email) {
    var re = /^([\w-]+(?:\.[\w-]+)*)@((?:[\w-]+\.)*\w[\w-]{0,66})\.([a-z]{2,6}(?:\.[a-z]{2})?)$/i;
    return re.test(email);
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

export class Suggestion extends React.Component {
  render():React.Element {
    return <div>
      <p>Have a suggestion? Email us at <EmailLink/> and we will work on it.</p>
    </div>
  }
}

export class EmailLink extends React.Component {
  render():React.Element {
    return <a href={EMAIL}>{EMAIL}</a>
  }
}

