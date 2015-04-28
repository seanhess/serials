// @flow
import React from 'react'

var EMAIL = "serials@orbit.al"

export class SuggestBook extends React.Component {
  render() {
    return <div>
      <p>Can't find a book? Email us at <EmailLink/> and we will add it.</p>
    </div>
  }
}

export class SomethingWrong extends React.Component {
  render() {
    return <div>
      <p>See something wrong? Email us at <EmailLink/> and we will fix it.</p>
    </div>
  }
}

export class EmailLink extends React.Component {
  render() {
    return <a href={EMAIL}>{EMAIL}</a>
  }
}

