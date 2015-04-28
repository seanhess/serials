
// @flow
import React from 'react'
import {EmailLink} from '../books/support'


export class About extends React.Component {
  render() {
    return <div>
      <h2>About Serials</h2>
      <p>Serials aims to be a podcast-like experience for reading serial publications on the web.</p>

      <p>Many excellent books and webcomics are being published openly on the web today. Most of those are published one chapter at a time. It's awesome that authors are self-publishing, but this format can make it hard to keep track of your progress.</p>

      <p>Serials lets you subscribe to your favorite works, keep track of your progress, and receive notifications when new content is published</p>

      <hr/>

      <p>Have a suggestion? Want to be notified when we launch? Email us at <EmailLink /></p>
    </div>
  }
}
