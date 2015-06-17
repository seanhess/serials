
// @flow
import React from 'react'
import {Link} from 'react-router'
import {EmailLink} from '../books/support'

export class Logo extends React.Component {
  render():React.Element {
    return <div>
      <img src="img/serials-icon-dark.png" style={{height: 100}}/>
      <h1 style={{fontWeight: 'bold', fontSize: 32}}>Web Fiction</h1>
    </div>
  }
}

export class About extends React.Component {
  render():React.Element {
    return <div>

      <div style={{textAlign: 'center', margin: 50}}>
        <Logo />
      </div>

      <h4>For Readers</h4>

      <p>Web Fiction is a podcast-like experience for reading serial publications on the web.</p>

      <p>Many excellent books and webcomics are being published openly on the web today. Most of those are published one chapter at a time. It's awesome that authors are self-publishing, but this format can make it hard to keep track of your progress.</p>

      <p>Web Fiction lets you subscribe to your favorite works, keep track of your progress, and receive notifications when new content is published</p>


      <h4>For Authors</h4>

      <p>Our long-term goal is to encourage self-publishing by supporting authors. We hope to add a publishing platform, customizable book pages, tools to engage with your readers, and let you charge for subscriptions. Please let us know what direction we should be heading!</p>

      <hr/>

      <div>
        <p style={{textAlign: 'center'}}><span className="fa fa-envelope" style={{fontSize: 50}}></span></p>
        <p>Have a suggestion or find anything wrong? Email us at <EmailLink /></p>
      </div>

      <hr />

      <div>
        <p style={{textAlign: 'center'}}>
          <a href="http://github.com/seanhess/serials"><img src="./landing/Octocat.png" style={{width: 200}}/></a>
        </p>

        <p>Web Fiction is open source! <a href="http://github.com/seanhess/serials">View the source on Github</a></p>
      </div>

      <hr/>

      <p>By using webfiction, you agree to our <a href="https://github.com/seanhess/serials/blob/master/doc/user-agreement.md">User Agreement</a> and <a href="https://github.com/seanhess/serials/blob/master/doc/privacy-policy.md">Privacy Policy</a></p>

    </div>
  }
}
