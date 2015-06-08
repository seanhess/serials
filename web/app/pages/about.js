
// @flow
import React from 'react'
import {Link} from 'react-router'
import {EmailLink} from '../books/support'


export class About extends React.Component {
  render():React.Element {
    return <div>

      <div style={{textAlign: 'center', margin: 50}}>
        <img src="./landing/serials-logo.png" alt="logo" style={{width: 100}}/>
      </div>

      <h4>For Readers</h4>

      <p>Serials is a podcast-like experience for reading serial publications on the web.</p>

      <p>Many excellent books and webcomics are being published openly on the web today. Most of those are published one chapter at a time. It's awesome that authors are self-publishing, but this format can make it hard to keep track of your progress.</p>

      <p>Serials lets you subscribe to your favorite works, keep track of your progress, and receive notifications when new content is published</p>


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

        <p>Serials is open source! <a href="http://github.com/seanhess/serials">View the source on Github</a></p>
      </div>


      <hr/>

      <Terms />

    </div>
  }
}

export class Terms extends React.Component {
  render():ReactElement {
    return <div className="row small-12 columns">

      <h3>Privacy Policy</h3>
      <p>Date: June 4 2015</p>

      <p>We collect your email and may use it to contact you about new features and books that we've added. You will be able to unsubscribe from these emails at any time. We will never give your personal information to anyone else.</p>

      <h3>Copyright Infringement</h3>

      <p>We want to help authors and creators, not compete with them or steal their work. Any infringement is unintentional. If you see anything wrong, just send us a quick email to <EmailLink/> and we will try to fix it</p>

      </div>
  }
}
