
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

      <p>Serials is a podcast-like experience for reading serial publications on the web.</p>

      <p>Many excellent books and webcomics are being published openly on the web today. Most of those are published one chapter at a time. It's awesome that authors are self-publishing, but this format can make it hard to keep track of your progress.</p>

      <p>Serials lets you subscribe to your favorite works, keep track of your progress, and receive notifications when new content is published</p>

      <p><Link to="terms">Privacy and Terms</Link></p>

      <hr/>

      <div>
        <p style={{textAlign: 'center'}}>
          <a href="http://github.com/seanhess/serials"><img src="./landing/Octocat.png" style={{width: 200}}/></a>
        </p>

        <p>Serials is open source! <a href="http://github.com/seanhess/serials">View the source on Github</a></p>
      </div>

      <hr />

      <div>
        <p style={{textAlign: 'center'}}><span className="fa fa-envelope" style={{fontSize: 50}}></span></p>
        <p>Have a suggestion or find anything wrong? Email us at <EmailLink /></p>
      </div>


    </div>
  }
}

export class Terms extends React.Component {
  render():ReactElement {
    return <div className="row small-12 columns">
      <h3>Our Intent</h3>
      <p>Date: June 4 2015</p>
      <p>We want to enable authors and content creators to do their work. Any infringement is unintentional. There's more formal language below, but please contact us at <EmailLink /> to discuss any time.</p>

      <hr />

      <h3>Privacy Policy</h3>
      <p>Coming soon</p>

      <h3>Content Takedown Requests Due to Copyright Infringement</h3>
      <p>If you believe that content on the Website has infringed upon your copyright rights, you may provide Orbital Labs with a notice of copyright infringement that complies with §512 of the Digital Millennium Copyright Act. This notice of copyright infringement must contain the following:</p>

      <ol>
        <li>The physical or electronic signature of a person authorized to act on behalf of the copyright owner;</li>
        <li>Identification of the copyrighted work(s) alleged to have been infringed;</li>
        <li>The location of the copyrighted work(s) on the Website;</li>
        <li>Your contact information, such as an address, telephone, fax number, or email address;</li>
        <li>A statement that you have a good-faith belief that the use of the allegedly infringing content is not authorized by the copyright owner, its agent, or the law; and</li>
        <li>A statement, under penalty of perjury, that the information in the notification is accurate and that you are authorized to act on behalf of the copyright owner.</li>
        <li>Notifications of copyright infringement and counter-notifications may be submitted to Orbital Labs at <EmailLink /></li>
      </ol>
    </div>
  }
}
