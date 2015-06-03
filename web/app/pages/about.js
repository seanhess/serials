
// @flow
import React from 'react'
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
